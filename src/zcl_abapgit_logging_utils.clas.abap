CLASS zcl_abapgit_logging_utils DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.
 
  PUBLIC SECTION.
    CLASS-METHODS:
      write_application_log
        IMPORTING
          iv_log_handle  TYPE balloghndl
          iv_log_type    TYPE c DEFAULT 'I'
          iv_message     TYPE string
          iv_detail      TYPE string OPTIONAL
        RAISING
          zcx_abapgit_exception,
      
      create_application_log
        IMPORTING
          iv_object       TYPE balobj_d DEFAULT 'ZABAPGIT'
          iv_subobject    TYPE balsubobj DEFAULT 'PR_CHECK'
          iv_extnumber    TYPE balnrext OPTIONAL
        RETURNING
          VALUE(rv_log_handle) TYPE balloghndl
        RAISING
          zcx_abapgit_exception.

ENDCLASS.

CLASS zcl_abapgit_logging_utils IMPLEMENTATION.

  METHOD write_application_log.
    DATA: ls_msg TYPE bal_s_msg.

    " Ensure log handle is provided
    IF iv_log_handle IS INITIAL.
      RETURN. " Skip logging if no log handle available
    ENDIF.

    " Prepare message
    ls_msg-msgty = iv_log_type.
    ls_msg-msgid = 'ZMC_LOG'.
    ls_msg-msgno = '000'.
    ls_msg-msgv1 = iv_message.
    ls_msg-msgv2 = iv_detail.

    " Write message to log
    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle = iv_log_handle
        i_s_msg      = ls_msg
      EXCEPTIONS
        OTHERS       = 1.

    " Save to database immediately for audit trail (especially critical for errors)
    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_client         = sy-mandt
        i_save_all       = abap_true
        i_t_log_handle   = VALUE bal_t_logh( ( iv_log_handle ) )
      EXCEPTIONS
        OTHERS           = 1.

    " For error messages, commit the database transaction to ensure persistence
    IF iv_log_type = 'E'.
      COMMIT WORK.
    ENDIF.

  ENDMETHOD.

  METHOD create_application_log.
    DATA: ls_log_header TYPE bal_s_log.

    " Create log header
    ls_log_header-extnumber = iv_extnumber.
    ls_log_header-object    = iv_object.
    ls_log_header-subobject = iv_subobject.
    ls_log_header-aldate    = sy-datum.
    ls_log_header-altime    = sy-uzeit.
    ls_log_header-aluser    = sy-uname.

    " Create application log
    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log      = ls_log_header
      IMPORTING
        e_log_handle = rv_log_handle
      EXCEPTIONS
        OTHERS       = 1.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Failed to create application log' ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
