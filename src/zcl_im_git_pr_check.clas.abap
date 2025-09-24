CLASS zcl_im_git_pr_check DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_ex_cts_request_check.

  PROTECTED SECTION.
    DATA mv_log_handle TYPE balloghndl.

    CONSTANTS:
      BEGIN OF c_request_type,
        workbench    TYPE trfunction VALUE 'K',
        tr_of_copies TYPE trfunction VALUE 'T',
      END OF c_request_type.

    CONSTANTS:
      BEGIN OF c_tr_status,
        development TYPE trstatus VALUE 'D',
        modifiable  TYPE trstatus VALUE 'O',
        released    TYPE trstatus VALUE 'R',
      END OF c_tr_status.

    METHODS is_parent_request
      IMPORTING iv_request        TYPE trkorr
      RETURNING VALUE(rv_is_main) TYPE abap_bool
      RAISING   zcx_abapgit_exception.

    METHODS get_repo_url
      IMPORTING iv_request         TYPE trkorr
      RETURNING VALUE(rv_repo_url) TYPE string
      RAISING   zcx_abapgit_exception.

    METHODS check_pr_requirements
      IMPORTING iv_request  TYPE trkorr
                iv_repo_url TYPE string
      RAISING   zcx_abapgit_exception.

    METHODS update_transport_to_released
      IMPORTING iv_request TYPE trkorr
      RAISING   zcx_abapgit_exception.

private section.
ENDCLASS.



CLASS ZCL_IM_GIT_PR_CHECK IMPLEMENTATION.


  METHOD if_ex_cts_request_check~check_before_release.
    DATA lv_repo_url TYPE string.
    DATA lv_is_main  TYPE abap_bool.

    TRY.
        " Skip for Transport of Copies
        IF type = c_request_type-tr_of_copies.
          RETURN.
        ENDIF.

        " 1. Check if its a Parent TR
        lv_is_main = is_parent_request( iv_request = request ).
        IF lv_is_main = abap_false.
          " Skip checks for non-main workbench requests (customizing, transport of copies, etc.)
          RETURN.
        ENDIF.

        " Initialize logging for main transport requests only
        mv_log_handle = zcl_abapgit_logging_utils=>create_application_log( iv_extnumber = CONV #( request )
                                                                           iv_object    = 'ZABAPGIT'
                                                                           iv_subobject = 'TR_CHECK' ).

        zcl_abapgit_logging_utils=>write_application_log( iv_log_handle = mv_log_handle
                                                          iv_log_type   = 'I'
                                                          iv_message    = 'PR Check BADI started for main transport'
                                                          iv_detail     = |Transport: { request }, Type: { type }| ).

        " Check for Exception
        SELECT SINGLE * FROM zdt_pull_request
          INTO @DATA(ls_pull_request)
         WHERE parent_request EQ @request.
        IF sy-subrc IS INITIAL.
          IF ls_pull_request-pr_status EQ 'EXCEPTION'.
            zcl_abapgit_logging_utils=>write_application_log( iv_log_handle = mv_log_handle
                                                              iv_log_type   = 'I'
                                                              iv_message    = 'TR was provided an exception to release'
                                                              iv_detail     = |Transport: { request }, Type: { type }| ).
            RETURN.
          ENDIF.
        ENDIF.

        lv_repo_url = get_repo_url( request ).
        IF lv_repo_url IS INITIAL.
          zcl_abapgit_logging_utils=>write_application_log(
              iv_log_handle = mv_log_handle
              iv_log_type   = 'E'
              iv_message    = 'Repository URL not configured'
              iv_detail     = 'ZGIT_REPO_URL entry missing in TVARVC table' ).

          MESSAGE 'Please maintain Github Repo URL in Config' TYPE 'E'.
        ENDIF.

        check_pr_requirements( iv_request  = request
                               iv_repo_url = lv_repo_url ).

        zcl_abapgit_logging_utils=>write_application_log( iv_log_handle = mv_log_handle
                                                          iv_log_type   = 'S'
                                                          iv_message    = 'PR requirements check completed successfully'
                                                          iv_detail     = 'All PR validations passed' ).

        " 4. Update transport status to released in our tracking table
        zcl_abapgit_logging_utils=>write_application_log( iv_log_handle = mv_log_handle
                                                          iv_log_type   = 'I'
                                                          iv_message    = 'Updating transport status to Released'
                                                          iv_detail     = 'Updating ZDT_PULL_REQUEST table' ).

        update_transport_to_released( request ).

        zcl_abapgit_logging_utils=>write_application_log(
            iv_log_handle = mv_log_handle
            iv_log_type   = 'S'
            iv_message    = 'PR Check BADI completed successfully'
            iv_detail     = |Transport { request } validation completed| ).

      CATCH cx_root INTO DATA(lx_root).
        " Log the error if logging is available
        IF mv_log_handle IS NOT INITIAL.
          zcl_abapgit_logging_utils=>write_application_log( iv_log_handle = mv_log_handle
                                                            iv_log_type   = 'E'
                                                            iv_message    = 'Unexpected error during PR check'
                                                            iv_detail     = |Error: { lx_root->get_text( ) }| ).
        ENDIF.
        " Handle any unexpected errors
        MESSAGE |Unexpected error during PR check: { lx_root->get_text( ) }| TYPE 'E'.
    ENDTRY.
  ENDMETHOD.


  METHOD get_repo_url.
    " TODO: parameter IV_REQUEST is never used (ABAP cleaner)

    " Get repository URL from TVARVC table
    SELECT SINGLE low FROM tvarvc
      INTO @rv_repo_url
      WHERE name = 'ZGIT_REPO_URL'.
    IF sy-subrc <> 0 OR rv_repo_url IS INITIAL.
      " No repository URL configured - skip PR checks
      RETURN.
    ENDIF.
  ENDMETHOD.


  METHOD check_pr_requirements.
    DATA lt_pr_links  TYPE zcl_abapgit_pr_status_manager=>tt_pr_links.
    DATA ls_pr_link   TYPE zdt_pull_request.
    DATA lv_error_msg TYPE string.

    " Sync with GitHub and check PR status for each linked PR
    zcl_abapgit_pr_status_manager=>sync_with_github( iv_parent_request = iv_request
                                                     iv_repo_url       = iv_repo_url ).

    zcl_abapgit_logging_utils=>write_application_log( iv_log_handle = mv_log_handle
                                                      iv_log_type   = 'S'
                                                      iv_message    = 'GitHub sync completed'
                                                      iv_detail     = 'PR status information updated from GitHub' ).

    lt_pr_links = zcl_abapgit_pr_status_manager=>get_pr_tr_linkage( iv_request ).

    zcl_abapgit_logging_utils=>write_application_log(
        iv_log_handle = mv_log_handle
        iv_log_type   = 'I'
        iv_message    = 'Refreshed PR links.' ).

    " Check the PR Status
    READ TABLE lt_pr_links INTO ls_pr_link
         WITH KEY parent_request = iv_request.
    IF sy-subrc = 0.
      zcl_abapgit_logging_utils=>write_application_log(
          iv_log_handle = mv_log_handle
          iv_log_type   = 'I'
          iv_message    = 'Checking PR status for release approval'
          iv_detail     = |PR ID: { ls_pr_link-pr_id }, Status: { ls_pr_link-pr_status }| ).

      IF    ls_pr_link-pr_status = zcl_abapgit_pr_status_manager=>c_pr_status-open
         OR ls_pr_link-pr_status = zcl_abapgit_pr_status_manager=>c_pr_status-draft
         OR ls_pr_link-pr_status = zcl_abapgit_pr_status_manager=>c_pr_status-changes.

        lv_error_msg =
          |Pull Request #{ ls_pr_link-pr_id } | &&
          |is still OPEN. Please ensure PR is merged before release.|.

        zcl_abapgit_logging_utils=>write_application_log(
            iv_log_handle = mv_log_handle
            iv_log_type   = 'E'
            iv_message    = |Pull Request #{ ls_pr_link-pr_id } is not closed/merged.|
            iv_detail     = |Current PR Status: { ls_pr_link-pr_status }| ).

        MESSAGE lv_error_msg TYPE 'E'.
      ELSE.
        zcl_abapgit_logging_utils=>write_application_log(
            iv_log_handle = mv_log_handle
            iv_log_type   = 'S'
            iv_message    = 'PR status validation passed'
            iv_detail     = |PR #{ ls_pr_link-pr_id } is merged or closed| ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD update_transport_to_released.
    zcl_abapgit_logging_utils=>write_application_log(
        iv_log_handle = mv_log_handle
        iv_log_type   = 'I'
        iv_message    = 'Updating PR tracking table status'
        iv_detail     = |Setting transport { iv_request } status to Released| ).

    " Update all PR links for this transport to reflect released status
    UPDATE zdt_pull_request
      SET request_status = @c_tr_status-released,
          changed_by     = @sy-uname,
          changed_on     = @sy-datum,
          changed_at     = @sy-uzeit
      WHERE parent_request = @iv_request.

    IF sy-subrc = 0.
      zcl_abapgit_logging_utils=>write_application_log(
          iv_log_handle = mv_log_handle
          iv_log_type   = 'S'
          iv_message    = 'Transport status updated successfully'
          iv_detail     = |{ sy-dbcnt } PR record(s) updated in ZDT_PULL_REQUEST| ).

      COMMIT WORK.
      MESSAGE |Transport status updated to Released in PR tracking table| TYPE 'S'.
    ELSE.
      zcl_abapgit_logging_utils=>write_application_log(
          iv_log_handle = mv_log_handle
          iv_log_type   = 'W'
          iv_message    = 'No PR records found to update'
          iv_detail     = |Transport { iv_request } has no entries in ZDT_PULL_REQUEST| ).

      " Don't fail the release for this - just log a warning
      MESSAGE |Warning: Could not update transport status in PR tracking table| TYPE 'W'.
    ENDIF.
  ENDMETHOD.


  METHOD if_ex_cts_request_check~check_before_add_objects.
  ENDMETHOD.


  METHOD if_ex_cts_request_check~check_before_changing_owner.
  ENDMETHOD.


  METHOD if_ex_cts_request_check~check_before_creation.
  ENDMETHOD.


  METHOD if_ex_cts_request_check~check_before_release_slin.
  ENDMETHOD.


  METHOD is_parent_request.
    DATA ls_request TYPE e070.

    " Get transport request details
    SELECT SINGLE trfunction, korrdev, strkorr
      FROM e070
      INTO CORRESPONDING FIELDS OF @ls_request
      WHERE trkorr = @iv_request.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Transport request { iv_request } not found| ).
    ENDIF.

    " Check if it's a main request (not a task)
    " Main requests have strkorr as blank
    IF ls_request-strkorr IS INITIAL.
      rv_is_main = abap_true.
    ELSE.
      rv_is_main = abap_false.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
