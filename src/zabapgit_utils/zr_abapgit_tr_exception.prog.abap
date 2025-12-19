*&---------------------------------------------------------------------*
*& Report ZR_ABAPGIT_TR_EXCEPTION
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zr_abapgit_tr_exception.

*---------------------------------------------------------------------*
* Selection Screen
*---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS:
    p_treq   TYPE e070-strkorr OBLIGATORY,
    p_reason TYPE string LOWER CASE OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

*---------------------------------------------------------------------*
* Start of Selection
*---------------------------------------------------------------------*
START-OF-SELECTION.

  TRY.
      zcl_abapgit_pr_status_manager=>create_pr_link(
        iv_parent_request    = p_treq
        iv_pr_id             = 0
        iv_pr_status         = 'EXCEPTION'
        iv_exception_reason  = p_reason
      ).

      WRITE: / 'Exception created successfully.'.
      WRITE: / 'Transport Request:', p_treq.
      WRITE: / 'Reason:', p_reason.

    CATCH zcx_abapgit_exception INTO DATA(lx_error).
      MESSAGE lx_error->get_text( ) TYPE 'E'.
  ENDTRY.
