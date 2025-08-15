*&---------------------------------------------------------------------*
*& Report ZR_ABAPGIT_PR_STATUS_DEMO
*&---------------------------------------------------------------------*
*& Demo program for Pull Request Status Management
*&---------------------------------------------------------------------*
REPORT zr_abapgit_pr_status_demo.

PARAMETERS: p_treq  TYPE strkorr OBLIGATORY,
            p_prid  TYPE int8,
            p_url   TYPE string LOWER CASE,
            p_action TYPE char10 DEFAULT 'DISPLAY'.

SELECTION-SCREEN BEGIN OF BLOCK actions WITH FRAME TITLE TEXT-001.
PARAMETERS: r_disp  RADIOBUTTON GROUP act DEFAULT 'X',
            r_creat RADIOBUTTON GROUP act,
            r_updat RADIOBUTTON GROUP act,
            r_sync  RADIOBUTTON GROUP act,
            r_delet RADIOBUTTON GROUP act.
SELECTION-SCREEN END OF BLOCK actions.

START-OF-SELECTION.

  DATA: lt_links TYPE zcl_abapgit_pr_status_manager=>ty_pr_links,
        lx_error TYPE REF TO zcx_abapgit_exception.

  TRY.
      CASE abap_true.
        WHEN r_disp.
          " Display PR status
          lt_links = zcl_abapgit_pr_status_manager=>get_pr_status( p_treq ).
          IF lines( lt_links ) = 0.
            WRITE: / 'No PR links found for transport request', p_treq.
          ELSE.
            WRITE: / 'PR Status for Transport Request:', p_treq.
            WRITE: / '=' CO 60.
            LOOP AT lt_links INTO DATA(ls_link).
              WRITE: / 'PR ID:', ls_link-pr_id,
                     / 'PR Status:', ls_link-pr_status,
                     / 'Transport Status:', ls_link-request_status,
                     / 'Created By:', ls_link-created_by, 'on', ls_link-created_on,
                     / 'Changed By:', ls_link-changed_by, 'on', ls_link-changed_on,
                     / '-' CO 40.
            ENDLOOP.
          ENDIF.

        WHEN r_creat.
          " Create new PR link
          IF p_prid IS INITIAL.
            WRITE: / 'PR ID is required for creation'.
          ELSE.
            zcl_abapgit_pr_status_manager=>create_pr_link(
              iv_parent_request = p_treq
              iv_pr_id = p_prid ).
            WRITE: / 'PR link created successfully for TR:', p_treq, 'PR:', p_prid.
          ENDIF.

        WHEN r_updat.
          " Update PR status
          IF p_prid IS INITIAL.
            WRITE: / 'PR ID is required for update'.
          ELSE.
            zcl_abapgit_pr_status_manager=>update_pr_status(
              iv_parent_request = p_treq
              iv_pr_id = p_prid
              iv_pr_status = 'APPROVED' ).
            WRITE: / 'PR status updated to APPROVED for TR:', p_treq, 'PR:', p_prid.
          ENDIF.

        WHEN r_sync.
          " Sync with GitHub
          IF p_url IS INITIAL.
            WRITE: / 'Repository URL is required for sync'.
          ELSE.
            zcl_abapgit_pr_status_manager=>sync_with_github(
              iv_parent_request = p_treq
              iv_repo_url = p_url ).
            WRITE: / 'Sync completed for TR:', p_treq.
          ENDIF.

        WHEN r_delet.
          " Delete PR link
          IF p_prid IS INITIAL.
            WRITE: / 'PR ID is required for deletion'.
          ELSE.
            zcl_abapgit_pr_status_manager=>delete_pr_link(
              iv_parent_request = p_treq
              iv_pr_id = p_prid ).
            WRITE: / 'PR link deleted for TR:', p_treq, 'PR:', p_prid.
          ENDIF.

      ENDCASE.

    CATCH zcx_abapgit_exception INTO lx_error.
      WRITE: / 'Error:', lx_error->get_text( ).
  ENDTRY.
