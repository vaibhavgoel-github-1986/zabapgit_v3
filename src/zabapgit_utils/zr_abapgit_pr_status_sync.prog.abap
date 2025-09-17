*&---------------------------------------------------------------------*
*& Sync program for Pull Request Status Management
*&---------------------------------------------------------------------*
REPORT zr_abapgit_pr_status_sync.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_treq TYPE e070-strkorr OBLIGATORY,
              p_prid TYPE int8,
              p_url  TYPE string LOWER CASE DEFAULT 'https://github.com/cisco-it-finance/sap-brim-github-repo.git'.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK actions WITH FRAME TITLE TEXT-002.
  PARAMETERS: r_disp  RADIOBUTTON GROUP act DEFAULT 'X',
              r_creat RADIOBUTTON GROUP act,
              r_updat RADIOBUTTON GROUP act,
              r_sync  RADIOBUTTON GROUP act,
              r_excep RADIOBUTTON GROUP act,
              r_delet RADIOBUTTON GROUP act,
              r_test  RADIOBUTTON GROUP act.
SELECTION-SCREEN END OF BLOCK actions.

START-OF-SELECTION.

  DATA: lt_links TYPE zcl_abapgit_pr_status_manager=>tt_pr_links,
        lx_error TYPE REF TO zcx_abapgit_exception.

  TRY.
      CASE abap_true.
        WHEN r_disp.
          " Display PR status
          lt_links = zcl_abapgit_pr_status_manager=>get_pr_tr_linkage( p_treq ).
          IF lines( lt_links ) = 0.
            WRITE: / 'No PR links found for transport request', p_treq.
          ELSE.
            WRITE: / 'PR Status for Transport Request:', p_treq.
            LOOP AT lt_links INTO DATA(ls_link).
              WRITE: / 'PR ID:', ls_link-pr_id,
                     / 'PR Status:', ls_link-pr_status,
                     / 'Transport Status:', ls_link-request_status,
                     / 'Created By:', ls_link-created_by, 'on', ls_link-created_on,
                     / 'Changed By:', ls_link-changed_by, 'on', ls_link-changed_on.
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

        WHEN r_excep.

          zcl_abapgit_pr_status_manager=>create_pr_link(
            iv_parent_request = p_treq
            iv_pr_id          = 0
            iv_pr_status      = 'EXCEPTION'
          ).
          WRITE: / 'TR was provided an exception successfully'.

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
            " Call the improved sync method with built-in error handling
            TRY.
                zcl_abapgit_pr_status_manager=>sync_with_github(
                  iv_parent_request = p_treq
                  iv_repo_url       = p_url ).
                WRITE: / 'Sync operation completed successfully.'.
                WRITE: / 'Check messages above for detailed results.'.
              CATCH zcx_abapgit_exception INTO DATA(lx_sync_error).
                WRITE: / 'Sync failed:', lx_sync_error->get_text( ).
                WRITE: / 'Please check:'.
                WRITE: / '- ZGIT_API_KEY is maintained in TVARVC (SM30)'.
                WRITE: / '- Repository URL is correct'.
                WRITE: / '- Network connectivity to GitHub'.
            ENDTRY.
          ENDIF.

        WHEN r_test.
          " Test individual PR status fetch
          IF p_prid IS INITIAL OR p_url IS INITIAL.
            WRITE: / 'Both PR ID and Repository URL are required for testing'.
          ELSE.
            TRY.
                DATA(lv_pr_status) = zcl_abapgit_pr_status_manager=>get_github_pr_status(
                  iv_repo_url = p_url
                  iv_pr_id    = p_prid ).
                WRITE: / 'PR Status Test Results:'.
                WRITE: / 'Repository URL:', p_url.
                WRITE: / 'PR ID:', p_prid.
                WRITE: / 'Current Status:', lv_pr_status.
              CATCH zcx_abapgit_exception INTO DATA(lx_test_error).
                WRITE: / 'Test failed:', lx_test_error->get_text( ).
            ENDTRY.
          ENDIF.

        WHEN r_delet.
          " Delete PR linkage
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
