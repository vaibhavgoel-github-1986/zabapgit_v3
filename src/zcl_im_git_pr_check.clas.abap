CLASS zcl_im_git_pr_check DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_ex_cts_request_check.

  PROTECTED SECTION.

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
      IMPORTING
        !iv_request       TYPE trkorr
      RETURNING
        VALUE(rv_is_main) TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
    METHODS get_repo_url
      IMPORTING
        !iv_request        TYPE trkorr
      RETURNING
        VALUE(rv_repo_url) TYPE string
      RAISING
        zcx_abapgit_exception .
    METHODS check_pr_requirements
      IMPORTING
        !iv_request  TYPE trkorr
        !iv_repo_url TYPE string
      RAISING
        zcx_abapgit_exception .
    METHODS update_transport_to_released
      IMPORTING
        !iv_request TYPE trkorr
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS ZCL_IM_GIT_PR_CHECK IMPLEMENTATION.


  METHOD if_ex_cts_request_check~check_before_release.

    DATA: lv_repo_url TYPE string.

    TRY.
        " Skip for Transport of Copies
        IF type = c_request_type-tr_of_copies.
          RETURN.
        ENDIF.

        " 1. Check if its a Parent TR
        IF is_parent_request( iv_request = request ) = abap_false.
          " Skip checks for non-main workbench requests (customizing, transport of copies, etc.)
          RETURN.
        ENDIF.

        " 2. Get repository URL for this transport request
        lv_repo_url = get_repo_url( request ).
        IF lv_repo_url IS INITIAL.
          MESSAGE 'Please maintain Github Repo URL in Config' TYPE 'E'.
        ENDIF.

        " 3. Check PR requirements (existence, sync status, approval)
        check_pr_requirements(
          iv_request  = request
          iv_repo_url = lv_repo_url ).

        " 4. Update transport status to released in our tracking table
        update_transport_to_released( request ).

      CATCH cx_root INTO DATA(lx_root).
        " Handle any unexpected errors
        MESSAGE |Unexpected error during PR check: { lx_root->get_text( ) }| TYPE 'E'.
    ENDTRY.

  ENDMETHOD.


  METHOD get_repo_url.

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

    DATA: lt_pr_links TYPE zcl_abapgit_pr_status_manager=>tt_pr_links,
          ls_pr_link  TYPE zdt_pull_request.

    " Sync with GitHub and check PR status for each linked PR
    zcl_abapgit_pr_status_manager=>sync_with_github(
      iv_parent_request = iv_request
      iv_repo_url       = iv_repo_url ).

    " Re-read the PR links after sync to get updated status
    lt_pr_links = zcl_abapgit_pr_status_manager=>get_pr_tr_linkage( iv_request ).

    " Check the PR Status
    READ TABLE lt_pr_links INTO ls_pr_link
      WITH KEY parent_request = iv_request.
    IF sy-subrc = 0.
      IF ls_pr_link-pr_status = zcl_abapgit_pr_status_manager=>c_pr_status-open OR
        ls_pr_link-pr_status = zcl_abapgit_pr_status_manager=>c_pr_status-draft OR
        ls_pr_link-pr_status = zcl_abapgit_pr_status_manager=>c_pr_status-changes.
        DATA(lv_error_msg) =
          |Pull Request #{ ls_pr_link-pr_id } | &&
          |is still OPEN. Please ensure PR is merged before release.|.
        MESSAGE lv_error_msg TYPE 'E'.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD update_transport_to_released.

    " Update all PR links for this transport to reflect released status
    UPDATE zdt_pull_request
      SET request_status = @c_tr_status-released,
          changed_by     = @sy-uname,
          changed_on     = @sy-datum,
          changed_at     = @sy-uzeit
      WHERE parent_request = @iv_request.

    IF sy-subrc = 0.
      COMMIT WORK.
      MESSAGE |Transport status updated to Released in PR tracking table| TYPE 'S'.
    ELSE.
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

    DATA: ls_request TYPE e070.

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
