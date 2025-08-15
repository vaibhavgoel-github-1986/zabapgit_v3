CLASS zcl_im_git_pr_check DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_ex_cts_request_check

  PRIVATE SECTION.
    
    CONSTANTS:
      BEGIN OF c_request_type,
        workbench TYPE trfunction VALUE 'K',
        transport TYPE trfunction VALUE 'T',
      END OF c_request_type.

    CONSTANTS:
      BEGIN OF c_tr_status,
        development TYPE trstatus VALUE 'D',
        modifiable  TYPE trstatus VALUE 'O',
        released    TYPE trstatus VALUE 'R',
      END OF c_tr_status.

    METHODS is_main_workbench_request
      IMPORTING
        iv_request          TYPE trkorr
      RETURNING
        VALUE(rv_is_main)   TYPE abap_bool
      RAISING
        zcx_abapgit_exception.

    METHODS get_repo_url
      IMPORTING
        iv_request          TYPE trkorr
      RETURNING
        VALUE(rv_repo_url)  TYPE string
      RAISING
        zcx_abapgit_exception.

    METHODS check_pr_requirements
      IMPORTING
        iv_request          TYPE trkorr
        iv_repo_url         TYPE string
      RAISING
        zcx_abapgit_exception.

    METHODS update_transport_to_released
      IMPORTING
        iv_request          TYPE trkorr
      RAISING
        zcx_abapgit_exception.

ENDCLASS.

CLASS zcl_im_git_pr_check IMPLEMENTATION.

  METHOD if_ex_cts_request_check~check_before_release.

    DATA: lv_repo_url TYPE string.

    TRY.
        " 1. Check if the TR being released is a workbench request and is a main TR
        IF is_main_workbench_request( iv_request = request ) = abap_false.
          " Skip checks for non-main workbench requests (customizing, transport of copies, etc.)
          RETURN.
        ENDIF.

        " 2. Get repository URL for this transport request
        lv_repo_url = get_repo_url( request ).
        
        IF lv_repo_url IS INITIAL.
          " No repository URL found - could be a non-abapGit transport
          " Skip PR checks for non-abapGit transports
          RETURN.
        ENDIF.

        " 3. Check PR requirements (existence, sync status, approval)
        check_pr_requirements(
          iv_request  = request
          iv_repo_url = lv_repo_url ).

        " 4. Update transport status to released in our tracking table
        update_transport_to_released( request ).

        " Log successful PR check
        MESSAGE |Transport { request } passed PR validation checks| TYPE 'S'.

      CATCH zcx_abapgit_exception INTO DATA(lx_error).
        " Block the release by raising an exception
        MESSAGE lx_error->get_text( ) TYPE 'E'.
      CATCH cx_root INTO DATA(lx_root).
        " Handle any unexpected errors
        MESSAGE |Unexpected error during PR check: { lx_root->get_text( ) }| TYPE 'E'.
    ENDTRY.

  ENDMETHOD.

  METHOD is_main_workbench_request.

    DATA: ls_request TYPE e070.

    " Get transport request details
    SELECT SINGLE trfunction, korrdev, strkorr
      FROM e070
      INTO CORRESPONDING FIELDS OF @ls_request
      WHERE trkorr = @iv_request.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Transport request { iv_request } not found| ).
    ENDIF.

    " Check if it's a workbench request (not customizing)
    IF ls_request-trfunction <> c_request_type-workbench.
      rv_is_main = abap_false.
      RETURN.
    ENDIF.

    " Check if it's a main request (not a task)
    " Main requests have strkorr = trkorr (self-referencing)
    " Tasks have strkorr pointing to their parent request
    IF ls_request-strkorr = iv_request.
      rv_is_main = abap_true.
    ELSE.
      rv_is_main = abap_false.
    ENDIF.

  ENDMETHOD.

  METHOD get_repo_url.

    " Get repository URL from TVARVC table
    SELECT SINGLE low FROM tvarvc
      INTO @rv_repo_url
      WHERE name = 'ZGIT_REPO_URL'
        AND type = 'P'.

    IF sy-subrc <> 0 OR rv_repo_url IS INITIAL.
      " No repository URL configured - skip PR checks
      RETURN.
    ENDIF.

  ENDMETHOD.

  METHOD check_pr_requirements.

    DATA: lt_pr_links TYPE zcl_abapgit_pr_status_manager=>tt_pr_links,
          ls_pr_link  TYPE zdt_pull_request.

    " 2. Check if main TR is present in our PR-TR linkage table
    lt_pr_links = zcl_abapgit_pr_status_manager=>get_pr_tr_linkage( iv_request ).

    IF lines( lt_pr_links ) = 0.
      zcx_abapgit_exception=>raise(
        |Transport { iv_request } cannot be released: No Pull Request found. | &&
        |Please create a Pull Request for this transport before release.| ).
    ENDIF.

    " 3. Sync with GitHub and check PR status for each linked PR
    zcl_abapgit_pr_status_manager=>sync_with_github(
      iv_parent_request = iv_request
      iv_repo_url       = iv_repo_url ).

    " Re-read the PR links after sync to get updated status
    lt_pr_links = zcl_abapgit_pr_status_manager=>get_pr_tr_linkage( iv_request ).

    " Check if there are any PRs that would block the transport release
    " Focus on main transport context - check for any blocking status
    READ TABLE lt_pr_links INTO ls_pr_link
      WITH KEY pr_status = zcl_abapgit_pr_status_manager=>c_pr_status-open.
    IF sy-subrc = 0.
      zcx_abapgit_exception=>raise(
        |Transport { iv_request } cannot be released: Pull Request #{ ls_pr_link-pr_id } | &&
        |is still OPEN. Please ensure PR is merged before release.| ).
    ENDIF.

    READ TABLE lt_pr_links INTO ls_pr_link
      WITH KEY pr_status = zcl_abapgit_pr_status_manager=>c_pr_status-draft.
    IF sy-subrc = 0.
      zcx_abapgit_exception=>raise(
        |Transport { iv_request } cannot be released: Pull Request #{ ls_pr_link-pr_id } | &&
        |is still DRAFT. Please ensure PR is merged before release.| ).
    ENDIF.

    READ TABLE lt_pr_links INTO ls_pr_link
      WITH KEY pr_status = zcl_abapgit_pr_status_manager=>c_pr_status-changes.
    IF sy-subrc = 0.
      zcx_abapgit_exception=>raise(
        |Transport { iv_request } cannot be released: Pull Request #{ ls_pr_link-pr_id } | &&
        |has requested CHANGES. Please ensure PR is merged before release.| ).
    ENDIF.

    READ TABLE lt_pr_links INTO ls_pr_link
      WITH KEY pr_status = zcl_abapgit_pr_status_manager=>c_pr_status-closed.
    IF sy-subrc = 0.
      zcx_abapgit_exception=>raise(
        |Transport { iv_request } cannot be released: Pull Request #{ ls_pr_link-pr_id } | &&
        |was CLOSED without merging. Please reopen and merge the PR before release.| ).
    ENDIF.

    " If we get here, all PRs are in acceptable states (merged/approved)
    MESSAGE |Transport { iv_request } passed PR validation. All PRs are merged/approved. Release allowed.| TYPE 'S'.

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

ENDCLASS.
