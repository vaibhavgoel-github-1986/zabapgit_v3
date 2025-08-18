CLASS zcl_abapgit_pr_status_manager DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: tt_pr_links TYPE TABLE OF zdt_pull_request WITH DEFAULT KEY.

    CONSTANTS:
      BEGIN OF c_pr_status,
        open     TYPE zde_pr_status VALUE 'OPEN',
        draft    TYPE zde_pr_status VALUE 'DRAFT',
        approved TYPE zde_pr_status VALUE 'APPROVED',
        merged   TYPE zde_pr_status VALUE 'MERGED',
        closed   TYPE zde_pr_status VALUE 'CLOSED',
        changes  TYPE zde_pr_status VALUE 'CHANGES',
      END OF c_pr_status.

    CLASS-METHODS create_pr_link
      IMPORTING
        iv_parent_request TYPE strkorr
        iv_pr_id          TYPE int8
        iv_pr_status      TYPE zde_pr_status DEFAULT c_pr_status-open
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS update_pr_status
      IMPORTING
        iv_parent_request TYPE strkorr
        iv_pr_id          TYPE int8
        iv_pr_status      TYPE zde_pr_status
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS get_pr_tr_linkage
      IMPORTING
        iv_parent_request TYPE strkorr
        iv_pr_id          TYPE int8 OPTIONAL
      RETURNING
        VALUE(rt_links)   TYPE tt_pr_links
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS delete_pr_link
      IMPORTING
        iv_parent_request TYPE strkorr
        iv_pr_id          TYPE int8
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS sync_with_github
      IMPORTING
        iv_parent_request TYPE strkorr
        iv_repo_url       TYPE string
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS get_github_pr_status
      IMPORTING
        iv_repo_url     TYPE string
        iv_pr_id        TYPE int8
      RETURNING
        VALUE(rv_status) TYPE zde_pr_status
      RAISING
        zcx_abapgit_exception.
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS get_transport_status
      IMPORTING
        iv_parent_request   TYPE strkorr
      RETURNING
        VALUE(rv_tr_status) TYPE trstatus.

ENDCLASS.



CLASS ZCL_ABAPGIT_PR_STATUS_MANAGER IMPLEMENTATION.


  METHOD create_pr_link.

    DATA: ls_pr_link TYPE zdt_pull_request.

    " Check if link already exists
    SELECT SINGLE parent_request FROM zdt_pull_request
      INTO @DATA(lv_existing)
      WHERE parent_request = @iv_parent_request
        AND pr_id = @iv_pr_id.
    IF sy-subrc = 0.
      zcx_abapgit_exception=>raise( |PR link already exists for request { iv_parent_request } and PR { iv_pr_id }| ).
    ENDIF.

    " Create new PR link
    ls_pr_link-parent_request = iv_parent_request.
    ls_pr_link-pr_id = iv_pr_id.
    ls_pr_link-request_status = get_transport_status( iv_parent_request ).
    ls_pr_link-pr_status = iv_pr_status.
    ls_pr_link-created_by = sy-uname.
    ls_pr_link-created_on = sy-datum.
    ls_pr_link-created_at = sy-uzeit.
    ls_pr_link-changed_by = sy-uname.
    ls_pr_link-changed_on = sy-datum.
    ls_pr_link-changed_at = sy-uzeit.

    INSERT zdt_pull_request FROM ls_pr_link.
    IF sy-subrc <> 0.
      "Try Updating, if insert fails
      UPDATE zdt_pull_request
         SET request_status = ls_pr_link-request_status
             pr_id          = ls_pr_link-pr_id
             pr_status      = ls_pr_link-pr_status
             changed_by     = ls_pr_link-changed_by
             changed_on     = ls_pr_link-changed_on
             changed_at     = ls_pr_link-changed_at
       WHERE parent_request = iv_parent_request.
      IF sy-subrc IS NOT INITIAL.
        zcx_abapgit_exception=>raise( |Failed to insert entry in DB| ).
      ENDIF.
    ENDIF.

    COMMIT WORK.

  ENDMETHOD.


  METHOD update_pr_status.

    DATA: ls_pr_link TYPE zdt_pull_request.

    " Read existing record
    SELECT SINGLE * FROM zdt_pull_request
      INTO ls_pr_link
      WHERE parent_request = iv_parent_request
        AND pr_id = iv_pr_id.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |PR link not found for request { iv_parent_request } and PR { iv_pr_id }| ).
    ENDIF.

    " Update status and change info
    ls_pr_link-pr_status = iv_pr_status.
    ls_pr_link-request_status = get_transport_status( iv_parent_request ).
    ls_pr_link-changed_by = sy-uname.
    ls_pr_link-changed_on = sy-datum.
    ls_pr_link-changed_at = sy-uzeit.

    UPDATE zdt_pull_request FROM ls_pr_link.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Failed to update PR status: { sy-subrc }| ).
    ENDIF.

    COMMIT WORK.

  ENDMETHOD.


  METHOD get_pr_tr_linkage.

    DATA: lr_pr_id TYPE RANGE OF int8.

    IF NOT iv_pr_id IS INITIAL.
      lr_pr_id = VALUE #( ( sign = 'I' option = 'EQ' low = iv_pr_id ) ).
    ENDIF.

    SELECT *
      FROM zdt_pull_request
      WHERE parent_request = @iv_parent_request
        AND pr_id IN @lr_pr_id
          INTO TABLE @rt_links.

  ENDMETHOD.


  METHOD delete_pr_link.

    DELETE FROM zdt_pull_request
      WHERE parent_request = iv_parent_request
        AND pr_id = iv_pr_id.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Failed to delete PR link: { sy-subrc }| ).
    ENDIF.

    COMMIT WORK.

  ENDMETHOD.


  METHOD sync_with_github.

    DATA: lt_links        TYPE tt_pr_links,
          lv_new_status   TYPE zde_pr_status,
          lv_updated_count TYPE i,
          lv_current_tr_status TYPE trstatus,
          lv_status_updated TYPE abap_bool.

    FIELD-SYMBOLS: <ls_link> TYPE zdt_pull_request.

    " Get all PR links for this transport request
    lt_links = get_pr_tr_linkage( iv_parent_request ).
    IF lines( lt_links ) = 0.
      MESSAGE |No Pull Request was found for the Request.|
       TYPE 'E'.
      RETURN.
    ENDIF.

    " Get current transport status from SAP system
    lv_current_tr_status = get_transport_status( iv_parent_request ).

    " Update status for each linked PR
    LOOP AT lt_links ASSIGNING <ls_link>.
      TRY.
          " Check if transport status has changed and update if needed
          IF <ls_link>-request_status <> lv_current_tr_status.
            <ls_link>-request_status = lv_current_tr_status.
            <ls_link>-changed_by = sy-uname.
            <ls_link>-changed_on = sy-datum.
            <ls_link>-changed_at = sy-uzeit.
            lv_status_updated = abap_true.
          ENDIF.

          " Get detailed PR status from GitHub API using our new method
          lv_new_status = get_github_pr_status(
            iv_repo_url = iv_repo_url
            iv_pr_id    = <ls_link>-pr_id ).

          " Update if PR status changed
          IF <ls_link>-pr_status <> lv_new_status.
            <ls_link>-pr_status = lv_new_status.
            <ls_link>-changed_by = sy-uname.
            <ls_link>-changed_on = sy-datum.
            <ls_link>-changed_at = sy-uzeit.
            lv_status_updated = abap_true.
          ENDIF.

          " Update database record if any status changed
          IF lv_status_updated = abap_true.
            UPDATE zdt_pull_request FROM <ls_link>.
            IF sy-subrc = 0.
              lv_updated_count = lv_updated_count + 1.
            ELSE.
              MESSAGE |Failed to update PR { <ls_link>-pr_id } in database| TYPE 'W'.
            ENDIF.
            CLEAR lv_status_updated.
          ENDIF.

        CATCH zcx_abapgit_exception INTO DATA(lx_error).
          MESSAGE |Failed to sync PR { <ls_link>-pr_id }: { lx_error->get_text( ) }| TYPE 'W'.
      ENDTRY.
    ENDLOOP.

    " Commit all changes
    IF lv_updated_count > 0.
      COMMIT WORK.
    ENDIF.

    MESSAGE |Sync completed. { lv_updated_count } PR(s) updated out of { lines( lt_links ) }. Transport status: { lv_current_tr_status }| TYPE 'S'.

  ENDMETHOD.


  METHOD get_github_pr_status.

    DATA: lv_user         TYPE string,
          lv_repo         TYPE string,
          lv_auth         TYPE string,
          lv_api_key      TYPE string,
          li_http_agent   TYPE REF TO zif_abapgit_http_agent,
          li_github_pr    TYPE REF TO zcl_abapgit_pr_enum_github.

    " Initialize return value
    rv_status = c_pr_status-open.

    TRY.
        " Extract user/repo from URL (for GitHub: https://github.com/user/repo.git)
        FIND PCRE 'github\.com[/:]([^/]+)/([^/]+)' IN iv_repo_url
          SUBMATCHES lv_user lv_repo.

        IF sy-subrc <> 0.
          zcx_abapgit_exception=>raise( |Invalid GitHub URL format: { iv_repo_url }| ).
        ENDIF.

        " Clean repository name (remove .git extension if present)
        lv_repo = replace(
          val   = lv_repo
          regex = '\.git$'
          with  = '' ).

        " Get GitHub API key from TVARVC
        SELECT SINGLE low FROM tvarvc
          INTO @lv_api_key
          WHERE name = 'ZGIT_API_KEY'
            AND type = 'P'.

        IF sy-subrc <> 0 OR lv_api_key IS INITIAL.
          zcx_abapgit_exception=>raise( 'GitHub API key not found in TVARVC table (ZGIT_API_KEY)' ).
        ENDIF.

        " Set authentication using API key
        lv_auth = zcl_abapgit_login_manager=>set(
          iv_uri      = iv_repo_url
          iv_username = 'vaibhago_cisco'
          iv_password = lv_api_key ).

        IF lv_auth IS INITIAL.
          zcx_abapgit_exception=>raise( 'Failed to set GitHub authentication' ).
        ENDIF.

        " Create GitHub PR provider with authentication
        li_http_agent = zcl_abapgit_http_agent=>create( ).
        li_github_pr = NEW #(
          iv_user_and_repo = |{ lv_user }/{ lv_repo }|
          ii_http_agent    = li_http_agent ).

        " Get detailed PR status from GitHub API
        rv_status = li_github_pr->get_pr_detailed_status( CONV i( iv_pr_id ) ).

      CATCH zcx_abapgit_exception.
        " Re-raise the exception with additional context
        zcx_abapgit_exception=>raise( |Failed to fetch PR status for PR { iv_pr_id } from { iv_repo_url }| ).
      CATCH cx_root INTO DATA(lx_root).
        " Handle any other exceptions
        zcx_abapgit_exception=>raise( |Unexpected error while fetching PR status: { lx_root->get_text( ) }| ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_transport_status.

    SELECT SINGLE trstatus FROM e070
      INTO rv_tr_status
      WHERE trkorr = iv_parent_request.

    IF sy-subrc <> 0.
      rv_tr_status = 'D'. " Default to Development
    ENDIF.

  ENDMETHOD.
ENDCLASS.
