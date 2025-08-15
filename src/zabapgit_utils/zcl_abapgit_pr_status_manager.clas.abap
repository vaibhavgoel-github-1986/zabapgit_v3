CLASS zcl_abapgit_pr_status_manager DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: tt_pr_links TYPE TABLE OF zdt_pull_request with DEFAULT KEY.

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

    CLASS-METHODS get_pr_status
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
      zcx_abapgit_exception=>raise( |Failed to insert entry in DB| ).
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


  METHOD get_pr_status.

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
          li_github_pr    TYPE REF TO zcl_abapgit_pr_enum_github,
          li_http_agent   TYPE REF TO zif_abapgit_http_agent,
          lv_user         TYPE string,
          lv_repo         TYPE string,
          lv_new_status   TYPE zde_pr_status.

    FIELD-SYMBOLS: <ls_link> TYPE zdt_pull_request.

    " Get all PR links for this transport request
    lt_links = get_pr_status( iv_parent_request ).

    IF lines( lt_links ) = 0.
      RETURN.
    ENDIF.

    TRY.
        " Extract user/repo from URL (for GitHub: https://github.com/user/repo.git)
        FIND PCRE 'github\.com[/:]([^/]+)/([^/]+)' IN iv_repo_url
          SUBMATCHES lv_user lv_repo.

        IF sy-subrc <> 0.
          MESSAGE 'Invalid GitHub URL format' TYPE 'W'.
          RETURN.
        ENDIF.

        " Clean repository name (remove .git extension if present)
        lv_repo = replace(
          val   = lv_repo
          regex = '\.git$'
          with  = '' ).

        " Create GitHub PR provider
        li_http_agent = zcl_abapgit_http_agent=>create( ).
        li_github_pr = NEW #(
          iv_user_and_repo = |{ lv_user }/{ lv_repo }|
          ii_http_agent    = li_http_agent ).

        " Update status for each linked PR
        LOOP AT lt_links ASSIGNING <ls_link>.
          " Get detailed PR status from GitHub API
          lv_new_status = li_github_pr->get_pr_detailed_status( CONV i( <ls_link>-pr_id ) ).

          " Update if status changed
          IF <ls_link>-pr_status <> lv_new_status.
            update_pr_status(
              iv_parent_request = <ls_link>-parent_request
              iv_pr_id          = <ls_link>-pr_id
              iv_pr_status      = lv_new_status ).
          ENDIF.
        ENDLOOP.

      CATCH zcx_abapgit_exception.
        MESSAGE 'Failed to sync PR status with GitHub' TYPE 'W'.
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
