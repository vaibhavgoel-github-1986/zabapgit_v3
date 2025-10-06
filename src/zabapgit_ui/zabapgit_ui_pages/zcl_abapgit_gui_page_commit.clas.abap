CLASS zcl_abapgit_gui_page_commit DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_gui_event_handler.
    INTERFACES zif_abapgit_gui_renderable.

    CLASS-METHODS create
      IMPORTING ii_repo_online TYPE REF TO zif_abapgit_repo_online
                io_stage       TYPE REF TO zcl_abapgit_stage
                iv_sci_result  TYPE zif_abapgit_definitions=>ty_sci_result DEFAULT zif_abapgit_definitions=>c_sci_result-no_run
                ii_obj_filter  TYPE REF TO zif_abapgit_object_filter       OPTIONAL
      RETURNING VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING   zcx_abapgit_exception.

    METHODS constructor
      IMPORTING ii_repo_online TYPE REF TO zif_abapgit_repo_online
                io_stage       TYPE REF TO zcl_abapgit_stage
                iv_sci_result  TYPE zif_abapgit_definitions=>ty_sci_result
                ii_obj_filter  TYPE REF TO zif_abapgit_object_filter OPTIONAL
      RAISING   zcx_abapgit_exception.

  PROTECTED SECTION.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_id,
        committer       TYPE string VALUE 'committer',
        committer_name  TYPE string VALUE 'committer_name',
        committer_email TYPE string VALUE 'committer_email',
        message         TYPE string VALUE 'message',
        comment         TYPE string VALUE 'comment',
        body            TYPE string VALUE 'body',
        author          TYPE string VALUE 'author',
        author_name     TYPE string VALUE 'author_name',
        author_email    TYPE string VALUE 'author_email',
        new_branch_name TYPE string VALUE 'new_branch_name',
      END OF c_id.

    CONSTANTS:
      BEGIN OF c_event,
        commit TYPE string VALUE 'commit',
      END OF c_event.

    DATA mo_form           TYPE REF TO zcl_abapgit_html_form.
    DATA mo_form_data      TYPE REF TO zcl_abapgit_string_map.
    DATA mo_form_util      TYPE REF TO zcl_abapgit_html_form_utils.
    DATA mo_validation_log TYPE REF TO zcl_abapgit_string_map.
    DATA mo_settings       TYPE REF TO zcl_abapgit_settings.
    DATA mi_repo_online    TYPE REF TO zif_abapgit_repo_online.
    DATA mo_stage          TYPE REF TO zcl_abapgit_stage.
    DATA mt_stage          TYPE zif_abapgit_definitions=>ty_stage_tt.
    DATA ms_commit         TYPE zif_abapgit_services_git=>ty_commit_fields.
    DATA mv_sci_result     TYPE zif_abapgit_definitions=>ty_sci_result.
    DATA mv_log_handle     TYPE balloghndl.
    DATA mi_obj_filter     TYPE REF TO zif_abapgit_object_filter.

    METHODS render_stage_summary
      RETURNING VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING   zcx_abapgit_exception.

    METHODS render_stage_details
      RETURNING VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING   zcx_abapgit_exception.

    METHODS validate_form
      IMPORTING io_form_data             TYPE REF TO zcl_abapgit_string_map
      RETURNING VALUE(ro_validation_log) TYPE REF TO zcl_abapgit_string_map
      RAISING   zcx_abapgit_exception.

    METHODS get_form_schema
      RETURNING VALUE(ro_form) TYPE REF TO zcl_abapgit_html_form.

    METHODS get_defaults
      RAISING zcx_abapgit_exception.

    METHODS get_committer_name
      RETURNING VALUE(rv_user) TYPE string
      RAISING   zcx_abapgit_exception.

    METHODS get_committer_email
      RETURNING VALUE(rv_email) TYPE string
      RAISING   zcx_abapgit_exception.

    METHODS get_comment_default
      RETURNING VALUE(rv_text) TYPE string.

    METHODS get_comment_object
      IMPORTING it_stage       TYPE zif_abapgit_definitions=>ty_stage_tt
      RETURNING VALUE(rv_text) TYPE string.

    METHODS get_comment_file
      IMPORTING it_stage       TYPE zif_abapgit_definitions=>ty_stage_tt
      RETURNING VALUE(rv_text) TYPE string.

    METHODS branch_name_to_internal
      IMPORTING iv_branch_name            TYPE string
      RETURNING VALUE(rv_new_branch_name) TYPE string.

    METHODS find_main_branch
      RETURNING VALUE(rv_main_branch) TYPE string
      RAISING   zcx_abapgit_exception.

    METHODS create_pull_request_auto
      IMPORTING iv_source_branch TYPE string
                iv_target_branch TYPE string
                iv_pr_title      TYPE string
                iv_pr_body       TYPE string
      RAISING   zcx_abapgit_exception.

    METHODS get_transport_from_stage
      RETURNING VALUE(rv_transport) TYPE string.

    METHODS get_transport_description
      IMPORTING iv_transport          TYPE string
      RETURNING VALUE(rv_description) TYPE string.

    METHODS escape_json_string
      IMPORTING iv_input         TYPE string
      RETURNING VALUE(rv_output) TYPE string.

    METHODS is_development_branch
      IMPORTING iv_branch_name              TYPE string
      RETURNING VALUE(rv_is_development)    TYPE abap_bool.
ENDCLASS.


CLASS zcl_abapgit_gui_page_commit IMPLEMENTATION.
  METHOD branch_name_to_internal.
    rv_new_branch_name = zcl_abapgit_git_branch_utils=>complete_heads_branch_name(
                             zcl_abapgit_git_branch_utils=>normalize_branch_name( iv_branch_name ) ).
  ENDMETHOD.

  METHOD find_main_branch.
    DATA lt_branches TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt.
    DATA ls_branch   TYPE zif_abapgit_git_definitions=>ty_git_branch.

    " Get all branches from repository
    lt_branches = zcl_abapgit_git_factory=>get_git_transport(
      )->branches( mi_repo_online->get_url( )
      )->get_branches_only( ).

    " Look for a branch matching the pattern release/SHA*
    LOOP AT lt_branches INTO ls_branch.
      IF ls_branch-display_name CP |release/{ to_upper( sy-sysid ) }*|.
        rv_main_branch = ls_branch-name.
        RETURN.
      ENDIF.
    ENDLOOP.

    " If no release/SHA* pattern found, fallback to main/master
    LOOP AT lt_branches INTO ls_branch.
      IF    ls_branch-display_name = 'main'
         OR ls_branch-display_name = 'master'.
        rv_main_branch = ls_branch-name.
        RETURN.
      ENDIF.
    ENDLOOP.

    " If neither pattern found, use HEAD symref
    rv_main_branch = zcl_abapgit_git_factory=>get_git_transport(
      )->branches( mi_repo_online->get_url( )
      )->get_head_symref( ).
  ENDMETHOD.

  METHOD create_pull_request_auto.
    DATA lv_repo_url      TYPE string.
    DATA lv_user_and_repo TYPE string.
    DATA lv_user          TYPE string.
    DATA lv_repo          TYPE string.
    DATA lv_head_branch   TYPE string.
    DATA li_http_agent    TYPE REF TO zif_abapgit_http_agent.
    DATA li_pr_provider   TYPE REF TO zcl_abapgit_pr_enum_github.
    DATA lx_error         TYPE REF TO zcx_abapgit_exception.
    DATA lt_reviewers     TYPE string_table.
    DATA lt_users         TYPE STANDARD TABLE OF tvarvc WITH DEFAULT KEY.
    DATA ls_user          TYPE tvarvc.

    " Get repository URL
    lv_repo_url = mi_repo_online->get_url( ).

    " Extract user/repo from URL (for GitHub: https://github.com/user/repo.git)
    FIND REGEX 'github\.com[/:]([^/]+)/([^/]+)' IN lv_repo_url
         SUBMATCHES lv_user lv_repo.

    IF sy-subrc <> 0.
      " Not a GitHub repository, skip PR creation
      MESSAGE 'Automatic PR creation is only supported for GitHub repositories' TYPE 'W'.
      RETURN.
    ENDIF.

    " Clean repository name (remove .git extension if present)
    lv_repo = replace( val   = lv_repo
                       regex = '\.git$'
                       with  = '' ).
    lv_user_and_repo = |{ lv_user }/{ lv_repo }|.

    " Get clean branch names for PR
    lv_head_branch = zcl_abapgit_git_branch_utils=>get_display_name( iv_source_branch ).

    " Get Reviewers from TVARVC table
    MESSAGE |Fetching reviewers from TVARVC table...| TYPE 'S'.
    zcl_abapgit_logging_utils=>write_application_log( iv_log_handle = mv_log_handle
                                                      iv_log_type   = 'I'
                                                      iv_message    = 'Fetching reviewers from TVARVC table'
                                                      iv_detail     = 'Table: TVARVC, Name: ZGIT_REVIEWER' ).

    SELECT * FROM tvarvc
      INTO TABLE @lt_users
      WHERE name = 'Z_CODE_REVIEWERS'.
    IF sy-subrc IS INITIAL.
      LOOP AT lt_users INTO ls_user.
        APPEND to_lower( ls_user-low ) TO lt_reviewers.
      ENDLOOP.
      MESSAGE |Found { lines( lt_reviewers ) } reviewer(s) in configuration| TYPE 'S'.

      zcl_abapgit_logging_utils=>write_application_log( iv_log_handle = mv_log_handle
                                                        iv_log_type   = 'I'
                                                        iv_message    = 'Reviewers found in configuration'
                                                        iv_detail     = |Count: { lines( lt_reviewers ) }| ).
    ELSE.
      " Default to sekanaga_cisco if no reviewers configured
      APPEND 'sekanaga_cisco' TO lt_reviewers.
      MESSAGE |No reviewers configured, defaulting to sekanaga_cisco| TYPE 'W'.
    ENDIF.

    " Remove current user from reviewer list (prevent self-review)
    DATA lv_current_user_cisco TYPE string.
    lv_current_user_cisco = to_lower( |{ sy-uname }_cisco| ).

    DELETE lt_reviewers WHERE table_line = lv_current_user_cisco.

    " Check if we still have reviewers after removing current user
    IF lines( lt_reviewers ) = 0.
      " If current user was the only reviewer, add default
      APPEND 'sekanaga_cisco' TO lt_reviewers.
      MESSAGE |Current user removed from reviewers, defaulting to sekanaga_cisco| TYPE 'W'.
      zcl_abapgit_logging_utils=>write_application_log(
          iv_log_handle = mv_log_handle
          iv_log_type   = 'W'
          iv_message    = 'Current user was only reviewer, using default'
          iv_detail     = |User: { lv_current_user_cisco }, Default: sekanaga_cisco| ).
    ELSE.
      MESSAGE |Current user ({ lv_current_user_cisco }) excluded from reviewer list| TYPE 'S'.
      zcl_abapgit_logging_utils=>write_application_log(
          iv_log_handle = mv_log_handle
          iv_log_type   = 'S'
          iv_message    = 'Current user excluded from reviewers'
          iv_detail     = |User: { lv_current_user_cisco }, Remaining reviewers: { lines( lt_reviewers ) }| ).
    ENDIF.

    TRY.
        " Create HTTP agent
        li_http_agent = zcl_abapgit_http_agent=>create( ).

        zcl_abapgit_logging_utils=>write_application_log( iv_log_handle = mv_log_handle
                                                          iv_log_type   = 'I'
                                                          iv_message    = 'HTTP agent created for GitHub API'
                                                          iv_detail     = 'Ready for PR creation' ).

        " Create GitHub PR provider
        li_pr_provider = NEW zcl_abapgit_pr_enum_github( iv_user_and_repo = lv_user_and_repo
                                                         ii_http_agent    = li_http_agent ).

        " Step 1: Create pull request as draft using form data
        MESSAGE |Creating pull request for branch { lv_head_branch }...| TYPE 'S'.

        zcl_abapgit_logging_utils=>write_application_log(
            iv_log_handle = mv_log_handle
            iv_log_type   = 'I'
            iv_message    = 'Starting pull request creation'
            iv_detail     = |{ lv_head_branch } -> | &&
                            |{ zcl_abapgit_git_branch_utils=>get_display_name( iv_target_branch ) }| ).

        DATA lv_pr_number TYPE i.

        " Sanitize PR title and body for JSON compatibility
        DATA(lv_sanitized_title) = escape_json_string( iv_pr_title ).
        DATA(lv_sanitized_body) = escape_json_string( iv_pr_body ).

        " Log the JSON escaping process for debugging
        zcl_abapgit_logging_utils=>write_application_log(
            iv_log_handle = mv_log_handle
            iv_log_type   = 'I'
            iv_message    = 'PR title and body sanitized for JSON'
            iv_detail     = |Title: { strlen( iv_pr_title ) } -> { strlen( lv_sanitized_title ) } chars, Body: { strlen( iv_pr_body ) } -> { strlen( lv_sanitized_body ) } chars| ).

        lv_pr_number = li_pr_provider->create_pull_request(
                           iv_title = lv_sanitized_title
                           iv_body  = lv_sanitized_body
                           iv_head  = lv_head_branch
                           iv_base  = zcl_abapgit_git_branch_utils=>get_display_name( iv_target_branch ) ).

        MESSAGE |Pull request #{ lv_pr_number } created successfully| TYPE 'S'.
        zcl_abapgit_logging_utils=>write_application_log(
            iv_log_handle = mv_log_handle
            iv_log_type   = 'S'
            iv_message    = 'Pull request created successfully'
            iv_detail     = |PR Number: { lv_pr_number }, Title: { iv_pr_title }| ).

        " Step 1.5: Automatically link PR to transport request in database
        TRY.
            DATA(lv_transport) = get_transport_from_stage( ).
            IF lv_transport IS NOT INITIAL.
              zcl_abapgit_pr_status_manager=>create_pr_link(
                  iv_parent_request = CONV strkorr( lv_transport )
                  iv_pr_id          = CONV int8( lv_pr_number )
                  iv_pr_status      = zcl_abapgit_pr_status_manager=>c_pr_status-open ).

              zcl_abapgit_logging_utils=>write_application_log(
                  iv_log_handle = mv_log_handle
                  iv_log_type   = 'I'
                  iv_message    = 'PR linked to transport request'
                  iv_detail     = |Transport: { lv_transport }, PR: { lv_pr_number }| ).
            ELSE.
              zcl_abapgit_logging_utils=>write_application_log(
                  iv_log_handle = mv_log_handle
                  iv_log_type   = 'W'
                  iv_message    = 'No transport request found for PR linking'
                  iv_detail     = |PR: { lv_pr_number } created but not linked to transport| ).
            ENDIF.
          CATCH zcx_abapgit_exception INTO DATA(lx_link_error).
            zcl_abapgit_logging_utils=>write_application_log( iv_log_handle = mv_log_handle
                                                              iv_log_type   = 'W'
                                                              iv_message    = 'Failed to link PR to transport request'
                                                              iv_detail     = |Error: { lx_link_error->get_text( ) }| ).
        ENDTRY.

        " Step 2: Assign reviewers (we always have reviewers due to default logic above)
        DATA lv_reviewers_display TYPE string.

        " Build reviewer list for display
        LOOP AT lt_reviewers INTO DATA(lv_reviewer).
          IF sy-tabix > 1.
            lv_reviewers_display = |{ lv_reviewers_display }, |.
          ENDIF.
          lv_reviewers_display = |{ lv_reviewers_display }{ lv_reviewer }|.
        ENDLOOP.

        MESSAGE |Assigning reviewers: { lv_reviewers_display }| TYPE 'S'.
        zcl_abapgit_logging_utils=>write_application_log(
            iv_log_handle = mv_log_handle
            iv_log_type   = 'S'
            iv_message    = 'Starting reviewer assignment'
            iv_detail     = |PR: { lv_pr_number }, Reviewers: { lv_reviewers_display }| ).

        li_pr_provider->assign_reviewers( iv_pull_number = lv_pr_number
                                          it_reviewers   = lt_reviewers ).

        MESSAGE |Reviewers assigned successfully to PR #{ lv_pr_number }| TYPE 'S'.
        zcl_abapgit_logging_utils=>write_application_log(
            iv_log_handle = mv_log_handle
            iv_log_type   = 'S'
            iv_message    = 'Reviewers assigned successfully'
            iv_detail     = |PR: { lv_pr_number }, Count: { lines( lt_reviewers ) }| ).

        " Final success message
        MESSAGE |PR #{ lv_pr_number } created and assigned for code review|
                TYPE 'I' DISPLAY LIKE 'S'.

        zcl_abapgit_logging_utils=>write_application_log(
            iv_log_handle = mv_log_handle
            iv_log_type   = 'S'
            iv_message    = 'PR created successfully'
            iv_detail     = |PR #{ lv_pr_number } created successfully|  ).

        " Setting PR 'Ready for Review'
        li_pr_provider->ready_for_review( lv_pr_number ).

        zcl_abapgit_logging_utils=>write_application_log(
            iv_log_handle = mv_log_handle
            iv_log_type   = 'S'
            iv_message    = |'PR was set 'Ready for Review'|
            iv_detail     = |PR #{ lv_pr_number } marked ready for review|  ).

      CATCH zcx_abapgit_exception INTO lx_error.
        zcl_abapgit_logging_utils=>write_application_log( iv_log_handle = mv_log_handle
                                                          iv_log_type   = 'E'
                                                          iv_message    = 'Error in pull request automation'
                                                          iv_detail     = |Error: { lx_error->get_text( ) }| ).
        MESSAGE |Error creating pull request: { lx_error->get_text( ) }| TYPE 'W'.
    ENDTRY.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( ).

    mi_repo_online = ii_repo_online.
    mo_stage       = io_stage.
    mt_stage       = mo_stage->get_all( ).
    mv_sci_result  = iv_sci_result.
    mi_obj_filter  = ii_obj_filter.

    " Get settings from DB
    mo_settings = zcl_abapgit_persist_factory=>get_settings( )->read( ).

    mo_validation_log = NEW #( ).
    mo_form_data = NEW #( ).
    mo_form = get_form_schema( ).
    mo_form_util = zcl_abapgit_html_form_utils=>create( mo_form ).
  ENDMETHOD.

  METHOD create.
    DATA lo_component TYPE REF TO zcl_abapgit_gui_page_commit.

    lo_component = NEW #( ii_repo_online = ii_repo_online
                          io_stage       = io_stage
                          iv_sci_result  = iv_sci_result
                          ii_obj_filter  = ii_obj_filter ).

    ri_page = zcl_abapgit_gui_page_hoc=>create( iv_page_title      = 'Commit'
                                                ii_child_component = lo_component ).
  ENDMETHOD.

  METHOD get_comment_default.
    rv_text = mo_settings->get_commitmsg_comment_default( ).

    IF rv_text IS INITIAL.
      RETURN.
    ENDIF.

    REPLACE '$FILE'   IN rv_text WITH get_comment_file( mt_stage ).
    REPLACE '$OBJECT' IN rv_text WITH get_comment_object( mt_stage ).
  ENDMETHOD.

  METHOD get_comment_file.
    DATA lv_count TYPE i.

    FIELD-SYMBOLS <ls_stage> LIKE LINE OF it_stage.

    lv_count = lines( it_stage ).

    IF lv_count = 1.
      " Just one file so we use the file name
      ASSIGN it_stage[ 1 ] TO <ls_stage>.
      ASSERT sy-subrc = 0.

      rv_text = <ls_stage>-file-filename.
    ELSE.
      " For multiple file we use the count instead
      rv_text = |{ lv_count } files|.
    ENDIF.
  ENDMETHOD.

  METHOD get_comment_object.
    DATA lv_count TYPE i.
    DATA ls_item  TYPE zif_abapgit_definitions=>ty_item.
    DATA lt_items TYPE zif_abapgit_definitions=>ty_items_tt.

    FIELD-SYMBOLS <ls_stage> LIKE LINE OF it_stage.

    " Get objects
    LOOP AT it_stage ASSIGNING <ls_stage>.
      CLEAR ls_item.
      ls_item-obj_type = <ls_stage>-status-obj_type.
      ls_item-obj_name = <ls_stage>-status-obj_name.
      COLLECT ls_item INTO lt_items.
    ENDLOOP.

    lv_count = lines( lt_items ).

    IF lv_count = 1.
      " Just one object so we use the object name
      READ TABLE lt_items INTO ls_item INDEX 1.
      ASSERT sy-subrc = 0.

      CONCATENATE ls_item-obj_type ls_item-obj_name INTO rv_text SEPARATED BY space.
    ELSE.
      " For multiple objects we use the count instead
      rv_text = |{ lv_count } objects|.
    ENDIF.
  ENDMETHOD.

  METHOD get_committer_email.
    DATA li_user TYPE REF TO zif_abapgit_persist_user.

    li_user = zcl_abapgit_persist_factory=>get_user( ).

    rv_email = li_user->get_repo_git_user_email( mi_repo_online->get_url( ) ).
    IF rv_email IS INITIAL.
      rv_email = li_user->get_default_git_user_email( ).
    ENDIF.
    IF rv_email IS INITIAL.
      " get default from user record
      rv_email = zcl_abapgit_env_factory=>get_user_record( )->get_email( sy-uname ).
    ENDIF.
  ENDMETHOD.

  METHOD get_committer_name.
    DATA li_user TYPE REF TO zif_abapgit_persist_user.

    li_user = zcl_abapgit_persist_factory=>get_user( ).

    rv_user = li_user->get_repo_git_user_name( mi_repo_online->get_url( ) ).
    IF rv_user IS INITIAL.
      rv_user = li_user->get_default_git_user_name( ).
    ENDIF.
    IF rv_user IS INITIAL.
      " get default from user record
      rv_user = zcl_abapgit_env_factory=>get_user_record( )->get_name( sy-uname ).
    ENDIF.
  ENDMETHOD.

  METHOD get_defaults.
    DATA li_exit TYPE REF TO zif_abapgit_exit.

    ms_commit-committer_name  = get_committer_name( ).
    ms_commit-committer_email = get_committer_email( ).
    ms_commit-comment         = get_comment_default( ).

    li_exit = zcl_abapgit_exit=>get_instance( ).
    li_exit->change_committer_info( EXPORTING iv_repo_url = mi_repo_online->get_url( )
                                    CHANGING  cv_name     = ms_commit-committer_name
                                              cv_email    = ms_commit-committer_email ).

    " Committer
    mo_form_data->set( iv_key = c_id-committer_name
                       iv_val = ms_commit-committer_name ).
    mo_form_data->set( iv_key = c_id-committer_email
                       iv_val = ms_commit-committer_email ).

    " Message
    mo_form_data->set( iv_key = c_id-comment
                       iv_val = ms_commit-comment ).

    " Auto-populate new branch name and comment with main transport
    " Only if current branch is a release/* branch
    DATA(lv_current_branch) = mi_repo_online->get_selected_branch( ).
    DATA(lv_transport) = get_transport_from_stage( ).

    IF     lv_transport      IS NOT INITIAL AND lv_transport <> 'DIRECT_COMMIT'
       AND lv_current_branch CP |*release/{ to_upper( sy-sysid ) }*|.
      " Auto-populate branch name only when staging from release branch
      mo_form_data->set( iv_key = c_id-new_branch_name
                         iv_val = |feature/{ to_upper( lv_transport ) }| ).
    ENDIF.

    IF lv_transport IS NOT INITIAL.
      " Auto-populate comment with transport description if available
      DATA(lv_transport_desc) = get_transport_description( lv_transport ).
      IF lv_transport_desc IS NOT INITIAL.
        mo_form_data->set( iv_key = c_id-comment
                           iv_val = lv_transport_desc ).
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD get_form_schema.
    DATA lv_commitmsg_comment_length TYPE i.
    CONSTANTS lc_commitmsg_comment_min_len TYPE i VALUE 1.
*    CONSTANTS lc_commitmsg_comment_max_len TYPE i VALUE 255.

    ro_form = zcl_abapgit_html_form=>create( iv_form_id   = 'commit-form'
                                             iv_help_page = 'https://docs.abapgit.org/guide-stage-commit.html' ).

    lv_commitmsg_comment_length = mo_settings->get_commitmsg_comment_length( ).

    ro_form->text( iv_name        = c_id-comment
                   iv_label       = 'Comment'
                   iv_required    = abap_true
                   iv_min         = lc_commitmsg_comment_min_len
                   iv_max         = lv_commitmsg_comment_length
                   iv_placeholder = |Auto-filled from transport description or add your own...|
    )->textarea( iv_name        = c_id-body
                 iv_label       = 'Technical Summary'
                 iv_required    = abap_true
                 iv_rows        = 6
                 iv_cols        = mo_settings->get_commitmsg_body_size( )
                 iv_placeholder = 'Add Technical Unit Test Document Links. Provide a summary of code changes done'
    )->text( iv_name     = c_id-committer_name
             iv_label    = 'Committer Name'
             iv_required = abap_true
    )->text( iv_name     = c_id-committer_email
             iv_label    = 'Committer Email'
             iv_required = abap_true ).

*    IF mo_settings->get_commitmsg_hide_author( ) IS INITIAL.
*      ro_form->text(
*        iv_name        = c_id-author_name
*        iv_label       = 'Author Name'
*        iv_placeholder = 'Optionally, specify an author (same as committer by default)'
*      )->text(
*        iv_name        = c_id-author_email
*        iv_label       = 'Author Email' ).
*    ENDIF.

    ro_form->text( iv_name        = c_id-new_branch_name
                   iv_label       = 'New Branch Name'
                   iv_placeholder = 'feature/TRANSPORT_NUMBER'
                   iv_required    = abap_false
                   iv_condense    = abap_true ).

    ro_form->command( iv_label    = 'Commit'
                      iv_cmd_type = zif_abapgit_html_form=>c_cmd_type-input_main
                      iv_action   = c_event-commit
    )->command( iv_label  = 'Back'
                iv_action = zif_abapgit_definitions=>c_action-go_back ).
  ENDMETHOD.

  METHOD render_stage_details.
    FIELD-SYMBOLS <ls_stage> LIKE LINE OF mt_stage.

    ri_html = NEW zcl_abapgit_html( ).

    ri_html->add( '<table class="stage_tab">' ).
    ri_html->add( '<thead>' ).
    ri_html->add( '<tr>' ).
    ri_html->add( '<th colspan="3">Staged Files (See <a href="#top">Summary</a> Above)</th>' ).
    ri_html->add( '</tr>' ).
    ri_html->add( '</thead>' ).

    ri_html->add( '<tbody>' ).
    LOOP AT mt_stage ASSIGNING <ls_stage>.
      ri_html->add( '<tr>' ).
      ri_html->add( '<td>' ).
      ri_html->add( zcl_abapgit_gui_chunk_lib=>render_item_state( iv_lstate = <ls_stage>-status-lstate
                                                                  iv_rstate = <ls_stage>-status-rstate ) ).
      ri_html->add( '</td>' ).
      ri_html->add( '<td class="method">' ).
      ri_html->add( zcl_abapgit_stage=>method_description( <ls_stage>-method ) ).
      ri_html->add( '</td>' ).
      ri_html->add( '<td>' ).
      ri_html->add( <ls_stage>-file-path && <ls_stage>-file-filename ).
      ri_html->add( '</td>' ).
      ri_html->add( '</tr>' ).
    ENDLOOP.
    ri_html->add( '</tbody>' ).

    ri_html->add( '</table>' ).
  ENDMETHOD.

  METHOD render_stage_summary.
    DATA:
      BEGIN OF ls_sum,
        method TYPE string,
        count  TYPE i,
      END OF ls_sum,
      lt_sum LIKE STANDARD TABLE OF ls_sum WITH DEFAULT KEY.

    FIELD-SYMBOLS <ls_stage> LIKE LINE OF mt_stage.

    ri_html = NEW zcl_abapgit_html( ).

    LOOP AT mt_stage ASSIGNING <ls_stage>.
      ls_sum-method = <ls_stage>-method.
      ls_sum-count  = 1.
      COLLECT ls_sum INTO lt_sum.
    ENDLOOP.

    ri_html->add( 'Stage Summary: ' ).

    READ TABLE lt_sum INTO ls_sum WITH TABLE KEY method = zif_abapgit_definitions=>c_method-add.
    IF sy-subrc = 0.
      ri_html->add( |<span class="diff_banner diff_ins" title="add">+ { ls_sum-count }</span>| ).
    ENDIF.
    READ TABLE lt_sum INTO ls_sum WITH TABLE KEY method = zif_abapgit_definitions=>c_method-rm.
    IF sy-subrc = 0.
      ri_html->add( |<span class="diff_banner diff_del" title="remove">- { ls_sum-count }</span>| ).
    ENDIF.
    READ TABLE lt_sum INTO ls_sum WITH TABLE KEY method = zif_abapgit_definitions=>c_method-ignore.
    IF sy-subrc = 0.
      ri_html->add( |<span class="diff_banner diff_upd" title="ignore">~ { ls_sum-count }</span>| ).
    ENDIF.

    IF lines( mt_stage ) = 1.
      ri_html->add( 'file' ).
    ELSE.
      ri_html->add( 'files' ).
    ENDIF.

    ri_html->add( '(See <a href="#stage-details">Details</a> Below)' ).
  ENDMETHOD.

  METHOD validate_form.
    DATA lt_branches        TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt.
    DATA lv_new_branch_name TYPE string.

    ro_validation_log = mo_form_util->validate( io_form_data ).

    IF zcl_abapgit_utils=>is_valid_email( io_form_data->get( c_id-committer_email ) ) = abap_false.
      ro_validation_log->set( iv_key = c_id-committer_email
                              iv_val = |Invalid email address| ).
    ENDIF.

    IF zcl_abapgit_utils=>is_valid_email( io_form_data->get( c_id-author_email ) ) = abap_false.
      ro_validation_log->set( iv_key = c_id-author_email
                              iv_val = |Invalid email address| ).
    ENDIF.

    lv_new_branch_name = io_form_data->get( c_id-new_branch_name ).

    " Check if we're on a release branch - if so, new branch is mandatory
    DATA(lv_current_branch) = mi_repo_online->get_selected_branch( ).
    IF lv_current_branch CP |release/{ to_upper( sy-sysid ) }*| AND lv_new_branch_name IS INITIAL.
      ro_validation_log->set( iv_key = c_id-new_branch_name
                              iv_val = |You cannot commit to a release branch| ).
    ENDIF.

    IF lv_new_branch_name IS NOT INITIAL.
      " check if branch already exists
      lt_branches = zcl_abapgit_git_factory=>get_git_transport(
                                          )->branches( mi_repo_online->get_url( )
                                          )->get_branches_only( ).
      IF line_exists( lt_branches[ KEY name_key
                                   name = branch_name_to_internal( lv_new_branch_name ) ] ).
        ro_validation_log->set( iv_key = c_id-new_branch_name
                                iv_val = |Branch already exists| ).
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD zif_abapgit_gui_event_handler~on_event.
    DATA lv_new_branch_name TYPE string.
    DATA lv_main_branch     TYPE string.
    DATA lx_error           TYPE REF TO zcx_abapgit_exception.
    DATA lv_message         TYPE string.
    DATA lv_selected_branch TYPE string.

    mo_form_data = mo_form_util->normalize( ii_event->form_data( ) ).

    CASE ii_event->mv_action.
      WHEN c_event-commit.
        " Initialize application log
        TRY.
            DATA(lv_transport) = get_transport_from_stage( ).
            mv_log_handle = zcl_abapgit_logging_utils=>create_application_log( iv_extnumber = CONV #( lv_transport ) ).
          CATCH zcx_abapgit_exception.
            " Continue even if logging fails
        ENDTRY.

        " Validate form entries before committing
        mo_validation_log = validate_form( mo_form_data ).

        IF mo_validation_log->is_empty( ) = abap_true.

          zcl_abapgit_logging_utils=>write_application_log( iv_log_handle = mv_log_handle
                                                            iv_log_type   = 'S'
                                                            iv_message    = 'Form validation successful'
                                                            iv_detail     = 'Starting commit process' ).

          " new branch fields not needed in commit data
          mo_form_data->strict( abap_false ).

          mo_form_data->to_abap( CHANGING cs_container = ms_commit ).

          REPLACE ALL OCCURRENCES
                  OF cl_abap_char_utilities=>cr_lf
                  IN ms_commit-body
                  WITH cl_abap_char_utilities=>newline.

          " Store the current branch before any operations
          lv_selected_branch = mi_repo_online->get_selected_branch( ).

          " New Branch Name - in the Commit form
          lv_new_branch_name = mo_form_data->get( c_id-new_branch_name ).

          " create new branch and commit to it if branch name is not empty
          IF lv_new_branch_name IS NOT INITIAL.
            " Validate the branch name follows feature/* pattern with uppercase transport
            IF lv_new_branch_name NP |feature/{ to_upper( sy-sysid ) }*|.
              zcl_abapgit_logging_utils=>write_application_log(
                  iv_log_handle = mv_log_handle
                  iv_log_type   = 'E'
                  iv_message    = 'Invalid branch name pattern'
                  iv_detail     = |Branch: { lv_new_branch_name }, Expected pattern: feature/TRANSPORT_NUMBER| ).

              zcx_abapgit_exception=>raise( 'Branch name must follow pattern: feature/TRANSPORT_NUMBER' ).
            ENDIF.

            " Create new branch
            zcl_abapgit_logging_utils=>write_application_log(
                iv_log_handle = mv_log_handle
                iv_log_type   = 'S'
                iv_message    = 'Creating new branch'
                iv_detail     = |Branch: { lv_new_branch_name }, From: { lv_selected_branch }| ).

            lv_new_branch_name = branch_name_to_internal( lv_new_branch_name ).

            " creates a new branch and automatically switches to it
            mi_repo_online->create_branch( lv_new_branch_name ).

            zcl_abapgit_logging_utils=>write_application_log( iv_log_handle = mv_log_handle
                                                              iv_log_type   = 'S'
                                                              iv_message    = 'New branch created successfully'
                                                              iv_detail     = |Internal name: { lv_new_branch_name }| ).
          ENDIF.

          zcl_abapgit_logging_utils=>write_application_log( iv_log_handle = mv_log_handle
                                                            iv_log_type   = 'S'
                                                            iv_message    = 'Starting Git commit'
                                                            iv_detail     = |Title: { ms_commit-comment }| ).

          zcl_abapgit_services_git=>commit( is_commit      = ms_commit
                                            ii_repo_online = mi_repo_online
                                            io_stage       = mo_stage ).

          zcl_abapgit_logging_utils=>write_application_log( iv_log_handle = mv_log_handle
                                                            iv_log_type   = 'S'
                                                            iv_message    = 'Git commit completed successfully'
                                                            iv_detail     = |Files committed: { lines( mt_stage ) }| ).

          " If a new branch was created, switch back to main branch (pattern release/SHA*)
          IF lv_new_branch_name IS NOT INITIAL.
            TRY.
                zcl_abapgit_logging_utils=>write_application_log(
                    iv_log_handle = mv_log_handle
                    iv_log_type   = 'S'
                    iv_message    = 'Finding main branch for PR target'
                    iv_detail     = |Looking for release/{ to_upper( sy-sysid ) }* pattern or main/master| ).

                " Find the release branch
                lv_main_branch = find_main_branch( ).

                IF lv_main_branch IS NOT INITIAL.
                  zcl_abapgit_logging_utils=>write_application_log(
                      iv_log_handle = mv_log_handle
                      iv_log_type   = 'S'
                      iv_message    = 'Switching back to main branch'
                      iv_detail     = |Target branch: { lv_main_branch }| ).

                  " Switch back to main/release branch
                  mi_repo_online->select_branch( lv_main_branch ).

                  zcl_abapgit_logging_utils=>write_application_log(
                      iv_log_handle = mv_log_handle
                      iv_log_type   = 'S'
                      iv_message    = 'Starting automatic PR creation'
                      iv_detail     = |Source: { lv_new_branch_name } -> Target: { lv_main_branch }| ).

                  " Automatically create pull request
                  create_pull_request_auto( iv_source_branch = lv_new_branch_name
                                            iv_target_branch = lv_main_branch
                                            iv_pr_title      = ms_commit-comment
                                            iv_pr_body       = ms_commit-body ).

                  zcl_abapgit_logging_utils=>write_application_log(
                      iv_log_handle = mv_log_handle
                      iv_log_type   = 'S'
                      iv_message    = 'PR automation completed successfully'
                      iv_detail     = 'Process finished' ).

                  lv_message = |Commit successful. Switched back to {
                    zcl_abapgit_git_branch_utils=>get_display_name( lv_main_branch ) }|.
                  MESSAGE lv_message TYPE 'S'.
                ELSE.
                  MESSAGE 'Commit successful. Could not find main branch to switch back to.' TYPE 'W'.
                ENDIF.
              CATCH zcx_abapgit_exception INTO lx_error.
                lv_message = |Commit successful. Error switching back to main branch: { lx_error->get_text( ) }|.
                MESSAGE lv_message TYPE 'W'.
            ENDTRY.
          ELSE.
            " No new branch was created - check if we need to switch back to release branch
            " This handles commits made directly on feature branches or other development branches
            IF is_development_branch( lv_selected_branch ).
              TRY.
                  lv_main_branch = find_main_branch( ).

                  IF lv_main_branch IS NOT INITIAL.
                    zcl_abapgit_logging_utils=>write_application_log(
                        iv_log_handle = mv_log_handle
                        iv_log_type   = 'S'
                        iv_message    = 'Switching back to release branch'
                        iv_detail     = |{ lv_selected_branch } -> { lv_main_branch }| ).

                    mi_repo_online->select_branch( lv_main_branch ).

                    lv_message = |Commit successful. Switched back to {
                      zcl_abapgit_git_branch_utils=>get_display_name( lv_main_branch ) }|.
                    MESSAGE lv_message TYPE 'S'.
                  ELSE.
                    MESSAGE 'Commit successful. Could not find main branch to switch back to.' TYPE 'W'.
                  ENDIF.
                CATCH zcx_abapgit_exception INTO lx_error.
                  lv_message = |Commit successful. Error switching back to release branch: { lx_error->get_text( ) }|.
                  MESSAGE lv_message TYPE 'W'.
              ENDTRY.
            ELSE.
              MESSAGE 'Commit was successful' TYPE 'S'.
            ENDIF.
          ENDIF.

          rs_handled-state = zcl_abapgit_gui=>c_event_state-go_back_to_bookmark.
        ELSE.
          rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
        ENDIF.
      WHEN OTHERS.
        ASSERT 1 = 1.
    ENDCASE.
  ENDMETHOD.

  METHOD zif_abapgit_gui_renderable~render.
    register_handlers( ).

    IF mo_form_util->is_empty( mo_form_data ) = abap_true.
      get_defaults( ).
    ENDIF.

    ri_html = NEW zcl_abapgit_html( ).

    ri_html->add( '<div class="repo">' ).
    ri_html->add( '<div id="top" class="paddings">' ).
    ri_html->add( zcl_abapgit_gui_chunk_lib=>render_repo_top( iv_show_commit = abap_false
                                                              ii_repo        = mi_repo_online ) ).
    ri_html->add( '</div>' ).

    ri_html->add( '<div id="stage-summary" class="dialog w800px paddings">' ).
    ri_html->add( render_stage_summary( ) ).
    ri_html->add( '</div>' ).

    ri_html->add( mo_form->render( iv_form_class     = 'w800px'
                                   io_values         = mo_form_data
                                   io_validation_log = mo_validation_log ) ).

    ri_html->add( '<div id="stage-details" class="dialog w800px">' ).
    ri_html->add( render_stage_details( ) ).
    ri_html->add( '</div>' ).
    ri_html->add( '</div>' ).
  ENDMETHOD.

  METHOD get_transport_from_stage.
    DATA lo_transport_filter TYPE REF TO zcl_abapgit_object_filter_tran.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA lv_package          TYPE tadir-devclass.
    DATA lt_transport_range  TYPE zif_abapgit_definitions=>ty_trrngtrkor_tt.
    DATA ls_stage_item       TYPE zif_abapgit_definitions=>ty_stage.
    DATA ls_transport_entry  LIKE LINE OF lt_transport_range.

    " If we have a transport filter, get the transport from it (proper approach)
    IF mi_obj_filter IS BOUND.
      TRY.
          lo_transport_filter ?= mi_obj_filter.
          lo_transport_filter->get_filter_values( IMPORTING ev_package  = lv_package
                                                            et_r_trkorr = lt_transport_range ).

          " Get the transport from user selection (should be only one due to validation)
          READ TABLE lt_transport_range INTO ls_transport_entry INDEX 1.
          IF sy-subrc = 0.
            rv_transport = ls_transport_entry-low.
            " Ensure transport doesn't exceed SAP length limits
            IF strlen( rv_transport ) > 20.
              rv_transport = rv_transport(20).
            ENDIF.
            RETURN.
          ENDIF.
        CATCH cx_sy_move_cast_error.
          " Not a transport filter, continue with fallback
      ENDTRY.
    ENDIF.

    " Fallback approach for direct commits (no transport filter)
    READ TABLE mt_stage INTO ls_stage_item INDEX 1.
    IF sy-subrc = 0 AND ls_stage_item-status-obj_type IS NOT INITIAL.
      " Get any transport containing this object (fallback approach)
      SELECT SINGLE trkorr FROM e071
        WHERE pgmid    = 'R3TR'
          AND object   = @ls_stage_item-status-obj_type
          AND obj_name = @ls_stage_item-status-obj_name
        INTO @rv_transport.

      IF sy-subrc <> 0.
        rv_transport = 'NO_TRANSPORT'.
      ENDIF.
    ELSE.
      " Fallback if no staged objects
      rv_transport = 'DIRECT_COMMIT'.
    ENDIF.

    " Ensure transport doesn't exceed SAP length limits
    IF strlen( rv_transport ) > 20.
      rv_transport = rv_transport(20).
    ENDIF.
  ENDMETHOD.

  METHOD get_transport_description.
    DATA lv_as4text TYPE e07t-as4text.

    " Return empty if no transport provided
    IF iv_transport IS INITIAL OR iv_transport = 'DIRECT_COMMIT' OR iv_transport = 'NO_TRANSPORT'.
      RETURN.
    ENDIF.

    " Get transport description from E07T table
    SELECT SINGLE as4text FROM e07t
      WHERE trkorr = @iv_transport
        AND langu  = @sy-langu
      INTO @lv_as4text.

    " If not found in current language, try English
    IF sy-subrc <> 0.
      SELECT SINGLE as4text FROM e07t
        WHERE trkorr = @iv_transport
          AND langu  = @sy-langu
        INTO @lv_as4text.
    ENDIF.

    " Return description (max 60 chars as per SAP standard)
    rv_description = lv_as4text.
  ENDMETHOD.

  METHOD escape_json_string.
    DATA lv_pos  TYPE i.
    DATA lv_char TYPE c LENGTH 1.
    DATA lv_temp TYPE string.

    " Handle empty input
    IF iv_input IS INITIAL.
      rv_output = iv_input.
      RETURN.
    ENDIF.

    rv_output = iv_input.

    " Escape JSON special characters (order matters - backslash first!)
    " 1. Escape backslashes first (must be first to avoid double-escaping)
    REPLACE ALL OCCURRENCES OF '\' IN rv_output WITH '\\'.

    " 2. Escape double quotes
    REPLACE ALL OCCURRENCES OF '"' IN rv_output WITH '\"'.

    " 3. Handle newline variations (common in multiline text)
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN rv_output WITH '\n'.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN rv_output WITH '\n'.

    " 4. Handle other control characters
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN rv_output WITH '\t'.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>backspace IN rv_output WITH '\b'.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>form_feed IN rv_output WITH '\f'.

    " 5. Handle problematic characters by scanning and replacing
    CLEAR lv_temp.
    DO strlen( rv_output ) TIMES.
      lv_pos = sy-index - 1.
      lv_char = rv_output+lv_pos(1).

      " Check for problematic characters and replace with safe alternatives
      CASE lv_char.
        WHEN cl_abap_char_utilities=>vertical_tab.
          " Replace vertical tab with regular tab
          lv_temp = |{ lv_temp }\\t|.
        WHEN OTHERS.
          " Keep the character as is
          lv_temp = lv_temp && lv_char.
      ENDCASE.
    ENDDO.

    rv_output = lv_temp.
  ENDMETHOD.

  METHOD is_development_branch.
    " Determine if a branch is a development/feature branch (not main/release)
    DATA lv_branch_display TYPE string.

    " Get the display name (without refs/heads/ prefix)
    lv_branch_display = zcl_abapgit_git_branch_utils=>get_display_name( iv_branch_name ).

    " Check if this is NOT a main/release branch
    " Main/release branches: main, master, release/*
    rv_is_development = xsdbool( NOT ( lv_branch_display = 'main'
                                    OR lv_branch_display = 'master'
                                    OR lv_branch_display CP |release/*| ) ).
  ENDMETHOD.
ENDCLASS.

