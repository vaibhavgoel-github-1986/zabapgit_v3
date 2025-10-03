CLASS lhc_pullrequest DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR pullrequest RESULT result.

ENDCLASS.


CLASS lhc_pullrequest IMPLEMENTATION.
  METHOD get_instance_authorizations.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA update_requested TYPE abap_bool.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA delete_requested TYPE abap_bool.
    DATA update_granted   TYPE abap_bool.
    DATA delete_granted   TYPE abap_bool.

    READ ENTITIES OF zi_pullrequest IN LOCAL MODE
         ENTITY pullrequest
         FIELDS ( parentrequest )
         WITH CORRESPONDING #( keys )
         RESULT DATA(lt_pull_requests)
         FAILED failed.

    IF lt_pull_requests IS INITIAL.
      RETURN.
    ENDIF.

    update_requested = COND #( WHEN requested_authorizations-%update = if_abap_behv=>mk-on
                               THEN abap_true
                               ELSE abap_false ).

    delete_requested = COND #( WHEN requested_authorizations-%delete = if_abap_behv=>mk-on
                               THEN abap_true
                               ELSE abap_false ).

    update_granted = abap_true.
    delete_granted = abap_true.

    APPEND VALUE #( LET upd_auth = COND #( WHEN update_granted = abap_true
                                           THEN if_abap_behv=>auth-allowed
                                           ELSE if_abap_behv=>auth-unauthorized )
                        del_auth = COND #( WHEN delete_granted = abap_true
                                           THEN if_abap_behv=>auth-allowed
                                           ELSE if_abap_behv=>auth-unauthorized )
                    IN  %tky         = lt_pull_requests[ 1 ]-%tky
                        %update      = upd_auth
                        %delete      = del_auth )
           TO result.
  ENDMETHOD.
ENDCLASS.
