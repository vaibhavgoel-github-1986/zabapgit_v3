CLASS zcl_bp_i_pullrequest DEFINITION
  PUBLIC
  FINAL
  FOR BEHAVIOR OF zi_pullrequest.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF pr_status,
        open      TYPE zde_pr_status VALUE 'OPEN',
        closed    TYPE zde_pr_status VALUE 'CLOSED',
        merged    TYPE zde_pr_status VALUE 'MERGED',
        exception TYPE zde_pr_status VALUE 'EXCEPTION',
      END OF pr_status.

ENDCLASS.



CLASS zcl_bp_i_pullrequest IMPLEMENTATION.

ENDCLASS.