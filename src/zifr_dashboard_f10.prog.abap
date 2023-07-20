*&---------------------------------------------------------------------*
*& Include zifr_dashboard_f10
*&---------------------------------------------------------------------*
MODULE status_9001 OUTPUT.
  SET PF-STATUS 'STATUS_9001'.
  SET TITLEBAR 'TITLE_9001'.

  IF go_controller->is_main_tree_bound( ) = abap_false.
    go_controller->run_detail( ).
  ENDIF.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9001 INPUT.
  CASE gv_okcode.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.

    WHEN OTHERS.
      go_controller->handler_9001_usercommand( gv_okcode ).
  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  EXIT_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_9001 INPUT.
  "CLEAN UP
  LEAVE TO SCREEN 0.
ENDMODULE.
