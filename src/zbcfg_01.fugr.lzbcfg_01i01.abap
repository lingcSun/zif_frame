*----------------------------------------------------------------------*
***INCLUDE LZBCFG_01I01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9001 INPUT.
  CASE sy-ucomm.
    WHEN 'OKAY' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN 'CHANGE'.
      IF gv_is_show_json = abap_true.
        gv_is_show_json = abap_false.
      ELSE.
        gv_is_show_json = abap_true.
      ENDIF.
    WHEN 'CHG_FIELDS'.
      IF gv_is_tree_show_all_fields = abap_true.
        gv_is_tree_show_all_fields = abap_false.
      ELSE.
        gv_is_tree_show_all_fields = abap_true.
      ENDIF.
    WHEN OTHERS.
      "Pass
  ENDCASE.
ENDMODULE.
