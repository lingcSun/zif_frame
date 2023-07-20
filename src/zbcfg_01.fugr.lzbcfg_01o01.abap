*----------------------------------------------------------------------*
***INCLUDE LZBCFG_001O01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_9001 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9001 OUTPUT.
  SET PF-STATUS 'ZSTATUS_9001'.
  SET TITLEBAR 'ZTITLE_9001'.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module TREE_DISPLAY OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE tree_display OUTPUT.
  IF go_msg_tree IS NOT BOUND.
    go_msg_tree = NEW zcl_msg_tree( iv_mode = zcl_msg_tree=>c_mode_cust_container
                                    iv_repid = sy-repid
                                    iv_dynnr = sy-dynnr
                                    iv_cust_container_nam = 'CONTAINER_9001'
                                    iv_hide_controller = gv_hide_controller ).
  ENDIF.

  TRY .
    IF gv_is_show_json = abap_true.
      "增加当点击按钮切换显示方式时，从树/HTML切换
      CALL TRANSFORMATION sjson2html SOURCE XML gv_json
                                     RESULT XML DATA(html).
      cl_abap_browser=>show_html( html_string = cl_abap_codepage=>convert_from( html )
                                  container   = go_msg_tree->get_container( ) ).
    ELSE.
      cl_abap_browser=>close_browser( ).  "需要关闭HTML Browser，才能正常显示Tree。
      go_msg_tree->show_empty_fields( gv_is_tree_show_all_fields ).
      go_msg_tree->display_from_json( iv_json        = gv_json
                                      iv_ddic_type   = SWITCH #( gv_is_tree_show_all_fields
                                                                   WHEN abap_true THEN gv_ddic_type
                                                                   ELSE '' )
                                      iv_pretty_name = gv_pretty_name ).
    ENDIF.
  CATCH zcx_msg_tree INTO DATA(lo_x_msgtree).
    DATA(lv_msg) = lo_x_msgtree->get_text( ).
    MESSAGE lv_msg TYPE 'E' DISPLAY LIKE 'S'.
  ENDTRY.
ENDMODULE.
