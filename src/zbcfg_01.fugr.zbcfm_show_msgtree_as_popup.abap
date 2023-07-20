FUNCTION zbcfm_show_msgtree_as_popup.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     REFERENCE(IV_JSON) TYPE  STRING
*"     REFERENCE(IV_PRETTY_NAME) TYPE  /UI2/CL_JSON=>PRETTY_NAME_MODE
*"       OPTIONAL
*"     REFERENCE(IV_DDIC_TYPE) TYPE  ZTBS0002-ZIFINPUT_STRUC OPTIONAL
*"     REFERENCE(IV_HIDE_CONTROLLER) TYPE  XSDBOOLEAN OPTIONAL
*"----------------------------------------------------------------------
  IF iv_json EQ |""| OR iv_json = '{}'.
    MESSAGE s001(zbc_msg_001) DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  "查找用户默认设置，看是否展示所有字段
  GET PARAMETER ID 'ZSHOW_ALL_MTFIELDS' FIELD gv_is_tree_show_all_fields.

  gv_json = iv_json.
  gv_pretty_name = iv_pretty_name.
  gv_ddic_type = iv_ddic_type.
  gv_hide_controller = iv_hide_controller.
  CALL SCREEN 9001 STARTING AT 5 10.
ENDFUNCTION.
