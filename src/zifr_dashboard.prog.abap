*&---------------------------------------------------------------------*
*& Report zifr_dashboard
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zifr_dashboard MESSAGE-ID zif_msg_001.

*--------------------------------------------------------------------------------*
* DATA DEFINITION
*--------------------------------------------------------------------------------*
DATA: gs_if_data TYPE zift_data.
DATA: gv_okcode    TYPE sy-ucomm,
      gv_no_auth   TYPE c,
      gv_user_type TYPE usertypen.

TABLES: ztbs0002.

*--------------------------------------------------------------------------------*
* SELECTION - SCREEN
*--------------------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK bk1 WITH FRAME TITLE TEXT-b01.
  SELECT-OPTIONS:
    s_ifnum    FOR gs_if_data-zif_num MATCHCODE OBJECT zhif_num MODIF ID m0 MEMORY ID zif_num,
    s_ifguid   FOR gs_if_data-zif_guid,
    s_addkey   FOR gs_if_data-zif_add_key.
SELECTION-SCREEN END OF BLOCK bk1.

SELECTION-SCREEN BEGIN OF BLOCK bk2 WITH FRAME TITLE TEXT-b02.
  SELECT-OPTIONS:
    s_cdate    FOR gs_if_data-zif_cdate DEFAULT sy-datum,
    s_ctime    FOR gs_if_data-zif_ctime.
SELECTION-SCREEN END OF BLOCK bk2.

SELECTION-SCREEN BEGIN OF BLOCK bk3 WITH FRAME TITLE TEXT-b03.
  SELECT-OPTIONS:
    s_status   FOR gs_if_data-zif_status.
SELECTION-SCREEN END OF BLOCK bk3.

SELECTION-SCREEN BEGIN OF BLOCK bk4 WITH FRAME TITLE TEXT-b04.
  SELECT-OPTIONS:
    s_zres1    FOR gs_if_data-zres1,
    s_zres2    FOR gs_if_data-zres2,
    s_zres3    FOR gs_if_data-zres3,
    s_zres4    FOR gs_if_data-zres4.
SELECTION-SCREEN END OF BLOCK bk4.

SELECTION-SCREEN BEGIN OF BLOCK bk5 WITH FRAME TITLE TEXT-b05.
  PARAMETERS: r_detail RADIOBUTTON GROUP g1 DEFAULT 'X',
              r_overv  RADIOBUTTON GROUP g1.
  SELECTION-SCREEN SKIP 1.
  PARAMETERS r_new AS CHECKBOX MODIF ID m1.   "Added by Xiangyi 2022/5/25
SELECTION-SCREEN END OF BLOCK bk5.

SELECTION-SCREEN BEGIN OF BLOCK bk0 WITH FRAME TITLE TEXT-b00.
  SELECT-OPTIONS:
    s_ifsys    FOR ztbs0002-zifsystem.
SELECTION-SCREEN END OF BLOCK bk0.

*--------------------------------------------------------------------------------*
* INCLUDE
*--------------------------------------------------------------------------------*
INCLUDE: zifr_dashboard_f01,
         zifr_dashboard_f02.  "New Version of platform Added by Xiangyi 2022/5/25
*--------------------------------------------------------------------------------*
* INITIALIZATION
*--------------------------------------------------------------------------------*
INITIALIZATION.

  PERFORM frm_init.
*--------------------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT
*--------------------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

  IF gv_no_auth EQ abap_true.
    LOOP AT SCREEN.
      IF screen-group1 EQ 'M0'.
        screen-input = '0'.
      ELSEIF screen-group1 EQ 'M1'.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.
*--------------------------------------------------------------------------------*
* START-OF-SELECTION
*--------------------------------------------------------------------------------*
START-OF-SELECTION.

  IF r_new = abap_false.

    NEW cl_main( )->start_dashboard( ).
*---- Added by Xiangyi 2022/5/25 Begin
  ELSE.

    go_controller = NEW #( it_rg_ifnum  = s_ifnum[]
                           it_rg_ifguid = s_ifguid[]
                           it_rg_addkey = s_addkey[]
                           it_rg_cdate  = s_cdate[]
                           it_rg_ctime  = s_ctime[]
                           it_rg_status = s_status[]
                           it_rg_zres1  = s_zres1[]
                           it_rg_zres2  = s_zres2[]
                           it_rg_zres3  = s_zres3[]
                           it_rg_zres4  = s_zres4[] ).
    CASE abap_true.
      WHEN r_detail.
        CALL SCREEN 9001.
      WHEN r_overv.
        go_controller->run_overview( ).
    ENDCASE.

  ENDIF.
*---- Added by Xiangyi 2022/5/25 End

*&---------------------------------------------------------------------*
*& Form FRM_init
*&---------------------------------------------------------------------*
*& "检查开放权限用户参数配置相关
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_init .

  SELECT SINGLE lic_type
    FROM usr06
    INTO @gv_user_type
    WHERE bname EQ @sy-uname.

  IF gv_user_type EQ '06'."工作台用户
    gv_no_auth = abap_false.
  ELSE.
    gv_no_auth = abap_true.
  ENDIF.

  GET PARAMETER ID 'ZIF_NUM' FIELD DATA(lv_pid).

  IF lv_pid IS INITIAL.
    CHECK gv_no_auth EQ abap_true.
    MESSAGE TEXT-e01 TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE PROGRAM.
  ELSE.
    SPLIT lv_pid AT space INTO TABLE DATA(lt_pid).
    s_ifnum[] = VALUE #( FOR ls_pid IN lt_pid sign = 'I' option = 'EQ' ( low = ls_pid ) ).
  ENDIF.

ENDFORM.
