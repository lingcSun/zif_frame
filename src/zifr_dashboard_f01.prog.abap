*&---------------------------------------------------------------------*
*& Include zifr_dashboard_f01
*&---------------------------------------------------------------------*
DEFINE set_alv_column_text.

  zcl_abap_comm=>salv_column_text( EXPORTING im_column_name = &1
                                             im_short_text  = CONV #( &2 )
                                             im_r_salv      = &3 ).

END-OF-DEFINITION.

*----------------------------------------------------------------------*
*   CLASS Definition
*----------------------------------------------------------------------*
CLASS cl_main DEFINITION.

  PUBLIC SECTION.
    METHODS:
*----- 启动处理平台
      start_dashboard,

*----- 处理详细列表热点单击事件
      on_link_click_detail  FOR EVENT link_click OF cl_salv_events_table
        IMPORTING
          row
          column,

*----- 处理接口总览热点单击事件
      on_link_click_overview  FOR EVENT link_click OF cl_salv_events_table
        IMPORTING
          row
          column,

      "处理：自定义按钮被点击时
      handle_user_command_det  FOR EVENT added_function    OF cl_salv_events
        IMPORTING
          e_salv_function,
*--------add by chenxinyu 18.11.2022 13:48:55 增加刷新
      "处理：自定义按钮被点击时
      handle_user_command_overview FOR EVENT added_function    OF cl_salv_events
        IMPORTING
          e_salv_function.
  PROTECTED SECTION.

    TYPES:
      BEGIN OF typ_alv_detail,
        zif_num          TYPE zift_data-zif_num,
        zifnam           TYPE ztbs0002-zifnam,
        zif_guid         TYPE zift_data-zif_guid,
        zif_add_key      TYPE zift_data-zif_add_key,
        zifsystem        TYPE ztbs0002-zifsystem,
        zif_cdate        TYPE zift_data-zif_cdate,
        zif_ctime        TYPE zift_data-zif_ctime,
        zif_fdate        TYPE zift_data-zif_fdate,
        zif_ftime        TYPE zift_data-zif_ftime,
        zres1            TYPE zift_data-zres1,
        zres2            TYPE zift_data-zres2,
        zres3            TYPE zift_data-zres3,
        zres4            TYPE zift_data-zres4,
        zif_status       TYPE zift_data-zif_status,
        status_icon      TYPE icon_d,
        zif_repeat       TYPE zift_data-zif_repeat,
        zif_repeat_prio  TYPE zift_data-zif_repeat_prio,
        zif_proc_counter TYPE i,
        input_icon       TYPE icon_d,
        output_icon      TYPE icon_d,
        msgid            TYPE zift_log-msgid,
        msgno            TYPE zift_log-msgno,
        msgty            TYPE zift_log-msgty,
        msgtxt           TYPE char255,
      END OF typ_alv_detail,

      BEGIN OF typ_alv_overview,
        zif_num   TYPE zift_data-zif_num,
        zifnam    TYPE ztbs0002-zifnam,
        zifsystem TYPE ztbs0002-zifsystem,
        zifdirect TYPE ztbs0002-zifdirect,
        zifpmode  TYPE ztbs0002-zifpmode,
        total     TYPE i,
        init      TYPE i,
        success   TYPE i,
        error     TYPE i,
        delete    TYPE i,
        repeat    TYPE i,
      END OF typ_alv_overview,

      typ_t_alv_detail   TYPE STANDARD TABLE OF typ_alv_detail,
      typ_t_alv_overview TYPE STANDARD TABLE OF typ_alv_overview.

    DATA:
      gt_alv_detail      TYPE STANDARD TABLE OF typ_alv_detail,
      gt_alv_detail_show TYPE STANDARD TABLE OF typ_alv_detail,
      gt_alv_overview    TYPE STANDARD TABLE OF typ_alv_overview,
      go_alv_detail      TYPE REF TO cl_salv_table,
      go_alv_overview    TYPE REF TO cl_salv_table,
      gt_if_config       TYPE STANDARD TABLE OF ztbs0002,
      gs_stable          TYPE lvc_s_stbl.

    METHODS:
*----- 获取接口数据和接口日志
      get_data,

      display_detail_data,

      refresh_detail,
*--------add by chenxinyu 18.11.2022 13:49:55
      refresh_overview,
*--------add by chenxinyu 18.11.2022 13:49:57
      process_data,

      display_overview_data.

  PRIVATE SECTION.
    METHODS get_if_input_ddic_type
      IMPORTING
        iv_ifnum         TYPE zifnum
      RETURNING
        VALUE(rv_result) TYPE ZTBS0002-ZIFINPUT_STRUC.

    METHODS get_if_output_ddic_type
      IMPORTING
        iv_ifnum         TYPE zifnum
      RETURNING
        VALUE(rv_result) TYPE ZTBS0002-ZIFOUTPUT_STRUC.

ENDCLASS.

*----------------------------------------------------------------------*
*   CLASS IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS cl_main IMPLEMENTATION.

*----------------------------------------------------------------------*
*   METHOD start_dashboard
*----------------------------------------------------------------------*
  METHOD start_dashboard.

    get_data( ).

    IF gt_alv_detail IS INITIAL.
      MESSAGE TEXT-001 TYPE 'S'.
      LEAVE LIST-PROCESSING.
    ENDIF.

    IF r_detail = abap_true.

      gt_alv_detail_show = gt_alv_detail.

      display_detail_data( ).

    ELSEIF r_overv = abap_true.

      process_data( ).

      display_overview_data( ).

    ENDIF.

  ENDMETHOD.

*----------------------------------------------------------------------*
*   METHOD get_data
*----------------------------------------------------------------------*
  METHOD get_data.

*----- 获取每个接口的执行次数    ---- Commented out by Xiangyi 2022/6/20 -- 考虑到效率问题，调整为inner join结果内表
*    SELECT zif_num,
*           zif_guid,
*           zif_add_key,
*           zif_proc_counter,
*           zif_msg_counter,
*           msgid,
*           msgno,
*           msgty,
*           msgtxt
*      INTO TABLE @DATA(lt_proc_counter)
*      FROM zift_log
*     WHERE zif_num     IN @s_ifnum
*       AND zif_guid    IN @s_ifguid
*       AND zif_add_key IN @s_addkey.
*    SORT lt_proc_counter BY zif_num          ASCENDING
*                            zif_guid         ASCENDING
*                            zif_add_key      ASCENDING
*                            zif_proc_counter DESCENDING
*                            zif_msg_counter  DESCENDING.
*
*    DELETE ADJACENT DUPLICATES FROM lt_proc_counter COMPARING zif_num
*                                                              zif_guid
*                                                              zif_add_key.

*----- 获取接口数据和配置
    SELECT FROM zift_data
*           LEFT OUTER JOIN @lt_proc_counter AS counter           "Commented out by Xaingyi 2022/6/20
*                 ON counter~zif_num     = zift_data~zif_num      "Commented out by Xaingyi 2022/6/20
*                AND counter~zif_guid    = zift_data~zif_guid     "Commented out by Xaingyi 2022/6/20
*                AND counter~zif_add_key = zift_data~zif_add_key  "Commented out by Xaingyi 2022/6/20
           LEFT OUTER JOIN ztbs0002
                 ON ztbs0002~zifnum   = zift_data~zif_num
         FIELDS zift_data~zif_num,
                zifnam,
                zift_data~zif_guid,
                zift_data~zif_add_key,
                zifsystem,
                zif_cdate,
                zif_ctime,
                zif_fdate,
                zif_ftime,
                zres1,
                zres2,
                zres3,
                zres4,
                zif_status,
                zif_repeat,
                zift_data~zif_repeat_prio
*                zif_proc_counter  "Commented out by Xaingyi 2022/6/20
*                msgid,            "Commented out by Xaingyi 2022/6/20
*                msgno,            "Commented out by Xaingyi 2022/6/20
*                msgty,            "Commented out by Xaingyi 2022/6/20
*                msgtxt            "Commented out by Xaingyi 2022/6/20
     WHERE zift_data~zif_num     IN @s_ifnum
       AND zift_data~zif_guid    IN @s_ifguid
       AND zift_data~zif_add_key IN @s_addkey
       AND zifsystem             IN @s_ifsys
       AND zif_cdate             IN @s_cdate
       AND zif_ctime             IN @s_ctime
       AND zif_status            IN @s_status
       AND zres1                 IN @s_zres1
       AND zres2                 IN @s_zres2
       AND zres3                 IN @s_zres3
       AND zres4                 IN @s_zres4
      INTO CORRESPONDING FIELDS OF TABLE @gt_alv_detail.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    SORT gt_alv_detail BY zif_num
                          zif_cdate
                          zif_ctime
                          zif_guid
                          zif_add_key.

    "Added by Xiangyi 2022/6/20 --- Begin --- 增加消息日志的查询
    SELECT FROM @gt_alv_detail AS a
      INNER JOIN zift_log AS b ON  a~zif_num = b~zif_num
                               AND a~zif_guid = b~zif_guid
                               AND a~zif_add_key = b~zif_add_key
      FIELDS
        a~zif_num,
        a~zif_guid,
        a~zif_add_key,
        MAX( b~zif_proc_counter ) AS zif_proc_counter,
        MAX( b~zif_msg_counter ) AS zif_msg_counter,
        b~msgid,
        b~msgno,
        b~msgty,
        b~msgtxt
      GROUP BY
        a~zif_num,
        a~zif_guid,
        a~zif_add_key,
        b~msgid,
        b~msgno,
        b~msgty,
        b~msgtxt
    INTO TABLE @DATA(lt_message).

    SORT lt_message BY zif_num
                       zif_guid
                       zif_add_key
                       zif_proc_counter DESCENDING
                       zif_msg_counter DESCENDING.
    "Added by Xiangyi 2022/6/20 --- End ---

    LOOP AT gt_alv_detail REFERENCE INTO DATA(lo_detail).
      lo_detail->input_icon  = icon_wd_inbound_plug.
      lo_detail->output_icon = icon_wd_outbound_plug.

      lo_detail->status_icon = SWITCH #( lo_detail->zif_status WHEN 'S' THEN icon_led_green
                                                               WHEN 'E' THEN icon_led_red
                                                               WHEN 'D' THEN icon_delete
                                                               WHEN ''  THEN icon_wd_radio_button_empty ).

      "Added by Xiangyi 2022/6/20 --- Begin --- 增加消息日志的查询
      READ TABLE lt_message REFERENCE INTO DATA(lr_message)
        WITH KEY zif_num = lo_detail->zif_num
                 zif_guid = lo_detail->zif_guid
                 zif_add_key = lo_detail->zif_add_key
                 BINARY SEARCH.
      IF sy-subrc = 0.
        lo_detail->zif_proc_counter = lr_message->zif_proc_counter.
        lo_detail->msgid = lr_message->msgid.
        lo_detail->msgno = lr_message->msgno.
        lo_detail->msgty = lr_message->msgty.
        lo_detail->msgtxt = lr_message->msgtxt.
      ENDIF.
      "Added by Xiangyi 2022/6/20 --- End ---

    ENDLOOP.

    SELECT zifnum
           zifnam
           zifsystem
           zifdirect
           zifpmode
           zifprocess_class
      INTO CORRESPONDING FIELDS OF TABLE gt_if_config
      FROM ztbs0002.

  ENDMETHOD.

*----------------------------------------------------------------------*
*   METHOD display_detail_data
*----------------------------------------------------------------------*
  METHOD display_detail_data.

    DATA: lo_column  TYPE REF TO cl_salv_column_table.

    DEFINE set_column_key.
      lo_column ?= go_alv_detail->get_columns( )->get_column( &1 ).
      lo_column->set_key( abap_true ).
    END-OF-DEFINITION.

    DEFINE set_column_hotspot.
      lo_column ?= go_alv_detail->get_columns( )->get_column( &1 ).
      lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
    END-OF-DEFINITION.

    DEFINE set_column_f4.
      zcl_abap_comm=>salv_column_f4( im_r_salv     = go_alv_detail
                                     im_filename   = &1
                                     im_ref_table  = &2
                                     im_ref_field  = &1 ).
    END-OF-DEFINITION.

    FREE go_alv_detail.

    go_alv_detail = zcl_abap_comm=>salv_factory( EXPORTING im_column_optimized = 'X'
                                                           im_select_mode      = if_salv_c_selection_mode=>cell
                                                           im_save_layout      = 'X'
                                                 CHANGING  im_t_table          = gt_alv_detail_show ).
    TRY .
        set_alv_column_text: 'ZIF_CDATE'         TEXT-f01    go_alv_detail,
                             'ZIF_CTIME'         TEXT-f02    go_alv_detail,
                             'ZIF_FDATE'         TEXT-f03    go_alv_detail,
                             'ZIF_FTIME'         TEXT-f04    go_alv_detail,
                             'ZIF_PROC_COUNTER'  TEXT-f05    go_alv_detail,
                             'INPUT_ICON'        TEXT-f06    go_alv_detail,
                             'OUTPUT_ICON'       TEXT-f07    go_alv_detail,
                             'STATUS_ICON'       TEXT-f08    go_alv_detail.

        set_column_key: 'ZIF_NUM',
                        'ZIFNAM',
                        'ZIF_GUID',
                        'ZIF_ADD_KEY'.

        set_column_hotspot: 'ZIF_PROC_COUNTER',
                            'INPUT_ICON',
                            'OUTPUT_ICON'.

        set_column_f4: 'ZIF_STATUS'       'ZIFT_DATA',
                       'ZIF_REPEAT_PRIO'  'ZIFT_DATA'.

        go_alv_detail->get_layout( )->set_key( VALUE #( report = sy-cprog
                                                        handle = '001' ) ).

      CATCH cx_salv_not_found.

    ENDTRY.

    "设置pf status
    go_alv_detail->set_screen_status(
      pfstatus      = 'ZSTATUS_OLD'
      report        = sy-repid
      set_functions = go_alv_detail->c_functions_all ).

    "隐藏function
*    "1.Simple one:
*    go_alv_detail->get_functions( )->set_function( name = 'REPEAT' boolean = abap_false ).
    "2.More complex:
    DATA(lt_function_list) = go_alv_detail->get_functions( )->get_functions( ).
    LOOP AT lt_function_list INTO DATA(ls_function_list).
      IF gv_user_type EQ '07'."企业人力资源用户
        IF ls_function_list-r_function->get_name( ) = 'REPEAT' OR
           ls_function_list-r_function->get_name( ) = 'COMPLETE' OR
           ls_function_list-r_function->get_name( ) = 'DELETE'.
          ls_function_list-r_function->set_visible( abap_false ).
        ENDIF.
      ELSEIF gv_user_type EQ '06'."工作台用户
      ELSE."操作用户
        IF ls_function_list-r_function->get_name( ) = 'COMPLETE' OR
           ls_function_list-r_function->get_name( ) = 'DELETE'.
          ls_function_list-r_function->set_visible( abap_false ).
        ENDIF.
      ENDIF.
    ENDLOOP.

    go_alv_detail->get_display_settings( )->set_striped_pattern( cl_salv_display_settings=>true ).

    SET HANDLER on_link_click_detail    FOR go_alv_detail->get_event( ).
    SET HANDLER handle_user_command_det FOR go_alv_detail->get_event( ).

    go_alv_detail->display( ).

  ENDMETHOD.

*----------------------------------------------------------------------*
*   METHOD on_link_click_detail
*----------------------------------------------------------------------*
  METHOD on_link_click_detail.

    DATA: ls_if_data  TYPE zift_data.

    ASSIGN gt_alv_detail_show[ row ] TO FIELD-SYMBOL(<ls_detail>).

    CHECK sy-subrc = 0.

    IF column = 'INPUT_ICON' OR column = 'OUTPUT_ICON'.
      SELECT SINGLE * INTO ls_if_data
                      FROM zift_data
                     WHERE zif_num     = <ls_detail>-zif_num
                       AND zif_guid    = <ls_detail>-zif_guid
                       AND zif_add_key = <ls_detail>-zif_add_key.
    ENDIF.

    CASE column.
      WHEN 'INPUT_ICON'.
        IF ls_if_data-zif_input IS NOT INITIAL.
          zcl_abap_comm=>display_json_msgtree_popup( iv_json        = ls_if_data-zif_input
                                                     iv_ddic_type   = get_if_input_ddic_type( ls_if_data-zif_num ) ).
*                                                     iv_pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).
        ELSE.
          MESSAGE s007.
        ENDIF.

      WHEN 'OUTPUT_ICON'.
        IF ls_if_data-zif_output IS NOT INITIAL.
          zcl_abap_comm=>display_json_msgtree_popup( iv_json        = ls_if_data-zif_output
                                                     iv_ddic_type   = get_if_output_ddic_type( ls_if_data-zif_num ) ).
*                                                     iv_pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).
        ELSE.
          MESSAGE s007.
        ENDIF.

      WHEN 'ZIF_PROC_COUNTER'.

        SELECT * INTO TABLE @DATA(lt_if_log)
                      FROM zift_log
                     WHERE zif_num     = @<ls_detail>-zif_num
                       AND zif_guid    = @<ls_detail>-zif_guid
                       AND zif_add_key = @<ls_detail>-zif_add_key
                  ORDER BY zif_num,
                           zif_guid,
                           zif_add_key,
                           zif_proc_counter,
                           zif_msg_counter.

        DATA(lo_alv_log) = zcl_abap_comm=>salv_factory( EXPORTING im_column_optimized = 'X'
                                                        CHANGING  im_t_table          = lt_if_log ).
        lo_alv_log->set_screen_popup(
                    start_column = 10
                    end_column   = 150
                    start_line   = 1
                    end_line     = 30 ).

        lo_alv_log->display( ).

      WHEN OTHERS.

    ENDCASE.

  ENDMETHOD.

*----------------------------------------------------------------------*
*   METHOD handle_user_command_det
*----------------------------------------------------------------------*
  METHOD handle_user_command_det.

    DATA: lt_alv_detail TYPE typ_t_alv_detail,
          lv_question   TYPE char128,
          lv_answer     TYPE c,
          ls_if_data    TYPE zift_data,
          lv_class      TYPE string,
          lv_method     TYPE string,
          lo_instance   TYPE REF TO object.

    CASE e_salv_function.
      WHEN 'REPEAT' OR 'DELETE' OR 'COMPLETE'.    "重处理和删除可公用代码

        DATA(lt_sel_rows) = go_alv_detail->get_selections( )->get_selected_rows( ).

        IF lt_sel_rows IS INITIAL.
          MESSAGE w005.
          RETURN.
        ENDIF.

        LOOP AT lt_sel_rows INTO DATA(ls_row).
          APPEND gt_alv_detail_show[ ls_row ] TO lt_alv_detail.
        ENDLOOP.

        DELETE lt_alv_detail WHERE zif_status = 'D' OR zif_status = 'S'. "过滤状态是D和S的记录

        IF e_salv_function = 'REPEAT'.
          DELETE lt_alv_detail WHERE zif_repeat = ''.
        ENDIF.

        DESCRIBE TABLE lt_alv_detail LINES DATA(lv_counter).

        IF e_salv_function = 'REPEAT'.
          lv_question = TEXT-t01.
          lv_method   = 'EXECUTE_JSON'.

        ELSEIF e_salv_function = 'DELETE'.
          lv_question = TEXT-t04.
          lv_method   = 'DELETE'.

        ELSEIF e_salv_function = 'COMPLETE'.
          lv_question = TEXT-t05.
          lv_method   = 'MARK_COMPLETE'.

        ENDIF.

        IF lv_counter > 0.

          REPLACE '&1' IN lv_question WITH CONV string( lv_counter ).

          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              text_question = lv_question
              text_button_1 = TEXT-t02
              text_button_2 = TEXT-t03
            IMPORTING
              answer        = lv_answer.

          IF lv_answer = '1'.

            LOOP AT lt_alv_detail INTO DATA(ls_alv_detail).

              MOVE-CORRESPONDING ls_alv_detail TO ls_if_data.

              lv_class = gt_if_config[ zifnum =  ls_if_data-zif_num ]-zifprocess_class.

              FREE lo_instance.
              FREE:zcl_if_inbound=>go_instance,
                   zcl_if_outbound=>go_instance.

              CALL METHOD (lv_class)=>get_instance
                EXPORTING
                  iv_ifnum    = ls_if_data-zif_num
                RECEIVING
                  ro_instance = lo_instance.

              CALL METHOD lo_instance->(lv_method)
                CHANGING
                  cs_if_data = ls_if_data.

              CLEAR ls_if_data.

            ENDLOOP.

            "处理完成后需要重新刷新数据
            refresh_detail( ).

          ENDIF.

        ENDIF.

      WHEN 'REFRESH'.

        refresh_detail( ).

    ENDCASE.

  ENDMETHOD.
*----------------------------------------------------------------------*
*   METHOD handle_user_command_overview
*----------------------------------------------------------------------*
  METHOD handle_user_command_overview.

    DATA: lt_alv_overview TYPE typ_t_alv_overview,
          lv_question     TYPE char128,
          lv_answer       TYPE c,
          ls_if_data      TYPE zift_data,
          lv_class        TYPE string,
          lv_method       TYPE string,
          lo_instance     TYPE REF TO object.

    CASE e_salv_function.
      WHEN 'REFRESH'.

        refresh_overview( ).

    ENDCASE.

  ENDMETHOD.
*----------------------------------------------------------------------*
*   METHOD refresh_detail
*----------------------------------------------------------------------*
  METHOD refresh_detail.

    get_data( ).

    gt_alv_detail_show = gt_alv_detail.

    go_alv_detail->refresh( s_stable = gs_stable ).

  ENDMETHOD.
*----------------------------------------------------------------------*
*   METHOD refresh_overview
*----------------------------------------------------------------------*
  METHOD refresh_overview.

    get_data( ).

*    gt_alv_overview_show = gt_alv_overview.
    process_data( ).

    go_alv_overview->refresh( s_stable = gs_stable ).

  ENDMETHOD.
*----------------------------------------------------------------------*
*   METHOD process_data
*----------------------------------------------------------------------*
  METHOD process_data.

    DATA: ls_overview TYPE typ_alv_overview.

    REFRESH gt_alv_overview.

    LOOP AT gt_alv_detail INTO DATA(ls_detail).
      CLEAR ls_overview.

      ls_overview-zif_num = ls_detail-zif_num.
      ls_overview-total   = 1.
      ls_overview-repeat  = ls_detail-zif_proc_counter.

      CASE ls_detail-zif_status.
        WHEN 'S'.     ls_overview-success = 1.
        WHEN 'D'.     ls_overview-delete  = 1.
        WHEN 'E'.     ls_overview-error   = 1.
        WHEN ''.      ls_overview-init    = 1.
        WHEN OTHERS.
      ENDCASE.

      COLLECT ls_overview INTO gt_alv_overview.

    ENDLOOP.

    LOOP AT gt_alv_overview REFERENCE INTO DATA(lr_overview).

      ASSIGN gt_if_config[ zifnum = lr_overview->zif_num ] TO FIELD-SYMBOL(<ls_if_config>).
      IF sy-subrc = 0.
        lr_overview->zifnam     = <ls_if_config>-zifnam.
        lr_overview->zifsystem  = <ls_if_config>-zifsystem.
        lr_overview->zifdirect  = <ls_if_config>-zifdirect.
        lr_overview->zifpmode   = <ls_if_config>-zifpmode.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

*----------------------------------------------------------------------*
*   METHOD display_overview_data
*----------------------------------------------------------------------*
  METHOD display_overview_data.

    DATA: lo_column  TYPE REF TO cl_salv_column_table.

    DEFINE set_column_hotspot.
      lo_column ?= go_alv_overview->get_columns( )->get_column( &1 ).
      lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
    END-OF-DEFINITION.

    DEFINE set_aggregation.
      go_alv_overview->get_aggregations( )->add_aggregation( columnname  = &1  aggregation = if_salv_c_aggregation=>total ).
    END-OF-DEFINITION.

    go_alv_overview = zcl_abap_comm=>salv_factory( EXPORTING im_column_optimized = 'X'
                                                             im_select_mode      = if_salv_c_selection_mode=>cell
                                                             im_save_layout      = 'X'
                                                   CHANGING  im_t_table          = gt_alv_overview ).
    TRY .
        set_alv_column_text: 'TOTAL'     TEXT-f09    go_alv_overview,
                             'INIT'      TEXT-f10    go_alv_overview,
                             'SUCCESS'   TEXT-f11    go_alv_overview,
                             'ERROR'     TEXT-f12    go_alv_overview,
                             'DELETE'    TEXT-f13    go_alv_overview,
                             'REPEAT'    TEXT-f14    go_alv_overview.

        set_column_hotspot: 'TOTAL',
                            'INIT',
                            'SUCCESS',
                            'ERROR',
                            'DELETE'.

        lo_column ?= go_alv_overview->get_columns( )->get_column( 'TOTAL' ).
        lo_column->set_color( VALUE #( col = 3  int = 1  inv = 0 )  ).

        lo_column ?= go_alv_overview->get_columns( )->get_column( 'ERROR' ).
        lo_column->set_color( VALUE #( col = 6  int = 1  inv = 0 )  ).

        lo_column ?= go_alv_overview->get_columns( )->get_column( 'SUCCESS' ).
        lo_column->set_color( VALUE #( col = 5  int = 1  inv = 0 )  ).

        lo_column ?= go_alv_overview->get_columns( )->get_column( 'DELETE' ).
        lo_column->set_color( VALUE #( col = 1  int = 1  inv = 0 )  ).

        lo_column ?= go_alv_overview->get_columns( )->get_column( 'INIT' ).
        lo_column->set_color( VALUE #( col = 7  int = 1  inv = 0 )  ).

        set_aggregation: 'TOTAL',
                         'ERROR',
                         'SUCCESS',
                         'DELETE',
                         'INIT',
                         'REPEAT'.

        go_alv_overview->get_layout( )->set_key( VALUE #( report = sy-cprog
                                                          handle = '002' ) ).

      CATCH cx_salv_not_found.

      CATCH cx_salv_existing.

      CATCH cx_salv_data_error.

    ENDTRY.
*--------add by chenxinyu 18.11.2022 13:44:50 增加刷新   功能
    "设置pf status
    go_alv_overview->set_screen_status(
    pfstatus      = 'ZSTATUS_OLD1'
    report        = sy-repid
    set_functions = go_alv_overview->c_functions_all ).

    "隐藏function
*    "1.Simple one:
*    go_alv_detail->get_functions( )->set_function( name = 'REPEAT' boolean = abap_false ).
    "2.More complex:
    DATA(lt_function_list) = go_alv_overview->get_functions( )->get_functions( ).
    LOOP AT lt_function_list INTO DATA(ls_function_list).

      IF ls_function_list-r_function->get_name( ) = 'REPEAT' OR
         ls_function_list-r_function->get_name( ) = 'COMPLETE' OR
         ls_function_list-r_function->get_name( ) = 'DELETE'.
        ls_function_list-r_function->set_visible( abap_false ).
      ENDIF.
    ENDLOOP.

    SET HANDLER on_link_click_overview FOR go_alv_overview->get_event( ).
    SET HANDLER handle_user_command_overview FOR go_alv_overview->get_event( ).

    go_alv_overview->display( ).

  ENDMETHOD.

*----------------------------------------------------------------------*
*   METHOD on_link_click_overview
*----------------------------------------------------------------------*
  METHOD on_link_click_overview.

    DATA: ls_if_data TYPE zift_data,
          lv_status  TYPE char1,
          lr_status  TYPE RANGE OF zeif_status.

    ASSIGN gt_alv_overview[ row ] TO FIELD-SYMBOL(<ls_overview>).

    CHECK sy-subrc = 0.

    CASE column.
      WHEN 'TOTAL'.
        lv_status = '*'.
      WHEN 'ERROR'.
        lv_status = 'E'.
      WHEN 'INIT'.
        lv_status = ''.
      WHEN 'SUCCESS'.
        lv_status = 'S'.
      WHEN 'DELETE'.
        lv_status = 'D'.
    ENDCASE.

    gt_alv_detail_show = gt_alv_detail.

    IF lv_status = '*'.
      DELETE gt_alv_detail_show WHERE zif_num <> <ls_overview>-zif_num.
    ELSE.
      DELETE gt_alv_detail_show WHERE zif_num <> <ls_overview>-zif_num OR zif_status <> lv_status.
    ENDIF.

    CHECK gt_alv_detail_show IS NOT INITIAL.

    display_detail_data( ).

  ENDMETHOD.

*----------------------------------------------------------------------*
*   METHOD get_if_input_ddic_type
*----------------------------------------------------------------------*
  METHOD get_if_input_ddic_type.

    SELECT SINGLE zifinput_struc FROM ztbs0002
      WHERE zifnum = @iv_ifnum
    INTO @rv_result.

  ENDMETHOD.

*----------------------------------------------------------------------*
*   METHOD get_if_output_ddic_type
*----------------------------------------------------------------------*
  METHOD get_if_output_ddic_type.

    SELECT SINGLE zifoutput_struc FROM ztbs0002
      WHERE zifnum = @iv_ifnum
    INTO @rv_result.

  ENDMETHOD.

ENDCLASS.
