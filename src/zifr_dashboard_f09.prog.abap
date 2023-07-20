*&---------------------------------------------------------------------*
*& Include zifr_dashboard_f09
*&---------------------------------------------------------------------*
CLASS lcl_dashboard_controller DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        it_rg_ifnum  TYPE table
        it_rg_ifguid TYPE table
        it_rg_addkey TYPE table
        it_rg_cdate  TYPE table
        it_rg_ctime  TYPE table
        it_rg_status TYPE table
        it_rg_zres1  TYPE table
        it_rg_zres2  TYPE table
        it_rg_zres3  TYPE table
        it_rg_zres4  TYPE table.
    METHODS run_detail RAISING lcx_comm.
    METHODS run_overview RAISING lcx_comm.
    METHODS handler_9001_usercommand IMPORTING iv_ucomm TYPE sy-ucomm.
    METHODS is_main_tree_bound RETURNING VALUE(rv_result) TYPE xsdboolean.

  PRIVATE SECTION.
    CONSTANTS c_empty_dynnr TYPE sy-dynnr VALUE '9999'.

    "Screen select options
    DATA mt_rg_ifnum  TYPE RANGE OF zift_data-zif_num.
    DATA mt_rg_ifguid TYPE RANGE OF zift_data-zif_guid.
    DATA mt_rg_addkey TYPE RANGE OF zift_data-zif_add_key.
    DATA mt_rg_cdate  TYPE RANGE OF zift_data-zif_cdate.
    DATA mt_rg_ctime  TYPE RANGE OF zift_data-zif_ctime.
    DATA mt_rg_status TYPE RANGE OF zift_data-zif_status.
    DATA mt_rg_zres1  TYPE RANGE OF zift_data-zres1.
    DATA mt_rg_zres2  TYPE RANGE OF zift_data-zres2.
    DATA mt_rg_zres3  TYPE RANGE OF zift_data-zres3.
    DATA mt_rg_zres4  TYPE RANGE OF zift_data-zres4.

    "Main objects
    DATA mo_overview       TYPE REF TO lcl_overview.
    DATA mo_main_tree      TYPE REF TO lcl_main_tree.
    DATA mo_msg_log_grid   TYPE REF TO lcl_msg_log_grid.
    DATA mo_i_payload_tree TYPE REF TO zcl_msg_tree.
    DATA mo_o_payload_tree TYPE REF TO zcl_msg_tree.
    DATA mo_payload_grid   TYPE REF TO lcl_payload_grid.

    "Screen container
    DATA mo_main_9001_container TYPE REF TO cl_gui_custom_container.
    DATA mo_main_splitter       TYPE REF TO cl_gui_splitter_container.

    DATA mo_container_main        TYPE REF TO cl_gui_container.
    DATA mo_container_msg_log     TYPE REF TO cl_gui_container.
    DATA mo_container_ipayload    TYPE REF TO cl_gui_container.
    DATA mo_container_opayload    TYPE REF TO cl_gui_container.
    DATA mo_container_payload_tab TYPE REF TO cl_gui_docking_container.

    DATA mv_is_main_tree_fullscreen TYPE xfeld.
    DATA mv_is_payload_grid_closed  TYPE xfeld.

    "Global control attributes
    DATA mv_payload_display_all_fields TYPE xfeld VALUE space.

    DATA ms_last_main_tree_line TYPE lcl_main_tree=>typ_alv_detail.

    "Container methods
    METHODS initialize_container.
    METHODS get_container_main RETURNING VALUE(result) TYPE REF TO cl_gui_container.
    METHODS get_container_msg_log RETURNING VALUE(result) TYPE REF TO cl_gui_container.
    METHODS get_container_ipayload RETURNING VALUE(result) TYPE REF TO cl_gui_container.
    METHODS get_container_opayload RETURNING VALUE(result) TYPE REF TO cl_gui_container.
    METHODS get_container_payload_tab RETURNING VALUE(result) TYPE REF TO cl_gui_docking_container.

    METHODS set_main_tree_fullscreen.
    METHODS set_all_splitter_display.

    METHODS display_payload_grid_if_closed.
    METHODS hide_payload_grid_if_open.

    "BO methods
    METHODS display_message_log IMPORTING is_data TYPE lcl_main_tree=>typ_alv_detail.
    METHODS display_payload IMPORTING is_data TYPE lcl_main_tree=>typ_alv_detail.
    METHODS display_ipayload_tree IMPORTING iv_ddic_type TYPE typename
                                            iv_json TYPE string.
    METHODS display_opayload_tree IMPORTING iv_ddic_type TYPE typename
                                            iv_json TYPE string.
    METHODS display_ipayload_json IMPORTING iv_json TYPE string.
    METHODS display_opayload_json IMPORTING iv_json TYPE string.
    METHODS refresh_main_tree.
    METHODS process_seleted_main_tree_line IMPORTING iv_ucomm TYPE sy-ucomm.
    METHODS download_main_tree_data.
    METHODS switch_payload_display_fields.
    METHODS cleanup_detail.

    "Evnets Handler
    METHODS on_main_tree_line_selected FOR EVENT single_record_selected OF lcl_main_tree
      IMPORTING es_data.
    METHODS on_payload_tab_double_click FOR EVENT flat_table_double_clicked OF zcl_msg_tree
      IMPORTING es_relation.
    METHODS on_payloadgrid_temporary_close FOR EVENT on_temporary_close OF lcl_payload_grid.
    METHODS on_overview_data_selected FOR EVENT overview_data_selected OF lcl_overview
      IMPORTING ev_zif_num ev_zif_status.

ENDCLASS.

CLASS lcl_dashboard_controller IMPLEMENTATION.

  METHOD run_detail.

    initialize_container( ).

    mo_main_tree = NEW #( it_rg_ifnum  = mt_rg_ifnum[]
                          it_rg_ifguid = mt_rg_ifguid[]
                          it_rg_addkey = mt_rg_addkey[]
                          it_rg_cdate  = mt_rg_cdate[]
                          it_rg_ctime  = mt_rg_ctime[]
                          it_rg_status = mt_rg_status[]
                          it_rg_zres1  = mt_rg_zres1[]
                          it_rg_zres2  = mt_rg_zres2[]
                          it_rg_zres3  = mt_rg_zres3[]
                          it_rg_zres4  = mt_rg_zres4[]
                          io_container = get_container_main( ) ).

    set_main_tree_fullscreen( ).

    SET HANDLER me->on_main_tree_line_selected FOR mo_main_tree.

    TRY.
        mo_main_tree->run( ).
      CATCH lcx_comm INTO DATA(lo_error).
        MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE TO SCREEN 0.
    ENDTRY.

  ENDMETHOD.

  METHOD get_container_main.

    IF mo_container_main IS NOT BOUND.

      mo_container_main = mo_main_splitter->get_container( row = 1 column = 1 ).

    ENDIF.

    IF result IS SUPPLIED.
      result = mo_container_main.
    ENDIF.

  ENDMETHOD.

  METHOD initialize_container.
    CHECK mo_main_9001_container IS NOT BOUND.

    mo_main_9001_container = NEW #( 'MAIN_CONTAINER' ).

    mo_main_splitter = NEW #( parent  = mo_main_9001_container
                              rows    = 2
                              columns = 2 ).
  ENDMETHOD.

  METHOD on_main_tree_line_selected.

    CHECK es_data IS NOT INITIAL.

    ms_last_main_tree_line = es_data.

    "显示隐藏的splitter container
    set_all_splitter_display( ).

    "Display message log
    display_message_log( es_data ).

    "Display payload
    display_payload( es_data ).

  ENDMETHOD.

  METHOD get_container_msg_log.
    IF mo_container_msg_log IS NOT BOUND.

      mo_container_msg_log = mo_main_splitter->get_container( row = 1 column = 2 ).

    ENDIF.

    IF result IS SUPPLIED.
      result = mo_container_msg_log.
    ENDIF.
  ENDMETHOD.

  METHOD get_container_ipayload.
    IF mo_container_ipayload IS NOT BOUND.

      mo_container_ipayload = mo_main_splitter->get_container( row = 2 column = 1 ).

    ENDIF.

    IF result IS SUPPLIED.
      result = mo_container_ipayload.
    ENDIF.
  ENDMETHOD.

  METHOD get_container_opayload.
    IF mo_container_opayload IS NOT BOUND.

      mo_container_opayload = mo_main_splitter->get_container( row = 2 column = 2 ).

    ENDIF.

    IF result IS SUPPLIED.
      result = mo_container_opayload.
    ENDIF.
  ENDMETHOD.

  METHOD on_payload_tab_double_click.
    "检查行类型是否为REF，如果为REF，则输出错误
    FIELD-SYMBOLS <lt_show_table> TYPE STANDARD TABLE.
    ASSIGN es_relation-data_ref->* TO <lt_show_table>.
    CHECK sy-subrc = 0.

    LOOP AT <lt_show_table> ASSIGNING FIELD-SYMBOL(<ls_show_line>).
      IF sy-tabix <> 1.
        EXIT.
      ENDIF.
      IF cl_abap_datadescr=>get_data_type_kind( <ls_show_line> ) = cl_abap_datadescr=>typekind_dref.
        MESSAGE s035 DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.
    ENDLOOP.

    IF mo_payload_grid IS NOT BOUND.
      mo_payload_grid = NEW #( get_container_payload_tab( ) ).

      SET HANDLER on_payloadgrid_temporary_close FOR mo_payload_grid.
    ENDIF.

    display_payload_grid_if_closed( ).

    "这里需要强制触发以下屏幕PAI，来带动docking的ratio正确显示
    cl_gui_cfw=>set_new_ok_code( 'Z_INTERNAL_REFRESH' ).

    mo_payload_grid->run_from_data( es_relation-data_ref ).
  ENDMETHOD.

  METHOD get_container_payload_tab.
    IF mo_container_payload_tab IS NOT BOUND.
      mo_container_payload_tab = NEW #( ratio = 20
                                        repid = sy-repid
                                        dynnr = sy-dynnr
                                        side  = cl_gui_docking_container=>dock_at_bottom ).
    ENDIF.

    IF result IS SUPPLIED.
      result = mo_container_payload_tab.
    ENDIF.
  ENDMETHOD.

  METHOD set_main_tree_fullscreen.
    CHECK mv_is_main_tree_fullscreen = abap_false.

    mo_main_splitter->set_row_height(
      EXPORTING
        id                = 2
        height            = 0
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3 ).
    IF sy-subrc <> 0.
      "pass
    ENDIF.

    mo_main_splitter->set_column_width(
      EXPORTING
        id                = 2
        width             = 0
*      IMPORTING
*       result            =
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3 ).
    IF sy-subrc <> 0.
      "pass
    ENDIF.

    mo_main_splitter->set_row_sash(
      EXPORTING
        id                = 2
        type              = 1        "这里使用1才能改为不可调整宽度
        value             = mo_main_splitter->false
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3 ).
    IF sy-subrc <> 0.
      "pass
    ENDIF.

    mo_main_splitter->set_column_sash(
      EXPORTING
        id                = 2
        type              = 1       "这里使用1才能改为不可调整宽度
        value             = mo_main_splitter->false
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3 ).
    IF sy-subrc <> 0.
      "pass
    ENDIF.

    hide_payload_grid_if_open( ).

    mv_is_main_tree_fullscreen = abap_true.
  ENDMETHOD.

  METHOD set_all_splitter_display.
    CHECK mv_is_main_tree_fullscreen = abap_true.

    mo_main_splitter->set_row_height(
      EXPORTING
        id                = 2
        height            = 50
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3 ).
    IF sy-subrc <> 0.
      "pass
    ENDIF.

    mo_main_splitter->set_column_width(
      EXPORTING
        id                = 2
        width             = 50
*      IMPORTING
*       result            =
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3 ).
    IF sy-subrc <> 0.
      "pass
    ENDIF.

    mo_main_splitter->set_row_sash(
      EXPORTING
        id                = 2
        type              = 1        "这里使用1才能改为不可调整宽度
        value             = mo_main_splitter->true
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3 ).
    IF sy-subrc <> 0.
      "pass
    ENDIF.

    mo_main_splitter->set_column_sash(
      EXPORTING
        id                = 2
        type              = 1       "这里使用1才能改为不可调整宽度
        value             = mo_main_splitter->true
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3 ).
    IF sy-subrc <> 0.
      "pass
    ENDIF.

    CLEAR mv_is_main_tree_fullscreen.
  ENDMETHOD.

  METHOD on_payloadgrid_temporary_close.
    mo_container_payload_tab->link(
      EXPORTING
        repid                       = sy-repid
        dynnr                       = c_empty_dynnr
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        lifetime_dynpro_dynpro_link = 3
        OTHERS                      = 4
    ).
    IF sy-subrc <> 0.
      "pass
    ENDIF.
  ENDMETHOD.

  METHOD display_payload_grid_if_closed.
    IF mo_container_payload_tab->get_link_info( )-dynnr = c_empty_dynnr.

      mo_container_payload_tab->link(
        EXPORTING
          repid                       = sy-repid
          dynnr                       = sy-dynnr
        EXCEPTIONS
          cntl_error                  = 1
          cntl_system_error           = 2
          lifetime_dynpro_dynpro_link = 3
          OTHERS                      = 4 ).
      IF sy-subrc <> 0.
        "pass
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD display_message_log.
    IF mo_msg_log_grid IS NOT BOUND.
      mo_msg_log_grid = NEW #( get_container_msg_log( ) ).
    ENDIF.

    mo_msg_log_grid->run_from_key(
      iv_zif_num     = is_data-zif_num
      iv_zif_guid    = is_data-zif_guid
      iv_zif_add_key = is_data-zif_add_key ).
  ENDMETHOD.

  METHOD display_payload.
    mo_main_tree->get_payload(
      EXPORTING
        iv_zif_num     = is_data-zif_num
        iv_zif_guid    = is_data-zif_guid
        iv_zif_add_key = is_data-zif_add_key
      IMPORTING
        ev_i_ddictype  = DATA(lv_i_ddic_type)
        ev_i_json      = DATA(lv_i_json)
        ev_o_ddictype  = DATA(lv_o_ddic_type)
        ev_o_json      = DATA(lv_o_json) ).

    display_ipayload_tree( iv_json      = lv_i_json
                           iv_ddic_type = lv_i_ddic_type ).

    display_opayload_tree( iv_json      = lv_o_json
                           iv_ddic_type = lv_o_ddic_type ).

  ENDMETHOD.

  METHOD display_ipayload_tree.
    IF mo_i_payload_tree IS NOT BOUND.
      mo_i_payload_tree = lcl_payload_tree=>get_by_oref_container( io_container = get_container_ipayload( )
                                                                   iv_top_of_page_text = CONV #( '接口输入参数'(014) )
                                                                   iv_expand_flat_table = abap_false ).
      SET HANDLER on_payload_tab_double_click FOR mo_i_payload_tree.
    ENDIF.

    TRY.
        mo_i_payload_tree->show_empty_fields( mv_payload_display_all_fields ).
        mo_i_payload_tree->display_from_json(
          iv_json        = iv_json
*          iv_pretty_name = /ui2/cl_json=>pretty_mode-camel_case
          iv_ddic_type   = iv_ddic_type ).
      CATCH zcx_msg_tree.
        mo_i_payload_tree->clear_all_nodes( ).
    ENDTRY.
  ENDMETHOD.

  METHOD display_opayload_tree.
    IF mo_o_payload_tree IS NOT BOUND.
      mo_o_payload_tree = lcl_payload_tree=>get_by_oref_container( io_container = get_container_opayload( )
                                                                   iv_top_of_page_text = CONV #( '接口输出参数'(015) )
                                                                   iv_expand_flat_table = abap_false ).
      SET HANDLER on_payload_tab_double_click FOR mo_o_payload_tree.
    ENDIF.

    TRY.
        mo_i_payload_tree->show_empty_fields( mv_payload_display_all_fields ).
        mo_o_payload_tree->display_from_json(
          iv_json        = iv_json
*          iv_pretty_name = /ui2/cl_json=>pretty_mode-camel_case
          iv_ddic_type   = iv_ddic_type ).
      CATCH zcx_msg_tree.
        mo_o_payload_tree->clear_all_nodes( ).
    ENDTRY.
  ENDMETHOD.

  METHOD handler_9001_usercommand.
    CASE iv_ucomm.
      WHEN 'REFRESH'.
        refresh_main_tree( ).

      WHEN 'REPEAT' OR 'COMPLETE' OR 'DELETE'.
        process_seleted_main_tree_line( iv_ucomm ).

      WHEN 'DOWNLOAD'.
        download_main_tree_data( ).

      WHEN 'CHG_P_FIEL'. "消息树显示/隐藏空字段
        switch_payload_display_fields( ).

      WHEN OTHERS.

    ENDCASE.
  ENDMETHOD.

  METHOD refresh_main_tree.
*    set_main_tree_fullscreen( ).

    TRY.
        mo_main_tree->run( ).

        on_main_tree_line_selected( ms_last_main_tree_line ).
      CATCH lcx_comm INTO DATA(lo_error).
        MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE TO SCREEN 0.
    ENDTRY.
  ENDMETHOD.

  METHOD hide_payload_grid_if_open.
    CHECK mo_container_payload_tab IS BOUND.
    IF mo_container_payload_tab->get_link_info( )-dynnr = sy-dynnr.

      mo_container_payload_tab->link(
        EXPORTING
          repid                       = sy-repid
          dynnr                       = c_empty_dynnr
        EXCEPTIONS
          cntl_error                  = 1
          cntl_system_error           = 2
          lifetime_dynpro_dynpro_link = 3
          OTHERS                      = 4 ).
      IF sy-subrc <> 0.
        "pass
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD process_seleted_main_tree_line.

    TRY.
        DATA(lv_re_processed_count) = mo_main_tree->process_selected_nodes( iv_ucomm ).

        IF lv_re_processed_count >= 1.
          refresh_main_tree( ).
        ENDIF.
      CATCH lcx_comm.
        "handle exception
    ENDTRY.

  ENDMETHOD.

  METHOD download_main_tree_data.
    mo_main_tree->download_data_2_excel( ).
  ENDMETHOD.

  METHOD switch_payload_display_fields.
    CASE mv_payload_display_all_fields.
      WHEN abap_true.
        mv_payload_display_all_fields = abap_false.

      WHEN abap_false.
        mv_payload_display_all_fields = abap_true.

      WHEN OTHERS.
        "not possible
    ENDCASE.

    IF mv_is_main_tree_fullscreen = abap_true.
      RETURN.
    ENDIF.

    "rebuild tree
    display_payload( ms_last_main_tree_line ).

  ENDMETHOD.

  METHOD display_ipayload_json.

    CALL TRANSFORMATION sjson2html SOURCE XML iv_json
                                   RESULT XML DATA(lv_html).

    cl_abap_browser=>show_html( html_string = cl_abap_codepage=>convert_from( lv_html )
                                container   = get_container_ipayload( ) ).

  ENDMETHOD.

  METHOD display_opayload_json.

    CALL TRANSFORMATION sjson2html SOURCE XML iv_json
                                   RESULT XML DATA(lv_html).

    cl_abap_browser=>show_html( html_string = cl_abap_codepage=>convert_from( lv_html )
                                container   = get_container_opayload( ) ).

  ENDMETHOD.

  METHOD is_main_tree_bound.
    rv_result = xsdbool( mo_main_tree IS BOUND ).
  ENDMETHOD.

  METHOD run_overview.
    mo_overview = NEW #( it_rg_ifnum  = mt_rg_ifnum[]
                         it_rg_ifguid = mt_rg_ifguid[]
                         it_rg_addkey = mt_rg_addkey[]
                         it_rg_cdate  = mt_rg_cdate[]
                         it_rg_ctime  = mt_rg_ctime[]
                         it_rg_status = mt_rg_status[]
                         it_rg_zres1  = mt_rg_zres1[]
                         it_rg_zres2  = mt_rg_zres2[]
                         it_rg_zres3  = mt_rg_zres3[]
                         it_rg_zres4  = mt_rg_zres4[] ).

    SET HANDLER on_overview_data_selected FOR mo_overview.

    TRY.
        mo_overview->run( ).
      CATCH lcx_comm INTO DATA(lo_error).
        MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
        "pass
    ENDTRY.
  ENDMETHOD.

  METHOD constructor.
    mt_rg_ifnum  =  it_rg_ifnum .
    mt_rg_ifguid =  it_rg_ifguid.
    mt_rg_addkey =  it_rg_addkey.
    mt_rg_cdate  =  it_rg_cdate .
    mt_rg_ctime  =  it_rg_ctime .
    mt_rg_status =  it_rg_status.
    mt_rg_zres1  =  it_rg_zres1 .
    mt_rg_zres2  =  it_rg_zres2 .
    mt_rg_zres3  =  it_rg_zres3 .
    mt_rg_zres4  =  it_rg_zres4 .
  ENDMETHOD.

  METHOD on_overview_data_selected.
    "重置接口编号的range
    mt_rg_ifnum = VALUE #( ( sign = 'I' option = 'EQ' low = ev_zif_num ) ).

    "重置状态的range
    CASE ev_zif_status.
      WHEN '*'.
        mt_rg_status = VALUE #( ).
      WHEN OTHERS.
        mt_rg_status = VALUE #( ( sign = 'I' option = 'EQ' low = ev_zif_status ) ).
    ENDCASE.

    CALL SCREEN 9001.

    "返回原屏幕后，清空Instance
    cleanup_detail( ).
  ENDMETHOD.

  METHOD cleanup_detail.
    IF mo_container_main IS BOUND.
      mo_container_main->free( ).
    ENDIF.

    IF mo_container_msg_log IS BOUND.
      mo_container_msg_log->free( ).
    ENDIF.

    IF mo_container_ipayload IS BOUND.
      mo_container_ipayload->free( ).
    ENDIF.

    IF mo_container_opayload IS BOUND.
      mo_container_opayload->free( ).
    ENDIF.

    IF mo_container_payload_tab IS BOUND.
      mo_container_payload_tab->free( ).
    ENDIF.

    IF mo_main_splitter IS BOUND.
      mo_main_splitter->free( ).
    ENDIF.

    IF mo_main_9001_container IS BOUND.
      mo_main_9001_container->free( ).
    ENDIF.

    CLEAR:
      mo_main_tree     ,
      mo_msg_log_grid  ,
      mo_i_payload_tree,
      mo_o_payload_tree,
      mo_payload_grid  ,
      mo_main_9001_container ,
      mo_main_splitter          ,
      mo_container_main        ,
      mo_container_msg_log     ,
      mo_container_ipayload    ,
      mo_container_opayload    ,
      mo_container_payload_tab  ,
      mv_is_main_tree_fullscreen,
      mv_is_payload_grid_closed .
  ENDMETHOD.

ENDCLASS.
