*&---------------------------------------------------------------------*
*& Include zifr_dashboard_f05
*&---------------------------------------------------------------------*
CLASS lcl_main_tree DEFINITION.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF typ_alv_detail,
        total_count     TYPE aql_alvc,
        zif_num         TYPE zift_data-zif_num,
        zifnam          TYPE ztbs0002-zifnam,
        zif_guid        TYPE zift_data-zif_guid,
        zif_add_key     TYPE zift_data-zif_add_key,
        zifsystem       TYPE ztbs0002-zifsystem,
        zif_cdate       TYPE zift_data-zif_cdate,
        zif_ctime       TYPE zift_data-zif_ctime,
        zif_fdate       TYPE zift_data-zif_fdate,
        zif_ftime       TYPE zift_data-zif_ftime,
        zres1           TYPE zift_data-zres1,
        zres2           TYPE zift_data-zres2,
        zres3           TYPE zift_data-zres3,
        zres4           TYPE zift_data-zres4,
        zif_status      TYPE zift_data-zif_status,
        zif_repeat      TYPE zift_data-zif_repeat,
        zif_repeat_prio TYPE zift_data-zif_repeat_prio,
        msgid           TYPE zift_log-msgid,
        msgno           TYPE zift_log-msgno,
        msgty           TYPE zift_log-msgty,
        msgtxt          TYPE char255,
      END OF typ_alv_detail.

    METHODS constructor
      IMPORTING
        io_container TYPE REF TO cl_gui_container OPTIONAL
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

    METHODS run RAISING lcx_comm.
    METHODS get_payload
      IMPORTING
        iv_zif_num     TYPE zift_data-zif_num
        iv_zif_guid    TYPE zift_data-zif_guid
        iv_zif_add_key TYPE zift_data-zif_add_key
      EXPORTING
        ev_i_ddictype  TYPE typename
        ev_i_json      TYPE string
        ev_o_ddictype  TYPE typename
        ev_o_json      TYPE string.

    METHODS process_selected_nodes
      IMPORTING iv_ucomm TYPE sy-ucomm
      RETURNING VALUE(rv_count) TYPE i
      RAISING lcx_comm.

    METHODS download_data_2_excel.

    METHODS cleanup.

    EVENTS single_record_selected EXPORTING VALUE(es_data) TYPE typ_alv_detail.

  PRIVATE SECTION.
    CONSTANTS: BEGIN OF c_funcname_status,
                 error   TYPE salv_de_function VALUE 'ZERROR',
                 success TYPE salv_de_function VALUE 'ZSUCCESS',
                 deleted TYPE salv_de_function VALUE 'ZDELETED',
                 ready   TYPE salv_de_function VALUE 'ZREADY',
                 all     TYPE salv_de_function VALUE 'ZALL',
               END OF c_funcname_status.

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

    DATA mo_container TYPE REF TO cl_gui_container.
    DATA mo_salv_tree TYPE REF TO cl_salv_tree.

    DATA mt_if_config TYPE STANDARD TABLE OF ztbs0002.
    DATA mt_alvdata TYPE STANDARD TABLE OF typ_alv_detail.
    DATA mt_tree_out TYPE STANDARD TABLE OF typ_alv_detail.

    METHODS get_data RAISING lcx_comm.
    METHODS display_alv_tree.
    METHODS build_tree.
    METHODS build_header.
    METHODS build_body IMPORTING iv_display_type TYPE salv_de_function DEFAULT c_funcname_status-all.
    METHODS set_events.
    METHODS set_functions.
    METHODS set_columns.
    METHODS set_column_text_by_salv_tree
      IMPORTING
        iv_column_name TYPE lvc_fname
        iv_text        TYPE scrtext_m.
    METHODS set_column_text
      IMPORTING
        io_salv_column TYPE REF TO cl_salv_column
        iv_text        TYPE scrtext_m.

    METHODS on_double_click FOR EVENT double_click OF cl_salv_events_tree
      IMPORTING node_key.

    METHODS on_usercommand FOR EVENT added_function OF cl_salv_events_tree
      IMPORTING e_salv_function.

ENDCLASS.


CLASS lcl_main_tree IMPLEMENTATION.

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

    mo_container = io_container.

    "初始化接口配置
    SELECT zifnum,
           zifnam,
           zifsystem,
           zifdirect,
           zifpmode,
           zifprocess_class
      INTO CORRESPONDING FIELDS OF TABLE @mt_if_config
      FROM ztbs0002
      WHERE zifnum IN @mt_rg_ifnum.
  ENDMETHOD.

  METHOD run.
    TRY.
        get_data( ).
      CATCH lcx_comm INTO DATA(lo_error).
        RAISE EXCEPTION lo_error.
    ENDTRY.

    display_alv_tree( ).
  ENDMETHOD.

  METHOD get_data.
    SELECT FROM zift_data
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
     WHERE zift_data~zif_num     IN @mt_rg_ifnum
       AND zift_data~zif_guid    IN @mt_rg_ifguid
       AND zift_data~zif_add_key IN @mt_rg_addkey
       AND zif_cdate             IN @mt_rg_cdate
       AND zif_ctime             IN @mt_rg_ctime
       AND zif_status            IN @mt_rg_status
       AND zres1                 IN @mt_rg_zres1
       AND zres2                 IN @mt_rg_zres2
       AND zres3                 IN @mt_rg_zres3
       AND zres4                 IN @mt_rg_zres4
     ORDER BY
       zift_data~zif_num,
       zif_cdate DESCENDING,
       zif_ctime DESCENDING
     INTO CORRESPONDING FIELDS OF TABLE @mt_alvdata.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_comm MESSAGE e034.
    ENDIF.

    SELECT FROM @mt_alvdata AS a
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

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    SORT lt_message BY zif_num
                       zif_guid
                       zif_add_key
                       zif_proc_counter DESCENDING
                       zif_msg_counter DESCENDING.

    LOOP AT mt_alvdata REFERENCE INTO DATA(lr_alvdata).
      READ TABLE lt_message REFERENCE INTO DATA(lr_message)
        WITH KEY zif_num = lr_alvdata->zif_num
                 zif_guid = lr_alvdata->zif_guid
                 zif_add_key = lr_alvdata->zif_add_key
                 BINARY SEARCH.
      IF sy-subrc = 0.
        lr_alvdata->msgid = lr_message->msgid.
        lr_alvdata->msgno = lr_message->msgno.
        lr_alvdata->msgty = lr_message->msgty.
        lr_alvdata->msgtxt = lr_message->msgtxt.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD build_tree.
    build_header( ).
    build_body( ).
  ENDMETHOD.

  METHOD build_header.
    DATA: lo_settings TYPE REF TO cl_salv_tree_settings.

    lo_settings = mo_salv_tree->get_tree_settings( ).
    lo_settings->set_hierarchy_header( '接口总览'(002) ).
    lo_settings->set_hierarchy_tooltip( '接口总览'(002) ).
    lo_settings->set_hierarchy_size( 50 ).

    lo_settings->set_header( '接口总览'(002) ).

    "top of page set
    DATA(lo_top_content) = NEW cl_salv_form_header_info( text = '接口总览'(002) ).
    mo_salv_tree->set_top_of_list( lo_top_content ).
  ENDMETHOD.

  METHOD display_alv_tree.

    IF mo_salv_tree IS NOT BOUND.

      TRY.
          CALL METHOD cl_salv_tree=>factory
            EXPORTING
              r_container = mo_container
            IMPORTING
              r_salv_tree = mo_salv_tree
            CHANGING
              t_table     = mt_tree_out.

          me->build_tree( ).

          me->set_columns( ).

          me->set_functions( ).

          me->set_events( ).

        CATCH cx_salv_error INTO DATA(lo_cx_err).
          DATA(lv_msg) = lo_cx_err->get_message( ).
          RETURN.
      ENDTRY.

    ELSE.

      TRY.
          mo_salv_tree->get_nodes( )->delete_all( ).
          me->build_tree( ).
        CATCH cx_salv_error.
      ENDTRY.

    ENDIF.

    "只有一行时，默认显示所有明细
    IF lines( mt_alvdata ) = 1.
      RAISE EVENT single_record_selected
        EXPORTING
          es_data = mt_alvdata[ 1 ].
    ENDIF.

    me->mo_salv_tree->display( ).

  ENDMETHOD.


  METHOD build_body.
    DATA lo_temp_node TYPE REF TO cl_salv_node.
    DATA lv_temp_text TYPE lvc_value.
    DATA lv_date_o TYPE char10.
    DATA lv_time_o TYPE char8.
    DATA lv_result_interface_counter TYPE int4.  "查询结果有几个不同接口
    DATA lv_where_condition TYPE string.
    DATA ls_temp_alvdata_with_count TYPE typ_alv_detail.

    DATA(lo_nodes) = mo_salv_tree->get_nodes( ).

    "根据传入参数动态构建不同类型的节点显示
    CASE iv_display_type.
      WHEN c_funcname_status-all.
        lv_where_condition = ''.

      WHEN c_funcname_status-error.
        lv_where_condition = `zif_status = 'E'`.

      WHEN c_funcname_status-deleted.
        lv_where_condition = `zif_status = 'D'`.

      WHEN c_funcname_status-ready.
        lv_where_condition = `zif_status = ''`.

      WHEN c_funcname_status-success.
        lv_where_condition = `zif_status = 'S'`.

      WHEN OTHERS.
        RETURN.

    ENDCASE.

    LOOP AT mt_alvdata REFERENCE INTO DATA(ls_ref_alvdata) WHERE (lv_where_condition)
      GROUP BY ( zif_num = ls_ref_alvdata->zif_num
                 zifnam  = ls_ref_alvdata->zifnam
                 size    = GROUP SIZE )
      REFERENCE INTO DATA(ls_ref_key).

      TRY.
          ls_temp_alvdata_with_count-total_count = ls_ref_key->size.

          lo_temp_node = lo_nodes->add_node( related_node = ''
                                             text         = |{ ls_ref_key->zif_num } { ls_ref_key->zifnam }|
                                             folder       = abap_true
                                             data_row     = ls_temp_alvdata_with_count
                                             relationship = cl_gui_column_tree=>relat_last_child ).

          DATA(lv_parent_node_key) = lo_temp_node->get_key( ).

          lv_result_interface_counter += 1.
        CATCH cx_salv_msg.
          CONTINUE.
      ENDTRY.

      LOOP AT GROUP ls_ref_key REFERENCE INTO DATA(ls_ref_sub_data).

        ls_ref_sub_data->total_count = 1.

        TRY.
            cl_abap_datfm=>conv_date_int_to_ext(
              EXPORTING
                im_datint = ls_ref_sub_data->zif_cdate
              IMPORTING
                ex_datext = lv_date_o ).

            cl_abap_timefm=>conv_time_int_to_ext(
              EXPORTING
                time_int = ls_ref_sub_data->zif_ctime
              IMPORTING
                time_ext = DATA(lv_time_o_str) ).
            lv_time_o = lv_time_o_str.

            lv_temp_text = |{ lv_date_o } { lv_time_o }|.
          CATCH cx_abap_datfm_format_unknown cx_parameter_invalid_range.
            lv_temp_text = |{ ls_ref_sub_data->zif_cdate } { ls_ref_sub_data->zif_ctime }|.
        ENDTRY.

        TRY.
            DATA(lv_temp_icon) = VALUE salv_de_tree_image(  ).
            lv_temp_icon = SWITCH #( ls_ref_sub_data->zif_status
                                WHEN 'S' THEN icon_led_green
                                WHEN 'E' THEN icon_led_red
                                WHEN 'D' THEN icon_delete
                                WHEN ''  THEN icon_wd_radio_button_empty ).

            lo_nodes->add_node( related_node   = lv_parent_node_key
                                text           = lv_temp_text
                                data_row       = ls_ref_sub_data->*
                                expanded_icon  = lv_temp_icon
                                collapsed_icon = lv_temp_icon
                                relationship   = cl_gui_column_tree=>relat_last_child ).
          CATCH cx_salv_msg.
            CONTINUE.
        ENDTRY.

      ENDLOOP.

    ENDLOOP.

    "查询接口只有一个接口时，expand all
    IF lv_result_interface_counter = 1.
      lo_nodes->expand_all( ).
    ENDIF.

  ENDMETHOD.

  METHOD set_columns.
    DATA(lo_columns) = mo_salv_tree->get_columns( ).

    set_column_text_by_salv_tree( iv_column_name = 'ZIF_CDATE'
                                  iv_text        = CONV #( text-f01 ) ).
    set_column_text_by_salv_tree( iv_column_name = 'ZIF_CTIME'
                                  iv_text        = CONV #( text-f02 ) ).
    set_column_text_by_salv_tree( iv_column_name = 'ZIF_FDATE'
                                  iv_text        = CONV #( text-f03 ) ).
    set_column_text_by_salv_tree( iv_column_name = 'ZIF_FTIME'
                                  iv_text        = CONV #( text-f04 ) ).

    TRY.
        lo_columns->get_column( 'ZIF_NUM' )->set_visible( if_salv_c_bool_sap=>false ).
        lo_columns->get_column( 'ZIFNAM' )->set_visible( if_salv_c_bool_sap=>false ).
        lo_columns->get_column( 'ZIF_STATUS' )->set_visible( if_salv_c_bool_sap=>false ).
      CATCH cx_salv_not_found.
    ENDTRY.

    lo_columns->set_optimize( ).

  ENDMETHOD.

  METHOD set_column_text_by_salv_tree.
    DATA lo_column TYPE REF TO cl_salv_column_tree.

    DATA(lo_columns) = mo_salv_tree->get_columns( ).

    TRY.
        lo_column ?= lo_columns->get_column( iv_column_name ).
      CATCH cx_salv_not_found.
        RETURN.
    ENDTRY.

    set_column_text(
        io_salv_column = lo_column
        iv_text        = iv_text ).
  ENDMETHOD.

  METHOD set_events.
    DATA(lo_event) = me->mo_salv_tree->get_event( ).
    SET HANDLER me->on_double_click FOR lo_event.
    SET HANDLER me->on_usercommand FOR lo_event.
  ENDMETHOD.

  METHOD on_double_click.
    FIELD-SYMBOLS <ls_alv_detail> TYPE typ_alv_detail.

    TRY.
        DATA(lo_selected_node) = mo_salv_tree->get_nodes( )->get_node( node_key ).
      CATCH cx_salv_msg.
        RETURN.
    ENDTRY.

    DATA(lr_data_line) = lo_selected_node->get_data_row( ).

    ASSIGN lr_data_line->* TO <ls_alv_detail>.
    CHECK sy-subrc = 0.

    RAISE EVENT single_record_selected
      EXPORTING
        es_data = <ls_alv_detail>.
  ENDMETHOD.

  METHOD get_payload.
    SELECT SINGLE
      a~zif_input,
      a~zif_output,
      b~zifinput_struc,
      b~zifoutput_struc
      FROM zift_data AS a
      LEFT OUTER JOIN ztbs0002 AS b ON a~zif_num = b~zifnum
      WHERE a~zif_num     = @iv_zif_num
        AND a~zif_guid    = @iv_zif_guid
        AND a~zif_add_key = @iv_zif_add_key
    INTO ( @ev_i_json, @ev_o_json, @ev_i_ddictype, @ev_o_ddictype ).

    IF ev_i_json = `""` OR ev_i_json = `{}`.
      CLEAR ev_i_json.
    ENDIF.

    IF ev_o_json = `""` OR ev_o_json = `{}`.
      CLEAR ev_o_json.
    ENDIF.
  ENDMETHOD.

  METHOD process_selected_nodes.

    DATA: lt_alv_detail TYPE STANDARD TABLE OF typ_alv_detail,
          lv_question   TYPE char128,
          lv_answer     TYPE c,
          ls_if_data    TYPE zift_data,
          lv_class      TYPE string,
          lv_method     TYPE string,
          lo_instance   TYPE REF TO object.

    FIELD-SYMBOLS <ls_line> TYPE typ_alv_detail.

    DATA(lo_selection) = mo_salv_tree->get_selections( ).

    DATA(lt_selected_nodes) = lo_selection->get_selected_nodes( ).

    IF lt_selected_nodes IS INITIAL.
      RAISE EXCEPTION TYPE lcx_comm MESSAGE e005.
    ENDIF.

    LOOP AT lt_selected_nodes REFERENCE INTO DATA(lr_selected_node).
      "文件夹节点不执行重处理
      CHECK lr_selected_node->node->is_folder( ) = abap_false.

      DATA(ls_temp_line_ref) = lr_selected_node->node->get_data_row( ).

      ASSIGN ls_temp_line_ref->* TO <ls_line> CASTING.
      CHECK sy-subrc = 0.

      IF <ls_line>-zif_status = 'D' OR <ls_line>-zif_status = 'S'.
        CONTINUE.
      ENDIF.

      IF iv_ucomm = 'REPEAT' AND <ls_line>-zif_repeat = ''.
        CONTINUE.
      ENDIF.

      APPEND <ls_line> TO lt_alv_detail.
    ENDLOOP.

    DESCRIBE TABLE lt_alv_detail LINES DATA(lv_counter).

    IF iv_ucomm = 'REPEAT'.
      lv_question = TEXT-t01.
      lv_method   = 'EXECUTE_JSON'.

    ELSEIF iv_ucomm = 'DELETE'.
      lv_question = TEXT-t04.
      lv_method   = 'DELETE'.

    ELSEIF iv_ucomm = 'COMPLETE'.
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

          lv_class = mt_if_config[ zifnum =  ls_if_data-zif_num ]-zifprocess_class.

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

        rv_count = lv_counter.

      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD download_data_2_excel.

    DATA: lt_fieldcat   TYPE lvc_t_fcat,
          lv_xml        TYPE xstring,
          lt_xml_stream TYPE xml_rawdata,
          lv_lengh      TYPE i.

    "Step 1 - get save path
    DATA(lv_save_path) = zcl_abap_comm=>get_file_save_dialog_fullpath(
                            im_extension = 'xlsx'
                            im_file_name = |{ '接口日志'(016) }_{ sy-datum }_{ sy-uzeit }| ).

    CHECK lv_save_path IS NOT INITIAL.

    "Step 2 - create salv table instance
    DATA(lo_salv) = zcl_abap_comm=>salv_factory(
                      CHANGING
                        im_t_table          = mt_alvdata ).

    TRY.
        DATA(lo_columns) = lo_salv->get_columns( ).

        DATA(lo_column) = lo_columns->get_column( 'ZIF_CDATE' ).
        set_column_text(
            io_salv_column = lo_column
            iv_text        = CONV #( text-f01 ) ).

        lo_column = lo_columns->get_column( 'ZIF_CTIME' ).
        set_column_text(
            io_salv_column = lo_column
            iv_text        = CONV #( text-f02 ) ).

        lo_column = lo_columns->get_column( 'ZIF_FDATE' ).
        set_column_text(
            io_salv_column = lo_column
            iv_text        = CONV #( text-f03 ) ).

        lo_column = lo_columns->get_column( 'ZIF_FTIME' ).
        set_column_text(
            io_salv_column = lo_column
            iv_text        = CONV #( text-f04 ) ).
      CATCH cx_salv_not_found.
        RETURN.
    ENDTRY.

    "Step 3 - create Excel file and save
    lt_fieldcat = cl_salv_controller_metadata=>get_lvc_fieldcatalog( r_columns      = lo_salv->get_columns( )
                                                                     r_aggregations = lo_salv->get_aggregations( ) ).

    cl_salv_bs_tt_util=>if_salv_bs_tt_util~transform(
      EXPORTING
        xml_version   = if_salv_bs_xml=>version_27
        r_result_data = cl_salv_ex_util=>factory_result_data_table( EXPORTING r_data         = REF #( mt_alvdata )
                                                                              t_fieldcatalog = lt_fieldcat )
        xml_type      = if_salv_bs_xml=>c_type_xlsx
        xml_flavour   = if_salv_bs_c_tt=>c_tt_xml_flavour_full
        gui_type      = if_salv_bs_xml=>c_gui_type_gui
      IMPORTING
        xml           = lv_xml ).

    CLEAR lo_salv.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = lv_xml
      IMPORTING
        output_length = lv_lengh
      TABLES
        binary_tab    = lt_xml_stream.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        bin_filesize            = lv_lengh
        filename                = lv_save_path
        filetype                = 'BIN'
      TABLES
        data_tab                = lt_xml_stream
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        OTHERS                  = 22.
    IF sy-subrc <> 0.
      MESSAGE ID     sy-msgid
              TYPE   sy-msgty
              NUMBER sy-msgno
              WITH   sy-msgv1
                     sy-msgv2
                     sy-msgv3
                     sy-msgv4.
    ENDIF.

  ENDMETHOD.

  METHOD set_column_text.
    io_salv_column->set_long_text( CONV #( iv_text ) ).
    io_salv_column->set_medium_text( iv_text ).
    io_salv_column->set_short_text( CONV #( iv_text ) ).
  ENDMETHOD.

  METHOD set_functions.

    DATA(lo_functions) = mo_salv_tree->get_functions( ).

    lo_functions->set_all( ).

    TRY.

        lo_functions->add_function(
                        name = c_funcname_status-all
                        icon = CONV string( icon_color )
                        tooltip = CONV string( '显示所有'(003) )
                        position = if_salv_c_function_position=>left_of_salv_functions ).

        lo_functions->add_function(
                        name = c_funcname_status-ready
                        icon = CONV string( icon_wd_radio_button_empty )
                        tooltip = CONV string( '显示待处理'(004) )
                        position = if_salv_c_function_position=>left_of_salv_functions ).

        lo_functions->add_function(
                        name = c_funcname_status-success
                        icon = CONV string( icon_led_green )
                        tooltip = CONV string( '显示成功'(005) )
                        position = if_salv_c_function_position=>left_of_salv_functions ).

        lo_functions->add_function(
                        name = c_funcname_status-error
                        icon = CONV string( icon_led_red )
                        tooltip = CONV string( '显示错误'(006) )
                        position = if_salv_c_function_position=>left_of_salv_functions ).

        lo_functions->add_function(
                        name = c_funcname_status-deleted
                        icon = CONV string( icon_delete )
                        tooltip = CONV string( '显示删除'(007) )
                        position = if_salv_c_function_position=>left_of_salv_functions ).

      CATCH cx_salv_wrong_call cx_salv_existing.
    ENDTRY.

  ENDMETHOD.

  METHOD on_usercommand.

    CASE e_salv_function.
      WHEN c_funcname_status-all
        OR c_funcname_status-deleted
        OR c_funcname_status-error
        OR c_funcname_status-ready
        OR c_funcname_status-success.

          TRY.
              mo_salv_tree->get_nodes( )->delete_all( ).
            CATCH cx_salv_error.
              RETURN.
          ENDTRY.

          build_body( e_salv_function ).

      WHEN OTHERS.
        "pass
    ENDCASE.

  ENDMETHOD.

  METHOD cleanup.
    IF mo_salv_tree IS BOUND.
      TRY.
          mo_salv_tree->get_nodes( )->delete_all( ).
        CATCH cx_salv_error.
          "handle exception
      ENDTRY.
    ENDIF.

    CLEAR:
      mo_container,
      mo_salv_tree,
      mt_alvdata,
      mt_tree_out,
      mt_if_config,
      mt_rg_ifnum ,
      mt_rg_ifguid,
      mt_rg_addkey,
      mt_rg_cdate ,
      mt_rg_ctime ,
      mt_rg_status,
      mt_rg_zres1 ,
      mt_rg_zres2 ,
      mt_rg_zres3 ,
      mt_rg_zres4 .

  ENDMETHOD.

ENDCLASS.
