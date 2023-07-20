class ZCL_MSG_TREE definition
  public
  create public .

public section.

  types:
    BEGIN OF typ_s_output,
    value TYPE string,
    counter TYPE c LENGTH 10,
  END OF typ_s_output .
  types:
    typ_t_output TYPE STANDARD TABLE OF typ_s_output .
  types:
    typ_v_mode TYPE c LENGTH 1 .
  types:
    begin of typ_node_table_relation,
      node_key TYPE salv_de_node_key,
      data_ref TYPE REF TO data,
    end of typ_node_table_relation.

  constants C_MODE_FULLSCREEN type TYP_V_MODE value '1' ##NO_TEXT.
  constants C_MODE_DOCKING type TYP_V_MODE value '2' ##NO_TEXT.
  constants C_MODE_CUST_CONTAINER type TYP_V_MODE value '3' ##NO_TEXT.
  constants C_MODE_OREF_CONTAINER TYPE TYP_V_MODE value '4'.
  constants C_HIDDEN_DYNNR_HODER type SY-DYNNR value '9999' ##NO_TEXT.
  constants C_FUNCNAMM_CLOSE type SALV_DE_FUNCTION value 'ZCLOSE' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !IV_HIDE_CONTROLLER type XSDBOOLEAN default ABAP_TRUE
      !IV_MODE type TYP_V_MODE default C_MODE_FULLSCREEN
      !IV_REPID type SY-REPID default SY-REPID
      !IV_DYNNR type SY-DYNNR default SY-DYNNR
      !IV_CUST_CONTAINER_NAM type CHAR20 optional
      !IV_TOP_OF_PAGE_TEXT type string optional
      !IV_EXPAND_FLAT_TABLE type XSDBOOLEAN default abap_true
      !IV_SHOW_EMPTY_FIELDS type XSDBOOLEAN DEFAULT abap_false.
  methods DISPLAY
    importing
      !I_DYNAMIC_DATA type DATA .
  methods DISPLAY_FROM_JSON
    importing
      !IV_JSON type STRING
      !IV_PRETTY_NAME type /UI2/CL_JSON=>PRETTY_NAME_MODE default /UI2/CL_JSON=>PRETTY_MODE-NONE
      !iv_ddic_type TYPE typename OPTIONAL
    raising
      ZCX_MSG_TREE .
  METHODS get_container RETURNING VALUE(ro_container) TYPE REF TO cl_gui_container.
  METHODS clear_all_nodes.
  METHODS show_empty_fields IMPORTING iv_is_true TYPE xsdboolean.

  EVENTS flat_table_double_clicked EXPORTING VALUE(es_relation) TYPE typ_node_table_relation.

PROTECTED SECTION.
  DATA mv_mode TYPE typ_v_mode.
  DATA mv_dynnr TYPE sy-dynnr.
  DATA mv_repid TYPE sy-repid.
  DATA mv_cust_container_nam TYPE char20.
  DATA mv_is_docking_hidden TYPE xsdboolean.
  DATA mv_top_of_page_text TYPE string.
  DATA mv_expand_flat_table TYPE xsdboolean.
  DATa mv_show_empty_fields TYPE xsdboolean.

  DATA mv_hide_controller TYPE xsdboolean .

  DATA mv_json TYPE string.
  DATA mt_tree_out TYPE typ_t_output .
  DATA mo_salv_tree TYPE REF TO cl_salv_tree .
  DATA mo_container TYPE REF TO cl_gui_container.
  DATA mt_node_table_relation TYPE STANDARD TABLE OF typ_node_table_relation.

  METHODS new_display
    IMPORTING
      !i_dynamic_data TYPE data.
  METHODS re_display
    IMPORTING
      !i_dynamic_data TYPE data.
  METHODS build_tree
    IMPORTING
      !i_dynamic_data TYPE data .
  METHODS build_single_tree_node
    IMPORTING
      VALUE(iv_dynamic_name) TYPE string OPTIONAL
      !i_dynamic_data TYPE data
      !io_parent_node TYPE REF TO cl_salv_node OPTIONAL .
  METHODS build_header .
  METHODS set_salv_columns_name .
  METHODS build_from_itab IMPORTING i_dynamic_data TYPE data
                                          io_related_node TYPE salv_de_node_key
                                          iv_dynamic_name TYPE string.
  METHODS build_from_struc IMPORTING i_dynamic_data TYPE data
                                           io_related_node TYPE salv_de_node_key
                                           iv_dynamic_name TYPE string.
  METHODS build_from_elem IMPORTING i_dynamic_data TYPE data
                                          io_related_node TYPE salv_de_node_key
                                          iv_dynamic_name TYPE string.
  METHODS get_struc_components IMPORTING i_dynamic_data TYPE data
                                     RETURNING VALUE(rt_component_table) TYPE cl_abap_structdescr=>component_table.
  METHODS hide_docking_container.
  METHODS show_docking_container.
  METHODS set_salv_functions.
  METHODS set_salv_events.
  METHODS on_salv_tree_usercommand FOR EVENT added_function OF cl_salv_events_tree
                                    IMPORTING e_salv_function.
  METHODS on_salv_tree_double_click FOR EVENT double_click OF cl_salv_events_tree
    IMPORTING node_key.

  METHODS is_structure_flat
    IMPORTING
      is_data TYPE data
    RETURNING
      VALUE(rv_result) TYPE xsdboolean.
PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_MSG_TREE IMPLEMENTATION.


  METHOD build_from_elem.
    DATA ls_current_tree_line TYPE typ_s_output.

    DATA(lo_nodes) = mo_salv_tree->get_nodes( ).
    ls_current_tree_line-value = i_dynamic_data.

    IF mv_show_empty_fields = abap_false.
      CHECK i_dynamic_data IS NOT INITIAL.
    ENDIF.

    TRY.
      lo_nodes->add_node(  related_node = io_related_node
                           data_row     = ls_current_tree_line
                           text         = CONV lvc_value( iv_dynamic_name )
                           collapsed_icon = CONV salv_de_tree_image( icon_wd_input_field )
                           expanded_icon = CONV salv_de_tree_image( icon_wd_input_field )
                           relationship = cl_gui_column_tree=>relat_last_child ).
    CATCH cx_salv_msg.
      "Pass
    ENDTRY.
  ENDMETHOD.


  METHOD build_from_itab.
    FIELD-SYMBOLS <fs_tab> TYPE ANY TABLE.
    FIELD-SYMBOLS <fs_flat> TYPE any.
    DATA ls_current_tree_line TYPE typ_s_output.

    DATA(lo_nodes) = mo_salv_tree->get_nodes( ).

    ASSIGN i_dynamic_data TO <fs_tab>.
    CHECK sy-subrc = 0.

    TRY .
      ls_current_tree_line-counter = lines( <fs_tab> ).
      DATA(lo_current_node) = lo_nodes->add_node( related_node = io_related_node
                                                  data_row     = ls_current_tree_line
                                                  text         = CONV lvc_value( iv_dynamic_name )
                                                  folder       = abap_true
                                                  collapsed_icon = CONV salv_de_tree_image( icon_list )
                                                  expanded_icon = CONV salv_de_tree_image( icon_list )
                                                  relationship = cl_gui_column_tree=>relat_last_child ).

      "Added by Xiangyi 2022/5/20 增加node和table数据对象的关联
      mt_node_table_relation = VALUE #( BASE mt_node_table_relation
                                          ( node_key = lo_current_node->get_key( )
                                            data_ref = REF #( <fs_tab> ) ) ).

      LOOP AT <fs_tab> ASSIGNING <fs_flat>.
        "根据传入参数决定是否展开扁平结构的内表 "Added by Xiangyi 2022/5/20
        IF mv_expand_flat_table = abap_false
          AND is_structure_flat( <fs_flat> )
          AND ls_current_tree_line-counter > 1.  "1行时仍然展开
          EXIT.
        ENDIF.

        me->build_single_tree_node( i_dynamic_data = <fs_flat>
                                    iv_dynamic_name = 'line'
                                    io_parent_node = lo_current_node ).
      ENDLOOP.
    CATCH cx_salv_msg.
      "Pass
    ENDTRY.
  ENDMETHOD.


  METHOD build_from_struc.
    DATA ls_current_tree_line TYPE typ_s_output.

    DATA(lo_nodes) = mo_salv_tree->get_nodes( ).

    DATA(lt_struc_component) = get_struc_components( i_dynamic_data ).

*   Added by Xiangyi 2022/5/20 hide structore node counter
*    IF mv_hide_controller = abap_true AND line_exists( lt_struc_component[ name = 'CONTROLLER' ] ).
*      ls_current_tree_line-counter = lines( lt_struc_component ) - 1.
*    ELSE.
*      ls_current_tree_line-counter = lines( lt_struc_component ).
*    ENDIF.

    TRY.
      DATA(lo_current_node) = lo_nodes->add_node( related_node   = io_related_node
                                                  data_row       = ls_current_tree_line
                                                  text           = CONV lvc_value( iv_dynamic_name )
                                                  folder         = abap_true
                                                  collapsed_icon = CONV salv_de_tree_image( icon_structure )
                                                  expanded_icon  = CONV salv_de_tree_image( icon_structure )
                                                  relationship   = cl_gui_column_tree=>relat_last_child ).
    CATCH cx_salv_msg.
      "Pass
    ENDTRY.

    LOOP AT lt_struc_component INTO DATA(ls_struc_component).
      IF mv_hide_controller = abap_true AND ls_struc_component-name = 'CONTROLLER'.
        CONTINUE.
      ENDIF.
      IF ls_struc_component-as_include = abap_false.
        ASSIGN COMPONENT ls_struc_component-name OF STRUCTURE i_dynamic_data TO FIELD-SYMBOL(<fs_any>).
        IF sy-subrc = 0.
          me->build_single_tree_node( i_dynamic_data = <fs_any>
                                      iv_dynamic_name = ls_struc_component-name
                                      io_parent_node  = lo_current_node ).
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD build_header.
    DATA: lo_settings TYPE REF TO cl_salv_tree_settings.

    lo_settings = mo_salv_tree->get_tree_settings( ).
    lo_settings->set_hierarchy_header( '关键字' ).
    lo_settings->set_hierarchy_tooltip( 'Key' ).
    lo_settings->set_hierarchy_size( 40 ).

    DATA: lv_title TYPE salv_de_tree_text.
    lv_title = sy-title.
    lo_settings->set_header( lv_title ).

    IF mv_top_of_page_text IS NOT INITIAL.
      mo_salv_tree->set_top_of_list( NEW cl_salv_form_action_info( text = mv_top_of_page_text ) ).
    ENDIF.
  ENDMETHOD.


  METHOD build_single_tree_node.
    DATA lv_related_node TYPE salv_de_node_key.

    IF io_parent_node IS NOT SUPPLIED.  "Root node
      lv_related_node = ''.
      iv_dynamic_name = 'Root'.
    ELSE.
      lv_related_node = io_parent_node->get_key( ).
    ENDIF.

    DATA(lv_type_kind) = cl_abap_datadescr=>get_data_type_kind( i_dynamic_data ).

    CASE lv_type_kind.
      WHEN cl_abap_datadescr=>typekind_table.

        me->build_from_itab( i_dynamic_data = i_dynamic_data
                             iv_dynamic_name = iv_dynamic_name
                             io_related_node = lv_related_node ).

      WHEN cl_abap_datadescr=>typekind_struct1  "flat structure
        OR cl_abap_datadescr=>typekind_struct2. "Deep structure

        me->build_from_struc( i_dynamic_data = i_dynamic_data
                              iv_dynamic_name = iv_dynamic_name
                              io_related_node = lv_related_node ).

      WHEN cl_abap_datadescr=>typekind_dref.
        "display_from_json时，生成的动态deep结构的数据对象均是data ref的形式，
        "检查到此类情况需要转变为数据对象来重新继续调用本method
        ASSIGN i_dynamic_data->* TO FIELD-SYMBOL(<fs_any>).
        IF sy-subrc = 0.
          me->build_single_tree_node( iv_dynamic_name = iv_dynamic_name
                                      i_dynamic_data = <fs_any>
                                      io_parent_node  = io_parent_node ).
        ENDIF.

      WHEN OTHERS.
        me->build_from_elem( i_dynamic_data = i_dynamic_data
                             iv_dynamic_name = iv_dynamic_name
                             io_related_node = lv_related_node ).

    ENDCASE.

  ENDMETHOD.


  METHOD build_tree.
    me->build_header( ).
    me->build_single_tree_node( i_dynamic_data ).
  ENDMETHOD.


  METHOD constructor.
    "说明： 根据mode值不同需要传入不同的以下参数
    mv_mode = iv_mode.

    "mode为docking及custom container时，需要传入
    mv_repid = iv_repid.
    mv_dynnr = iv_dynnr.

    "mode为custom container时，需要传入屏幕控件名
    mv_cust_container_nam = iv_cust_container_nam.

    "根据情况决定是否传入，其功能是ALV Tree输出时，隐藏Proxy等自动生成结构的Controller字段
    mv_hide_controller = iv_hide_controller.

    "Top of page
    mv_top_of_page_text = iv_top_of_page_text.

    "Expand flat table
    mv_expand_flat_table = iv_expand_flat_table.
  ENDMETHOD.


  METHOD display.

    IF me->mo_salv_tree IS NOT BOUND.
      me->new_display( i_dynamic_data ).
    ELSE.
      me->re_display( i_dynamic_data ).
    ENDIF.

  ENDMETHOD.


  METHOD display_from_json.
    DATA lr_data TYPE REF TO data.

    IF iv_ddic_type IS SUPPLIED AND iv_ddic_type IS NOT INITIAL.
      cl_abap_typedescr=>describe_by_name(
        EXPORTING
          p_name         = iv_ddic_type
        EXCEPTIONS
          type_not_found = 1
          others         = 2 ).
      IF sy-subrc = 0.
        CREATE DATA lr_data TYPE (iv_ddic_type).
      ENDIF.
    ENDIF.

    /ui2/cl_json=>deserialize(
      EXPORTING
        json = iv_json
        pretty_name = iv_pretty_name
      CHANGING
        data = lr_data ) .

    ASSIGN lr_data->* TO FIELD-SYMBOL(<fs_any>).
    IF <fs_any> IS NOT ASSIGNED.
      RAISE EXCEPTION TYPE zcx_msg_tree
        EXPORTING
          textid = zcx_msg_tree=>empty_json.
    ENDIF.

    mv_json = iv_json.

    me->display( <fs_any> ).
  ENDMETHOD.


  METHOD get_container.
    DATA lo_docking_con TYPE REF TO cl_gui_docking_container.
    DATA lo_custom_con TYPE REF TO cl_gui_custom_container.

    IF mo_container IS BOUND.
      ro_container = mo_container.
      RETURN.
    ENDIF.

    CASE me->mv_mode.

      WHEN me->c_mode_docking.

        CREATE OBJECT lo_docking_con
          EXPORTING
            repid = me->mv_repid  "Dociking 模式的时候需要传入
            dynnr = me->mv_dynnr  "Dociking 模式的时候需要传入
            side  = cl_gui_docking_container=>dock_at_right
            ratio = 40.

        mo_container = lo_docking_con.

      WHEN me->c_mode_cust_container.

        CREATE OBJECT lo_custom_con
          EXPORTING
            container_name = mv_cust_container_nam
            repid          = me->mv_repid
            dynnr          = me->mv_dynnr.

        mo_container = lo_custom_con.

      WHEN OTHERS.
        "Pass
    ENDCASE.

    ro_container = mo_container.

  ENDMETHOD.


  METHOD get_struc_components.
    DATA lo_struc_type TYPE REF TO cl_abap_structdescr.

    DATA(lo_typedesc) = cl_abap_datadescr=>describe_by_data( i_dynamic_data ).
    lo_struc_type ?= lo_typedesc.
    rt_component_table = lo_struc_type->get_components( ).

    CHECK rt_component_table IS NOT INITIAL.

    "考虑到有Append Structure的情况，把组件内表完整填充
    DO.
      READ TABLE rt_component_table INTO DATA(ls_component) WITH KEY as_include = abap_true.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      DATA(lv_temp_tabix) = sy-tabix.
      CLEAR lo_struc_type.
      lo_struc_type ?= ls_component-type.
      DATA(lt_temp_component_table) = lo_struc_type->get_components( ).
      DELETE rt_component_table INDEX lv_temp_tabix.
      IF lt_temp_component_table IS INITIAL.
        CONTINUE.
      ENDIF.
      APPEND LINES OF lt_temp_component_table TO rt_component_table.
    ENDDO.
  ENDMETHOD.


  METHOD hide_docking_container.
    DATA lo_docking_container TYPE REF TO cl_gui_docking_container.
    TRY.
      lo_docking_container ?= me->mo_container.
      lo_docking_container->link( EXPORTING repid = me->mv_repid
                                            dynnr = me->c_hidden_dynnr_hoder
                                  EXCEPTIONS OTHERS = 1 ).
      IF sy-subrc = 0.
        me->mv_is_docking_hidden = abap_true.
      ENDIF.
    CATCH cx_sy_move_cast_error.
      "Pass
    ENDTRY.
  ENDMETHOD.


  METHOD new_display.

    DATA(lo_container) = me->get_container( ).

    TRY.
        CALL METHOD cl_salv_tree=>factory
          EXPORTING
            r_container = lo_container
*            hide_header =
          IMPORTING
            r_salv_tree = me->mo_salv_tree
          CHANGING
            t_table     = me->mt_tree_out.

        me->build_tree( i_dynamic_data ).

        me->mo_salv_tree->get_nodes( )->expand_all( ).

        me->set_salv_columns_name( ).
        me->mo_salv_tree->get_columns( )->set_optimize( abap_true ).

        me->set_salv_functions( ).

        me->set_salv_events( ).

        me->mo_salv_tree->display( ).
      CATCH cx_salv_error INTO DATA(lo_cx_err).
        "Pass
        DATA(lv_msg) = lo_cx_err->get_message( ).
    ENDTRY.

  ENDMETHOD.


  METHOD on_salv_tree_usercommand.
    CASE e_salv_function.
      WHEN c_funcnamm_close.
        me->hide_docking_container( ).
    ENDCASE.
  ENDMETHOD.


  METHOD re_display.

    TRY .
      IF me->mv_mode = me->c_mode_docking
        AND me->mv_is_docking_hidden = abap_true.
        me->show_docking_container( ).
      ENDIF.

      me->mo_salv_tree->get_nodes( )->delete_all( ).
      me->build_tree( i_dynamic_data ).
      me->mo_salv_tree->get_nodes( )->expand_all( ).
      me->mo_salv_tree->display( ).
    CATCH cx_salv_error.
      "Pass
    ENDTRY.

  ENDMETHOD.


  METHOD set_salv_columns_name.
    TRY.
      DATA(lo_column) = mo_salv_tree->get_columns( )->get_column( 'VALUE' ).
      lo_column->set_long_text( '值' ).
      lo_column->set_medium_text(  '值' ).
      lo_column->set_short_text(  '值' ).

      lo_column = mo_salv_tree->get_columns( )->get_column( 'COUNTER' ).
      lo_column->set_long_text( '计数器' ).
      lo_column->set_medium_text(  '计数器' ).
      lo_column->set_short_text(  '计数器' ).
    CATCH cx_salv_not_found.
      "PASS
    ENDTRY.
  ENDMETHOD.


  METHOD set_salv_events.
    DATA(lo_salv_event) = me->mo_salv_tree->get_event( ).
    SET HANDLER me->on_salv_tree_usercommand FOR lo_salv_event.
    SET HANDLER me->on_salv_tree_double_click FOR lo_salv_event.
  ENDMETHOD.


  METHOD set_salv_functions.
    DATA(lo_salv_func) = me->mo_salv_tree->get_functions( ).
*    lo_salv_func->set_all( ).

    "Docking时，增加隐藏按钮
    IF me->mv_mode = me->c_mode_docking.
      TRY.
        lo_salv_func->add_function(
                          name = c_funcnamm_close
                          icon = CONV string( icon_close )
                          text = CONV string( '关闭' )
                          tooltip = CONV string( '关闭此界面' )
                          position = if_salv_c_function_position=>right_of_salv_functions ).
      CATCH cx_salv_wrong_call cx_salv_existing.
      ENDTRY.
    ENDIF.
  ENDMETHOD.


  METHOD show_docking_container.
    DATA lo_docking_container TYPE REF TO cl_gui_docking_container.
    TRY.
      lo_docking_container ?= me->mo_container.
      lo_docking_container->link( EXPORTING repid = me->mv_repid
                                            dynnr = me->mv_dynnr
                                  EXCEPTIONS OTHERS = 1 ).
      IF sy-subrc = 0.
        me->mv_is_docking_hidden = abap_false.
      ENDIF.
    CATCH cx_sy_move_cast_error.
      "Pass
    ENDTRY.
  ENDMETHOD.

  METHOD clear_all_nodes.
    CHECK me->mo_salv_tree IS BOUND.
    TRY.
        me->mo_salv_tree->get_nodes( )->delete_all( ).
      CATCH cx_salv_error.
        "handle exception
    ENDTRY.
  ENDMETHOD.

  METHOD is_structure_flat.
    "此方法在排除CONTROLLER后，判断结构是否扁平
    DATA lv_has_deep_element TYPE xsdboolean.
    FIELD-SYMBOLS <fs_any_struc> TYPE any.

    DATA(lv_type_kind) = cl_abap_datadescr=>get_data_type_kind( is_data ).

    CASE lv_type_kind.
      WHEN cl_abap_datadescr=>typekind_dref.
        ASSIGN is_data->* TO <fs_any_struc>.

      WHEN cl_abap_datadescr=>typekind_struct1. "flat
        rv_result = abap_true.
        RETURN.

      WHEN cl_abap_datadescr=>typekind_struct2. "deep
        ASSIGN is_data TO <fs_any_struc>.

      WHEN OTHERS.
        RETURN.

    ENDCASE.

    CHECK <fs_any_struc> IS ASSIGNED.

    DATA(lt_struc_component) = get_struc_components( <fs_any_struc> ).

    LOOP AT lt_struc_component REFERENCE INTO DATA(ls_ref_component) WHERE name <> 'CONTROLLER'.

      CASE ls_ref_component->type->type_kind.
        WHEN cl_abap_datadescr=>typekind_struct1 OR
             cl_abap_datadescr=>typekind_struct2 OR
             cl_abap_datadescr=>typekind_table.
          lv_has_deep_element = abap_true.
          EXIT.

        WHEN OTHERS.
          "pass

      ENDCASE.

    ENDLOOP.

    IF lv_has_deep_element = abap_true.
      rv_result = abap_false.
    ELSE.
      rv_result = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD on_salv_tree_double_click.

    FIELD-SYMBOLS <fs_tab> TYPE ANY TABLE.
    FIELD-SYMBOLS <fs_flat> TYPE any.
    DATA lv_is_flat_tab_double_clicked TYPE xsdboolean.

    TRY.
        DATA(ls_double_clicked_rel) = mt_node_table_relation[ node_key = node_key ].
      CATCH cx_sy_itab_line_not_found.
        RETURN.
    ENDTRY.

    ASSIGN ls_double_clicked_rel-data_ref->* TO <fs_tab>.

    CHECK <fs_tab> IS ASSIGNED.

    CHECK <fs_tab> IS NOT INITIAL.

    LOOP AT <fs_tab> ASSIGNING <fs_flat>.
      IF sy-tabix = 1.
        IF is_structure_flat( <fs_flat> ) = abap_true.
          lv_is_flat_tab_double_clicked = abap_true.
          EXIT.
        ENDIF.
      ELSE.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF lv_is_flat_tab_double_clicked = abap_true.
      RAISE EVENT flat_table_double_clicked
        EXPORTING
          es_relation = ls_double_clicked_rel.
    ENDIF.

  ENDMETHOD.

  METHOD SHOW_EMPTY_FIELDS.
    mv_show_empty_fields = iv_is_true.
  ENDMETHOD.

ENDCLASS.
