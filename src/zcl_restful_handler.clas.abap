class ZCL_RESTFUL_HANDLER definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .

  types:
    BEGIN OF s_query,
        name  TYPE string,
        value TYPE string,
      END OF s_query .
  types:
    BEGIN OF t_s_message,
      request  TYPE REF TO data,
      response TYPE REF TO data,
    END OF t_s_message .
  types:
    t_query TYPE STANDARD TABLE OF s_query .

  constants CO_QUERY_IF type STRING value 'ZIFNUM' ##NO_TEXT.
  class-data GS_IF_CONFIG type ZTBS0002 .

  class-methods JSON_MESSAGE_GENERATE
    importing
      !IV_MODE type CHAR1 default 'A'
    changing
      !CV_ZIFNUM type ZIFNUM optional
    returning
      value(RS_JSON) type T_S_MESSAGE .
protected section.

  methods SPLIT_QUERY_STR_2_TAB
    importing
      !IV_STR type STRING
    exporting
      !ET_QUERY type T_QUERY .
  methods DATA_CHECK
    importing
      !IV_IFNUM type ZIFNUM
    returning
      value(RV_OK) type OS_BOOLEAN .
private section.

  types:
    BEGIN OF t_s_symbol,
      name  TYPE string,
      type  TYPE REF TO cl_abap_datadescr,
      value TYPE REF TO data,
    END OF t_s_symbol .
  types:
    t_t_symbol TYPE STANDARD TABLE OF t_s_symbol WITH DEFAULT KEY .

  class-methods SERIALIZE_INT
    changing
      !DATA type DATA .
  class-methods DUMP_SYMBOLS
    changing
      value(CT_SYMBOLS) type T_T_SYMBOL .
  class-methods DUMP_INT
    importing
      !TYPE_DESCR type ref to CL_ABAP_TYPEDESCR
    changing
      value(DATA) type DATA .
  class-methods GET_SYMBOLS
    importing
      !TYPE_DESCR type ref to CL_ABAP_TYPEDESCR
      !DATA type ref to DATA
    returning
      value(RESULT) type T_T_SYMBOL .
ENDCLASS.



CLASS ZCL_RESTFUL_HANDLER IMPLEMENTATION.


METHOD data_check.

*----- 获取接口配置信息
  SELECT SINGLE *
    INTO gs_if_config
    FROM ztbs0002
   WHERE zifnum EQ iv_ifnum.

*----- 检查接口出入参是否配置
  IF gs_if_config-zifinput_struc IS INITIAL OR
     gs_if_config-zifoutput_struc IS INITIAL.
    rv_ok = abap_false.
    RETURN.
  ENDIF.

  rv_ok = abap_true.

ENDMETHOD.


METHOD dump_int.

  DATA:lo_typedesc   TYPE REF TO cl_abap_typedescr,
       lo_structdesc TYPE REF TO cl_abap_structdescr,
       lo_tabledescr TYPE REF TO cl_abap_tabledescr,
       lo_elem_descr TYPE REF TO cl_abap_elemdescr.

  DATA lo_data_ref TYPE REF TO data.

  DATA lt_symbols TYPE t_t_symbol.

  FIELD-SYMBOLS:<line>      TYPE any,
                <value>     TYPE any,
                <value_tab> TYPE STANDARD TABLE,
                <symbol>    LIKE LINE OF lt_symbols,
                <table>     TYPE ANY TABLE.

  CASE type_descr->kind.

    WHEN cl_abap_typedescr=>kind_struct.

      lo_structdesc ?= type_descr.
      GET REFERENCE OF data INTO lo_data_ref.
      lt_symbols = get_symbols( type_descr = lo_structdesc data = lo_data_ref ).
      dump_symbols( CHANGING ct_symbols = lt_symbols ).

    WHEN cl_abap_typedescr=>kind_table.

      lo_tabledescr ?= type_descr.
      lo_typedesc = lo_tabledescr->get_table_line_type( ).

      ASSIGN data TO <table>.

      LOOP AT <table> ASSIGNING <line>.
        IF lo_typedesc->kind EQ cl_abap_typedescr=>kind_struct.
          lo_structdesc ?= lo_typedesc.
          dump_int( EXPORTING type_descr = lo_structdesc CHANGING data = <line> ).
        ENDIF.
      ENDLOOP.

  ENDCASE.

ENDMETHOD.


METHOD dump_symbols.

  FIELD-SYMBOLS: <value>     TYPE any,
                 <symbol>    LIKE LINE OF ct_symbols,
                 <value_tab> TYPE STANDARD TABLE.

  LOOP AT ct_symbols ASSIGNING <symbol>.
    ASSIGN <symbol>-value->* TO <value>.
    IF <value> IS ASSIGNED.
      IF <symbol>-type->kind EQ cl_abap_typedescr=>kind_table.
        ASSIGN <value> TO <value_tab>.
        APPEND INITIAL LINE TO <value_tab>.
      ENDIF.
      IF <symbol>-type->kind NE cl_abap_typedescr=>kind_elem.
        dump_int( EXPORTING type_descr = <symbol>-type CHANGING data = <value> ).
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDMETHOD.


METHOD get_symbols.

  DATA:struct_descr TYPE REF TO cl_abap_structdescr.

  DATA:comp_tab TYPE cl_abap_structdescr=>component_table,
       symb_tab LIKE result,
       symb     LIKE LINE OF symb_tab.

  FIELD-SYMBOLS:<comp>  LIKE LINE OF comp_tab,
                <field> TYPE any.

  IF type_descr->kind EQ cl_abap_typedescr=>kind_struct.

    struct_descr ?= type_descr.
    comp_tab = struct_descr->get_components( ).

    DELETE comp_tab WHERE name EQ 'CONTROLLER'.

    LOOP AT comp_tab ASSIGNING <comp>.
      symb-name = <comp>-name.
      symb-type = <comp>-type.
      ASSIGN data->(symb-name) TO <field>.
      GET REFERENCE OF <field> INTO symb-value.
      APPEND symb TO result.
    ENDLOOP.

  ENDIF.

ENDMETHOD.


METHOD if_http_extension~handle_request.

  DATA lv_zifnum TYPE zifnum. "接口编号

  DATA: lo_input  TYPE REF TO data,
        lo_output TYPE REF TO data.

  DATA(lv_method_name) = server->request->get_method( ).  "请求方法

  CASE lv_method_name.  "POST OR GET

    WHEN server->request->co_request_method_post OR server->request->co_request_method_get.

      DATA(lv_query_parameter) = server->request->get_header_field( name = '~query_string' ).

      split_query_str_2_tab( EXPORTING iv_str = lv_query_parameter IMPORTING et_query = DATA(lt_query) ).

      READ TABLE lt_query INTO DATA(ls_query) WITH KEY name = co_query_if.
      IF sy-subrc EQ 0.
        lv_zifnum = ls_query-value.
      ELSE.
        server->response->set_status( code = 417 reason = 'Expectation Failed' ). "#EC NOTEXT
      ENDIF.

    WHEN OTHERS.

      server->response->set_status( code = 405 reason = 'Method Not Allowed' ). "#EC NOTEXT

  ENDCASE.

  IF data_check( lv_zifnum ) EQ abap_true.  "接口配置校验

    TRY .
        IF lv_method_name EQ server->request->co_request_method_post.
          "创建出入参引用并指向结构化的字段符号
          CREATE DATA lo_input TYPE (gs_if_config-zifinput_struc).
          ASSIGN lo_input->* TO FIELD-SYMBOL(<ls_input>).

          CREATE DATA lo_output TYPE (gs_if_config-zifoutput_struc).
          ASSIGN lo_output->* TO FIELD-SYMBOL(<ls_output>).

*----- 将入参从JSON转换为ABAP结构化数据
          /ui2/cl_json=>deserialize( EXPORTING json = server->request->if_http_entity~get_cdata( )
                                               pretty_name = gs_if_config-zifpretty_mode
                                     CHANGING  data = <ls_input> ).
        ENDIF.
      CATCH cx_sy_create_data_error.                    "#EC NO_HANDLER
        server->response->set_status( code = 400 reason = 'Bad Request' ). "#EC NOTEXT
    ENDTRY.

  ELSE.

    server->response->set_status( code = 405 reason = 'Internal Server Error' detailed_info = CONV string( TEXT-e01 ) ). "#EC NOTEXT

  ENDIF.

  server->response->get_status( IMPORTING code = DATA(lv_code) ).

  IF lv_code EQ 200.

    IF lv_method_name EQ server->request->co_request_method_post. "执行业务处理

      zcl_if_inbound=>get_instance( lv_zifnum )->execute( EXPORTING is_input  = <ls_input>
                                                          IMPORTING es_output = <ls_output> ).
*----- 将出参从ABAP结构化数据转换为JSON
      DATA(lv_resp) = /ui2/cl_json=>serialize( EXPORTING data = <ls_output>
                                                         compress = abap_true
                                                         pretty_name = gs_if_config-zifpretty_mode ).

    ELSEIF lv_method_name EQ server->request->co_request_method_get.  "获取接口出入参报文

      TRY .
          DATA(ls_json) = zcl_restful_handler=>json_message_generate( EXPORTING iv_mode = 'N'
                                                                      CHANGING cv_zifnum = lv_zifnum ).

          lv_resp = /ui2/cl_json=>serialize( EXPORTING data = ls_json
                                                       pretty_name = gs_if_config-zifpretty_mode ).

          REPLACE ALL OCCURRENCES OF |"CONTROLLER":[],| IN lv_resp WITH '' IN CHARACTER MODE.
          REPLACE ALL OCCURRENCES OF |"controller":[],| IN lv_resp WITH '' IN CHARACTER MODE.

        CATCH cx_root.

      ENDTRY.

    ENDIF.

    server->response->set_cdata( data = lv_resp ).

  ENDIF.

  server->response->set_content_type( 'application/json' ). "#EC NOTEXT

ENDMETHOD.


METHOD json_message_generate.

  DATA:lo_input  TYPE REF TO data,
       lo_output TYPE REF TO data.
  DATA:lv_input  TYPE string,
       lv_output TYPE string.

  IF iv_mode EQ 'A'.

    cl_demo_input=>request( EXPORTING text = CONV string( TEXT-001 )
                            CHANGING field = cv_zifnum ).

    IF NEW zcl_restful_handler( )->data_check( cv_zifnum ) EQ abap_false.
      MESSAGE TEXT-e01 TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

  ENDIF.

  TRY .
      CREATE DATA lo_input TYPE (gs_if_config-zifinput_struc).
      ASSIGN lo_input->* TO FIELD-SYMBOL(<ls_input>).

      serialize_int( CHANGING data = <ls_input> ).

      lv_input = /ui2/cl_json=>serialize( data = <ls_input> "compress = abap_true
                                          pretty_name = gs_if_config-zifpretty_mode ). "将入参转换为JSON格式字符串

      CREATE DATA lo_output TYPE (gs_if_config-zifoutput_struc).
      ASSIGN lo_output->* TO FIELD-SYMBOL(<ls_output>).

      serialize_int( CHANGING data = <ls_output> ).

      lv_output = /ui2/cl_json=>serialize( data = <ls_output> "compress = abap_true
                                           pretty_name = gs_if_config-zifpretty_mode ). "将出参转换为JSON格式字符串

    CATCH cx_root.

  ENDTRY.

  rs_json = VALUE #( request = lo_input response = lo_output ).

  CHECK iv_mode EQ 'A'.

  cl_demo_output=>write_text( TEXT-002 ).
  cl_demo_output=>write_json( lv_input ).
  cl_demo_output=>line( ).
  cl_demo_output=>write_text( TEXT-003 ).
  cl_demo_output=>write_json( lv_output ).
  cl_demo_output=>display( ).

ENDMETHOD.


METHOD serialize_int.

  DATA lo_descr TYPE REF TO cl_abap_typedescr.

  lo_descr = cl_abap_typedescr=>describe_by_data( data ).

  dump_int( EXPORTING type_descr = lo_descr CHANGING data = data ).

ENDMETHOD.


  METHOD split_query_str_2_tab.

    DATA lv_flag TYPE c VALUE abap_true.

    DATA:lv_query_url       TYPE string,
         lv_query_parameter TYPE string.

    DATA:ls_value TYPE s_query,
         lt_value TYPE t_query.

    lv_query_url = iv_str.

    WHILE lv_flag EQ abap_true.

      SPLIT lv_query_url AT '&' INTO lv_query_parameter lv_query_url.

      IF lv_query_parameter IS NOT INITIAL.

        SPLIT lv_query_parameter AT '=' INTO ls_value-name ls_value-value.

        IF |{ ls_value-name CASE = UPPER }| EQ co_query_if.
          ls_value-name = |{ ls_value-name CASE = UPPER }|.
          ls_value-value = |{ ls_value-value CASE = UPPER }|.
        ENDIF.

        APPEND ls_value TO lt_value.

        CLEAR ls_value.

      ENDIF.

      IF lv_query_url IS INITIAL.

        lv_flag = abap_false.

      ENDIF.

    ENDWHILE.

    et_query = lt_value.

    REFRESH lt_value.

  ENDMETHOD.
ENDCLASS.
