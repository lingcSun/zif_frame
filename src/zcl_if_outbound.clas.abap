class ZCL_IF_OUTBOUND definition
  public
  inheriting from ZCL_IF
  create public .

public section.

  class-data GO_INSTANCE type ref to ZCL_IF_OUTBOUND .

  methods EXECUTE
    importing
      !IS_INPUT type DATA
    changing
      !CS_OUTPUT type DATA optional
    returning
      value(RS_IF_DATA) type ZIFT_DATA .
  methods EXECUTE_JSON
    changing
      !CS_IF_DATA type ZIFT_DATA .
  class-methods GET_INSTANCE
    importing
      !IV_IFNUM type ZIFNUM
    returning
      value(RO_INSTANCE) type ref to ZCL_IF_OUTBOUND .
protected section.

  methods PROCESS_DATA
    importing
      !IS_INPUT type DATA
    exporting
      !ES_OUTPUT type DATA
    raising
      CX_AI_SYSTEM_FAULT
      CX_AI_APPLICATION_FAULT .

  methods SET_GUID
    redefinition .
private section.

  methods CALL_WEBSERVICES_IF_DEMO .
  methods CALL_RESTFUL_OR_ODATA_IF_DEMO .
ENDCLASS.



CLASS ZCL_IF_OUTBOUND IMPLEMENTATION.


  METHOD call_restful_or_odata_if_demo.

*    gs_input = is_input.
*
*    DATA:lv_len         TYPE i, "发送报文长度
*         lv_json_str    TYPE string, "json报文
*         lv_response    TYPE string, "http请求Response返回内容
*         lo_http_client TYPE REF TO if_http_client. "http客户端
*
*    "http调用响应状态
*    DATA:BEGIN OF ls_message,
*           code           TYPE sysubrc,
*           message        TYPE string,
*           message_class  TYPE arbgb,
*           message_number TYPE msgnr,
*         END OF ls_message.
*
*    "创建http客户端
*    CALL METHOD cl_http_client=>create_by_url
*      EXPORTING
*        url                = CONV string( gs_if_config-zifurl )
*      IMPORTING
*        client             = lo_http_client
*      EXCEPTIONS
*        argument_not_found = 1
*        plugin_not_active  = 2
*        internal_error     = 3
*        OTHERS             = 4.
*    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH
*                 sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*      RAISE EXCEPTION TYPE zcx_if_exception.
*    ENDIF.
*
*    "设定不显示目标URL的登录屏幕
*    lo_http_client->propertytype_logon_popup = lo_http_client->co_disabled.
*
*    "设定传输请求内容格式以及编码格式
*    lo_http_client->request->set_content_type( content_type = 'application/json; charset=utf-8' ).
*
*    "SAP调用外围提供的RESTFUL服务皆需通过网关做鉴权及映射处理，故SAP只需固定访问网关即可，SAP访问网关鉴权固定为GATEWAY
*    SELECT SINGLE * FROM ztbs0001 INTO @DATA(ls_auth) WHERE zres1 EQ 'GATEWAY'.
*
*    "设置用户名密码认证 Basic
*    lo_http_client->authenticate( username = CONV string( ls_auth-username ) password = CONV string( ls_auth-password ) ).
*
*    "添加用户名和密码到消息头
*    lo_http_client->request->set_header_field( name = 'username' value = CONV string( ls_auth-username ) ).
*    lo_http_client->request->set_header_field( name = 'password' value = CONV string( ls_auth-password ) ).
*
*    "设定RESTFUL服务的HEADERS(例如token等)
**    lo_http_client->request->set_header_field( name = 'token' value = '6efbd4669dc8171bf8270430cf605933' ).
*
*    "设定调用的 http method 为 POST
*    lo_http_client->request->set_method( if_http_request=>co_request_method_post ). "CO_REQUEST_METHOD_GET
*
*    "报文内容格式转换 此处采用Json格式(表内容->json string）
*    lv_json_str = /ui2/cl_json=>serialize( data = gs_input
*                                           compress = abap_true
*                                           pretty_name = gs_if_config-zifpretty_mode
*                                           expand_includes = abap_true ).
*
*    "待传输内容长度
*    lv_len = strlen( lv_json_str ).
*
*    "设定待传输的报文内容及长度
*    lo_http_client->request->set_cdata( data = lv_json_str offset = 0 length = lv_len ).
*
*    "发送HTTP服务请求
*    lo_http_client->send( EXCEPTIONS http_communication_failure = 1
*                                     http_invalid_state         = 2
*                                     http_processing_failed     = 3
*                                     http_invalid_timeout       = 4
*                                     OTHERS                     = 5 ).
*    IF sy-subrc <> 0.
*      "操作失败，获取失败原因
*      lo_http_client->get_last_error( IMPORTING code = ls_message-code message = ls_message-message
*                                                message_class = ls_message-message_class message_number = ls_message-message_number ).
*      MESSAGE ID ls_message-message_class TYPE 'E' NUMBER ls_message-message_number INTO DATA(lv_dummy)
*              WITH ls_message-code ls_message-message.
*    ENDIF.
*
*    "接收HTTP服务请求处理结果
*    lo_http_client->receive( EXCEPTIONS http_communication_failure = 1
*                                        http_invalid_state         = 2
*                                        http_processing_failed     = 3
*                                        OTHERS                     = 5 ).
*
*    IF sy-subrc <> 0 .
*      "操作失败，获取失败原因
*      lo_http_client->get_last_error( IMPORTING code = ls_message-code message = ls_message-message
*                                                message_class = ls_message-message_class message_number = ls_message-message_number ).
*      MESSAGE ID ls_message-message_class TYPE 'E' NUMBER ls_message-message_number INTO lv_dummy
*              WITH ls_message-code ls_message-message.
*    ELSE.
*      "获取HTTP请求返回结果Response
*      lv_response = lo_http_client->response->get_cdata( ).
*      "将字符串中的回车符替换，否则abap将会识别为# (具体特殊处理根据实际情况调整）
*      REPLACE ALL OCCURRENCES OF REGEX '\n' IN lv_response WITH space.
*      "返回结果内容格式转换 (json string->指定出参结构）
*      /ui2/cl_json=>deserialize( EXPORTING json = lv_response
*                                           assoc_arrays = abap_true
*                                 CHANGING data = gs_output ).
*    ENDIF.
*
*    "关闭http连接，释放连接资源
*    CALL METHOD lo_http_client->close.
*
*    "接口访问异常消息抛出
*    IF NOT lv_dummy IS INITIAL.
*      RAISE EXCEPTION TYPE cx_ai_system_fault.
*    ENDIF.
*
*    "接口返回消息日志处理
*    IF gs_output-returncode EQ '0000'.
*      gs_if_data-zif_status = 'S'.
*      MESSAGE s000(zif_msg_001) WITH gs_output-returnmsg INTO lv_dummy. "#EC NEEDED
*    ELSE.
*      gs_if_data-zif_status = 'E'.
*      MESSAGE e000(zif_msg_001) WITH gs_output-returnmsg INTO lv_dummy. "#EC NEEDED
*    ENDIF.
*
*    populate_syst_msg_log( )."消息记录到接口日志表中
*
*    es_output = gs_output.

  ENDMETHOD.


  METHOD call_webservices_if_demo.

*    gs_input = is_input.
*
*    "接口日志预留字段记录
*    gs_if_data-zres1 = gs_input-parameters-delete_order_information-del_order-order_no.
*
*    TRY .
*        "对方接口服务消费生成的代理类           "OPERATION方法
*        NEW zpxco_order_import_web_service( )->delete_order_info( EXPORTING delete_order_info = gs_input
*                                                                  IMPORTING delete_order_info_response = gs_output ).
*
*      CATCH cx_ai_system_fault.
*
*        RAISE EXCEPTION TYPE cx_ai_system_fault.
*
*    ENDTRY.
*
*    "接口返回消息日志处理
*    IF gs_output-returncode EQ '0000'.
*      gs_if_data-zif_status = 'S'.
*      MESSAGE s000(zif_msg_001) WITH gs_output-returnmsg INTO DATA(lv_dummy). "#EC NEEDED
*    ELSE.
*      gs_if_data-zif_status = 'E'.
*      MESSAGE e000(zif_msg_001) WITH gs_output-returnmsg INTO lv_dummy. "#EC NEEDED
*    ENDIF.
*
*    populate_syst_msg_log( )."消息记录到接口日志表中
*
*    es_output = gs_output.

  ENDMETHOD.


METHOD execute.

  DATA lo_ref_output TYPE REF TO data.

  TRY .

*----- 初始化数据
      init_data( ).

*----- 生成系统唯一消息标识号GUID
      set_guid( ).

*----- 接口配置数据校验
      CHECK data_check( is_input ) IS INITIAL.

*----- 根据调用者是否传入出参，构造数据引用变量
      IF cs_output IS SUPPLIED.
        lo_ref_output = REF #( cs_output ).
      ELSE.
        CREATE DATA lo_ref_output TYPE (gs_if_config-zifoutput_struc).
      ENDIF.
      ASSIGN lo_ref_output->* TO FIELD-SYMBOL(<ls_output>).

*----业务处理
      IF gv_flag_is_retry EQ 'X' OR gs_if_config-zifpmode NE 'A'.
        "重处理或非异步处理时执行process_data
        process_data( EXPORTING is_input  = is_input
                      IMPORTING es_output = <ls_output> ).
      ENDIF.

*----- 保存接口数据及日志
      save_log( EXPORTING is_input  = is_input
                          is_output = <ls_output> ).

    CATCH zcx_if_exception INTO DATA(lo_if_cx).

*----- 填充系统变量
      fill_system_msg( lo_if_cx ).

*----- 出错处理
      handle_exception_log( EXPORTING io_if_cx  = lo_if_cx
                            CHANGING  cs_output = <ls_output> ).

*----- 保存接口数据及日志
      save_log( EXPORTING is_input  = is_input
                          is_output = <ls_output> ).

    CATCH cx_ai_system_fault
          cx_ai_application_fault INTO DATA(lo_ai_fault).

*----- 异常消息处理
      handle_cx_root_exception( lo_ai_fault ).

*----- 保存接口数据及日志
      save_log( EXPORTING is_input  = is_input
                          is_output = <ls_output> ).

    CATCH cx_root INTO DATA(lo_cx_root).

*----- 异常消息处理
      handle_cx_root_exception( lo_cx_root ).

*----- 保存接口数据及日志
      save_log( EXPORTING is_input  = is_input
                          is_output = <ls_output> ).

    CLEANUP.

  ENDTRY.

  rs_if_data = gs_if_data.

ENDMETHOD.


METHOD execute_json.
*&--------------------------------------------------------------------&
* EXECUTE_JSON与接口重处理程序ZIFR_DASHBOARD对接
* 所有子类可继承直接使用此Method，从而使用共通的处理逻辑流
* 通常情况下不需要在子类中重写此方法，如有特殊情况可重写
*&--------------------------------------------------------------------&
  DATA: lo_input  TYPE REF TO data,
        lo_output TYPE REF TO data.

  gs_if_data = cs_if_data.

  REFRESH gt_if_logs.

*----- 为当前处理的接口数据加锁,避免多进程处理时,被重复执行.
  CHECK lock_if_data( ) = abap_true.        "如果锁失败,直接退出本执行.

*----- 保存传入的接口数据，加锁成功后 重新检查接口数据的状态，防止重复处理
  CHECK set_if_data( cs_if_data ) = 'X'.   "只有尚未成功的数据才继续往下处理

*----- 设置当前处理的序列号
  set_process_counter( ).

*----- 将入参从JSON转换为ABAP结构化数据
  TRY .
      CREATE DATA lo_input TYPE (gs_if_config-zifinput_struc).
      ASSIGN lo_input->* TO FIELD-SYMBOL(<ls_input>).
      IF sy-subrc = 0.
        /ui2/cl_json=>deserialize( EXPORTING json = gs_if_data-zif_input
                                             pretty_name = gs_if_config-zifpretty_mode
                                   CHANGING  data = <ls_input> ).
      ENDIF.
    CATCH cx_sy_create_data_error.                      "#EC NO_HANDLER
  ENDTRY.

*----- 将出参从JSON转换为ABAP结构化数据
  TRY .
      CREATE DATA lo_output TYPE (gs_if_config-zifoutput_struc).
      ASSIGN lo_output->* TO FIELD-SYMBOL(<ls_output>).
      IF sy-subrc = 0.
        /ui2/cl_json=>deserialize( EXPORTING json = gs_if_data-zif_output
                                             pretty_name = gs_if_config-zifpretty_mode
                                   CHANGING  data = <ls_output> ).
      ENDIF.
    CATCH cx_sy_create_data_error.                      "#EC NO_HANDLER
  ENDTRY.

*----- 调用EXECUTE处理数据
  execute( EXPORTING is_input  = <ls_input>
           CHANGING cs_output = <ls_output> ).

  cs_if_data = gs_if_data.

*----- 解锁接口数据
  unlock_if_data( ).

ENDMETHOD.


  METHOD get_instance.
*------------------------------------------------------------------
* 创建并获取单一的类实例
*-------------------------------------------------------------------
    IF go_instance IS NOT BOUND.
      SELECT SINGLE zifprocess_class
      INTO @DATA(lo_process_class)
            FROM ztbs0002
            WHERE zifnum EQ @iv_ifnum.

      IF sy-subrc EQ 0.
        CREATE OBJECT go_instance TYPE (lo_process_class)
        EXPORTING
          iv_ifnum = iv_ifnum.
      ELSE.
        go_instance = NEW #( iv_ifnum ).
      ENDIF.

    ENDIF.

    ro_instance = go_instance.

  ENDMETHOD.


  METHOD process_data.                                      "#EC NEEDED

*&--------------------------------------------------------------------&
* 出站接口子类的接口调用样例
* 每个子类需要重写此方法来实现不同的接口格式调用：
* 复制参考样例中的代码并根据实际情况进行调整
*---------------------------------------------------------------------&
* 对方接口类型:
* 1 ...SOAP
*   DEMO参考: ZCL_IF_OUTBOUND->CALL_WEBSERVICES_IF_DEMO
*
* 2 ...RESTFUL OR ODATA
*   DEMO参考: ZCL_IF_OUTBOUND->CALL_RESTFUL_OR_ODATA_IF_DEMO
*&--------------------------------------------------------------------&

  ENDMETHOD.


  METHOD set_guid.

    "调用基类set_guid方法
    super->set_guid( ).

*-- 导出入站接口中导入的接口数据GUID作为出站接口数据的ZIF_ADD_KEY扩展键值，
*-- 实现入站处理和出站处理的对应关系
    IF gs_if_data-zif_add_key IS INITIAL.
      IMPORT p1 = gs_if_data-zif_add_key FROM MEMORY ID 'GUID_INBOUND'."EXPORT在INBOUND相同位置中
    ENDIF.

  ENDMETHOD.
ENDCLASS.
