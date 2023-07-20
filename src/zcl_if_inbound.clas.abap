CLASS zcl_if_inbound DEFINITION
  PUBLIC
  INHERITING FROM zcl_if
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-DATA go_instance TYPE REF TO zcl_if_inbound .

    METHODS execute
      IMPORTING
        !is_input  TYPE data
      EXPORTING
        !es_output TYPE data .
    METHODS execute_json
      CHANGING
        !cs_if_data TYPE zift_data .
    METHODS constructor
      IMPORTING
        !iv_ifnum TYPE zifnum .
    CLASS-METHODS get_instance
      IMPORTING
        !iv_ifnum          TYPE zifnum
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_if_inbound .
  PROTECTED SECTION.

    METHODS set_io_parameter
      IMPORTING
        !is_input  TYPE data
      CHANGING
        !cs_output TYPE data .
    METHODS mandatory_check
      IMPORTING
        !iv_value TYPE data .
    METHODS process_data
      IMPORTING
        !is_input  TYPE data
      CHANGING
        !cs_output TYPE data .
    METHODS lock_business_data
      IMPORTING
        !iv_key1         TYPE char16 OPTIONAL
        !iv_key2         TYPE char18 OPTIONAL
        !iv_key3         TYPE char46 OPTIONAL
        !iv_key4         TYPE char46 OPTIONAL
      CHANGING
        !cs_output       TYPE data OPTIONAL
      RETURNING
        VALUE(rv_result) TYPE xfeld .
    METHODS unlock_business_data
      IMPORTING
        !iv_key1 TYPE char16 OPTIONAL
        !iv_key2 TYPE char18 OPTIONAL
        !iv_key3 TYPE char46 OPTIONAL
        !iv_key4 TYPE char46 OPTIONAL .

    METHODS set_guid
        REDEFINITION .
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_if_inbound IMPLEMENTATION.


  METHOD constructor.

    super->constructor( iv_ifnum ).

  ENDMETHOD.


  METHOD execute.
*&--------------------------------------------------------------------&
* 通过EXECUTE方法与SERVICE INTERFACE进行数据对接
* 所有子类可继承直接使用此Method，从而使用共通的处理逻辑流
* 通常情况下不建议在子类中重写此EXECUTE方法
* 接口数据校验可通过重写data_check方法实现
*&--------------------------------------------------------------------&
    TRY .
*----- 此方法为重要方法，建议重写，可实现功能参照该方法注释DEMO
        set_io_parameter( EXPORTING is_input  = is_input
                          CHANGING  cs_output = es_output ).

*----- 初始化数据
        init_data( ).

*----- 生成系统唯一消息标识号GUID
        set_guid( ).

*----- 接口数据校验（配置 + 内容格式自定义）
        data_check( EXPORTING is_input = is_input
                    CHANGING  cs_output = es_output ).

*----- 对该业务数据加锁，加锁失败则中止处理
        CHECK lock_business_data( CHANGING cs_output = es_output ) EQ abap_true.

*----- 入站数据处理
        IF gv_flag_is_retry EQ 'X' OR  "重处理或非异步处理时执行process_data
           gs_if_config-zifpmode NE 'A'.
          "异步处理且非重处理时，不执行process_data

          process_data( EXPORTING is_input   = is_input
                        CHANGING  cs_output  = es_output ).
        ENDIF.

*----- 保存接口数据及日志
        save_log( EXPORTING is_input  = is_input
                            is_output = es_output ).

*----- 对该业务数据解锁
        unlock_business_data( ).

      CATCH zcx_if_exception INTO DATA(lo_if_cx).

*----- 填充系统变量
        fill_system_msg( lo_if_cx ).

*----- 出错处理
        handle_exception_log( EXPORTING io_if_cx  = lo_if_cx
                              CHANGING  cs_output = es_output ).
*----- 保存接口数据及日志
        save_log( EXPORTING is_input  = is_input
                            is_output = es_output ).

*----- 对该业务数据解锁
        unlock_business_data( ).

      CATCH cx_sy_message_in_plugin_mode INTO DATA(lo_cx_plugin).

        MESSAGE ID lo_cx_plugin->msgid
              TYPE lo_cx_plugin->msgty
            NUMBER lo_cx_plugin->msgno
              WITH lo_cx_plugin->msgv1
                   lo_cx_plugin->msgv2
                   lo_cx_plugin->msgv3
                   lo_cx_plugin->msgv4 INTO DATA(lv_dummy). "#EC NEEDED

*----- 异常消息处理
        handle_exception_log( CHANGING cs_output = es_output ).

*----- 保存接口数据及日志
        save_log( EXPORTING is_input  = is_input
                            is_output = es_output ).

*----- 对该业务数据解锁
        unlock_business_data( ).

      CATCH cx_root INTO DATA(lo_cx_root).               "#EC CATCH_ALL

*----- 异常消息处理
        handle_cx_root_exception( lo_cx_root ).

*----- 保存接口数据及日志
        save_log( EXPORTING is_input  = is_input
                            is_output = es_output ).

*----- 对该业务数据解锁
        unlock_business_data( ).

      CLEANUP.

    ENDTRY.

    "INBOUND类执行结束释放全局内存变量
    FREE MEMORY ID 'GUID_INBOUND'.

  ENDMETHOD.


  METHOD execute_json.
*&--------------------------------------------------------------------&
* EXECUTE_JSON方法与接口重处理程序ZIFR_DASHBOARD对接
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
      CATCH cx_sy_create_data_error.                    "#EC NO_HANDLER
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
      CATCH cx_sy_create_data_error.                    "#EC NO_HANDLER
    ENDTRY.

*----- 调用EXECUTE处理数据
    execute( EXPORTING is_input  = <ls_input>
             IMPORTING es_output = <ls_output> ).

    cs_if_data = gs_if_data.

*----- 解锁接口数据
    unlock_if_data( ).

  ENDMETHOD.


  METHOD get_instance.
*  ------------------------------------------------------------------
*   创建并获取单一的类实例
*  -------------------------------------------------------------------
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


  METHOD lock_business_data.

*----此方法可根据实际情况重写，改写GS_OUTPUT全局出参值，将结果信息返回给调用方


    rv_result = abap_true.

    CHECK gs_if_data_locker IS NOT INITIAL.

    CALL FUNCTION 'ENQUEUE_EZIF_VALUE_KEY'
      EXPORTING
        mode_ztbusiness_lock = 'X'
        mandt                = sy-mandt
        key1                 = gs_if_data_locker-key1
        key2                 = gs_if_data_locker-key2
        key3                 = gs_if_data_locker-key3
        key4                 = gs_if_data_locker-key4
        x_key1               = 'X'
        x_key2               = 'X'
        x_key3               = 'X'
        x_key4               = 'X'
        _scope               = '1'
      EXCEPTIONS
        foreign_lock         = 1
        system_failure       = 2
        OTHERS               = 3.

    IF sy-subrc NE 0.

      rv_result = abap_false.

* Implement suitable error handling here

* 改写DEMO:
*      gs_output-status = 'E'.
*      MESSAGE e020(zif_msg_001) INTO gs_output-message.

    ENDIF.
* 重写之后，需要返回给调用方上边信息时，这里出参需赋值
*    cs_output = gs_output.

  ENDMETHOD.


  METHOD mandatory_check.

    DATA: lo_elemdescr TYPE REF TO cl_abap_elemdescr.

    IF iv_value IS INITIAL.
      lo_elemdescr ?= cl_abap_elemdescr=>describe_by_data( iv_value ).

      RAISE EXCEPTION TYPE zcx_if_exception MESSAGE e021(zif_msg_001) WITH lo_elemdescr->help_id.

    ENDIF.

  ENDMETHOD.


  METHOD process_data.                                      "#EC NEEDED

*&--------------------------------------------------------------------&
* 入站接口子类的处理数据
* 每个子类需要重写此方法来实现不同的处理逻辑：
*
*---------------------------------------------------------------------&
* 详细处理
* 1.1 ...
* 1.2 ...
*&--------------------------------------------------------------------&

  ENDMETHOD.


  METHOD set_guid.

    "调用基类set_guid方法
    super->set_guid( ).

*-- 将入站接口数据的GUID导入到内存中，在对应的出站中导出作为接口数据的扩展ZIF_ADD_KEY键值，
*-- 实现入站处理和出站处理的对应关系
    EXPORT p1 = gs_if_data-zif_guid TO MEMORY ID 'GUID_INBOUND'."IMPORT在OUTBOUND相同位置中

  ENDMETHOD.


  METHOD set_io_parameter.

*----- 1.各个接口继承并重写此方法，将入参，出参数据
*-----   从通用类型数据DATA转换成结构化数据使用
*----- 2.可对数据池结构GS_IF_DATA的预留字段进行赋值
*----- 3.做异步处理时，对出参进行赋值操作
*----- 4.设置业务数据加锁字段值
*----- DEMO如下:

*    gs_input = is_input.
*
*    预留字段赋值
*    gs_if_data-zres1 = gs_input-field1.
*    gs_if_data-zres2 = gs_input-field2.
*
*    接口做异步处理时，设置出参
*    IF gv_flag_is_retry IS INITIAL AND gs_if_config-zifpmode = 'A'. "初次处理且做异步处理
*
*      gs_output-status = 'S'.
*
*      MESSAGE s004(zif_msg_001) INTO gs_output-message.
*
*      cs_output = gs_output.
*
*    ENDIF.
*
*    设置业务数据加锁字段值(根据需求设置KEY1~KEY4锁值)
*    gs_if_data_locker = VALUE #( key1    = gs_input-field1
*                                 key2    = gs_input-field2
*                                 key3    = gs_input-field3
*                                 key4    = gs_input-field4 ).

  ENDMETHOD.


  METHOD unlock_business_data.

    CHECK gs_if_data_locker IS NOT INITIAL.

    CALL FUNCTION 'DEQUEUE_EZIF_VALUE_KEY'
      EXPORTING
        mode_ztbusiness_lock = 'X'
        mandt                = sy-mandt
        key1                 = gs_if_data_locker-key1
        key2                 = gs_if_data_locker-key2
        key3                 = gs_if_data_locker-key3
        key4                 = gs_if_data_locker-key4
        x_key1               = 'X'
        x_key2               = 'X'
        x_key3               = 'X'
        x_key4               = 'X'
        _scope               = '1'.

  ENDMETHOD.
ENDCLASS.
