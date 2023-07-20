CLASS zcl_if DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA gs_if_config TYPE ztbs0002 .
    DATA gs_if_data TYPE zift_data .
    DATA gs_if_data_locker TYPE ztbusiness_lock .
    DATA gv_flag_is_retry TYPE xfeld .
    DATA gv_process_counter TYPE int4 VALUE 0 ##NO_TEXT.
    DATA gt_if_logs TYPE ztif_logs .
    DATA gt_msg_priority TYPE ztif_msg_priority .

    METHODS constructor
      IMPORTING
        !iv_ifnum TYPE zifnum .
    METHODS delete
      CHANGING
        !cs_if_data TYPE zift_data .
    METHODS mark_complete
      CHANGING
        !cs_if_data TYPE zift_data .
    METHODS send_email
      IMPORTING
        !iv_commit TYPE xfeld DEFAULT abap_true .
  PROTECTED SECTION.

    METHODS populate_syst_msg_log
      IMPORTING
        !it_return TYPE bapiret2_t OPTIONAL .
    METHODS fill_system_msg
      IMPORTING
        !io_if_cx TYPE REF TO zcx_if_exception .
    METHODS save_log_if_data
      IMPORTING
        !is_input  TYPE data
        !is_output TYPE data .
    METHODS save_log_process_msg .
    METHODS set_repeat_proirity .
    METHODS set_if_data
      IMPORTING
        !is_if_data           TYPE zift_data
      RETURNING
        VALUE(rv_flag_repeat) TYPE xfeld .
    METHODS lock_if_data
      RETURNING
        VALUE(rv_locked) TYPE xfeld .
    METHODS unlock_if_data .
    METHODS data_check
      IMPORTING
        !is_input         TYPE data OPTIONAL
      CHANGING
        !cs_output        TYPE data OPTIONAL
      RETURNING
        VALUE(rv_message) TYPE char100 .
    METHODS set_process_counter .
    METHODS set_guid .
    METHODS init_interface
      IMPORTING
        !iv_ifnum TYPE zifnum .
    METHODS handle_exception_log
      IMPORTING
        !io_if_cx  TYPE REF TO zcx_if_exception OPTIONAL
      CHANGING
        !cs_output TYPE data OPTIONAL .
    METHODS handle_cx_root_exception
      IMPORTING
        !io_if_cx TYPE REF TO cx_root .
    METHODS save_log
      IMPORTING
        !is_input  TYPE data
        !is_output TYPE data .
    METHODS init_data .
  PRIVATE SECTION.

    CLASS-DATA mt_email_logo_solix TYPE solix_tab .

    CLASS-METHODS load_email_signature_image .
ENDCLASS.



CLASS zcl_if IMPLEMENTATION.


  METHOD constructor.

*----- 初始化接口配置
    init_interface( iv_ifnum ).

  ENDMETHOD.


  METHOD data_check.

    DEFINE mac_interface_check.
      "当接口配置没有维护时，保存日志和报文
      gs_if_config-zifsave_log = abap_true.
      gs_if_config-zifsave_payload = 1.
      RAISE EXCEPTION TYPE zcx_if_exception.
    END-OF-DEFINITION.

*----- 检查接口配置是否维护
    IF gs_if_config IS INITIAL.
      MESSAGE e002 INTO rv_message.
      mac_interface_check.
    ENDIF.

*----- 检查接口是否激活
    IF gs_if_config-zifactive IS INITIAL.
      MESSAGE e003 INTO rv_message.
      mac_interface_check.
    ENDIF.

*----- 检查接口出入参是否配置
    IF gs_if_config-zifinput_struc IS INITIAL OR
       gs_if_config-zifoutput_struc IS INITIAL.
      MESSAGE e031 INTO rv_message.
      mac_interface_check.
    ENDIF.

*-----该方法可根据实际情况进行重写，
*    重写的方法必须先执行超类的此方法。super->data_check( ).
*---DEMO:
*    IF gv_flag_is_retry IS INITIAL."初次报文接收时校验
*
**     可通过这两个字段灵活控制接口数据出现错误时是否保存日志和报文
*      gs_if_config-zifsave_log = abap_false.
*      gs_if_config-zifsave_payload = 1.
*      "将错误信息通过全局结构化出参返回给调用方
*      gs_output-mt*-type = 'E'.
*      MESSAGE e000(zif_msg_001) WITH TEXT-e00 INTO gs_output-mt*-message.
**     报错信息通过全局变量GS_OUTPUT给出参赋值，然后返回给调用方，或
**     通过重写HANDLE_EXCEPTION_LOG方法用全局变量GS_OUTPUT给出参赋值，将报错信息返回给调用方
*      cs_output = gs_output.
*      "抛出异常，并终止后续业务逻辑处理
*      RAISE EXCEPTION TYPE zcx_if_exception.
*
*    ENDIF.

  ENDMETHOD.


  METHOD delete.

*&--------------------------------------------------------------------&
* DELETE与接口处理平台程序对接
* 所有子类可继承直接使用此Method，从而使用共通的处理逻辑流
* 通常情况下不需要在子类种重写此方法，如有特殊情况可重写
*&--------------------------------------------------------------------&

    gs_if_data = cs_if_data.

*----- 为当前处理的接口数据加锁,避免多进程处理时,被重复执行.
    CHECK lock_if_data( ) = abap_true.        "如果锁失败,直接退出本执行.


*----- 保存传入的接口数据，加锁成功后 重新检查接口数据的状态，防止重复处理
    CHECK set_if_data( cs_if_data ) = 'X'.   "只有尚未成功的数据才继续往下处理


*----- 设置当前处理的序列号
    set_process_counter( ).


*----- 将接口数据状态改为D，并更新结束时间
    gs_if_data-zif_status = 'D'.
    UPDATE zift_data SET zif_status = 'D'
                         zif_fdate  = sy-datum
                         zif_ftime  = sy-uzeit
                   WHERE zif_num     = cs_if_data-zif_num
                     AND zif_guid    = cs_if_data-zif_guid
                     AND zif_add_key = cs_if_data-zif_add_key.
    IF sy-subrc = 0.
*----- 设置删除标记的报错信息保存
      MESSAGE s006 INTO DATA(lv_msg).

      populate_syst_msg_log( ).    "可复用此方法组装单条message 处理日志

      save_log_process_msg( ).

    ENDIF.

*----- 解锁接口数据
    unlock_if_data( ).

  ENDMETHOD.


  METHOD fill_system_msg.
    DATA lv_msgv1 TYPE sy-msgv1.
    DATA lv_msgv2 TYPE sy-msgv2.
    DATA lv_msgv3 TYPE sy-msgv3.
    DATA lv_msgv4 TYPE sy-msgv4.

    CHECK io_if_cx IS BOUND.
    CHECK io_if_cx->if_t100_message~t100key IS NOT INITIAL.
    CHECK io_if_cx->if_t100_message~t100key <> if_t100_message=>default_textid.

    IF io_if_cx->if_t100_message~t100key-attr1 IS NOT INITIAL.
      ASSIGN io_if_cx->(io_if_cx->if_t100_message~t100key-attr1) TO FIELD-SYMBOL(<lv_msgv>).
      CHECK sy-subrc = 0.
      lv_msgv1 = <lv_msgv>.
    ENDIF.

    IF io_if_cx->if_t100_message~t100key-attr2 IS NOT INITIAL.
      ASSIGN io_if_cx->(io_if_cx->if_t100_message~t100key-attr2) TO <lv_msgv>.
      CHECK sy-subrc = 0.
      lv_msgv2 = <lv_msgv>.
    ENDIF.

    IF io_if_cx->if_t100_message~t100key-attr3 IS NOT INITIAL.
      ASSIGN io_if_cx->(io_if_cx->if_t100_message~t100key-attr3) TO <lv_msgv>.
      CHECK sy-subrc = 0.
      lv_msgv3 = <lv_msgv>.
    ENDIF.

    IF io_if_cx->if_t100_message~t100key-attr4 IS NOT INITIAL.
      ASSIGN io_if_cx->(io_if_cx->if_t100_message~t100key-attr4) TO <lv_msgv>.
      CHECK sy-subrc = 0.
      lv_msgv4 = <lv_msgv>.
    ENDIF.

    MESSAGE ID io_if_cx->if_t100_message~t100key-msgid
            TYPE io_if_cx->if_t100_dyn_msg~msgty
            NUMBER io_if_cx->if_t100_message~t100key-msgno
            WITH lv_msgv1
                 lv_msgv2
                 lv_msgv3
                 lv_msgv4
            INTO DATA(lv_dummy).
  ENDMETHOD.


  METHOD handle_cx_root_exception.
*----- 此方法为处理非预见性异常消息，没必要重写

*----- 设置此接口处理为错误
    gs_if_data-zif_status = 'E'.

*----- 记录消息日志
    gt_if_logs = VALUE #( BASE gt_if_logs ( zif_num          = gs_if_data-zif_num
                                            zif_guid         = gs_if_data-zif_guid
                                            zif_add_key      = gs_if_data-zif_add_key
                                            zif_proc_counter = gv_process_counter
                                            zif_msg_counter  = lines( gt_if_logs ) + 1
                                            if_cdate         = sy-datum
                                            if_ctime         = sy-uzeit
                                            ernam            = sy-uname
                                            msgty            = 'E'
                                            msgtxt           = io_if_cx->get_text( ) ) ).

  ENDMETHOD.


  METHOD handle_exception_log.

*----- 设置此接口处理为错误
    gs_if_data-zif_status = 'E'.

*----- 收集系统消息SY-MSG
    populate_syst_msg_log( ).

*----- 每个接口子类可按需重写此方法，把报错放入相应的结构化字段返回给调用方。
*------DEMO:
*  IF gv_flag_is_retry IS INITIAL OR gs_if_config-zifpmode EQ 'S'.
*    gs_output-type = 'E'.
*    gs_output-message = gt_if_logs[ msgty = 'E' ]-msgtxt.
*    cs_output = gs_output.
*  ENDIF.

*     重写的方法必须先执行超类的此方法

  ENDMETHOD.


  METHOD init_data.

    REFRESH gt_if_logs.

    IF gv_flag_is_retry IS INITIAL."初次执行时

*----- 设置接口处理开始日期 时间
      gs_if_data-zif_cdate = sy-datum.
      gs_if_data-zif_ctime = sy-uzeit.

*----- 设置接口处理的参数：
      gs_if_data-zif_repeat = gs_if_config-zifrepeat.

    ENDIF.

  ENDMETHOD.


  METHOD init_interface.

*----- 保存接口编号
    gs_if_data-zif_num = iv_ifnum.

*----- 获取接口配置信息
    SELECT SINGLE * INTO gs_if_config
                    FROM ztbs0002
                   WHERE zifnum = iv_ifnum.

*----- 获取消息优先级配置表
    SELECT * INTO TABLE gt_msg_priority
                   FROM zift_msg_priorit.

  ENDMETHOD.


  METHOD load_email_signature_image.
    CONSTANTS lc_image_id TYPE w3objid VALUE 'ZIF_EMAIL_LOGO'.
    DATA ls_wwwdata TYPE wwwdatatab.

    IF mt_email_logo_solix IS NOT INITIAL.
      RETURN.
    ENDIF.

    SELECT SINGLE *
      FROM wwwdata
      INNER JOIN tadir
         ON wwwdata~objid = tadir~obj_name
      INTO CORRESPONDING FIELDS OF @ls_wwwdata
      WHERE wwwdata~srtf2  = 0
        AND wwwdata~relid  = 'MI'
        AND tadir~pgmid    = 'R3TR'
        AND tadir~object   = 'W3MI'
        AND tadir~obj_name = @lc_image_id.  "模板名

    CHECK sy-subrc = 0.

    CALL FUNCTION 'WWWDATA_IMPORT'
      EXPORTING
        key               = ls_wwwdata
      TABLES
        mime              = mt_email_logo_solix
      EXCEPTIONS
        wrong_object_type = 1
        import_error      = 2
        OTHERS            = 3.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
  ENDMETHOD.


  METHOD lock_if_data.

*----- 为当前处理的接口数据加锁,避免多进程处理时,被重复执行.
*      此处采用E类型锁而非X类型锁，是因为JOB执行时会提前加锁，
*      故这里允许同一GUI Session累加锁
    CALL FUNCTION 'ENQUEUE_EZZIFT_DATA'
      EXPORTING
        mode_zift_data = 'E'
        mandt          = sy-mandt
        zif_num        = gs_if_data-zif_num
        zif_guid       = gs_if_data-zif_guid
        zif_add_key    = gs_if_data-zif_add_key
        _scope         = '1'
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc <> 0.
      rv_locked = abap_false.
    ELSE.
      rv_locked = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD mark_complete.

*&--------------------------------------------------------------------&
* MARK_COMPLETE与接口处理平台程序对接
* 所有子类可继承直接使用此Method，从而使用共通的处理逻辑流
* 通常情况下不需要在子类种重写此方法，如有特殊情况可重写
*&--------------------------------------------------------------------&

    gs_if_data = cs_if_data.

*----- 为当前处理的接口数据加锁,避免多进程处理时,被重复执行.
    CHECK lock_if_data( ) = abap_true.        "如果锁失败,直接退出本执行.


*----- 保存传入的接口数据，加锁成功后 重新检查接口数据的状态，防止重复处理
    CHECK set_if_data( cs_if_data ) = 'X'.   "只有尚未成功的数据才继续往下处理


*----- 设置当前处理的序列号
    set_process_counter( ).


*----- 将接口数据状态改为D，并更新结束时间
    gs_if_data-zif_status = 'S'.
    UPDATE zift_data SET zif_status = 'S'
                         zif_fdate  = sy-datum
                         zif_ftime  = sy-uzeit
                   WHERE zif_num     = cs_if_data-zif_num
                     AND zif_guid    = cs_if_data-zif_guid
                     AND zif_add_key = cs_if_data-zif_add_key.
    IF sy-subrc = 0.
*----- 设置删除标记的报错信息保存
      MESSAGE s026 INTO DATA(lv_msg).

      populate_syst_msg_log( ).    "可复用此方法组装单条message 处理日志

      save_log_process_msg( ).

    ENDIF.

*----- 解锁接口数据
    unlock_if_data( ).

  ENDMETHOD.


  METHOD populate_syst_msg_log.

*---调用说明：Bapi返回消息结构类型为BAPIRET1时，populate_syst_msg_log( CORRESPONDING #( LT_RETURN ) ).
*            Bapi返回消息结构类型为BAPIRET2时，populate_syst_msg_log( LT_RETURN ).

    "整理批量报错信息记录到日志表中
    IF NOT it_return IS INITIAL.
      gt_if_logs = VALUE #( BASE gt_if_logs FOR wa IN it_return WHERE ( type EQ 'E' ) LET n = lines( gt_if_logs ) IN
                            ( zif_num = gs_if_data-zif_num zif_guid = gs_if_data-zif_guid
                              zif_proc_counter = gv_process_counter zif_msg_counter = n + 1
                              if_cdate = sy-datum if_ctime = sy-uzeit ernam = sy-uname
                              msgid = wa-id msgty = wa-type msgno = wa-number msgtxt = wa-message ) ).
    ENDIF.
    "防止消息类型为空DUMP
    IF sy-msgty EQ ''.
      RETURN.
    ENDIF.
    "收集系统消息(SY-MSG)j记录到日志表中
    DATA(ls_proc_log) = VALUE zift_log( zif_num           = gs_if_data-zif_num
                                        zif_guid          = gs_if_data-zif_guid
                                        zif_add_key       = gs_if_data-zif_add_key
                                        zif_proc_counter  = gv_process_counter
                                        zif_msg_counter   = lines( gt_if_logs ) + 1
                                        if_cdate          = sy-datum
                                        if_ctime          = sy-uzeit
                                        ernam             = sy-uname
                                        msgid             = sy-msgid
                                        msgty             = sy-msgty
                                        msgno             = sy-msgno ).
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO ls_proc_log-msgtxt.

    IF NOT ls_proc_log-msgtxt IS INITIAL."空消息不记录
      READ TABLE gt_if_logs WITH KEY zif_num = gs_if_data-zif_num
                                     zif_guid = gs_if_data-zif_guid
                                     zif_add_key = gs_if_data-zif_add_key
                                     msgtxt = ls_proc_log-msgtxt TRANSPORTING NO FIELDS.
      IF sy-subrc NE 0."已记录的消息不再重复记录
        APPEND ls_proc_log TO gt_if_logs.
      ENDIF.
    ENDIF.

    CLEAR ls_proc_log.

  ENDMETHOD.


  METHOD save_log.

    CHECK gs_if_config-zifsave_log = abap_true. "只有保存日志开关激活时才保存

*----- 如果有报错，设置出错自动重处理优先级
    set_repeat_proirity( ).

*----- 保存接口数据到接口数据池-报文是否保存可配置
    save_log_if_data( EXPORTING is_input  = is_input
                                is_output = is_output ).

*----- 保存接口处理日志
    save_log_process_msg( ).

    COMMIT WORK AND WAIT.

*----- 对接口数据是否已写入数据库进行验证，
*      以确保在解锁该数据之前，已经把正确的状态写入数据库中
*      避免前边针对接口数据重复推送的校验失效
    DO 1000 TIMES.
      SELECT SINGLE zif_status INTO @DATA(lv_status_new) FROM zift_data
                               WHERE zif_num     EQ @gs_if_data-zif_num
                                 AND zif_guid    EQ @gs_if_data-zif_guid
                                 AND zif_add_key EQ @gs_if_data-zif_add_key.
      IF sy-subrc EQ 0 AND lv_status_new EQ gs_if_data-zif_status.
        EXIT.
      ELSE.
        WAIT UP TO 1 SECONDS.
      ENDIF.
    ENDDO.

  ENDMETHOD.


  METHOD save_log_if_data.

    DATA: ls_if_data TYPE zift_data.

*----- 如果接口是同步接口时，且状态为空，默认为S:成功
    IF gs_if_config-zifpmode = 'S' AND gs_if_data-zif_status IS INITIAL.
      gs_if_data-zif_status = 'S'.
    ENDIF.

*----- 保存接口数据记录
    MOVE-CORRESPONDING gs_if_data TO ls_if_data.

    IF gs_if_config-zifsave_payload = '1' OR                                "保存报文
       gs_if_config-zifsave_payload = '2' AND gs_if_data-zif_status = 'E'.  "出错时保存报文

      IF gv_flag_is_retry IS INITIAL.
        ls_if_data-zif_input   = /ui2/cl_json=>serialize( data        = is_input
                                                          compress    = abap_true
                                                          pretty_name = gs_if_config-zifpretty_mode ). "将入参转换为JSON格式字符串且保留空节点
        ls_if_data-zif_output  = /ui2/cl_json=>serialize( data        = is_output
                                                          compress    = abap_true
                                                          pretty_name = gs_if_config-zifpretty_mode ). "将出参转换为JSON格式字符串且忽略空节点
      ELSE."重处理时,只更新出参报文
        ls_if_data-zif_output  = /ui2/cl_json=>serialize( data        = is_output
                                                          compress    = abap_true
                                                          pretty_name = gs_if_config-zifpretty_mode ). "将出参转换为JSON格式字符串且忽略空节点
      ENDIF.

    ENDIF.

    ls_if_data-zif_fdate   = sy-datum.
    ls_if_data-zif_ftime   = sy-uzeit.

    MODIFY zift_data FROM ls_if_data.

  ENDMETHOD.


  METHOD save_log_process_msg.

*----- 保存接口处理日志
    IF gt_if_logs IS NOT INITIAL.
      MODIFY zift_log FROM TABLE gt_if_logs.
    ENDIF.

  ENDMETHOD.


  METHOD send_email.
    CONSTANTS lc_email_template_name TYPE smtg_tmpl_id VALUE 'ZEMIF_BASIC'.
    DATA lt_email_recipient TYPE STANDARD TABLE OF REF TO cl_cam_address_bcs.
    DATA lo_bcs_document TYPE REF TO cl_document_bcs.
    DATA lv_error_message TYPE char200.

    CHECK gs_if_config-zifemail = abap_true. "启用邮件通知时，发送错误提示

    CHECK gs_if_data-zif_status = 'E'.

    "Step 1 - 收件人与发件人配置检查
    SELECT *
      FROM ztbs0003
      WHERE zifnum = @gs_if_data-zif_num
      AND zsender <> ''
      AND zemail <> ''
    INTO TABLE @DATA(lt_email_addr).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_if_exception MESSAGE w032.
    ENDIF.

    TRY.
        DATA(lo_email_sender) = cl_cam_address_bcs=>create_internet_address( lt_email_addr[ 1 ]-zsender ).

        lt_email_recipient = VALUE #( FOR line IN lt_email_addr
                                         ( cl_cam_address_bcs=>create_internet_address(
                                             i_address_string = line-zemail ) ) ).

      CATCH cx_address_bcs INTO DATA(lo_create_addr_error).
        lv_error_message = lo_create_addr_error->get_text( ).
        RAISE EXCEPTION TYPE zcx_if_exception
          MESSAGE w033
            WITH lv_error_message+0(50) lv_error_message+50(50) lv_error_message+100(50) lv_error_message+150(50).
    ENDTRY.


    "Step 2 - 准备调用邮件模板环境
    TRY.
        DATA(lo_bcs) = cl_bcs=>create_persistent( ).

      CATCH cx_send_req_bcs INTO DATA(lo_bcs_error).
        lv_error_message = lo_bcs_error->get_text( ).
        RAISE EXCEPTION TYPE zcx_if_exception
          MESSAGE w033
            WITH lv_error_message+0(50) lv_error_message+50(50) lv_error_message+100(50) lv_error_message+150(50).
    ENDTRY.

    TRY.
        DATA(lo_email_send_api) = cl_smtg_email_api=>get_instance(
          iv_template_id = lc_email_template_name ).

      CATCH cx_smtg_email_common INTO DATA(lo_smtg_error).
        lv_error_message = lo_smtg_error->get_text( ).
        RAISE EXCEPTION TYPE zcx_if_exception
          MESSAGE w033
            WITH lv_error_message+0(50) lv_error_message+50(50) lv_error_message+100(50) lv_error_message+150(50).
    ENDTRY.

    "Step 3 - 填充API的CDS查询参数
    DATA(lt_cds_data_key) = VALUE if_smtg_email_template=>ty_gt_data_key(
                                    ( name = 'zif_num'
                                      value = gs_if_data-zif_num )
                                    ( name = 'zif_guid'
                                      value = gs_if_data-zif_guid )
                                    ( name = 'zif_add_key'
                                      value = gs_if_data-zif_add_key ) ).

    "Step 4 - 根据CDS查询参数填充Email模板
    TRY.
        lo_email_send_api->render_bcs(
                    io_bcs = lo_bcs
                    iv_language = sy-langu
                    it_data_key = lt_cds_data_key ).

      CATCH cx_smtg_email_common INTO lo_smtg_error.
        lv_error_message = lo_smtg_error->get_text( ).
        RAISE EXCEPTION TYPE zcx_if_exception
          MESSAGE w033
            WITH lv_error_message+0(50) lv_error_message+50(50) lv_error_message+100(50) lv_error_message+150(50).
    ENDTRY.

    "额外操作：添加logo
    load_email_signature_image( ).
    TRY.
        lo_bcs_document ?= lo_bcs->document( ).
        lo_bcs_document->add_attachment(
            i_attachment_type     = 'BIN'
            i_attachment_subject  = 'image001.png'  "这里附件名需要和电子邮件模板内维持一致
            i_att_content_hex     = mt_email_logo_solix ).
      CATCH cx_document_bcs cx_send_req_bcs.
        "handle exception
      CATCH cx_sy_move_cast_error.
        "pass
    ENDTRY.

    "Step 5 - 添加发件人与收件人
    TRY.
        LOOP AT lt_email_recipient INTO DATA(lo_email_recipient).
          lo_bcs->add_recipient( lo_email_recipient ).
        ENDLOOP.

        lo_bcs->set_sender( lo_email_sender ).

      CATCH cx_send_req_bcs INTO lo_bcs_error.
        lv_error_message = lo_bcs_error->get_text( ).
        RAISE EXCEPTION TYPE zcx_if_exception
          MESSAGE w033
            WITH lv_error_message+0(50) lv_error_message+50(50) lv_error_message+100(50) lv_error_message+150(50).
    ENDTRY.

    "Step 6 - 发送
    TRY.
        lo_bcs->send( ).

      CATCH cx_send_req_bcs INTO lo_bcs_error.
        lv_error_message = lo_bcs_error->get_text( ).
        RAISE EXCEPTION TYPE zcx_if_exception
          MESSAGE w033
            WITH lv_error_message+0(50) lv_error_message+50(50) lv_error_message+100(50) lv_error_message+150(50).
    ENDTRY.

    IF iv_commit = abap_true.
      COMMIT WORK.
    ENDIF.

  ENDMETHOD.


  METHOD set_guid.
    DATA: lo_message_id_protocol TYPE REF TO if_wsprotocol_message_id.

    CHECK gs_if_data-zif_guid IS INITIAL."重处理时，GUID已经存在

*----- 通过代理类PROXY调用时，可直接使用代理类生成的GUID
    TRY.
*       Get message id protocol
        lo_message_id_protocol ?= cl_proxy_access=>get_server_context( )->get_protocol( if_wsprotocol=>message_id ).
        gs_if_data-zif_guid = lo_message_id_protocol->get_message_id( ).

        IF CONV sysuuid_c32( |{ gs_if_data-zif_guid ALPHA = OUT }| ) = '0'. "如果是outbound或者RFC，取到的guid是‘000000000000000000000000’，需要清空
          CLEAR gs_if_data-zif_guid.
        ENDIF.

      CATCH cx_ai_system_fault.                         "#EC NO_HANDLER

    ENDTRY.

    CHECK gs_if_data-zif_guid IS INITIAL."检查是否代理类已生成GUID

*----- 通过RFC调用时，生成新的GUID
    TRY.
        " 每次接口调用生成唯一的系统GUID
        gs_if_data-zif_guid = NEW cl_system_uuid( )->if_system_uuid~create_uuid_c32( ).

      CATCH cx_uuid_error.

        MESSAGE e001 INTO DATA(lv_dummy).

        gs_if_data-zif_guid = sy-datum && sy-uzeit. "生成GUID失败时，用日期和时间作为ID

        RAISE EXCEPTION TYPE zcx_if_exception.

    ENDTRY.

  ENDMETHOD.


  METHOD set_if_data.

    gs_if_data = is_if_data.

    gv_flag_is_retry = abap_true.

*----- 重新读取本接口数据的状态
    SELECT SINGLE *
             INTO gs_if_data
             FROM zift_data
            WHERE zif_num     = gs_if_data-zif_num
              AND zif_guid    = gs_if_data-zif_guid
              AND zif_add_key = gs_if_data-zif_add_key.

    IF gs_if_data-zif_status = 'E' OR gs_if_data-zif_status = ''.
      rv_flag_repeat = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD set_process_counter.

    IF gv_flag_is_retry = abap_true."当重处理时设置为最大序号加1，新处理时已默认为0

      SELECT MAX( zif_proc_counter ) AS counter
             FROM zift_log
             INTO @DATA(lv_max_counter)
            WHERE zif_num     = @gs_if_data-zif_num
              AND zif_guid    = @gs_if_data-zif_guid
              AND zif_add_key = @gs_if_data-zif_add_key.

      IF sy-subrc = 0.
        gv_process_counter = lv_max_counter + 1.
      ENDIF.
      "新处理时已默认为0，而不是1的目的是后续报表会统计repeat处理的次数，
      "初次处理不应该计入该统计。
    ENDIF.

  ENDMETHOD.


  METHOD set_repeat_proirity.

*----- 只有当接口有报错，并且该数据需要被自动重处理时，才设置出错自动重处理优先级
    CHECK gs_if_data-zif_status = 'E' AND gs_if_data-zif_repeat = 'X'.

    gs_if_data-zif_repeat_prio = gs_if_config-zif_repeat_prio."接口配置表中的出错自动重处理优先级

    LOOP AT gt_if_logs REFERENCE INTO DATA(lo_log) WHERE msgty = 'E'.
      ASSIGN gt_msg_priority[ msgid = lo_log->msgid
                              msgno = lo_log->msgno ] TO FIELD-SYMBOL(<ls_priority>).
      IF sy-subrc = 0.
        "循环所有报错，设置为能找到的最高级别的优先级
        IF gs_if_data-zif_repeat_prio IS INITIAL OR
           gs_if_data-zif_repeat_prio NE <ls_priority>-zif_repeat_prio.
          gs_if_data-zif_repeat_prio = <ls_priority>-zif_repeat_prio."优先以配置的消息对应的优先级为准
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF gs_if_data-zif_repeat_prio IS INITIAL.
      gs_if_data-zif_repeat_prio = '3'."默认为正常
    ENDIF.

  ENDMETHOD.


  METHOD unlock_if_data.

*----- 解锁接口数据
    CALL FUNCTION 'DEQUEUE_EZZIFT_DATA'
      EXPORTING
        mode_zift_data = 'E'
        mandt          = sy-mandt
        zif_num        = gs_if_data-zif_num
        zif_guid       = gs_if_data-zif_guid
        zif_add_key    = gs_if_data-zif_add_key
        _scope         = '1'.

  ENDMETHOD.
ENDCLASS.
