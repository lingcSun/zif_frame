*&---------------------------------------------------------------------*
*& Include zife_auto_reprocess_f01
*&---------------------------------------------------------------------*
CLASS cl_main DEFINITION.

  PUBLIC SECTION.
    METHODS:
*----- 启动
      execute.

  PROTECTED SECTION.

    TYPES:
      BEGIN OF typ_detail,
        zif_num          TYPE zift_data-zif_num,
        zifnam           TYPE ztbs0002-zifnam,
        zif_guid         TYPE zift_data-zif_guid,
        zif_add_key      TYPE zift_data-zif_add_key,
        zifsystem        TYPE ztbs0002-zifsystem,
        zif_cdate        TYPE zift_data-zif_cdate,
        zif_ctime        TYPE zift_data-zif_ctime,
        zifpriority      TYPE ztbs0002-zifpriority,
        zif_proc_counter TYPE zift_log-zif_proc_counter,
      END OF typ_detail,

      typ_t_alv_detail TYPE STANDARD TABLE OF typ_detail.


    DATA:
      gt_detail      TYPE STANDARD TABLE OF typ_detail,
      gt_detail_show TYPE STANDARD TABLE OF typ_detail,
      gt_if_config   TYPE STANDARD TABLE OF ztbs0002.

    METHODS:
*----- 获取接口数据和接口日志
      get_data,

      lock_data,

      process_data.

ENDCLASS.

*----------------------------------------------------------------------*
*   CLASS IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS cl_main IMPLEMENTATION.

*----------------------------------------------------------------------*
*   METHOD execute
*----------------------------------------------------------------------*
  METHOD execute.

    get_data( ).

    lock_data( ).

    process_data( ).

  ENDMETHOD.

*----------------------------------------------------------------------*
*   METHOD get_data
*----------------------------------------------------------------------*
  METHOD get_data.
*----- 获取每个接口的执行次数
    SELECT zift_log~zif_num,
           zift_log~zif_guid,
           zift_log~zif_add_key,
           MAX( zift_log~zif_proc_counter ) AS zif_proc_counter
      INTO TABLE @DATA(lt_proc_counter)
      FROM zift_log
      INNER JOIN zift_data ON  zift_log~zif_num = zift_data~zif_num         "Added by Xiangyi 2022/4/12
                           AND zift_log~zif_guid = zift_data~zif_guid       "Added by Xiangyi 2022/4/12
                           AND zift_log~zif_add_key = zift_data~zif_add_key "Added by Xiangyi 2022/4/12
     WHERE zift_log~zif_num    IN @s_ifnum
       AND zift_data~zif_cdate IN @s_cdate "Added by Xiangyi 2022/4/12  增加查询最大处理次数的日期限制
      GROUP BY zift_log~zif_num,
               zift_log~zif_guid,
               zift_log~zif_add_key.


*----- 获取接口数据和配置
    SELECT FROM zift_data
           LEFT OUTER JOIN @lt_proc_counter AS counter
                 ON counter~zif_num     = zift_data~zif_num
                AND counter~zif_guid    = zift_data~zif_guid
                AND counter~zif_add_key = zift_data~zif_add_key
           LEFT OUTER JOIN ztbs0002
                 ON ztbs0002~zifnum   = zift_data~zif_num
         FIELDS zift_data~zif_num,
                zift_data~zif_guid,
                zift_data~zif_add_key,
                zif_cdate,
                zif_ctime,
                zifpriority,
                zif_proc_counter
          WHERE zift_data~zif_num     IN @s_ifnum
            AND zif_cdate             IN @s_cdate
            AND zif_status            IN @s_status
            AND zif_status            NE 'S'
            AND zif_status            NE 'D'
            AND zif_repeat            EQ 'X'
            AND zift_data~zif_repeat_prio IN @s_prior
            INTO CORRESPONDING FIELDS OF TABLE @gt_detail.

    SORT gt_detail BY zifpriority  "根据接口配置表的优先级进行排序
                      zif_cdate    "根据创建日期时间做二级排序
                      zif_ctime
                      zif_guid
                      zif_add_key.

    DELETE gt_detail WHERE zif_proc_counter >= p_max.

    SELECT zifnum
           zifprocess_class
           zifinput_struc
           zifoutput_struc
      INTO CORRESPONDING FIELDS OF TABLE gt_if_config
      FROM ztbs0002
      WHERE zifrepeat EQ 'X'.

  ENDMETHOD.

*----------------------------------------------------------------------*
*   METHOD lock_data
*----------------------------------------------------------------------*
  METHOD lock_data.

    DATA:BEGIN OF ls_key,
           mandt       TYPE mandt,
           zif_num     TYPE zifnum,
           zif_guid    TYPE sysuuid_c32,
           zif_add_key TYPE zeif_additional_key,
         END OF ls_key,
         lt_key LIKE TABLE OF ls_key.

    DATA lt_enq TYPE STANDARD TABLE OF seqg3.

    FIELD-SYMBOLS:<fs_key>  TYPE x,
                  <fs_garg> TYPE x.

    CHECK NOT gt_detail IS INITIAL.
    "一次性读取当前已锁定的接口数据
    CALL FUNCTION 'ENQUE_READ'
      EXPORTING
        gclient = sy-mandt
        gname   = 'ZIFT_DATA'
      TABLES
        enq     = lt_enq.
    "将锁值从字符串类型转换为结构类型
    LOOP AT lt_enq INTO DATA(ls_enq).
      ASSIGN ls_enq-garg TO <fs_garg> CASTING TYPE x.
      APPEND INITIAL LINE TO lt_key ASSIGNING FIELD-SYMBOL(<lfs_key>).
      ASSIGN <lfs_key> TO <fs_key> CASTING TYPE x.
      <fs_key> = <fs_garg>.
      CLEAR ls_enq.
    ENDLOOP.

    SORT lt_key BY zif_num zif_guid zif_add_key.
    "锁值校验
    LOOP AT gt_detail INTO DATA(ls_detail).

      READ TABLE lt_key WITH KEY zif_num = ls_detail-zif_num
                                 zif_guid = ls_detail-zif_guid
                                 zif_add_key = ls_detail-zif_add_key BINARY SEARCH
                                 TRANSPORTING NO FIELDS.
      IF sy-subrc EQ 0."已加锁则本次JOB该数据移除

        DELETE gt_detail.

      ELSE."未加锁则进行加锁

        CALL FUNCTION 'ENQUEUE_EZZIFT_DATA'
          EXPORTING
            mode_zift_data = 'E'
            mandt          = sy-mandt
            zif_num        = ls_detail-zif_num
            zif_guid       = ls_detail-zif_guid
            zif_add_key    = ls_detail-zif_add_key
            _scope         = '1'
            _collect       = 'X'
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.
        IF sy-subrc NE 0.
          "因采用_collect参数，故不会抛异常1，所以这里不做异常处理
        ENDIF.

      ENDIF.

    ENDLOOP.
    "将待加锁信息统一提交至锁管理器，减少锁管理器访问，降低系统性能影响
    CALL FUNCTION 'FLUSH_ENQUEUE'
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2.
    IF sy-subrc NE 0.
      "如有异常1发生，则待加锁数据都不会被加锁
      MESSAGE s036.
      LEAVE PROGRAM.
    ENDIF.
* 本程序内不进行解锁动作，解锁由程序异常中止或执行完毕后自动解锁

  ENDMETHOD.

*----------------------------------------------------------------------*
*   METHOD process_data
*----------------------------------------------------------------------*
  METHOD process_data.

    DATA: ls_if_data   TYPE zift_data,
          lv_class     TYPE string,
          lo_instance  TYPE REF TO zcl_if,  "Changed from object type to zcl_if by Xiangyi 2022/4/12
          lv_success   TYPE i,
          lv_error     TYPE i,
          lv_untreated TYPE i. "未处理

    DATA(lv_line) = lines( gt_detail ).

    MESSAGE s010 WITH lv_line.

    LOOP AT gt_detail INTO DATA(ls_detail).

      CLEAR ls_if_data.
      MOVE-CORRESPONDING ls_detail TO ls_if_data.

      ASSIGN gt_if_config[ zifnum =  ls_if_data-zif_num ] TO FIELD-SYMBOL(<fs_config>).
      IF sy-subrc NE 0.

        lv_untreated = lv_untreated + 1.
        CONTINUE.

      ELSE.

        lv_class = <fs_config>-zifprocess_class.

        IF <fs_config>-zifinput_struc IS INITIAL OR <fs_config>-zifoutput_struc IS INITIAL.

          lv_untreated = lv_untreated + 1.
          CONTINUE.

        ENDIF.

      ENDIF.

      FREE lo_instance.

      FREE:zcl_if_inbound=>go_instance,
           zcl_if_outbound=>go_instance.

      TRY .
          MESSAGE s025 WITH ls_if_data-zif_num ls_if_data-zif_guid."当前正在处理的接口信息

          CALL METHOD (lv_class)=>get_instance
            EXPORTING
              iv_ifnum    = ls_if_data-zif_num
            RECEIVING
              ro_instance = lo_instance.

          CALL METHOD lo_instance->('EXECUTE_JSON')
            CHANGING
              cs_if_data = ls_if_data.

          "Added by Xiangyi 2022/4/12 -- Begin -- 增加到达最大处理次数时，邮件发送功能
          IF ls_detail-zif_proc_counter + 1 = p_max.
            lo_instance->send_email( ).
          ENDIF.

        CATCH zcx_if_exception INTO DATA(lo_cx_if).
          MESSAGE lo_cx_if TYPE 'S'.
          "Added by Xiangyi 2022/4/12 -- End

        CATCH cx_root INTO DATA(lo_cx_root).

          MESSAGE w000 WITH lo_cx_root->get_text( ).

      ENDTRY.

      IF ls_if_data-zif_status = 'E'.
        lv_error = lv_error + 1.

      ELSEIF ls_if_data-zif_status = 'S'.
        lv_success = lv_success + 1.

      ENDIF.

    ENDLOOP.

    MESSAGE s011 WITH lv_success lv_error lv_untreated.

  ENDMETHOD.

ENDCLASS.
