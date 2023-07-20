class ZCL_ABAP_COMM definition
  public
  create public .

public section.

  types:
    BEGIN OF typ_s_ecmascript,
        is_success TYPE abap_bool,
        date       TYPE d,
        time       TYPE t,
        timestamp  TYPE timestamp,
      END OF typ_s_ecmascript .

  class-data T_CABN type TT_CABN .
  class-data:
    t_dd07t TYPE STANDARD TABLE OF dd07t .

  class-methods CHECK_TVARVC_EXIST
    importing
      !IM_NAME type TVARVC-NAME
      !IM_TYPE type TVARVC-TYPE
      !IM_VALUE type TVARVC-LOW
    returning
      value(RV_IF_EXIST) type XFELD .
  class-methods CONVERT_CAMEL_TO_SNAKE
    importing
      !IV_CAMEL type STRING
    returning
      value(RV_SNAKE) type STRING .
  class-methods CONV_ABAP_DATE_TO_JAVA_TIMESTM
    importing
      !IV_DATE type SY-DATUM
      !IV_TIME type SY-UZEIT optional
    returning
      value(RV_JAVA_TS) type STRING .
  class-methods CONV_JAVA_TIMESTAMP_TO_DATE
    importing
      !IV_TIMESTAMP type STRING
    returning
      value(RV_DATE) type SY-DATUM .
  class-methods CONV_ISO8601_V1_DATETIME2ABAP
    importing
      !IV_STR type C
    returning
      value(RS_RESULT) type TYP_S_ECMASCRIPT .
    "! <p class="shorttext synchronized" lang="zh">转换iso8601日期格式</p>
    "! @parameter iv_str | <p class="shorttext synchronized" lang="zh">YYYY-MM-DDThh:mm:ss[.mmm]TZD</p>
    "! @parameter rs_result | <p class="shorttext synchronized" lang="zh">结构体</p>
  class-methods CONV_ISO8601_DATETIME
    importing
      !IV_STR type STRING
    returning
      value(RS_RESULT) type TYP_S_ECMASCRIPT .
  class-methods CONV_ATNAM_TO_ATINN
    importing
      !IM_ATNAM type CABN-ATNAM
    returning
      value(RV_ATINN) type CABN-ATINN .
  class-methods CONV_LANGUAGE_INTERNAL
    importing
      !IV_LANGUAGE type CHAR2
    returning
      value(RV_LANGU) type SPRAS .
  class-methods CREATE_FIELDCATALOG_LVC
    importing
      !IT_TABLE type STANDARD TABLE
    returning
      value(RT_FCAT) type LVC_T_FCAT .
  class-methods CREAT_TEXT
    importing
      !IV_ID type THEAD-TDID
      !IV_LANGUAGE type THEAD-TDSPRAS
      !IV_NAME type THEAD-TDNAME
      !IV_OBJECT type THEAD-TDOBJECT
      !IT_LINES type EFG_TAB_TLINE
    exporting
      !EV_OUT type C .
  class-methods DISPLAY_JSON_MSGTREE_DOCKING
    importing
      !IV_JSON type STRING
      !IV_PRETTY_NAME type /UI2/CL_JSON=>PRETTY_NAME_MODE default /UI2/CL_JSON=>PRETTY_MODE-NONE
      !IV_HIDE_CONTROLLER type XSDBOOLEAN default ABAP_TRUE
      !IV_REPID type SY-REPID default SY-REPID
      !IV_DYNNR type SY-DYNNR default SY-DYNNR .
  class-methods DISPLAY_JSON_MSGTREE_FULLSCRE
    importing
      !IV_JSON type STRING
      !IV_PRETTY_NAME type /UI2/CL_JSON=>PRETTY_NAME_MODE default /UI2/CL_JSON=>PRETTY_MODE-NONE
      !IV_HIDE_CONTROLLER type XSDBOOLEAN default ABAP_TRUE .
  class-methods DISPLAY_JSON_MSGTREE_POPUP
    importing
      !IV_JSON type STRING
      !IV_DDIC_TYPE type ZTBS0002-ZIFINPUT_STRUC optional
      !IV_PRETTY_NAME type /UI2/CL_JSON=>PRETTY_NAME_MODE default /UI2/CL_JSON=>PRETTY_MODE-NONE
      !IV_HIDE_CONTROLLER type XSDBOOLEAN default ABAP_TRUE .
  class-methods DOWNLOAD_FILE_FROM_SMW0
    importing
      !IV_OBJID type SOBJ_NAME
      !IV_EXTENSION type C default 'xlsx'
      !IV_PATH type LOCALFILE optional
      !IV_FILENAME type LOCALFILE default 'Template'
    returning
      value(EV_PATH) type LOCALFILE .
  class-methods DOWNLOAD_PRETTY_EXCEL
    importing
      !IM_FILENAME type STRING
      !IM_R_SALV type ref to CL_SALV_TABLE
    changing
      !CM_T_DATA type STANDARD TABLE .
  class-methods GET_DOMAIN_VALUE_TEXT
    importing
      !IM_DOMNAME type DD07T-DOMNAME
      !IM_DOMVALUE type DD07T-DOMVALUE_L
    returning
      value(RV_TEXT) type DD07T-DDTEXT .
  class-methods GET_DOMAIN_TEXT_VALUE
    importing
      !IM_DOMNAME type DD07T-DOMNAME
      !IM_DOMTEXT type DD07T-DOMVALUE_L
    returning
      value(RV_VALUE) type DD07T-DDTEXT .
  class-methods GET_FILE_SAVE_DIALOG_FULLPATH
    importing
      !IM_EXTENSION type STRING optional
      !IM_FILE_NAME type STRING optional
    returning
      value(RV_FULLPATH) type STRING .
  class-methods GET_TVARVC_P
    importing
      !IM_NAME type TVARVC-NAME
    returning
      value(RV_VALUE) type TVARVC-LOW .
  class-methods GET_TVARVC_S
    importing
      !IM_NAME type TVARVC-NAME
    returning
      value(RV_TVARVC_T) type ZTT_TVARVC .
  class-methods MOVE_CORRESPONDING_DYNM_TABLE
    importing
      !IM_T_IN type ref to DATA
      !CM_T_OUT type ref to DATA .
  class-methods PROCESS_EXCEL_XSTRING2ITAB
    importing
      !IV_XSTRING type XSTRING
    exporting
      !ET_TAB type STANDARD TABLE .
  class-methods RTS_GET_TAB_FIELD
    importing
      !IT_TABLE type TABLE
    exporting
      !ET_FLD_INF type LVC_T_FCAT .
  class-methods SALV_COLUMN_F4
    importing
      !IM_R_SALV type ref to CL_SALV_TABLE
      !IM_FILENAME type LVC_FNAME
      !IM_REF_TABLE type LVC_TNAME
      !IM_REF_FIELD type LVC_FNAME .
  class-methods SALV_COLUMN_TEXT
    importing
      !IM_COLUMN_NAME type LVC_FNAME
      !IM_SHORT_TEXT type SCRTEXT_S
      !IM_MEDIUM_TEXT type SCRTEXT_M optional
      !IM_LONG_TEXT type SCRTEXT_L optional
      !IM_R_SALV type ref to CL_SALV_TABLE .
  class-methods SALV_FACTORY
    importing
      !IM_R_CONTAINER type ref to CL_GUI_CONTAINER optional
      !IM_COLUMN_OPTIMIZED type XFELD optional
      !IM_TITLE type LVC_TITLE optional
      !IM_SELECT_MODE type I optional
      !IM_SAVE_LAYOUT type XFELD optional
    changing
      !IM_T_TABLE type TABLE
    returning
      value(IM_R_SALV_TABLE) type ref to CL_SALV_TABLE .
  class-methods UPLOAD_EXCEL_DIALOG2ITAB
    importing
      !IV_FILENAME type LOCALFILE optional
    exporting
      !ET_TAB type STANDARD TABLE .
  class-methods GET_RANGE_NUMBER
    importing
      !IV_BTYPE type CHAR3 default 'SAP'
    returning
      value(RV_NUM) type CHAR20 .
  class-methods GET_USER_NAME
    importing
      !IV_USER type SY-UNAME
    returning
      value(RV_NAME) type SY-UNAME .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA mo_msg_tree TYPE REF TO zcl_msg_tree .
ENDCLASS.



CLASS ZCL_ABAP_COMM IMPLEMENTATION.


  METHOD check_tvarvc_exist.

    SELECT *
      INTO TABLE @DATA(lt_tvarvc)
      FROM tvarvc
     WHERE name = @im_name
       AND type = @im_type
       AND low  = @im_value.
    IF sy-subrc = 0.
      rv_if_exist = 'X'.
    ENDIF.

  ENDMETHOD.


  METHOD convert_camel_to_snake.

    DATA: lv_pos              TYPE i,
          lv_flag_lower_front TYPE c.

    DATA(lv_len) = strlen( iv_camel ).

    DO lv_len TIMES.

      lv_pos = sy-index - 1.

      IF iv_camel+lv_pos(1) CA to_lower( sy-abcde ).  "小写字符

        lv_flag_lower_front = 'X'.

        rv_snake = rv_snake && to_upper( iv_camel+lv_pos(1) ).

      ELSEIF iv_camel+lv_pos(1) CA sy-abcde.   "大写字符

        IF lv_flag_lower_front = 'X'.
          rv_snake = rv_snake && '_'.
        ENDIF.

        rv_snake = rv_snake && iv_camel+lv_pos(1).

        CLEAR lv_flag_lower_front.

      ELSE.   "其他字符

        rv_snake = rv_snake && iv_camel+lv_pos(1).
        IF iv_camel+lv_pos(1) = '_'.
          CLEAR lv_flag_lower_front.
        ENDIF.

      ENDIF.

    ENDDO.

  ENDMETHOD.


  METHOD conv_abap_date_to_java_timestm.

    cl_pco_utility=>convert_abap_timestamp_to_java( EXPORTING iv_date      = iv_date
                                                              iv_time      = iv_time
                                                    IMPORTING ev_timestamp = rv_java_ts ).

  ENDMETHOD.


  METHOD conv_atnam_to_atinn.

    READ TABLE t_cabn REFERENCE INTO DATA(lr_cabn)
                      WITH KEY atnam = im_atnam.
    IF sy-subrc = 0.
      rv_atinn = lr_cabn->atinn.
    ELSE.
      CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
        EXPORTING
          input  = im_atnam
        IMPORTING
          output = rv_atinn.
      IF rv_atinn IS NOT INITIAL.
        t_cabn = VALUE #( BASE t_cabn ( atinn = rv_atinn atnam = im_atnam ) ).
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD conv_iso8601_datetime.
    CHECK iv_str IS NOT INITIAL.
    TRY.
        rs_result-timestamp = cl_xlf_date_time=>parse( iso8601 = iv_str ).
      CATCH cx_xlf_illegal_argument INTO DATA(lo_exception).
        rs_result-is_success = abap_false.
    ENDTRY.
    IF rs_result-timestamp IS INITIAL.
      rs_result-is_success = abap_false.
    ELSE.
      rs_result-is_success = abap_true.
      CONVERT TIME STAMP rs_result-timestamp TIME ZONE sy-zonlo INTO DATE rs_result-date TIME rs_result-time.
    ENDIF.

  ENDMETHOD.


  METHOD conv_iso8601_v1_datetime2abap.
*-----------------------------------------------*
* 说明——支持的日期时间格式 ：
*
*   YYYY-MM-DDTHH:mm:ss.sssZ
*
*   例： 2021-01-21T13:06:47.000+08:00
*       2021-01-21T13:06:47.000+04:00
*       2021-01-21T13:06:47.1000-04:00
*-----------------------------------------------*

    DATA lr_tzone_cond TYPE RANGE OF tznzone.

    FIND REGEX '(\d{4}-\d{2}-\d{2})T(\d{2}:\d{2}:\d{2})\.(\d{3})(.{6})'
      IN iv_str
      SUBMATCHES
        DATA(lv_orig_date)
        DATA(lv_orig_time)
        DATA(lv_orig_milliseconds)
        DATA(lv_orig_zone).
    IF sy-subrc <> 0.
      rs_result-is_success = abap_false.
      RETURN.
    ENDIF.

    DATA(lv_date) = CONV d( |{ lv_orig_date+0(4) }{ lv_orig_date+5(2) }{ lv_orig_date+8(2) }| ).
    DATA(lv_time) = CONV t( |{ lv_orig_time+0(2) }{ lv_orig_time+3(2) }{ lv_orig_time+6(2) }| ).
*    DATA(lv_fs)   = CONV decfloat34( lv_orig_milliseconds / 1000 ).

    DATA(lv_utcdiff) = CONV tznutcdiff( |{ lv_orig_zone+1(2) }{ lv_orig_zone+4(2) }00| ).
    lr_tzone_cond = VALUE #( ( sign = 'I' option = 'CP' low = 'UTC*' ) ).

    SELECT SINGLE b~tzone FROM ttzr AS a
      INNER JOIN ttzz AS b ON a~zonerule = b~zonerule
      WHERE a~utcdiff = @lv_utcdiff
      AND a~utcsign = @lv_orig_zone+0(1)
      AND a~flagactive = @abap_true
      AND b~tzone IN @lr_tzone_cond
    INTO @DATA(lv_zone).

    IF sy-subrc <> 0.
      rs_result-is_success = abap_false.
      RETURN.
    ENDIF.

    CONVERT
      DATE lv_date
      TIME lv_time
      INTO TIME STAMP DATA(lv_ts)
      TIME ZONE lv_zone.

    TRY.
        cl_abap_tstmp=>systemtstmp_utc2syst(
          EXPORTING
            utc_tstmp = lv_ts
          IMPORTING
            syst_date = rs_result-date
            syst_time = rs_result-time ).

        rs_result-is_success = abap_true.
        rs_result-timestamp  = lv_ts.
      CATCH cx_parameter_invalid_range.
        rs_result-is_success = abap_false.
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD conv_java_timestamp_to_date.

    DATA: lv_date  TYPE sy-datum,
          lv_uzeit TYPE sy-uzeit.

    CHECK iv_timestamp IS NOT INITIAL.

    cl_pco_utility=>convert_java_timestamp_to_abap( EXPORTING iv_timestamp = iv_timestamp
                                                    IMPORTING ev_date      = lv_date
                                                              ev_time      = lv_uzeit ).

    CONVERT DATE lv_date
            TIME lv_uzeit
            INTO TIME STAMP DATA(lw_timestamp)
            TIME ZONE 'UTC'.

    CONVERT TIME STAMP lw_timestamp
            TIME ZONE  sy-zonlo
            INTO DATE  rv_date.

  ENDMETHOD.


  METHOD conv_language_internal.

    CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
      EXPORTING
        input  = iv_language
      IMPORTING
        output = rv_langu.
    IF rv_langu IS INITIAL.
      rv_langu = iv_language.
    ENDIF.

  ENDMETHOD.


  METHOD create_fieldcatalog_lvc.
    DATA: lref_table TYPE REF TO data.
    CREATE DATA lref_table LIKE it_table.
    ASSIGN lref_table->* TO FIELD-SYMBOL(<lt_table>).

    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = DATA(lo_salv_table)
                                CHANGING  t_table      = <lt_table> ).

        rt_fcat = cl_salv_controller_metadata=>get_lvc_fieldcatalog(
            r_columns      = lo_salv_table->get_columns( )
            r_aggregations = lo_salv_table->get_aggregations( ) ).
      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.


  METHOD creat_text.


    DATA:lt_lines   TYPE efg_tab_tline.
    lt_lines = it_lines[].

    CALL FUNCTION 'CREATE_TEXT'
      EXPORTING
        fid       = iv_id
        flanguage = iv_language
        fname     = iv_name
        fobject   = iv_object
      TABLES
        flines    = lt_lines[].

    IF sy-subrc <> 0.
      ev_out = 'E'.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    ELSE.
      ev_out = 'S'.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

    ENDIF.

  ENDMETHOD.


  METHOD display_json_msgtree_docking.
    IF mo_msg_tree IS NOT BOUND.
      mo_msg_tree = NEW zcl_msg_tree(
        iv_mode            = zcl_msg_tree=>c_mode_docking
        iv_hide_controller = iv_hide_controller
        iv_repid           = iv_repid
        iv_dynnr           = iv_dynnr ).
    ENDIF.

    TRY .
        mo_msg_tree->display_from_json( iv_json        = iv_json
                                        iv_pretty_name = iv_pretty_name ).
      CATCH zcx_msg_tree INTO DATA(lo_x_msgtree).
        DATA(lv_msg) = lo_x_msgtree->get_text( ).
        MESSAGE lv_msg TYPE 'E' DISPLAY LIKE 'S'.
    ENDTRY.

  ENDMETHOD.


  METHOD display_json_msgtree_fullscre.
    IF mo_msg_tree IS NOT BOUND.
      mo_msg_tree = NEW zcl_msg_tree(
        iv_mode            = zcl_msg_tree=>c_mode_fullscreen
        iv_hide_controller = iv_hide_controller ).
    ENDIF.

    TRY .
        mo_msg_tree->display_from_json( iv_json        = iv_json
                                        iv_pretty_name = iv_pretty_name ).
      CATCH zcx_msg_tree INTO DATA(lo_x_msgtree).
        DATA(lv_msg) = lo_x_msgtree->get_text( ).
        MESSAGE lv_msg TYPE 'E' DISPLAY LIKE 'S'.
    ENDTRY.
  ENDMETHOD.


  METHOD display_json_msgtree_popup.
    CALL FUNCTION 'ZBCFM_SHOW_MSGTREE_AS_POPUP'
      EXPORTING
        iv_json            = iv_json
        iv_ddic_type       = iv_ddic_type
        iv_pretty_name     = iv_pretty_name
        iv_hide_controller = iv_hide_controller.
  ENDMETHOD.


  METHOD download_file_from_smw0.
    DATA ls_wwwdata TYPE wwwdatatab.

    ev_path = iv_path.

    SELECT SINGLE *
      FROM wwwdata
      INNER JOIN tadir
         ON wwwdata~objid = tadir~obj_name
      INTO CORRESPONDING FIELDS OF @ls_wwwdata
      WHERE wwwdata~srtf2  = 0
        AND wwwdata~relid  = 'MI'        "标识二进制的对象
        AND tadir~pgmid    = 'R3TR'
        AND tadir~object   = 'W3MI'
        AND tadir~obj_name = @iv_objid.  "模板名

    IF sy-subrc <> 0.
*      RAISE EXCEPTION.
      MESSAGE s002(zbc_msg_001) WITH iv_objid DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF ev_path IS INITIAL.
      ev_path = zcl_abap_comm=>get_file_save_dialog_fullpath( im_file_name = |{ iv_filename }_{ sy-datum }_{ sy-uzeit }|
                                                              im_extension = CONV #( iv_extension ) ).
    ENDIF.

    CHECK ev_path IS NOT INITIAL.

    CALL FUNCTION 'DOWNLOAD_WEB_OBJECT'
      EXPORTING
        key         = ls_wwwdata
        destination = ev_path.

  ENDMETHOD.


  METHOD download_pretty_excel.

    DATA: lt_fieldcat   TYPE lvc_t_fcat,
          lv_xml        TYPE xstring,
          lt_xml_stream TYPE xml_rawdata,
          lv_lengh      TYPE i.

    CHECK im_filename IS NOT INITIAL.

    lt_fieldcat = cl_salv_controller_metadata=>get_lvc_fieldcatalog( r_columns      = im_r_salv->get_columns( )
                                                                     r_aggregations = im_r_salv->get_aggregations( ) ).

    cl_salv_bs_tt_util=>if_salv_bs_tt_util~transform(
      EXPORTING
        xml_version   = if_salv_bs_xml=>version_27
        r_result_data = cl_salv_ex_util=>factory_result_data_table( EXPORTING r_data         = REF #( cm_t_data )
                                                                              t_fieldcatalog = lt_fieldcat )
        xml_type      = if_salv_bs_xml=>c_type_xlsx
        xml_flavour   = if_salv_bs_c_tt=>c_tt_xml_flavour_full
        gui_type      = if_salv_bs_xml=>c_gui_type_gui
      IMPORTING
        xml           = lv_xml ).

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
        filename                = im_filename
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


  METHOD get_domain_text_value.
    IF line_exists( t_dd07t[ domname = im_domname ddtext = im_domtext ] ).
      rv_value = t_dd07t[ domname = im_domname ddtext = im_domtext ]-domvalue_l.
    ELSE.
      SELECT SINGLE *
        INTO @DATA(ls_dd07t)
        FROM dd07t
       WHERE domname    = @im_domname
         AND ddlanguage = @sy-langu
         AND as4local   = 'A'
         AND ddtext = @im_domtext.
      IF sy-subrc = 0.
        rv_value = ls_dd07t-domvalue_l.
        APPEND ls_dd07t TO t_dd07t.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_domain_value_text.

    IF line_exists( t_dd07t[ domname = im_domname domvalue_l = im_domvalue ] ).
      rv_text = t_dd07t[ domname = im_domname domvalue_l = im_domvalue ]-ddtext.
    ELSE.
      SELECT SINGLE *
        INTO @DATA(ls_dd07t)
        FROM dd07t
       WHERE domname    = @im_domname
         AND ddlanguage = @sy-langu
         AND as4local   = 'A'
         AND domvalue_l = @im_domvalue.
      IF sy-subrc = 0.
        rv_text = ls_dd07t-ddtext.
        APPEND ls_dd07t TO t_dd07t.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_file_save_dialog_fullpath.
    DATA: lv_name TYPE string,
          lv_path TYPE string.

    CALL METHOD cl_gui_frontend_services=>file_save_dialog
      EXPORTING
        default_extension         = im_extension       "文件格式
        default_file_name         = im_file_name       "默认名称
      CHANGING
        filename                  = lv_name      "文件名
        path                      = lv_path      "保存路径
        fullpath                  = rv_fullpath  "文件名+保存路径
      EXCEPTIONS
        cntl_error                = 1
        error_no_gui              = 2
        not_supported_by_gui      = 3
        invalid_default_file_name = 4
        OTHERS                    = 5.
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


  METHOD get_range_number.

    DO 5 TIMES.
      CALL FUNCTION 'ENQUEUE_EZNUMCRANGE'
        EXPORTING
          mode_zabapt_numcrange = 'E'
          mandt                 = sy-mandt
          btype                 = iv_btype
          bdate                 = sy-datum
        EXCEPTIONS
          foreign_lock          = 1
          system_failure        = 2
          OTHERS                = 3.
      IF sy-subrc EQ 0.
        SELECT SINGLE bnumc INTO @DATA(lv_bnumc) FROM zabapt_numcrange
          WHERE btype EQ @iv_btype
            AND bdate EQ @sy-datum.
        IF sy-subrc EQ 0.
          ADD 1 TO lv_bnumc.
          CONCATENATE iv_btype '_' sy-datum lv_bnumc INTO rv_num.
          CONDENSE rv_num NO-GAPS.
          UPDATE zabapt_numcrange SET bnumc = lv_bnumc WHERE btype EQ iv_btype AND bdate EQ sy-datum.
        ELSE.
          CONCATENATE iv_btype '_' sy-datum '00000001' INTO rv_num.
          DATA(ls_ranggenum) = VALUE zabapt_numcrange( btype = iv_btype bdate = sy-datum bnumc = 00000001 ).
          INSERT zabapt_numcrange FROM ls_ranggenum.
        ENDIF.
        COMMIT WORK.
        CALL FUNCTION 'DEQUEUE_EZNUMCRANGE'
          EXPORTING
            mode_zabapt_numcrange = 'E'
            mandt                 = sy-mandt
            btype                 = iv_btype
            bdate                 = sy-datum.
        EXIT.
      ELSE.
        WAIT UP TO '0.1' SECONDS.
      ENDIF.

    ENDDO.
  ENDMETHOD.


  METHOD get_tvarvc_p.

    SELECT SINGLE low
             INTO rv_value
             FROM tvarvc
            WHERE name = im_name
              AND type = 'P'
              AND numb = '0000'.

  ENDMETHOD.


  METHOD get_tvarvc_s.

    SELECT *
      INTO TABLE rv_tvarvc_t
      FROM tvarvc
     WHERE name = im_name
       AND type = 'S'.

  ENDMETHOD.


  METHOD get_user_name.
    DATA: lv_name_first TYPE ad_namefir,
          lv_name_last  TYPE ad_namelas.
    SELECT SINGLE adrp~name_first adrp~name_last INTO (lv_name_first,lv_name_last)
    FROM usr21
    INNER JOIN adrp ON usr21~persnumber EQ adrp~persnumber
    WHERE bname = iv_user.
    IF sy-subrc = 0.
      CONCATENATE lv_name_last  lv_name_first INTO rv_name.
      CONDENSE rv_name NO-GAPS.
    ENDIF.
  ENDMETHOD.


  METHOD move_corresponding_dynm_table.

    FIELD-SYMBOLS:
      <lf_t_in>  TYPE ANY TABLE,
      <lf_t_out> TYPE STANDARD TABLE.

    ASSIGN im_t_in->*  TO <lf_t_in>.
    ASSIGN cm_t_out->* TO <lf_t_out>.

    DATA: lr_table_descr TYPE REF TO cl_abap_tabledescr,
          lr_sturcdescr  TYPE REF TO cl_abap_structdescr.

    lr_table_descr ?= cl_abap_typedescr=>describe_by_data( <lf_t_in> ).
    lr_sturcdescr  ?= lr_table_descr->get_table_line_type( ).

    LOOP AT <lf_t_in> ASSIGNING FIELD-SYMBOL(<lf_in>).
      APPEND INITIAL LINE TO <lf_t_out> ASSIGNING FIELD-SYMBOL(<lf_out>).
      LOOP AT lr_sturcdescr->components REFERENCE INTO DATA(lr_field).
        ASSIGN COMPONENT lr_field->name OF STRUCTURE <lf_in> TO FIELD-SYMBOL(<lf_value_in>).
        IF sy-subrc = 0.
          ASSIGN COMPONENT lr_field->name OF STRUCTURE <lf_out> TO FIELD-SYMBOL(<lf_value_out>).
          IF sy-subrc = 0.
            <lf_value_out> = <lf_value_in>.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD process_excel_xstring2itab.
    DATA : lo_excel_ref TYPE REF TO cl_fdt_xl_spreadsheet .
    FIELD-SYMBOLS <lt_data> TYPE ANY TABLE.

    TRY .
        lo_excel_ref = NEW cl_fdt_xl_spreadsheet(
          document_name = 'TEMP_NAME'
          xdocument     = iv_xstring ).
      CATCH cx_fdt_excel_core.
        "Implement suitable error handling here
        RETURN.
    ENDTRY .

    lo_excel_ref->if_fdt_doc_spreadsheet~get_worksheet_names(
      IMPORTING
        worksheet_names = DATA(lt_worksheets) ).

    IF lt_worksheets IS INITIAL.
      RETURN.
    ENDIF.

    READ TABLE lt_worksheets INTO DATA(lv_woksheetname) INDEX 1.

    DATA(lo_data_ref) = lo_excel_ref->if_fdt_doc_spreadsheet~get_itab_from_worksheet( lv_woksheetname ).

    ASSIGN lo_data_ref->* TO FIELD-SYMBOL(<ls_any_data>).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CHECK cl_abap_datadescr=>get_data_type_kind( <ls_any_data> ) = cl_abap_datadescr=>typekind_table.

    ASSIGN <ls_any_data> TO <lt_data>.
    CHECK sy-subrc = 0.

    DATA(lv_counter) = VALUE sy-index( ).

    LOOP AT <lt_data> ASSIGNING FIELD-SYMBOL(<ls_data>).
*      IF iv_with_headerline = abap_true AND sy-tabix = 1.  "跳过第一行
*        CONTINUE.
*      ENDIF.
      IF <ls_data> IS INITIAL.  "数据为空时，跳过
        CONTINUE.
      ENDIF.
      lv_counter = 0.
      APPEND INITIAL LINE TO et_tab ASSIGNING FIELD-SYMBOL(<ls_out_tab>).
      DO.
        lv_counter = lv_counter + 1.
        ASSIGN COMPONENT lv_counter OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_data>).
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.

        ASSIGN COMPONENT lv_counter OF STRUCTURE <ls_out_tab> TO FIELD-SYMBOL(<lv_out_data>).
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        <lv_out_data> = <lv_data>.
      ENDDO.
    ENDLOOP.

  ENDMETHOD.


  METHOD rts_get_tab_field.
    DATA:
      lv_index       TYPE sy-tabix,
      ls_dfies       TYPE dfies,
      ls_fld_inf     TYPE lvc_s_fcat,
      ls_component   TYPE LINE OF cl_abap_structdescr=>component_table,
      lt_component   TYPE cl_abap_structdescr=>component_table,
      lo_elemdescr   TYPE REF TO cl_abap_elemdescr,
      lo_structdescr TYPE REF TO cl_abap_structdescr,
      lo_tabledescr  TYPE REF TO cl_abap_tabledescr,
      lo_typedescr   TYPE REF TO cl_abap_typedescr.

    lo_typedescr   = cl_abap_tabledescr=>describe_by_data( it_table ).
    lo_tabledescr  = CAST cl_abap_tabledescr( lo_typedescr ).

    lo_structdescr = CAST cl_abap_structdescr( lo_tabledescr->get_table_line_type( ) ).

    lt_component   = lo_structdescr->get_components( ).

    LOOP AT lt_component INTO ls_component.
      lv_index = sy-tabix.

      IF ls_component-as_include = abap_on.
        cl_abap_typedescr=>describe_by_name(
          EXPORTING
            p_name         = ls_component-name      " Type name
          RECEIVING
            p_descr_ref    = lo_typedescr " Reference to description object
          EXCEPTIONS
            type_not_found = 1           " Type with name p_name could not be found
            OTHERS         = 2
        ).
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        lo_structdescr = CAST cl_abap_structdescr( lo_typedescr ).
        INSERT LINES OF lo_structdescr->get_components( ) INTO lt_component INDEX ( lv_index + 1 ).

        CONTINUE.
      ENDIF.

      CLEAR ls_dfies.


      IF ls_component-type->kind EQ cl_abap_typedescr=>kind_elem.

        lo_elemdescr = CAST cl_abap_elemdescr( ls_component-type ).
        lo_elemdescr->get_ddic_field(
          RECEIVING
            p_flddescr   = ls_dfies " Field Description
          EXCEPTIONS
            not_found    = 1          " Type could not be found
            no_ddic_type = 2          " Typ is not a dictionary type
            OTHERS       = 3
        ).
        IF sy-subrc <> 0.
          CLEAR ls_dfies.
        ENDIF.

      ENDIF.

      ls_fld_inf-fieldname = ls_component-name.
      ls_fld_inf-scrtext_l = ls_dfies-scrtext_l.
      ls_fld_inf-scrtext_m = ls_dfies-scrtext_m.
      ls_fld_inf-scrtext_s = ls_dfies-scrtext_s.

      APPEND ls_fld_inf TO et_fld_inf.
      CLEAR ls_fld_inf.
    ENDLOOP.

  ENDMETHOD.


  METHOD salv_column_f4.

    DATA: lr_column  TYPE REF TO cl_salv_column_table.

    "处理状态的F4帮助
    lr_column ?= im_r_salv->get_columns( )->get_column( im_filename ).
    lr_column->set_ddic_reference( VALUE #( table = im_ref_table field = im_ref_field ) ).
    lr_column->set_f4( if_salv_c_bool_sap=>true ).

  ENDMETHOD.


  METHOD salv_column_text.

    DATA: lr_column  TYPE REF TO cl_salv_column_table.

    lr_column ?= im_r_salv->get_columns( )->get_column( im_column_name ).
    lr_column->set_short_text( im_short_text ).

    IF im_medium_text IS SUPPLIED.
      lr_column->set_medium_text( im_medium_text ).
    ELSE.
      lr_column->set_medium_text( CONV #( im_short_text ) ).
    ENDIF.

    IF im_long_text IS SUPPLIED.
      lr_column->set_long_text( im_long_text ).
    ELSE.
      lr_column->set_long_text( CONV #( im_short_text ) ).
    ENDIF.

  ENDMETHOD.


  METHOD salv_factory.
    IF im_r_container IS SUPPLIED.
      TRY .
          CALL METHOD cl_salv_table=>factory
            EXPORTING
              r_container  = im_r_container
            IMPORTING
              r_salv_table = im_r_salv_table
            CHANGING
              t_table      = im_t_table.
        CATCH cx_salv_msg.                              "#EC NO_HANDLER
      ENDTRY.
    ELSE.
      TRY .
          CALL METHOD cl_salv_table=>factory
            IMPORTING
              r_salv_table = im_r_salv_table
            CHANGING
              t_table      = im_t_table.
        CATCH cx_salv_msg.                              "#EC NO_HANDLER
      ENDTRY.

    ENDIF.

    im_r_salv_table->get_columns( )->set_optimize( im_column_optimized ).
    im_r_salv_table->get_display_settings( )->set_list_header( im_title ).
    im_r_salv_table->get_columns( )->set_key_fixation( )..

    "添加保存布局功能
    IF im_save_layout = abap_true.
      im_r_salv_table->get_layout( )->set_key( VALUE #( report = sy-cprog ) ).
      im_r_salv_table->get_layout( )->set_save_restriction( if_salv_c_layout=>restrict_none ).
      im_r_salv_table->get_layout( )->set_default( abap_true ).
    ENDIF.

    IF im_select_mode IS NOT INITIAL.
      im_r_salv_table->get_selections( )->set_selection_mode( im_select_mode ).
    ENDIF.

    im_r_salv_table->get_functions( )->set_all( abap_true ).

  ENDMETHOD.


  METHOD upload_excel_dialog2itab.

    DATA: lt_file_table TYPE filetable,
          ls_file_table TYPE file_table,
          lv_file_path  TYPE string.

    DATA: lv_rc TYPE i.

    IF iv_filename IS SUPPLIED.
      lv_file_path = iv_filename.
    ELSE.
      CALL METHOD cl_gui_frontend_services=>file_open_dialog
        EXPORTING
          window_title = '选择文件'
        CHANGING
          file_table   = lt_file_table
          rc           = lv_rc.
      IF sy-subrc = 0.
        READ TABLE lt_file_table INTO ls_file_table INDEX 1.
        lv_file_path = ls_file_table-filename.
      ELSE.
        RETURN.
      ENDIF.

    ENDIF.

    DATA : lt_records       TYPE solix_tab,
           lv_headerxstring TYPE xstring,
           lv_filelength    TYPE i.

    CALL FUNCTION 'GUI_UPLOAD'
      EXPORTING
        filename                = lv_file_path
        filetype                = 'BIN'
      IMPORTING
        filelength              = lv_filelength
        header                  = lv_headerxstring
      TABLES
        data_tab                = lt_records
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        OTHERS                  = 17.

    CHECK lt_records IS NOT INITIAL.

    "convert binary data to xstring
    "if you are using cl_fdt_xl_spreadsheet in odata then skips this step
    "as excel file will already be in xstring
    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = lv_filelength
      IMPORTING
        buffer       = lv_headerxstring
      TABLES
        binary_tab   = lt_records
      EXCEPTIONS
        failed       = 1
        OTHERS       = 2.

    IF sy-subrc <> 0.
      "Implement suitable error handling here
    ENDIF.

    process_excel_xstring2itab( EXPORTING iv_xstring = lv_headerxstring
                                IMPORTING et_tab     = et_tab ).

  ENDMETHOD.
ENDCLASS.
