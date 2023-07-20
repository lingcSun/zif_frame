*&---------------------------------------------------------------------*
*& Include zifr_dashboard_f04
*&---------------------------------------------------------------------*
CLASS lcl_overview DEFINITION.
  PUBLIC SECTION.
    METHODS run RAISING lcx_comm.

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

    EVENTS overview_data_selected
      EXPORTING
        VALUE(ev_zif_num)    TYPE zift_data-zif_num
        VALUE(ev_zif_status) TYPE zift_data-zif_status.

   PRIVATE SECTION.
    TYPES:
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

      typ_t_alv_overview TYPE STANDARD TABLE OF typ_alv_overview.


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

     DATA mt_alv_overview TYPE STANDARD TABLE OF typ_alv_overview.
     DATA mo_salv TYPE REF TO cl_salv_table.

     METHODS get_data RAISING lcx_comm.
     METHODS display_data.

     METHODS on_link_click_overview  FOR EVENT link_click OF cl_salv_events_table
        IMPORTING
          row column.
ENDCLASS.

CLASS lcl_overview IMPLEMENTATION.

  METHOD run.
    TRY.
        get_data( ).
      CATCH lcx_comm INTO DATA(lo_error).
        RAISE EXCEPTION lo_error.
    ENDTRY.
    display_data( ).
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

  METHOD display_data.
    DATA: lo_column  TYPE REF TO cl_salv_column_table.

    DEFINE set_column_hotspot.
      lo_column ?= mo_salv->get_columns( )->get_column( &1 ).
      lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
    END-OF-DEFINITION.

    DEFINE set_aggregation.
      mo_salv->get_aggregations( )->add_aggregation( columnname  = &1  aggregation = if_salv_c_aggregation=>total ).
    END-OF-DEFINITION.

    mo_salv = zcl_abap_comm=>salv_factory( EXPORTING im_column_optimized = 'X'
                                                     im_select_mode      = if_salv_c_selection_mode=>cell
                                                     im_save_layout      = 'X'
                                           CHANGING  im_t_table          = mt_alv_overview ).
    TRY .
        set_alv_column_text: 'TOTAL'     TEXT-f09    mo_salv,
                             'INIT'      TEXT-f10    mo_salv,
                             'SUCCESS'   TEXT-f11    mo_salv,
                             'ERROR'     TEXT-f12    mo_salv,
                             'DELETE'    TEXT-f13    mo_salv,
                             'REPEAT'    TEXT-f14    mo_salv.

        set_column_hotspot: 'TOTAL',
                            'INIT',
                            'SUCCESS',
                            'ERROR',
                            'DELETE'.

        lo_column ?= mo_salv->get_columns( )->get_column( 'TOTAL' ).
        lo_column->set_color( VALUE #( col = 3  int = 1  inv = 0 )  ).

        lo_column ?= mo_salv->get_columns( )->get_column( 'ERROR' ).
        lo_column->set_color( VALUE #( col = 6  int = 1  inv = 0 )  ).

        lo_column ?= mo_salv->get_columns( )->get_column( 'SUCCESS' ).
        lo_column->set_color( VALUE #( col = 5  int = 1  inv = 0 )  ).

        lo_column ?= mo_salv->get_columns( )->get_column( 'DELETE' ).
        lo_column->set_color( VALUE #( col = 1  int = 1  inv = 0 )  ).

        lo_column ?= mo_salv->get_columns( )->get_column( 'INIT' ).
        lo_column->set_color( VALUE #( col = 7  int = 1  inv = 0 )  ).

        set_aggregation: 'TOTAL',
                         'ERROR',
                         'SUCCESS',
                         'DELETE',
                         'INIT',
                         'REPEAT'.

        mo_salv->get_layout( )->set_key( VALUE #( report = sy-cprog
                                                  handle = '002' ) ).

      CATCH cx_salv_not_found.

      CATCH cx_salv_existing.

      CATCH cx_salv_data_error.

    ENDTRY.

    SET HANDLER on_link_click_overview FOR mo_salv->get_event( ).

    mo_salv->display( ).
  ENDMETHOD.

  METHOD get_data.
*----- 获取每个接口的执行次数
*    SELECT zif_num,
*           zif_guid,
*           zif_add_key,
*           zif_proc_counter
*      INTO TABLE @DATA(lt_proc_counter)
*      FROM zift_log
*     WHERE zif_num     IN @s_ifnum
*       AND zif_guid    IN @s_ifguid
*       AND zif_add_key IN @s_addkey.
*    SORT lt_proc_counter BY zif_num          ASCENDING
*                            zif_guid         ASCENDING
*                            zif_add_key      ASCENDING
*                            zif_proc_counter DESCENDING.
*
*    DELETE ADJACENT DUPLICATES FROM lt_proc_counter COMPARING zif_num
*                                                              zif_guid
*                                                              zif_add_key.

*----- 获取接口数据和配置
    SELECT FROM zift_data AS a
      "replaced by cds view by Xiangyi 2022/7/5 --- Begin
      LEFT OUTER JOIN zifv_log_proc_counter AS c ON c~zif_num     = a~zif_num
                                                 AND c~zif_guid    = a~zif_guid
                                                 AND c~zif_add_key = a~zif_add_key
*      LEFT OUTER JOIN @lt_proc_counter AS c ON c~zif_num     = a~zif_num
*                                            AND c~zif_guid    = a~zif_guid
*                                            AND c~zif_add_key = a~zif_add_key
      "replaced by cds view by Xiangyi 2022/7/5 --- End
      LEFT OUTER JOIN ztbs0002 AS b ON b~zifnum   = a~zif_num
         FIELDS
            a~zif_num,
            b~zifnam,
            b~zifsystem,
            b~zifdirect,
            b~zifpmode,
            SUM( CASE WHEN 1 = 1 THEN 1 END ) AS total,
            SUM( CASE a~zif_status WHEN @space THEN 1 ELSE 0 END ) AS init,
            SUM( CASE a~zif_status WHEN 'S' THEN 1 ELSE 0 END ) AS success,
            SUM( CASE a~zif_status WHEN 'E' THEN 1 ELSE 0 END ) AS error,
            SUM( CASE a~zif_status WHEN 'D' THEN 1 ELSE 0 END ) AS delete,
            SUM( c~zif_proc_counter ) AS repeat
      WHERE a~zif_num     IN @mt_rg_ifnum
        AND a~zif_guid    IN @mt_rg_ifguid
        AND a~zif_add_key IN @mt_rg_addkey
*       AND zifsystem     IN @mt_rg_ifsys
        AND zif_cdate     IN @mt_rg_cdate
        AND zif_ctime     IN @mt_rg_ctime
        AND zif_status    IN @mt_rg_status
        AND zres1         IN @mt_rg_zres1
        AND zres2         IN @mt_rg_zres2
        AND zres3         IN @mt_rg_zres3
        AND zres4         IN @mt_rg_zres4
      GROUP BY a~zif_num,
               b~zifnam,
               b~zifsystem,
               b~zifdirect,
               b~zifpmode
      ORDER BY a~zif_num
      INTO TABLE @mt_alv_overview.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_comm MESSAGE e034.
    ENDIF.

  ENDMETHOD.

  METHOD on_link_click_overview.
    DATA: ls_if_data TYPE zift_data,
          lv_status  TYPE char1,
          lr_status  TYPE RANGE OF zeif_status.

    ASSIGN mt_alv_overview[ row ] TO FIELD-SYMBOL(<ls_overview>).

    CHECK sy-subrc = 0.

    CASE column.
      WHEN 'TOTAL'.
        CHECK <ls_overview>-total IS NOT INITIAL.
        RAISE EVENT overview_data_selected
          EXPORTING
            ev_zif_num    = <ls_overview>-zif_num
            ev_zif_status = '*'.

      WHEN 'ERROR'.
        CHECK <ls_overview>-error IS NOT INITIAL.
        RAISE EVENT overview_data_selected
          EXPORTING
            ev_zif_num    = <ls_overview>-zif_num
            ev_zif_status = 'E'.

      WHEN 'INIT'.
        CHECK <ls_overview>-init IS NOT INITIAL.
        RAISE EVENT overview_data_selected
          EXPORTING
            ev_zif_num    = <ls_overview>-zif_num
            ev_zif_status = ''.

      WHEN 'SUCCESS'.
        CHECK <ls_overview>-success IS NOT INITIAL.
        RAISE EVENT overview_data_selected
          EXPORTING
            ev_zif_num    = <ls_overview>-zif_num
            ev_zif_status = 'S'.

      WHEN 'DELETE'.
        CHECK <ls_overview>-delete IS NOT INITIAL.
        RAISE EVENT overview_data_selected
          EXPORTING
            ev_zif_num    = <ls_overview>-zif_num
            ev_zif_status = 'D'.

      WHEN OTHERS.
        RETURN.

    ENDCASE.
  ENDMETHOD.

ENDCLASS.
