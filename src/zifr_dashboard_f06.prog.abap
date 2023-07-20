*&---------------------------------------------------------------------*
*& Include zifr_dashboard_f06
*&---------------------------------------------------------------------*
CLASS lcl_msg_log_grid DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        io_container TYPE REF TO cl_gui_container OPTIONAL.

    METHODS run_from_key
      IMPORTING
        iv_zif_num     TYPE zift_data-zif_num
        iv_zif_guid    TYPE zift_data-zif_guid
        iv_zif_add_key TYPE zift_data-zif_add_key.

  PRIVATE SECTION.
    DATA mt_logs TYPE STANDARD TABLE OF zift_log.
    DATA mo_salv_table TYPE REF TO cl_salv_table.
    DATA mo_container TYPE REF TO cl_gui_container.

    METHODS get_data
      IMPORTING
        iv_zif_num     TYPE zift_data-zif_num
        iv_zif_guid    TYPE zift_data-zif_guid
        iv_zif_add_key TYPE zift_data-zif_add_key.

    METHODS display_alv_grid.
ENDCLASS.

CLASS lcl_msg_log_grid IMPLEMENTATION.

  METHOD get_data.
    SELECT * INTO TABLE @mt_logs
                  FROM zift_log
                 WHERE zif_num     = @iv_zif_num
                   AND zif_guid    = @iv_zif_guid
                   AND zif_add_key = @iv_zif_add_key
              ORDER BY zif_num,
                       zif_guid,
                       zif_add_key,
                       zif_proc_counter,
                       zif_msg_counter.
  ENDMETHOD.

  METHOD run_from_key.
    get_data( iv_zif_num     = iv_zif_num
              iv_zif_guid    = iv_zif_guid
              iv_zif_add_key = iv_zif_add_key ).

    display_alv_grid( ).
  ENDMETHOD.

  METHOD display_alv_grid.
    IF mo_salv_table IS NOT BOUND.
      mo_salv_table = zcl_abap_comm=>salv_factory(
        EXPORTING
          im_r_container      = mo_container
          im_column_optimized = abap_true
          im_title            = '接口历史处理日志'(008)
        CHANGING
          im_t_table          = mt_logs ).

      mo_salv_table->set_top_of_list( NEW cl_salv_form_header_info( text = '接口历史处理日志'(008) ) ).
      mo_salv_table->get_columns( )->set_key_fixation( if_salv_c_bool_sap=>false ).
    ENDIF.

    mo_salv_table->display( ).
  ENDMETHOD.

  METHOD constructor.
    mo_container = io_container.
  ENDMETHOD.

ENDCLASS.
