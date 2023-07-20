*&---------------------------------------------------------------------*
*& Include zifr_dashboard_f08
*&---------------------------------------------------------------------*
CLASS lcl_payload_grid DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        io_container TYPE REF TO cl_gui_container.

    METHODS run_from_data
      IMPORTING
        ir_data TYPE REF TO data.

    EVENTS on_temporary_close.

  PRIVATE SECTION.
    CONSTANTS c_funcnamm_close TYPE salv_de_function VALUE 'ZCLOSE' .

    DATA mr_data TYPE REF TO data.
    DATA mo_container TYPE REF TO cl_gui_container.
    DATA mo_salv_table TYPE REF TO cl_salv_table.

    METHODS set_alv_columns.
    METHODS set_functions.

    METHODS on_salv_tree_usercommand FOR EVENT added_function OF cl_salv_events_table
      IMPORTING e_salv_function.
ENDCLASS.

CLASS lcl_payload_grid IMPLEMENTATION.

  METHOD run_from_data.
    FIELD-SYMBOLS <lt_table> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <lt_table_display> TYPE STANDARD TABLE.

    CLEAR mr_data.

    ASSIGN ir_data->* TO <lt_table>.
    CHECK sy-subrc = 0.

    CREATE DATA mr_data LIKE <lt_table>.
    ASSIGN mr_data->* TO <lt_table_display>.
    CHECK sy-subrc = 0.

    <lt_table_display> = <lt_table>.

    IF mo_salv_table IS NOT BOUND.
      mo_salv_table = zcl_abap_comm=>salv_factory(
        EXPORTING
          im_r_container      = mo_container
          im_column_optimized = abap_true
          im_title            = '表类型接口日志查看'(011)
        CHANGING
          im_t_table          = <lt_table_display> ).

      set_functions( ).

      SET HANDLER on_salv_tree_usercommand FOR mo_salv_table->get_event( ).
    ELSE.
      TRY.
          mo_salv_table->set_data(
            CHANGING
              t_table = <lt_table_display> ).
        CATCH cx_salv_no_new_data_allowed.
          "handle exception
          RETURN.
      ENDTRY.
    ENDIF.

    set_alv_columns( ).

    mo_salv_table->display( ).
  ENDMETHOD.

  METHOD constructor.
    mo_container = io_container.
  ENDMETHOD.

  METHOD set_alv_columns.

    DATA lo_columns TYPE REF TO cl_salv_columns_table.
    DATA lo_column TYPE REF TO cl_salv_column_table.

    lo_columns ?= mo_salv_table->get_columns( ).

    DATA(lt_fcat) = cl_salv_controller_metadata=>get_lvc_fieldcatalog(
                      r_columns      = lo_columns
                      r_aggregations = mo_salv_table->get_aggregations( ) ).

    LOOP AT lt_fcat REFERENCE INTO DATA(ls_ref_fcat).

      TRY.
          lo_column ?= lo_columns->get_column( ls_ref_fcat->fieldname ).
        CATCH cx_salv_not_found.
          CONTINUE.
      ENDTRY.

      IF ls_ref_fcat->fieldname = 'CONTROLLER'.
        lo_column->set_technical( ).
      ELSE.
        lo_column->set_medium_text( CONV #( ls_ref_fcat->fieldname ) ).
        lo_column->set_long_text( CONV #( ls_ref_fcat->fieldname ) ).
        lo_column->set_short_text( CONV #( ls_ref_fcat->fieldname ) ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD set_functions.
    DATA(lo_functions) = mo_salv_table->get_functions( ).

    TRY.
        lo_functions->add_function(
          name     = c_funcnamm_close
          icon     = CONV string( icon_close )
          text     = CONV string( '关闭'(012) )
          tooltip  = CONV string( '关闭此界面'(013) )
          position = if_salv_c_function_position=>right_of_salv_functions ).
      CATCH cx_salv_wrong_call cx_salv_existing.
    ENDTRY.
  ENDMETHOD.

  METHOD on_salv_tree_usercommand.
    CASE e_salv_function.
      WHEN c_funcnamm_close.
        RAISE EVENT on_temporary_close.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
