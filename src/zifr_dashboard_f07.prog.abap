*&---------------------------------------------------------------------*
*& Include zifr_dashboard_f07
*&---------------------------------------------------------------------*
CLASS lcl_payload_tree DEFINITION INHERITING FROM zcl_msg_tree.

  PUBLIC SECTION.

    CLASS-METHODS get_by_oref_container
      IMPORTING
        io_container TYPE REF TO cl_gui_container
        iv_hide_controller TYPE xsdboolean default abap_true
        iv_top_of_page_text TYPE string OPTIONAL
        iv_expand_flat_table TYPE xsdboolean default abap_true
      RETURNING
        VALUE(ro_me) TYPE REF TO lcl_payload_tree.

  PROTECTED SECTION.
    CONSTANTS c_funcnamm_show_json TYPE salv_de_function VALUE 'ZSHOW_JSON' ##NO_TEXT.

    METHODS set_salv_functions REDEFINITION.
    METHODS on_salv_tree_usercommand REDEFINITION.

  PRIVATE SECTION.

    METHODS popup_display_json.

ENDCLASS.

CLASS lcl_payload_tree IMPLEMENTATION.

  METHOD get_by_oref_container.
    ro_me = NEW #( iv_hide_controller = iv_hide_controller
                   iv_top_of_page_text = iv_top_of_page_text
                   iv_expand_flat_table = iv_expand_flat_table ).
    ro_me->mv_mode = c_mode_oref_container.
    ro_me->mo_container = io_container.
  ENDMETHOD.

  METHOD set_salv_functions.
    DATA(lo_salv_func) = me->mo_salv_tree->get_functions( ).
    lo_salv_func->set_all( ).

    TRY.
        lo_salv_func->add_function(
                        name = c_funcnamm_show_json
                        icon = CONV string( icon_translation_show )
                        text = CONV string( 'JSON'(009) )
                        tooltip = CONV string( '显示JSON'(010) )
                        position = if_salv_c_function_position=>right_of_salv_functions ).
      CATCH cx_salv_wrong_call cx_salv_existing.
    ENDTRY.
  ENDMETHOD.

  METHOD on_salv_tree_usercommand.
    CASE e_salv_function.
      WHEN c_funcnamm_show_json.
        popup_display_json( ).
    ENDCASE.
  ENDMETHOD.

  METHOD popup_display_json.
    CHECK mv_json IS NOT INITIAL.

    CALL TRANSFORMATION sjson2html SOURCE XML mv_json
                                   RESULT XML DATA(lv_html).

    cl_abap_browser=>show_html( html_string = cl_abap_codepage=>convert_from( lv_html ) ).
  ENDMETHOD.

ENDCLASS.
