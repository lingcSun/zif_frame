CLASS zcl_if_outbound_rest DEFINITION
  PUBLIC
  INHERITING FROM zcl_if_outbound
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS set_json_conv_pretty_name
      IMPORTING
        !iv_pretty_name LIKE /ui2/cl_json=>pretty_mode-camel_case OPTIONAL.
    METHODS set_extend_request_headers
      IMPORTING
        !it_fields TYPE tihttpnvp .
  PROTECTED SECTION.

    DATA go_http_client TYPE REF TO if_http_client .

    METHODS set_http_header
      IMPORTING
        !io_request TYPE REF TO if_http_request .
    METHODS set_http_body
      IMPORTING
        !io_request TYPE REF TO if_http_request
        !is_input   TYPE data .
    METHODS http_call .
    METHODS process_response
      IMPORTING
        !io_response TYPE REF TO if_http_response
      EXPORTING
        !es_output   TYPE data .

    METHODS prepare_request
      IMPORTING
        io_request TYPE REF TO if_http_request
        is_input   TYPE data.

    METHODS process_data
        REDEFINITION .
  PRIVATE SECTION.

    DATA gv_json_conv_pretty_name TYPE char1 .
    DATA gt_extend_header_fields TYPE tihttpnvp .

    METHODS create_http_client .
    METHODS default_json_conv_pretty_name .


ENDCLASS.



CLASS zcl_if_outbound_rest IMPLEMENTATION.


  METHOD create_http_client.

    IF gs_if_config-zifurl IS INITIAL.
      RAISE EXCEPTION TYPE zcx_if_exception MESSAGE e027(zif_msg_001).
    ENDIF.

    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url                = CONV #( gs_if_config-zifurl )
      IMPORTING
        client             = go_http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_if_exception MESSAGE e028(zif_msg_001).
    ENDIF.

  ENDMETHOD.


  METHOD default_json_conv_pretty_name.
    IF gv_json_conv_pretty_name IS INITIAL.
      gv_json_conv_pretty_name = /ui2/cl_json=>pretty_mode-camel_case.
    ENDIF.
  ENDMETHOD.


  METHOD http_call.
    CALL METHOD go_http_client->send
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_if_exception MESSAGE e029(zif_msg_001).
    ENDIF.

    CALL METHOD go_http_client->receive
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_if_exception MESSAGE e029(zif_msg_001).
    ENDIF.
  ENDMETHOD.

  METHOD process_data.

    create_http_client( ).

    prepare_request( io_request = go_http_client->request
                     is_input   = is_input ).

    http_call( ).

    process_response( EXPORTING io_response = go_http_client->response
                      IMPORTING es_output   = es_output ).

    go_http_client->close( ).

  ENDMETHOD.


  METHOD process_response.
    DATA lv_output_json_payload TYPE string.

    CALL METHOD io_response->get_status
      IMPORTING
        code   = DATA(lv_status_code)
        reason = DATA(lv_status_text).

    IF lv_status_code <> '200'.
      RAISE EXCEPTION TYPE zcx_if_exception MESSAGE e029(zif_msg_001) WITH lv_status_code lv_status_text.
    ENDIF.

    lv_output_json_payload = io_response->get_cdata( ).

    /ui2/cl_json=>deserialize(
      EXPORTING
        json        = lv_output_json_payload
        pretty_name = gv_json_conv_pretty_name
      CHANGING
        data        = es_output ).

  ENDMETHOD.


  METHOD set_extend_request_headers.
    gt_extend_header_fields = it_fields.
  ENDMETHOD.


  METHOD set_http_body.

    DATA lv_input_json_payload TYPE string.

*    default_json_conv_pretty_name( ).

    set_json_conv_pretty_name( ).

    lv_input_json_payload = /ui2/cl_json=>serialize( data        = is_input
                                                     compress    = abap_true
                                                     pretty_name = gv_json_conv_pretty_name ).

    io_request->set_cdata(
      data   = lv_input_json_payload
      offset = 0
      length = strlen( lv_input_json_payload ) ).

  ENDMETHOD.


  METHOD set_http_header.
    io_request->set_header_field(
      name  = '~request_method'
      value = 'POST' ).

    io_request->set_header_field(
      name  = 'Content-Type'
      value = 'application/json; charset=utf-8' ).

    IF gt_extend_header_fields IS NOT INITIAL.
      io_request->set_header_fields( gt_extend_header_fields ).
    ENDIF.

    "说明：如果需要额外增加或重写http header，可重写此方法
  ENDMETHOD.


  METHOD set_json_conv_pretty_name.
    IF iv_pretty_name IS SUPPLIED.
      gv_json_conv_pretty_name = iv_pretty_name.
    ELSE.
      gv_json_conv_pretty_name = /ui2/cl_json=>pretty_mode-camel_case.
    ENDIF.
  ENDMETHOD.

  METHOD prepare_request.
    set_http_header( io_request ).

    set_http_body( io_request = io_request
                   is_input   = is_input ).
  ENDMETHOD.

ENDCLASS.
