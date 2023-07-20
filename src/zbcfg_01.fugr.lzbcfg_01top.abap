FUNCTION-POOL ZBCFG_01.                     "MESSAGE-ID ..

* INCLUDE LZBCFG_01D...                      " Local class definition
DATA go_msg_tree TYPE REF TO zcl_msg_tree.
DATA gv_json TYPE string.
DATA gv_pretty_name TYPE /ui2/cl_json=>pretty_name_mode.
DATA gv_ddic_type TYPE ZTBS0002-ZIFINPUT_STRUC.
DATA gv_hide_controller TYPE xsdboolean.

*----- 控制字段
DATA gv_is_show_json TYPE xsdboolean.
DATA gv_is_tree_show_all_fields TYPE xsdboolean VALUE abap_false.
