*&---------------------------------------------------------------------*
*& Report zifr_dashboard
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zife_auto_reprocess MESSAGE-ID zif_msg_001.

*--------------------------------------------------------------------------------*
* DATA DEFINITION
*--------------------------------------------------------------------------------*
DATA: gs_if_data   TYPE zift_data.

*--------------------------------------------------------------------------------*
* SELECTION - SCREEN
*--------------------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bk1 WITH FRAME TITLE TEXT-b01.
SELECT-OPTIONS:
  s_ifnum    FOR gs_if_data-zif_num MATCHCODE OBJECT zhif_num.
SELECTION-SCREEN END OF BLOCK bk1.

SELECTION-SCREEN BEGIN OF BLOCK bk2 WITH FRAME TITLE TEXT-b02.
SELECT-OPTIONS:
  s_cdate    FOR gs_if_data-zif_cdate.
SELECTION-SCREEN END OF BLOCK bk2.

SELECTION-SCREEN BEGIN OF BLOCK bk3 WITH FRAME TITLE TEXT-b03.
SELECT-OPTIONS:
  s_status   FOR gs_if_data-zif_status,
  s_prior    FOR gs_if_data-zif_repeat_prio.
SELECTION-SCREEN END OF BLOCK bk3.


SELECTION-SCREEN BEGIN OF BLOCK bk5 WITH FRAME TITLE TEXT-b05.
PARAMETERS: p_max TYPE int2 DEFAULT 99.
SELECTION-SCREEN END OF BLOCK bk5.

*--------------------------------------------------------------------------------*
* INCLUDE
*--------------------------------------------------------------------------------*
INCLUDE: zife_auto_reprocess_f01.

*--------------------------------------------------------------------------------*
* SELECTION - SCREEN
*--------------------------------------------------------------------------------*
START-OF-SELECTION.

  NEW cl_main( )->execute( ).
