*&---------------------------------------------------------------------*
*& Include zifr_dashboard_f02
*&---------------------------------------------------------------------*
CLASS lcl_dashboard_controller DEFINITION DEFERRED.
DATA go_controller TYPE REF TO lcl_dashboard_controller.

INCLUDE zifr_dashboard_f03. "Exception class
INCLUDE zifr_dashboard_f04. "overview class
INCLUDE zifr_dashboard_f05. "main Tree class
INCLUDE zifr_dashboard_f06. "message log class
INCLUDE zifr_dashboard_f07. "payload_tree class
INCLUDE zifr_dashboard_f08. "payload_grid class
INCLUDE zifr_dashboard_f09. "Controller class
INCLUDE zifr_dashboard_f10. "9001 Screen Module
