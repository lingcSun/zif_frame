*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTBS0002........................................*
DATA:  BEGIN OF STATUS_ZTBS0002                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTBS0002                      .
CONTROLS: TCTRL_ZTBS0002
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTBS0002                      .
TABLES: ZTBS0002                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
