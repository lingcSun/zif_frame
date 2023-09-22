*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZAUTH2_CFG......................................*
DATA:  BEGIN OF STATUS_ZAUTH2_CFG                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZAUTH2_CFG                    .
CONTROLS: TCTRL_ZAUTH2_CFG
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZAUTH2_CFG                    .
TABLES: ZAUTH2_CFG                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
