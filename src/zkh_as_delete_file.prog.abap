*&---------------------------------------------------------------------*
*& Report ZKH_AS_DELETE_FILE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZKH_AS_DELETE_FILE.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

PARAMETERS: p_path TYPE epsf-epsdirnam OBLIGATORY default '/tmp/',
            p_days TYPE  i OBLIGATORY,
            p_test TYPE abap_bool  AS CHECKBOX default 'x'.

SELECTION-SCREEN END OF BLOCK B1.

START-OF-SELECTION.

DATA: lo_file_handler TYPE REF TO zkh_as_file_handler.

CREATE OBJECT lo_file_handler.

CALL METHOD lo_file_handler->process_files
  EXPORTING
    iv_path = p_path
    iv_days = p_days
    iv_test = p_test.
