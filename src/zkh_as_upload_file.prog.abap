REPORT zkh_as_upload_file.

INCLUDE zkh_as_upload_file_inc.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  PARAMETERS: p_source TYPE localfile OBLIGATORY,
              p_lname  TYPE string OBLIGATORY DEFAULT 'ZKH_FOLDER_FLN'.

SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.

  lo_file_upload = NEW zkh_as_file_upload( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_source.
  lo_file_upload->select_file(
    CHANGING
      cv_file_path = p_source
  ).

START-OF-SELECTION.


  TRY.
      lv_target_path = lo_file_upload->set_logical_file_name( iv_lname = p_lname ).
    CATCH cx_root INTO DATA(lx_root).
      MESSAGE lx_root->get_text( ) TYPE 'E'.
      RETURN.
  ENDTRY.


  gv_file_path = p_source.
  gv_target_path = lv_target_path.

  TRY.
      lo_file_upload->upload_file(
        iv_source = gv_file_path
        iv_target = gv_target_path
      ).
    CATCH cx_root INTO lx_root.

      MESSAGE lx_root->get_text( ) TYPE 'I'.
  ENDTRY.
