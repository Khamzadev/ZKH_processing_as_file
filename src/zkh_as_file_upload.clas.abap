CLASS zkh_as_file_upload DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: zkh_file_upload_i.


    METHODS:
      upload_file
        IMPORTING
          iv_source TYPE string
          iv_target TYPE string,

      select_file
        CHANGING
          cv_file_path TYPE localfile,

      set_logical_file_name
        IMPORTING
          iv_lname TYPE string
        RETURNING
          VALUE(rv_target_path) TYPE string,

      get_file_name
        IMPORTING
          iv_file_path TYPE string
        RETURNING
          VALUE(rv_file_name) TYPE string,

      check_file_exists
        IMPORTING
          iv_file_path TYPE string
        RETURNING
          VALUE(rv_exists) TYPE abap_bool,

      save_file
        IMPORTING
          iv_file_path TYPE string
          iv_target_path TYPE string
        RETURNING
          VALUE(rv_result) TYPE abap_bool,

      get_subrc
        RETURNING
          VALUE(rv_subrc) TYPE sy-subrc,




      constructor.

  PROTECTED SECTION.

  PRIVATE SECTION.

    ALIASES:
      create_log FOR zkh_file_upload_i~create_log,
      display_log FOR zkh_file_upload_i~display_log,
      log_exc FOR zkh_file_upload_i~log_exc,
      log_msg FOR zkh_file_upload_i~log_msg.

    " Переменные класса
    DATA:
      mv_log_handle TYPE balloghndl,
      gv_subrc TYPE sy-subrc.

ENDCLASS.



CLASS ZKH_AS_FILE_UPLOAD IMPLEMENTATION.


  METHOD check_file_exists.
    OPEN DATASET iv_file_path FOR INPUT IN TEXT MODE ENCODING UTF-8.
    IF sy-subrc = 0.
      rv_exists = abap_true.
      CLOSE DATASET iv_file_path.
    ELSE.
      rv_exists = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD constructor.
    me->create_log( ).
  ENDMETHOD.


  METHOD get_file_name.
    DATA: lt_parts     TYPE TABLE OF string,
          lv_file_name TYPE string.
    SPLIT iv_file_path AT '\' INTO TABLE lt_parts.
    READ TABLE lt_parts INTO lv_file_name INDEX lines( lt_parts ).
    rv_file_name = lv_file_name.
  ENDMETHOD.


  METHOD get_subrc.
    rv_subrc = gv_subrc.
  ENDMETHOD.


  METHOD save_file.
    DATA: lt_file_content TYPE TABLE OF x255,
          lv_line         TYPE x255.
    TRY.
        CALL METHOD cl_gui_frontend_services=>gui_upload
          EXPORTING
            filename     = iv_file_path
            read_by_line = 'X'
            codepage     = '4110'
          CHANGING
            data_tab     = lt_file_content
          EXCEPTIONS
            OTHERS       = 1.
        IF sy-subrc <> 0.
          rv_result = abap_false.
          me->log_msg( iv_msg = 'Error uploading file.' iv_msg_type = 'E' ).
          RETURN.
        ENDIF.
        OPEN DATASET iv_target_path FOR OUTPUT IN BINARY MODE.
        IF sy-subrc <> 0.
          rv_result = abap_false.
          me->log_msg( iv_msg = 'Error opening target file.' iv_msg_type = 'E' ).
          RETURN.
        ENDIF.
        LOOP AT lt_file_content INTO lv_line.
          TRANSFER lv_line TO iv_target_path.
        ENDLOOP.
        CLOSE DATASET iv_target_path.
        IF sy-subrc = 0.
          rv_result = abap_true.
          me->log_msg( iv_msg = 'File saved successfully.' iv_msg_type = 'S' ).
        ELSE.
          rv_result = abap_false.
          me->log_msg( iv_msg = 'Error saving file.' iv_msg_type = 'E' ).
        ENDIF.
      CATCH cx_sy_conversion_codepage INTO DATA(lx_conv).
        rv_result = abap_false.
        me->log_msg( iv_msg = |Conversion codepage error: { lx_conv->get_text( ) }| iv_msg_type = 'E' ).
      CATCH cx_sy_file_io INTO DATA(lx_file_io).
        rv_result = abap_false.
        me->log_msg( iv_msg = |File I/O error: { lx_file_io->get_text( ) }| iv_msg_type = 'E' ).
    ENDTRY.
  ENDMETHOD.


  METHOD select_file.
    TRY.
        DATA: lt_filetable TYPE filetable,
              ls_filetable LIKE LINE OF lt_filetable,
              gv_rc        TYPE i.
        CALL METHOD cl_gui_frontend_services=>file_open_dialog
          EXPORTING
            window_title            = 'Select File'
          CHANGING
            file_table              = lt_filetable
            rc                      = gv_rc
          EXCEPTIONS
            file_open_dialog_failed = 1
            cntl_error              = 2
            error_no_gui            = 3
            not_supported_by_gui    = 4
            OTHERS                  = 5.
        IF sy-subrc = 0.
          READ TABLE lt_filetable INTO ls_filetable INDEX 1.
          cv_file_path = ls_filetable-filename.
        ELSE.
          me->log_msg( iv_msg = 'File selection failed.' iv_msg_type = 'E' ).
        ENDIF.
      CATCH cx_root INTO DATA(lx_root).
        me->log_msg( iv_msg = lx_root->get_text( ) iv_msg_type = 'E' ).
    ENDTRY.
  ENDMETHOD.


  METHOD upload_file.
    DATA: lv_file_name TYPE string,
          lv_answer    TYPE c.
    lv_file_name = me->get_file_name( iv_file_path = iv_source ).
    IF me->check_file_exists( iv_file_path = iv_target && '/' && lv_file_name ) = abap_true.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar       = 'File exists'
          text_question  = 'Overwrite existing file?'
          text_button_1  = 'Yes'
          text_button_2  = 'No'
        IMPORTING
          answer         = lv_answer
        EXCEPTIONS
          text_not_found = 1
          OTHERS         = 2.
      IF sy-subrc <> 0 OR lv_answer <> '1'.
        gv_subrc = 1.
        me->log_msg( iv_msg = 'File overwrite canceled.' iv_msg_type = 'I' ).
        RETURN.
      ENDIF.
    ENDIF.
    IF NOT me->save_file( iv_file_path = iv_source iv_target_path = iv_target && '/' && lv_file_name ).
      gv_subrc = 1.
    ELSE.
      gv_subrc = 0.
      me->log_msg( iv_msg = 'File uploaded successfully.' iv_msg_type = 'S' ).
    ENDIF.
    me->display_log( ).
  ENDMETHOD.


   METHOD set_logical_file_name.
    IF iv_lname = 'ZKH_FOLDER_FLN'.
      rv_target_path = '/tmp'.
    ELSE.
      CALL FUNCTION 'FILE_GET_NAME'
        EXPORTING
          logical_filename = iv_lname
        IMPORTING
          file_name        = rv_target_path
        EXCEPTIONS
          others           = 1.
      IF sy-subrc <> 0.
        me->log_msg( iv_msg = 'Logical name conversion error' iv_msg_type = 'E' ).
        rv_target_path = ''.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD zkh_file_upload_i~create_log.
    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log = VALUE bal_s_log( extnumber = 'File upload to AppServer'
                                   aldate    = sy-datum
                                   altime    = sy-uzeit
                                   altcode   = sy-tcode
                                   aluser    = sy-uname )
      IMPORTING
        e_log_handle = mv_log_handle
      EXCEPTIONS
        log_header_inconsistent = 1
        OTHERS                  = 2.
    ASSERT sy-subrc EQ 0.
  ENDMETHOD.


  METHOD zkh_file_upload_i~display_log.
    DATA: ls_profile TYPE bal_s_prof.
    CALL FUNCTION 'BAL_DSP_PROFILE_NO_TREE_GET'
      IMPORTING
        e_s_display_profile = ls_profile.
    ls_profile-use_grid = abap_true.
    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_s_display_profile  = ls_profile
        i_t_log_handle       = VALUE bal_t_logh( ( mv_log_handle ) )
      EXCEPTIONS
        profile_inconsistent = 1
        internal_error       = 2
        no_data_available    = 3
        no_authority         = 4
        OTHERS               = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD zkh_file_upload_i~log_exc.
    " Implement logging for exceptions if needed
  ENDMETHOD.


  METHOD zkh_file_upload_i~log_msg.
    DATA lv_msg TYPE text255.
    lv_msg = |{ iv_msg }|.
    CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
      EXPORTING
        i_log_handle     = mv_log_handle
        i_msgty          = iv_msg_type
        i_text           = lv_msg
      EXCEPTIONS
        log_not_found    = 1
        msg_inconsistent = 2
        log_is_full      = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
