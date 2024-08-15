CLASS zkh_as_file_handler DEFINITION
  PUBLIC
  CREATE PUBLIC .
  PUBLIC SECTION.
    INTERFACES : zkh_ofile_delete_i.
    METHODS process_files
      IMPORTING
        iv_path TYPE epsf-epsdirnam
        iv_days TYPE i
        iv_test TYPE abap_bool .
    METHODS constructor.
  PRIVATE SECTION.
    ALIASES create_log
      FOR zkh_ofile_delete_i~create_log .
    ALIASES display_log
      FOR zkh_ofile_delete_i~display_log .
    ALIASES log_exc
      FOR zkh_ofile_delete_i~log_exc .
    ALIASES log_msg
      FOR zkh_ofile_delete_i~log_msg .
    DATA mv_log_handle TYPE balloghndl.
    DATA gv_subrc TYPE sy-subrc.
    TYPES:
      tt_file_list TYPE STANDARD TABLE OF epsfili WITH EMPTY KEY .
    METHODS retrieve_file_list
      IMPORTING
        iv_path TYPE epsf-epsdirnam
      RETURNING
        VALUE(rt_file_list) TYPE tt_file_list .
    METHODS retrieve_file_attributes
      IMPORTING
        iv_file_name TYPE epsf-epsfilnam
        iv_path TYPE epsf-epsdirnam
      RETURNING
        VALUE(rv_file_mtime) TYPE string .
    METHODS remove_file
      IMPORTING
        iv_file_name TYPE epsf-epsfilnam
        iv_path TYPE epsf-epsdirnam .
    METHODS handle_file
      IMPORTING
        iv_file_entry TYPE epsfili
        iv_path TYPE epsf-epsdirnam
        iv_days TYPE i
        iv_test TYPE abap_bool .
ENDCLASS.



CLASS ZKH_AS_FILE_HANDLER IMPLEMENTATION.


  METHOD constructor.
    me->create_log( ).
  ENDMETHOD.


  METHOD handle_file.
    DATA: lv_file_mtime     TYPE string,
          lv_unix_time      TYPE i,
          lv_timestamp_msec TYPE string,
          lv_date           TYPE d,
          lv_current_date   TYPE d,
          lv_days_diff      TYPE i.

    lv_current_date = sy-datum.
    lv_file_mtime = retrieve_file_attributes( iv_file_name = iv_file_entry-name iv_path = iv_path ).

    IF lv_file_mtime IS INITIAL.
      me->log_msg( iv_msg = |Failed to retrieve attributes for file { iv_file_entry-name }| iv_msg_type = 'E' ).
      RETURN.
    ENDIF.

    lv_unix_time = lv_file_mtime.
    lv_timestamp_msec = lv_unix_time * 1000.

    CALL METHOD cl_pco_utility=>convert_java_timestamp_to_abap
      EXPORTING
        iv_timestamp = lv_timestamp_msec
      IMPORTING
        ev_date      = lv_date.

    lv_days_diff = lv_current_date - lv_date.

    IF lv_days_diff > iv_days.
      IF iv_test = abap_false.
        me->remove_file( iv_file_name = iv_file_entry-name iv_path = iv_path ).
      ELSE.
        me->log_msg( iv_msg = |File { iv_file_entry-name } would be deleted.| iv_msg_type = 'I' ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD process_files.
    DATA: lt_file_list  TYPE tt_file_list,
          ls_file_entry TYPE epsfili.

    lt_file_list = retrieve_file_list( iv_path ).

    LOOP AT lt_file_list INTO ls_file_entry.
      me->handle_file(
        iv_file_entry = ls_file_entry
        iv_path       = iv_path
        iv_days       = iv_days
        iv_test       = iv_test
      ).
    ENDLOOP.

    me->display_log( ). " Call to display logs at the end
  ENDMETHOD.


  METHOD remove_file.
    CALL FUNCTION 'EPS_DELETE_FILE'
      EXPORTING
        file_name = iv_file_name
        dir_name  = iv_path
      EXCEPTIONS
        OTHERS    = 2.

    IF sy-subrc = 0.
      me->log_msg( iv_msg = |File { iv_file_name } deleted successfully.| iv_msg_type = 'S' ).
    ELSE.
      me->log_msg( iv_msg = |Failed to delete file { iv_file_name }.| iv_msg_type = 'E' ).
    ENDIF.
  ENDMETHOD.


  METHOD retrieve_file_attributes.
    CALL FUNCTION 'EPS_GET_FILE_ATTRIBUTES'
      EXPORTING
        file_name  = iv_file_name
        dir_name   = iv_path
      IMPORTING
        file_mtime = rv_file_mtime
      EXCEPTIONS
        OTHERS     = 3.

    IF sy-subrc <> 0.
      me->log_msg( iv_msg = |Failed to retrieve attributes for file { iv_file_name }.| iv_msg_type = 'E' ).
      CLEAR rv_file_mtime.
    ENDIF.
  ENDMETHOD.


  METHOD retrieve_file_list.
    CALL FUNCTION 'EPS_GET_DIRECTORY_LISTING'
      EXPORTING
        dir_name = iv_path
      TABLES
        dir_list = rt_file_list
      EXCEPTIONS
        OTHERS   = 8.

    IF sy-subrc <> 0.
      me->log_msg( iv_msg = |Failed to retrieve file list for path { iv_path }.| iv_msg_type = 'E' ).
      CLEAR rt_file_list.
    ENDIF.
  ENDMETHOD.


  METHOD zkh_ofile_delete_i~create_log.
    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log                 = VALUE bal_s_log( extnumber = 'File Handler Log'
                                                   aldate    = sy-datum
                                                   altime    = sy-uzeit
                                                   altcode   = sy-tcode
                                                   aluser    = sy-uname )
      IMPORTING
        e_log_handle            = mv_log_handle
      EXCEPTIONS
        log_header_inconsistent = 1
        OTHERS                  = 2.
    ASSERT sy-subrc EQ 0.
  ENDMETHOD.


  METHOD zkh_ofile_delete_i~display_log.
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


  METHOD zkh_ofile_delete_i~log_exc.
    " Implement logging for exceptions if needed
  ENDMETHOD.


  METHOD zkh_ofile_delete_i~log_msg.
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
