interface ZKH_OFILE_DELETE_I
public .


  methods CREATE_LOG .
  methods DISPLAY_LOG .
  methods LOG_MSG
  IMPORTING
    iv_msg TYPE string
    iv_msg_type TYPE symsgty.

  methods LOG_EXC
    IMPORTING io_exc TYPE REF TO zkh_as_file_upload.
endinterface.
