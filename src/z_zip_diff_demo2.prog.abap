*&---------------------------------------------------------------------*
*& Report z_zip_diff_demo2
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_zip_diff_demo2.

DATA zip_diff TYPE REF TO zcl_zip_diff_viewer2.

PARAMETERS dummy.

AT SELECTION-SCREEN OUTPUT.

  DATA(excluded_functions) = VALUE ui_functions( ( 'ONLI' ) ( 'PRIN' ) ( 'SPOS' ) ).
  CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
    EXPORTING
      p_status  = sy-pfkey
    TABLES
      p_exclude = excluded_functions.

  IF zip_diff IS NOT BOUND.
    zip_diff = NEW #( cl_gui_container=>screen0 ).
    DATA(zip_old) = NEW cl_abap_zip( ).
    zip_old->add( name = 'changed.txt' content = cl_abap_codepage=>convert_to( 'hello world' ) ).
    zip_old->add( name = 'deleted.txt' content = cl_abap_codepage=>convert_to( 'I was deleted' ) ).
    DATA(zip_new) = NEW cl_abap_zip( ).
    zip_new->add( name = 'added.txt' content = cl_abap_codepage=>convert_to( 'I was added' ) ).
    zip_new->add( name = 'changed.txt' content = cl_abap_codepage=>convert_to( 'hey world' ) ).
    zip_diff->diff_and_view( title_old = 'Web Repository .zip file' title_new = 'Demo program .zip file'
                              zip_old = zip_old zip_new = zip_new ).
  ENDIF.

  DATA(screen_field) = VALUE screen( ).
  LOOP AT SCREEN INTO screen_field.
    screen-active = '0'.
    MODIFY SCREEN FROM screen_field.
  ENDLOOP.
