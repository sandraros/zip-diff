CLASS zcl_zip_diff_file_ext DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS class_constructor.

    METHODS gui_download
      IMPORTING
        i_content   TYPE xstring
        i_file_path TYPE string.

    METHODS xml_pretty_print
      CHANGING
        c_content TYPE xstring.

    METHODS on_selection_changed
                  FOR EVENT selection_changed OF zcl_zip_diff_viewer2
      IMPORTING node zip_old zip_new.

  PROTECTED SECTION.

  PRIVATE SECTION.

    CLASS-DATA: temp_dir TYPE string.

    METHODS zip_get_and_gui_download
      IMPORTING
        zip           TYPE REF TO cl_abap_zip
        name          TYPE string
        prefix        TYPE string
      RETURNING
        VALUE(result) TYPE string
      RAISING
        zcx_zip_diff.

    METHODS gui_execute
      IMPORTING
        i_parameter TYPE string
      RAISING
        zcx_zip_diff.

    CONSTANTS: state LIKE zcl_zip_diff_item=>state VALUE zcl_zip_diff_item=>state.

ENDCLASS.



CLASS zcl_zip_diff_file_ext IMPLEMENTATION.


  METHOD class_constructor.

    CALL METHOD cl_gui_frontend_services=>get_temp_directory
      CHANGING
        temp_dir     = temp_dir
      EXCEPTIONS
        cntl_error   = 1
        error_no_gui = 2.
    IF sy-subrc <> 0.
      " Error handling
    ENDIF.

  ENDMETHOD.


  METHOD on_selection_changed.
    DATA: downloaded_file_new TYPE string,
          downloaded_file_old TYPE string,
          error               TYPE REF TO cx_root.

    TRY.

        CASE node-diff_state.
          WHEN state-added.

            downloaded_file_new = zip_get_and_gui_download( zip = zip_new name = node-full_path prefix = 'added' ).

            gui_execute( |"{ downloaded_file_new }"| ).

          WHEN state-changed
            OR state-only_attribute_changed
            OR state-only_content_changed.

            downloaded_file_old = zip_get_and_gui_download( zip = zip_old name = node-full_path prefix = 'old' ).
            downloaded_file_new = zip_get_and_gui_download( zip = zip_new name = node-full_path prefix = 'new' ).

            gui_execute( |-d "{ downloaded_file_old }" "{ downloaded_file_new }"| ).

          WHEN state-deleted.

            downloaded_file_old = zip_get_and_gui_download( zip = zip_old name = node-full_path prefix = 'deleted' ).

            gui_execute( |"{ downloaded_file_old }"| ).

          WHEN state-same.

            downloaded_file_old = zip_get_and_gui_download( zip = zip_old name = node-full_path prefix = 'same' ).

            gui_execute( |"{ downloaded_file_old }"| ).

        ENDCASE.

      CATCH cx_root INTO error.
        MESSAGE error TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.


  METHOD gui_download.

    DATA(solix_tab) = cl_bcs_convert=>xstring_to_solix( i_content ).

    cl_gui_frontend_services=>gui_download(
      EXPORTING
        bin_filesize              = xstrlen( i_content )
        filename                  = i_file_path
        filetype                  = 'BIN'
      CHANGING
        data_tab                  = solix_tab
      EXCEPTIONS
        file_write_error          = 1
        no_batch                  = 2
        gui_refuse_filetransfer   = 3
        invalid_type              = 4
        no_authority              = 5
        unknown_error             = 6
        header_not_allowed        = 7
        separator_not_allowed     = 8
        filesize_not_allowed      = 9
        header_too_long           = 10
        dp_error_create           = 11
        dp_error_send             = 12
        dp_error_write            = 13
        unknown_dp_error          = 14
        access_denied             = 15
        dp_out_of_memory          = 16
        disk_full                 = 17
        dp_timeout                = 18
        file_not_found            = 19
        dataprovider_exception    = 20
        control_flush_error       = 21
        not_supported_by_gui      = 22
        error_no_gui              = 23
        OTHERS                    = 24
    ).
    IF sy-subrc <> 0.
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


  METHOD xml_pretty_print.

    DATA xml_document TYPE REF TO if_ixml_document.

    CALL FUNCTION 'SDIXML_XML_TO_DOM'
      EXPORTING
        xml      = c_content
      IMPORTING
        document = xml_document
      EXCEPTIONS
        OTHERS   = 1.
    CALL FUNCTION 'SDIXML_DOM_TO_XML'
      EXPORTING
        document      = xml_document
        pretty_print  = abap_true
      IMPORTING
        xml_as_string = c_content
      EXCEPTIONS
        OTHERS        = 2.

  ENDMETHOD.



  METHOD gui_execute.

    cl_gui_frontend_services=>execute(
        EXPORTING
          application = 'code'
          parameter   = i_parameter
          minimized   = 'X'
          synchronous = ''
        EXCEPTIONS
          cntl_error             = 1
          error_no_gui           = 2
          bad_parameter          = 3
          file_not_found         = 4
          path_not_found         = 5
          file_extension_unknown = 6
          error_execute_failed   = 7
          synchronous_failed     = 8
          not_supported_by_gui   = 9
          OTHERS                 = 10 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zcx_zip_diff( |cl_gui_frontend_services=>execute { sy-subrc } { i_parameter }| ).
    ENDIF.

  ENDMETHOD.


  METHOD zip_get_and_gui_download.
    DATA: content TYPE xstring.

    zip->get(
        EXPORTING
          name                    = name
        IMPORTING
          content                 = content
        EXCEPTIONS
          zip_index_error         = 1
          zip_decompression_error = 2
          OTHERS                  = 3 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zcx_zip_diff( |zip->get { sy-subrc } { name }| ).
    ENDIF.

    SPLIT name AT '/' INTO TABLE DATA(parts).
    DATA(new_parts) = VALUE string_table( ).
    LOOP AT parts REFERENCE INTO DATA(part).
      INSERT part->* INTO new_parts INDEX 1.
    ENDLOOP.

    result = |{ temp_dir }\\{ prefix }_{ concat_lines_of( table = new_parts sep = '_' ) }.xml|.
    IF name CS '.xml'.
      xml_pretty_print( CHANGING c_content = content ).
    ENDIF.

    gui_download( i_content   = content
                  i_file_path = result ).

  ENDMETHOD.

ENDCLASS.
