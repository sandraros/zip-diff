*&---------------------------------------------------------------------*
*& Report z_zip_diff_demo
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_zip_diff_demo.

CLASS lcl_app DEFINITION.
  PUBLIC SECTION.
    METHODS set_sscrfields IMPORTING sscrfields TYPE REF TO sscrfields.
    METHODS at_selection_screen_output.
    METHODS at_selection_screen.

  PRIVATE SECTION.

    TYPES : ty_tree_nodes TYPE STANDARD TABLE OF ixmltree1 WITH EMPTY KEY,
            ty_diff_state TYPE zcl_zip_diff_item=>ty_diff_item-diff_state.
    CONSTANTS state LIKE zcl_zip_diff_item=>state VALUE zcl_zip_diff_item=>state.

    METHODS display_excel
      IMPORTING
        container TYPE REF TO cl_gui_container.
    METHODS get_excel
      RETURNING
        VALUE(result) TYPE xstring.
    METHODS get_last_and_previous_zip
      EXPORTING
        eo_zip_old TYPE REF TO cl_abap_zip
        eo_zip     TYPE REF TO cl_abap_zip.
    METHODS gui_download
      IMPORTING
        i_content   TYPE xstring
        i_file_path TYPE string.
    METHODS xml_pretty_print
      CHANGING
        c_content TYPE xstring.
    METHODS on_selection_changed
                  FOR EVENT selection_changed OF zcl_zip_diff_viewer2
      IMPORTING node.

    DATA: sscrfields            TYPE REF TO sscrfields,
          go_splitter_container TYPE REF TO cl_gui_splitter_container,
          go_container_left     TYPE REF TO cl_gui_container,
          go_container_right    TYPE REF TO cl_gui_container,
          error                 TYPE REF TO i_oi_error,
          t_errors              TYPE STANDARD TABLE OF REF TO i_oi_error WITH NON-UNIQUE DEFAULT KEY,
          cl_control            TYPE REF TO i_oi_container_control,
          cl_document           TYPE REF TO i_oi_document_proxy,
          xdata                 TYPE xstring,
          t_rawdata             TYPE solix_tab,
          bytecount             TYPE i,
          go_tree               TYPE REF TO cl_gui_simple_tree,
          gt_tree               TYPE ty_tree_nodes,
          diff_files            TYPE zcl_zip_diff_item=>ty_diff_items,
          retcode               TYPE soi_ret_string,
          zip_old               TYPE REF TO cl_abap_zip,
          zip_new               TYPE REF TO cl_abap_zip,
          temp_dir              TYPE string,
          viewer                TYPE REF TO zcl_zip_diff_viewer2.
ENDCLASS.

CLASS lcl_app IMPLEMENTATION.

  METHOD at_selection_screen_output.

    IF go_splitter_container IS NOT BOUND.

      CREATE OBJECT go_splitter_container
        EXPORTING
          parent  = cl_gui_container=>screen0
          rows    = 1
          columns = 2.
      go_container_left = go_splitter_container->get_container( row = 1 column = 1 ).
      go_container_right = go_splitter_container->get_container( row = 1 column = 2 ).
      sscrfields->functxt_01 = '@46@Compare'.
      display_excel( go_container_left ).
      xdata = get_excel( ).

      CALL METHOD cl_gui_frontend_services=>get_temp_directory
        CHANGING
          temp_dir     = temp_dir
        EXCEPTIONS
          cntl_error   = 1
          error_no_gui = 2.
      IF sy-subrc <> 0.
* Error handling
      ENDIF.
* flush to send previous call to frontend
      CALL METHOD cl_gui_cfw=>flush
        EXCEPTIONS
          cntl_system_error = 1
          cntl_error        = 2
          OTHERS            = 3.
      IF sy-subrc <> 0.
* Error handling
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD display_excel.

    c_oi_container_control_creator=>get_container_control( IMPORTING control = cl_control
                                                                     error   = error ).
    APPEND error TO t_errors.

    cl_control->init_control( EXPORTING  inplace_enabled     = 'X'
                                         no_flush            = 'X'
                                         r3_application_name = 'Demo Document Container'
                                         parent              = container
                              IMPORTING  error               = error
                              EXCEPTIONS OTHERS              = 2 ).
    APPEND error TO t_errors.

    cl_control->get_document_proxy( EXPORTING document_type  = 'Excel.Sheet'                " EXCEL
                                              no_flush       = ' '
                                    IMPORTING document_proxy = cl_document
                                              error          = error ).
    APPEND error TO t_errors.

    cl_document->create_document(
      EXPORTING
        open_inplace = 'X'
      IMPORTING
        error        = error
        retcode      = retcode ).
    APPEND error TO t_errors.

  ENDMETHOD.

  METHOD at_selection_screen.

    CASE sscrfields->ucomm.

      WHEN 'FC01'. " Compare

        get_last_and_previous_zip(
            IMPORTING
              eo_zip_old = zip_old
              eo_zip     = zip_new ).


        IF viewer IS NOT BOUND.
          viewer = NEW zcl_zip_diff_viewer2(
              io_container = go_container_right ).
          SET HANDLER on_selection_changed FOR viewer.
        ENDIF.

        viewer->diff_and_view(
            zip_1 = zip_old
            zip_2 = zip_new ).

    ENDCASE.

  ENDMETHOD.

  METHOD get_last_and_previous_zip.

    DATA(old_xdata) = xdata.
    xdata = get_excel( ).
    DATA: lo_zip_old TYPE REF TO cl_abap_zip,
          lo_zip     TYPE REF TO cl_abap_zip.
    CREATE OBJECT eo_zip_old.
    CALL METHOD eo_zip_old->load
      EXPORTING
        zip             = old_xdata
      EXCEPTIONS
        zip_parse_error = 1
        OTHERS          = 2.
    CREATE OBJECT eo_zip.
    CALL METHOD eo_zip->load
      EXPORTING
        zip             = xdata
      EXCEPTIONS
        zip_parse_error = 1
        OTHERS          = 2.


  ENDMETHOD.



  METHOD get_excel.

    cl_document->save_document_to_table(
      IMPORTING
        error          = error
        retcode        = retcode
      CHANGING
        document_size  = bytecount
        document_table = t_rawdata ).

    result = cl_bcs_convert=>solix_to_xstring(
        it_solix   = t_rawdata
        iv_size    = bytecount ).

  ENDMETHOD.


  METHOD set_sscrfields.

    me->sscrfields = sscrfields.

  ENDMETHOD.


  METHOD on_selection_changed.
    DATA: content      TYPE xstring,
          solix_tab    TYPE solix_tab,
          xml_document TYPE REF TO if_ixml_document.

*    ASSIGN tree_file_links[ node_key = node_key ] TO FIELD-SYMBOL(<tree_file_link>).
    CASE node-diff_state.
      WHEN state-changed
        OR state-only_attribute_changed
        OR state-only_content_changed.

        zip_old->get(
          EXPORTING
            name                    = node-full_path
          IMPORTING
            content                 = content
          EXCEPTIONS
            zip_index_error         = 1
            zip_decompression_error = 2
            OTHERS                  = 3 ).
        IF sy-subrc <> 0.
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
        SPLIT node-full_path AT '/' INTO TABLE DATA(parts).
        DATA(new_parts) = VALUE string_table( ).
        LOOP AT parts REFERENCE INTO DATA(part).
          INSERT part->* INTO new_parts INDEX 1.
        ENDLOOP.
        DATA(file_old) = temp_dir && '\old_' && concat_lines_of( table = new_parts sep = '_' ) && '.xml'.
        IF node-full_path CS '.xml'.
          xml_pretty_print( CHANGING c_content = content ).
        ENDIF.
        gui_download(
              i_content   = content
              i_file_path = file_old ).

        zip_new->get(
          EXPORTING
            name                    = node-full_path
          IMPORTING
            content                 = content
          EXCEPTIONS
            zip_index_error         = 1
            zip_decompression_error = 2
            OTHERS                  = 3 ).
        IF sy-subrc <> 0.
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
        DATA(file_new) = temp_dir && '\new_' && concat_lines_of( table = new_parts sep = '_' ) && '.xml'.
        IF node-full_path CS '.xml'.
          xml_pretty_print( CHANGING c_content = content ).
        ENDIF.
        gui_download(
              i_content   = content
              i_file_path = file_new ).

*      cl_gui_frontend_services=>execute( document = file_new ).
        cl_gui_frontend_services=>execute(
              application = 'code'
              parameter   = |-d "{ file_old }" "{ file_new }"|
              minimized   = 'X'
              synchronous = '' ).

    ENDCASE.


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

ENDCLASS.


TABLES sscrfields.
PARAMETERS dummy.
SELECTION-SCREEN FUNCTION KEY 1.

INITIALIZATION.
  DATA(app) = NEW lcl_app( ).
  app->set_sscrfields( REF #( sscrfields ) ).

AT SELECTION-SCREEN OUTPUT.
  app->at_selection_screen_output( ).

AT SELECTION-SCREEN.
  app->at_selection_screen( ).
