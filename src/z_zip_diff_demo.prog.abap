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
    METHODS at_selection_screen_on_exit.
    METHODS popup_f4
      IMPORTING
        current_file  TYPE string
      RETURNING
        VALUE(result) TYPE string.

  PRIVATE SECTION.

    TYPES : ty_tree_nodes TYPE STANDARD TABLE OF ixmltree1 WITH EMPTY KEY,
            ty_diff_state TYPE zcl_zip_diff_item=>ty_diff_item-diff_state.
    CONSTANTS state LIKE zcl_zip_diff_item=>state VALUE zcl_zip_diff_item=>state.

    METHODS load_binary_file
      IMPORTING
        path           TYPE csequence
      RETURNING
        VALUE(content) TYPE xstring.
    METHODS display_document
      IMPORTING
        container     TYPE REF TO cl_gui_container
      RETURNING
        VALUE(result) TYPE REF TO i_oi_document_proxy.

    METHODS get_document
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

    TYPES ty_table_sel TYPE STANDARD TABLE OF rsparamsl_255 WITH DEFAULT KEY.

    METHODS sel
      IMPORTING
        it_sel        TYPE ty_table_sel
        selname       TYPE rsparamsl_255-selname
      RETURNING
        VALUE(result) TYPE string.

    METHODS on_selection_changed
                  FOR EVENT selection_changed OF zcl_zip_diff_viewer2
      IMPORTING node.

    DATA: sscrfields            TYPE REF TO sscrfields,
          go_splitter_container TYPE REF TO cl_gui_splitter_container,
          go_container_left     TYPE REF TO cl_gui_container,
          go_container_right    TYPE REF TO cl_gui_container,
          error                 TYPE REF TO i_oi_error,
          t_errors              TYPE STANDARD TABLE OF REF TO i_oi_error WITH NON-UNIQUE DEFAULT KEY,
          go_control            TYPE REF TO i_oi_container_control,
          go_document           TYPE REF TO i_oi_document_proxy,
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
          viewer                TYPE REF TO zcl_zip_diff_viewer2,
          lt_sel_255            TYPE TABLE OF rsparamsl_255.
ENDCLASS.

CLASS lcl_app IMPLEMENTATION.

  METHOD sel.

    result = VALUE #( it_sel[ selname = selname ]-low OPTIONAL ).

  ENDMETHOD.


  METHOD at_selection_screen_output.

    DATA: lt_dummy  TYPE TABLE OF rsparams,
          ls_screen TYPE screen,
          lt_itab   TYPE ui_functions.

    CASE sy-dynnr.

      WHEN 1000.

        DATA(lt_value) = VALUE vrm_values(
            ( key = 'excel.sheet'      text = 'MS Excel' )
            ( key = 'word.document'    text = 'MS Word' )
            ( key = 'powerpoint.slide' text = 'MS Powerpoint' ) ).

        CALL FUNCTION 'VRM_SET_VALUES'
          EXPORTING
            id              = 'P_PROGID'
            values          = lt_value
          EXCEPTIONS
            id_illegal_name = 1
            OTHERS          = 2.

        ASSIGN ('P_PROGID') TO FIELD-SYMBOL(<progid>).
        ASSERT sy-subrc = 0.
        IF NOT line_exists( lt_value[ key = <progid> ] ).
          <progid> = lt_value[ 1 ]-key.
        ENDIF.

        CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
          EXPORTING
            curr_report         = sy-repid
          TABLES
            selection_table     = lt_dummy
            selection_table_255 = lt_sel_255
          EXCEPTIONS
            not_found           = 1
            no_report           = 2
            OTHERS              = 3.

        IF abap_false = sel( it_sel = lt_sel_255 selname = 'R_COMPA2' ).

          CASE abap_true.
            WHEN sel( it_sel = lt_sel_255 selname = 'R_PROGID' ).

              LOOP AT SCREEN INTO ls_screen.
                CASE ls_screen-group1.
                  WHEN 'MIM'.
                    ls_screen-active = '1'.
                    MODIFY SCREEN FROM ls_screen.
                  WHEN 'OPN'.
                    ls_screen-active = '0'.
                    MODIFY SCREEN FROM ls_screen.
                  WHEN 'CMP'.
                    ls_screen-active = '0'.
                    MODIFY SCREEN FROM ls_screen.
                ENDCASE.
              ENDLOOP.

            WHEN sel( it_sel = lt_sel_255 selname = 'R_OPNXLS' ).

              LOOP AT SCREEN INTO ls_screen.
                CASE ls_screen-group1.
                  WHEN 'MIM'.
                    ls_screen-active = '0'.
                    MODIFY SCREEN FROM ls_screen.
                  WHEN 'OPN'.
                    ls_screen-active = '1'.
                    MODIFY SCREEN FROM ls_screen.
                  WHEN 'CMP'.
                    ls_screen-active = '0'.
                    MODIFY SCREEN FROM ls_screen.
                ENDCASE.
              ENDLOOP.

            WHEN OTHERS.

              LOOP AT SCREEN INTO ls_screen.
                CASE ls_screen-group1.
                  WHEN 'MIM'.
                    ls_screen-active = '0'.
                    MODIFY SCREEN FROM ls_screen.
                  WHEN 'OPN'.
                    ls_screen-active = '0'.
                    MODIFY SCREEN FROM ls_screen.
                  WHEN 'CMP'.
                    ls_screen-active = '1'.
                    MODIFY SCREEN FROM ls_screen.
                ENDCASE.
              ENDLOOP.

          ENDCASE.

        ENDIF.

      WHEN 1001.

        IF go_splitter_container IS NOT BOUND.

          CREATE OBJECT go_splitter_container
            EXPORTING
              parent  = cl_gui_container=>screen0
              rows    = 1
              columns = 2.
          go_container_left = go_splitter_container->get_container( row = 1 column = 1 ).
          go_container_right = go_splitter_container->get_container( row = 1 column = 2 ).
          sscrfields->functxt_01 = '@46@Compare'.

          CALL METHOD cl_gui_frontend_services=>get_temp_directory
            CHANGING
              temp_dir     = temp_dir
            EXCEPTIONS
              cntl_error   = 1
              error_no_gui = 2.
          IF sy-subrc <> 0.
            " Error handling
          ENDIF.

          IF abap_true = sel( it_sel = lt_sel_255 selname = 'R_COMPA2' ).

            FIELD-SYMBOLS <xstring> TYPE xstring.
            ASSIGN ('P_XZIP_1') TO <xstring>.

            CREATE OBJECT zip_old.
            CALL METHOD zip_old->load
              EXPORTING
                zip             = <xstring>
              EXCEPTIONS
                zip_parse_error = 1
                OTHERS          = 2.

            ASSIGN ('P_XZIP_2') TO <xstring>.

            CREATE OBJECT zip_new.
            CALL METHOD zip_new->load
              EXPORTING
                zip             = <xstring>
              EXCEPTIONS
                zip_parse_error = 1
                OTHERS          = 2.

          ELSE.

            CASE abap_true.
              WHEN sel( it_sel = lt_sel_255 selname = 'R_PROGID' ).

                lt_itab = VALUE ui_functions( ( 'ONLI' ) ).
                CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
                  EXPORTING
                    p_status  = sy-pfkey
                  TABLES
                    p_exclude = lt_itab.

                go_document = display_document( go_container_left ).
                go_document->create_document(
                  EXPORTING
                    open_inplace = abap_true
                  IMPORTING
                    error        = error
                    retcode      = retcode ).
                APPEND error TO t_errors.
                xdata = get_document( ).

                CREATE OBJECT zip_new.
                CALL METHOD zip_new->load
                  EXPORTING
                    zip             = xdata
                  EXCEPTIONS
                    zip_parse_error = 1
                    OTHERS          = 2.

                zip_old = zip_new.

              WHEN sel( it_sel = lt_sel_255 selname = 'R_OPNXLS' ).

                lt_itab = VALUE ui_functions( ( 'ONLI' ) ).
                CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
                  EXPORTING
                    p_status  = sy-pfkey
                  TABLES
                    p_exclude = lt_itab.

                go_document = display_document( go_container_left ).
                go_document->open_document(
                  EXPORTING
                    document_url = CONV rvari_val_255( 'file://' && sel( it_sel = lt_sel_255 selname = 'P_OPNXLS' ) )
                    open_inplace = abap_true
                  IMPORTING
                    error        = error
                    retcode      = retcode ).
                APPEND error TO t_errors.
                xdata = get_document( ).

                CREATE OBJECT zip_new.
                CALL METHOD zip_new->load
                  EXPORTING
                    zip             = xdata
                  EXCEPTIONS
                    zip_parse_error = 1
                    OTHERS          = 2.

                zip_old = zip_new.

              WHEN OTHERS.

                lt_itab = VALUE ui_functions( ( 'ONLI' ) ( 'FC01' ) ).
                CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
                  EXPORTING
                    p_status  = sy-pfkey
                  TABLES
                    p_exclude = lt_itab.

                xdata = load_binary_file( path = sel( it_sel = lt_sel_255 selname = 'P_ZIP_1' ) ).

                CREATE OBJECT zip_old.
                CALL METHOD zip_old->load
                  EXPORTING
                    zip             = xdata
                  EXCEPTIONS
                    zip_parse_error = 1
                    OTHERS          = 2.

                xdata = load_binary_file( path = sel( it_sel = lt_sel_255 selname = 'P_ZIP_2' ) ).
                CREATE OBJECT zip_new.
                CALL METHOD zip_new->load
                  EXPORTING
                    zip             = xdata
                  EXCEPTIONS
                    zip_parse_error = 1
                    OTHERS          = 2.

            ENDCASE.

          ENDIF.

          viewer = NEW zcl_zip_diff_viewer2( io_container = go_container_right ).
          SET HANDLER on_selection_changed FOR viewer.

          viewer->diff_and_view(
              title_old = 'Old version'
              title_new = 'New version'
              zip_old   = zip_old
              zip_new   = zip_new ).

        ENDIF.

    ENDCASE.

  ENDMETHOD.


  METHOD at_selection_screen.

    DATA: lt_dummy TYPE TABLE OF rsparams.

    CASE sy-dynnr.

      WHEN 1000.

        CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
          EXPORTING
            curr_report         = sy-repid
          TABLES
            selection_table     = lt_dummy
            selection_table_255 = lt_sel_255
          EXCEPTIONS
            not_found           = 1
            no_report           = 2
            OTHERS              = 3.

        CASE sscrfields->ucomm.
          WHEN 'ONLI'.
            CALL SELECTION-SCREEN 1001.
        ENDCASE.

      WHEN 1001.

        CASE sscrfields->ucomm.

          WHEN 'FC01'. " Compare

            IF sel( it_sel = lt_sel_255 selname = 'R_PROGID' ) = abap_true
            OR sel( it_sel = lt_sel_255 selname = 'R_OPNXLS' ) = abap_true.

              get_last_and_previous_zip(
                  IMPORTING
                    eo_zip_old = zip_old
                    eo_zip     = zip_new ).

              viewer->diff_and_view(
                  zip_old = zip_old
                  zip_new = zip_new ).

            ENDIF.

        ENDCASE.

    ENDCASE.

  ENDMETHOD.


  METHOD at_selection_screen_on_exit.

    CASE sy-dynnr.

      WHEN 1001.

        go_splitter_container->free( ).
        FREE: go_splitter_container, go_container_left, go_container_right, go_document, viewer.
        LEAVE TO SCREEN 0.

    ENDCASE.

  ENDMETHOD.


  METHOD popup_f4.

    DATA: lt_filetable     TYPE filetable,
          default_filename TYPE string,
          l_rc             TYPE i,
          l_action         TYPE i.
    FIELD-SYMBOLS <ls_file> TYPE file_table.

    result = current_file.
    default_filename = current_file.
    CALL METHOD cl_gui_frontend_services=>file_open_dialog
      EXPORTING
        window_title            = 'Select file'
        default_filename        = default_filename
      CHANGING
        file_table              = lt_filetable
        rc                      = l_rc
        user_action             = l_action
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5.
    IF sy-subrc NE 0.
      " TODO process error
    ELSEIF l_action NE cl_gui_frontend_services=>action_ok.
      " dialog cancelled by user
    ELSE.
      " 1 or more files selected
      READ TABLE lt_filetable INDEX 1 ASSIGNING <ls_file>.
      IF sy-subrc = 0.
        result = <ls_file>-filename.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD display_document.

    c_oi_container_control_creator=>get_container_control( IMPORTING control = go_control
                                                                     error   = error ).
    APPEND error TO t_errors.

    go_control->init_control( EXPORTING  inplace_enabled     = 'X'
                                         no_flush            = 'X'
                                         r3_application_name = 'Demo Document Container'
                                         parent              = container
                              IMPORTING  error               = error
                                         retcode             = retcode
                              EXCEPTIONS OTHERS              = 2 ).
    APPEND error TO t_errors.

    go_control->get_document_proxy( EXPORTING document_type  = CONV text255( sel( it_sel = lt_sel_255 selname = 'P_PROGID' ) )
                                              no_flush       = ' '
                                    IMPORTING document_proxy = result
                                              error          = error
                                              retcode        = retcode ).
    APPEND error TO t_errors.

  ENDMETHOD.


  METHOD get_last_and_previous_zip.

    DATA: lo_zip_old TYPE REF TO cl_abap_zip,
          lo_zip     TYPE REF TO cl_abap_zip.

    DATA(old_xdata) = xdata.
    xdata = get_document( ).

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



  METHOD get_document.

    go_document->save_document_to_table(
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
            EXPORTING
              application = 'code'
              parameter   = |-d "{ file_old }" "{ file_new }"|
              minimized   = 'X'
              synchronous = ''
            EXCEPTIONS
              OTHERS      = 1 ).
        IF sy-subrc <> 0.
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

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


  METHOD load_binary_file.
    DATA l_filename TYPE string.
    DATA l_length TYPE i.
    DATA lt_x255 TYPE TABLE OF x255.

    l_filename = path.

    CALL METHOD cl_gui_frontend_services=>gui_upload
      EXPORTING
        filename   = l_filename
        filetype   = 'BIN'
      IMPORTING
        filelength = l_length
      CHANGING
        data_tab   = lt_x255
      EXCEPTIONS
        OTHERS     = 1.

    IF sy-subrc = 0.

      cl_swf_utl_convert_xstring=>table_to_xstring(
        EXPORTING
          i_table  = lt_x255
          i_size   = l_length
        RECEIVING
          r_stream = content
        EXCEPTIONS
          OTHERS   = 3 ).

    ENDIF.
  ENDMETHOD.


ENDCLASS.


TABLES sscrfields.

PARAMETERS r_progid RADIOBUTTON GROUP rb1 USER-COMMAND switch DEFAULT 'X'.
PARAMETERS p_progid TYPE c LENGTH 60 LOWER CASE AS LISTBOX VISIBLE LENGTH 60 MODIF ID mim OBLIGATORY.
PARAMETERS r_opnxls RADIOBUTTON GROUP rb1.
PARAMETERS p_opnxls TYPE string LOWER CASE MODIF ID opn.
PARAMETERS r_compar RADIOBUTTON GROUP rb1.
PARAMETERS p_zip_1  TYPE string LOWER CASE MODIF ID cmp.
PARAMETERS p_zip_2  TYPE string LOWER CASE MODIF ID cmp.
PARAMETERS r_compa2 NO-DISPLAY.
PARAMETERS p_xzip_1 TYPE xstring NO-DISPLAY.
PARAMETERS p_xzip_2 TYPE xstring NO-DISPLAY.

SELECTION-SCREEN BEGIN OF SCREEN 1001.
SELECTION-SCREEN FUNCTION KEY 1.
SELECTION-SCREEN END OF SCREEN 1001.

INITIALIZATION.
  DATA(app) = NEW lcl_app( ).
  app->set_sscrfields( REF #( sscrfields ) ).

AT SELECTION-SCREEN OUTPUT.
  app->at_selection_screen_output( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_opnxls.
  p_opnxls = app->popup_f4( p_opnxls ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_zip_1.
  p_zip_1 = app->popup_f4( p_zip_1 ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_zip_2.
  p_zip_2 = app->popup_f4( p_zip_2 ).

AT SELECTION-SCREEN.
  app->at_selection_screen( ).

AT SELECTION-SCREEN ON EXIT-COMMAND.
  app->at_selection_screen_on_exit( ).
