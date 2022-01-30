CLASS zcl_zip_diff_viewer2 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        io_container TYPE REF TO cl_gui_container.

    METHODS diff_and_view
      IMPORTING
        title_old TYPE csequence OPTIONAL
        title_new TYPE csequence OPTIONAL
        zip_old   TYPE REF TO cl_abap_zip
        zip_new   TYPE REF TO cl_abap_zip.

    EVENTS selection_changed
      EXPORTING
        VALUE(node) TYPE zcl_zip_diff_item=>ty_diff_item
        VALUE(zip_old) TYPE REF TO cl_abap_zip
        VALUE(zip_new) TYPE REF TO cl_abap_zip.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS: state    LIKE zcl_zip_diff_item=>state VALUE zcl_zip_diff_item=>state,
               icon_old TYPE tv_image VALUE icon_create,
               icon_new TYPE tv_image VALUE icon_msg.

    METHODS add_tree_nodes
      IMPORTING
        parent_node_key TYPE tm_nodekey
        path            TYPE string
        diff_object     TYPE REF TO zcl_zip_diff_item.

    METHODS view
      IMPORTING
        i_zip_diff TYPE REF TO zcl_zip_diff_item.

    METHODS on_selection_changed
                  FOR EVENT selection_changed OF cl_tree_model
      IMPORTING node_key.

    DATA: container   TYPE REF TO cl_gui_container,
          go_splitter TYPE REF TO cl_gui_splitter_container,
          go_toolbar  TYPE REF TO cl_gui_toolbar,
          go_tree     TYPE REF TO cl_column_tree_model,
          node_key    TYPE i,
          title_old   TYPE text40,
          title_new   TYPE text40,
          zip_old     TYPE REF TO cl_abap_zip,
          zip_new     TYPE REF TO cl_abap_zip,
          diff_file   TYPE REF TO zcl_zip_diff_file_ext.

ENDCLASS.



CLASS zcl_zip_diff_viewer2 IMPLEMENTATION.


  METHOD constructor.

    me->container = io_container.
    diff_file = NEW zcl_zip_diff_file_ext( ).
    SET HANDLER diff_file->on_selection_changed FOR me.

  ENDMETHOD.


  METHOD diff_and_view.

    me->title_old = title_old.
    me->title_new = title_new.
    me->zip_old = zip_old.
    me->zip_new = zip_new.

    DATA(zip_diff) = zcl_zip_diff_item=>get_diff( zip_1 = zip_old zip_2 = zip_new ).

    view( zip_diff ).

  ENDMETHOD.


  METHOD view.

    DATA ls_hierarchy_header TYPE treemhhdr.

    IF go_tree IS NOT BOUND.
      ls_hierarchy_header-heading = 'ZIP Hierarchy'(001).

      go_splitter = NEW #( parent = container rows = 2 columns = 1 ).
      go_splitter->set_row_height( id = 1 height = 5 ).
      go_splitter->set_row_sash( id = 1 type = go_splitter->type_sashvisible value = go_splitter->false ).

      go_toolbar = NEW #( parent = go_splitter->get_container( row = 1 column = 1 ) display_mode = cl_gui_toolbar=>m_mode_horizontal ).
      go_toolbar->add_button(
                fcode       = 'TITLE_OLD'
                icon        = icon_old
                butn_type   = cntb_btype_button
                text        = title_old ).
      go_toolbar->add_button(
                fcode       = 'TITLE_NEW'
                icon        = icon_new
                butn_type   = cntb_btype_button
                text        = title_new ).

      go_tree = NEW #(
          node_selection_mode   = cl_gui_simple_tree=>node_sel_mode_single
          hierarchy_column_name = 'C1'
          hierarchy_header      = ls_hierarchy_header ).

      go_tree->add_column(
            name         = 'DATE_1'
            width        = 0
            header_image = icon_old
            header_text  = 'Date' ).

      go_tree->add_column(
            name         = 'DATE_2'
            width        = 0
            header_image = icon_new
            header_text  = 'Date' ).

      go_tree->add_column(
            name         = 'TIME_1'
            width        = 0
            header_image = icon_old
            header_text  = 'Time' ).

      go_tree->add_column(
            name         = 'TIME_2'
            width        = 0
            header_image = icon_new
            header_text  = 'Time' ).

      go_tree->add_column(
            name         = 'SIZE_1'
            width        = 0
            header_image = icon_old
            header_text  = 'Size' ).

      go_tree->add_column(
            name         = 'SIZE_2'
            width        = 0
            header_image = icon_new
            header_text  = 'Size' ).

      go_tree->create_tree_control( parent = go_splitter->get_container( row = 2 column = 1 ) ).

      SET HANDLER on_selection_changed FOR go_tree.
      go_tree->set_registered_events(
        EXPORTING
          events                    = VALUE #( ( eventid = cl_tree_model=>eventid_selection_changed ) )
        EXCEPTIONS
          illegal_event_combination = 1
          unknown_event             = 2
          OTHERS                    = 3 ).
      IF sy-subrc <> 0.
*       MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

    ELSE.

      go_tree->delete_all_nodes( ).

    ENDIF.

    go_tree->add_node(
          node_key    = '1'
          isfolder    = COND #( WHEN lines( i_zip_diff->items ) >= 1 THEN abap_true )
          item_table  = VALUE #(
                        ( item_name = 'C1'
                          class     = cl_column_tree_model=>item_class_text
                          text      = 'Root' ) ) ).

    node_key = 1.
    add_tree_nodes(
        EXPORTING diff_object     = i_zip_diff
                  path            = ''
                  parent_node_key = '1' ).

    go_tree->expand_root_nodes(
      EXCEPTIONS
        illegal_level_count = 1
        OTHERS              = 2 ).
    IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    go_tree->adjust_column_width(
      EXPORTING
        all_columns               = abap_true
        include_heading           = abap_true
      EXCEPTIONS
        control_not_existing      = 1
        control_dead              = 2
        cntl_system_error         = 3
        failed                    = 4
        start_column_not_found    = 5
        end_column_not_found      = 6
        start_column_in_hierarchy = 7
        end_column_in_hierarchy   = 8
        start_column_empty        = 9
        OTHERS                    = 10 ).
    IF sy-subrc <> 0.
*       MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


  METHOD add_tree_nodes.

    FIELD-SYMBOLS:
          <sub_files> TYPE zcl_zip_diff_item=>ty_diff_items.
    DATA: lt_column  TYPE treemcitab.

    LOOP AT diff_object->items REFERENCE INTO DATA(diff_item).

      lt_column = VALUE #(
      ( item_name = 'C1'
        class     = cl_column_tree_model=>item_class_text
        text      = diff_item->local_name
        style     = SWITCH #( diff_item->diff_state
                      WHEN state-deleted THEN cl_tree_control_base=>style_emphasized_negative
                      WHEN state-added THEN cl_tree_control_base=>style_emphasized_positive
                      WHEN state-changed
                        OR state-only_attribute_changed
                        OR state-only_content_changed
                        THEN COND #( WHEN diff_item->folder_diff IS NOT BOUND
                                               THEN cl_tree_control_base=>style_emphasized
                                               ELSE cl_tree_control_base=>style_intensified )
                      ELSE cl_tree_control_base=>style_default ) )
      ( item_name = 'DATE_1'
        class     = cl_column_tree_model=>item_class_text
        text      = COND #( WHEN diff_item->attr_1-date IS NOT INITIAL THEN |{ diff_item->attr_1-date DATE = USER }| ) )
      ( item_name = 'DATE_2'
        class     = cl_column_tree_model=>item_class_text
        text      = COND #( WHEN diff_item->attr_2-date IS NOT INITIAL THEN |{ diff_item->attr_2-date DATE = USER }| ) )
      ( item_name = 'TIME_1'
        class     = cl_column_tree_model=>item_class_text
        text      = COND #( WHEN diff_item->attr_2-time IS NOT INITIAL THEN |{ diff_item->attr_1-time TIME = USER }| ) )
      ( item_name = 'TIME_2'
        class     = cl_column_tree_model=>item_class_text
        text      = COND #( WHEN diff_item->attr_2-time IS NOT INITIAL THEN |{ diff_item->attr_2-time TIME = USER }| ) )
      ( item_name = 'SIZE_1'
        class     = cl_column_tree_model=>item_class_text
        text      = |{ diff_item->attr_1-size }| )
      ( item_name = 'SIZE_2'
        class     = cl_column_tree_model=>item_class_text
        text      = |{ diff_item->attr_2-size }| ) ).

      ADD 1 TO node_key.
      DATA(save_node_key) = node_key.
      go_tree->add_node(
            node_key          = |{ node_key }|
            relationship      = cl_column_tree_model=>relat_first_child
            relative_node_key = parent_node_key
            isfolder          = xsdbool( diff_item->folder_diff IS BOUND )
            item_table        = lt_column
            user_object       = diff_object ).

      IF diff_item->folder_diff IS BOUND
            AND lines( diff_item->folder_diff->items ) > 0.
        add_tree_nodes(
            EXPORTING diff_object     = diff_item->folder_diff
                      path            = path && diff_item->local_name
                      parent_node_key = |{ node_key }| ).
        IF diff_item->diff_state <> state-same.
          go_tree->expand_node(
            EXPORTING
              node_key       = |{ save_node_key }|
            EXCEPTIONS
              node_not_found = 1
              OTHERS         = 2 ).
          IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD on_selection_changed.

    go_tree->node_get_item(
      EXPORTING
        node_key       = node_key
        item_name      = 'C1'
      IMPORTING
        item           = DATA(item)
      EXCEPTIONS
        node_not_found = 1
        item_not_found = 2
        OTHERS         = 3 ).
    IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    go_tree->node_get_user_object(
      EXPORTING
        node_key       = node_key
      IMPORTING
        user_object    = DATA(user_object)
      EXCEPTIONS
        node_not_found = 1
        OTHERS         = 2
    ).
    IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    IF user_object IS BOUND.

      DATA(diff_item) = CAST zcl_zip_diff_item( user_object ).

      RAISE EVENT selection_changed
          EXPORTING
              node    = diff_item->items[ local_name = item-text ]
              zip_old = zip_old
              zip_new = zip_new.

    ENDIF.

  ENDMETHOD.


ENDCLASS.
