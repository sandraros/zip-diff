CLASS zcl_zip_diff_viewer DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        io_container TYPE REF TO cl_gui_container.

    METHODS diff_and_view
      IMPORTING
        zip_1 TYPE REF TO cl_abap_zip
        zip_2 TYPE REF TO cl_abap_zip.

    EVENTS selection_changed
      EXPORTING
        VALUE(node) TYPE zcl_zip_diff_item=>ty_diff_item.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES: ty_tree_nodes TYPE STANDARD TABLE OF ixmltree1 WITH EMPTY KEY,
           BEGIN OF ty_tree_file_link,
             node_key  TYPE tv_nodekey,
             diff_item TYPE REF TO zcl_zip_diff_item=>ty_diff_item,
           END OF ty_tree_file_link,
           ty_tree_file_links TYPE STANDARD TABLE OF ty_tree_file_link WITH EMPTY KEY.
    CONSTANTS state LIKE zcl_zip_diff_item=>state VALUE zcl_zip_diff_item=>state.

    METHODS add_tree_nodes
      IMPORTING
        parent_node_key TYPE tv_nodekey
        path            TYPE string
        diff_items      TYPE zcl_zip_diff_item=>ty_diff_items
      CHANGING
        tree_nodes      TYPE ty_tree_nodes.

    METHODS on_selection_changed
                  FOR EVENT selection_changed OF cl_tree_control_base
      IMPORTING node_key.

    DATA: container       TYPE REF TO cl_gui_container,
          go_tree         TYPE REF TO cl_gui_simple_tree,
          gt_tree         TYPE ty_tree_nodes,
          tree_file_links TYPE ty_tree_file_links.

ENDCLASS.



CLASS zcl_zip_diff_viewer IMPLEMENTATION.


  METHOD constructor.
    me->container = io_container.
  ENDMETHOD.

  METHOD diff_and_view.

    DATA(zip_diff) = zcl_zip_diff_item=>get_diff( zip_1 = zip_1 zip_2 = zip_2 ).

    CLEAR gt_tree.

    APPEND INITIAL LINE TO gt_tree ASSIGNING FIELD-SYMBOL(<gs_tree>).
    <gs_tree>-node_key = '1'.
    <gs_tree>-text = 'root'.
    <gs_tree>-isfolder = 'X'.

    tree_file_links = VALUE #( ).
    add_tree_nodes(
        EXPORTING diff_items      = zip_diff->items
                  path            = ''
                  parent_node_key = '1'
        CHANGING  tree_nodes      = gt_tree ).

    IF go_tree IS NOT BOUND.
      CREATE OBJECT go_tree
        EXPORTING
          parent                      = container
          node_selection_mode         = cl_gui_simple_tree=>node_sel_mode_single
        EXCEPTIONS
          lifetime_error              = 1
          cntl_system_error           = 2
          create_error                = 3
          failed                      = 4
          illegal_node_selection_mode = 5
          OTHERS                      = 6.
      IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ELSE.
      go_tree->delete_all_nodes(
        EXCEPTIONS
          failed            = 1
          cntl_system_error = 2
          OTHERS            = 3 ).
      IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.
    go_tree->add_nodes(
        EXPORTING
          table_structure_name = 'IXMLTREE1'
          node_table           = gt_tree
        EXCEPTIONS
          error_in_node_table            = 1
          failed                         = 2
          dp_error                       = 3
          table_structure_name_not_found = 4
          OTHERS                         = 5 ).
    IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    go_tree->expand_root_nodes(
      EXPORTING
        expand_subtree      = abap_true
      EXCEPTIONS
        failed              = 1
        illegal_level_count = 2
        cntl_system_error   = 3
        OTHERS              = 4 ).
    IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    SET HANDLER on_selection_changed FOR go_tree.
    go_tree->set_registered_events(
      EXPORTING
        events                    = VALUE #( ( eventid = go_tree->eventid_selection_changed appl_event = abap_false ) )
      EXCEPTIONS
        cntl_error                = 1
        cntl_system_error         = 2
        illegal_event_combination = 3
        OTHERS                    = 4 ).
    IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD add_tree_nodes.

    FIELD-SYMBOLS:
        <sub_files> TYPE zcl_zip_diff_item=>ty_diff_items.

    LOOP AT diff_items REFERENCE INTO DATA(diff_item).

      APPEND INITIAL LINE TO tree_nodes ASSIGNING FIELD-SYMBOL(<gs_tree>).
      <gs_tree>-node_key = |{ sy-tabix }|.
      <gs_tree>-relatkey = parent_node_key.
      <gs_tree>-relatship = cl_tree_control_base=>relat_first_child.
      <gs_tree>-text = diff_item->local_name.
      <gs_tree>-style = SWITCH #( diff_item->diff_state
          WHEN state-deleted THEN cl_tree_control_base=>style_emphasized_negative
          WHEN state-added THEN cl_tree_control_base=>style_emphasized_positive
          WHEN state-changed
            OR state-only_attribute_changed
            OR state-only_content_changed
            THEN COND #( WHEN diff_item->folder_diff IS NOT BOUND
                                   THEN cl_tree_control_base=>style_emphasized
                                   ELSE cl_tree_control_base=>style_intensified )
          ELSE cl_tree_control_base=>style_default ).

      tree_file_links = VALUE #( BASE tree_file_links
        ( node_key  = <gs_tree>-node_key
          diff_item = diff_item ) ).

      IF diff_item->folder_diff IS BOUND.
        IF lines( diff_item->folder_diff->items ) > 0.
          <gs_tree>-isfolder = abap_true.
          add_tree_nodes(
              EXPORTING diff_items      = diff_item->folder_diff->items
                        path            = path && diff_item->local_name
                        parent_node_key = <gs_tree>-node_key
              CHANGING  tree_nodes      = gt_tree ).
        ENDIF.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD on_selection_changed.

    ASSIGN tree_file_links[ node_key = node_key ] TO FIELD-SYMBOL(<tree_file_link>).
    ASSERT sy-subrc = 0.

    RAISE EVENT selection_changed EXPORTING node = <tree_file_link>-diff_item->*.

  ENDMETHOD.


ENDCLASS.
