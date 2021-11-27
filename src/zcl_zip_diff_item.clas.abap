CLASS zcl_zip_diff_item DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES : BEGIN OF ENUM enum_diff_state STRUCTURE state,
              same,
              only_content_changed,
              only_attribute_changed,
              changed,
              added,
              deleted,
              undefined,
              folder_undefined,
            END OF ENUM enum_diff_state STRUCTURE state,
            ty_diff_state TYPE enum_diff_state,
            BEGIN OF ty_diff_item_attr,
              date TYPE cl_abap_zip=>t_file-date,
              time TYPE cl_abap_zip=>t_file-time,
              size TYPE cl_abap_zip=>t_file-size,
            END OF ty_diff_item_attr,
            BEGIN OF ty_diff_item,
              local_name  TYPE string,
              full_path   TYPE cl_abap_zip=>t_file-name,
              attr_1      TYPE ty_diff_item_attr,
              attr_2      TYPE ty_diff_item_attr,
              diff_state  TYPE ty_diff_state,
              folder_diff TYPE REF TO zcl_zip_diff_item,
            END OF ty_diff_item,
            ty_diff_items TYPE STANDARD TABLE OF ty_diff_item WITH EMPTY KEY,
            BEGIN OF ty_item,
              local_name TYPE string,
              full_path  TYPE cl_abap_zip=>t_file-name,
              date       TYPE cl_abap_zip=>t_file-date,
              time       TYPE cl_abap_zip=>t_file-time,
              size       TYPE cl_abap_zip=>t_file-size,
            END OF ty_item,
            ty_items TYPE SORTED TABLE OF ty_item WITH UNIQUE KEY local_name.

    CLASS-METHODS get_diff
      IMPORTING
        zip_1         TYPE REF TO cl_abap_zip
        zip_2         TYPE REF TO cl_abap_zip
      RETURNING
        VALUE(result) TYPE REF TO zcl_zip_diff_item."ty_diff_items.

    DATA: path  TYPE string READ-ONLY,
          items TYPE ty_diff_items READ-ONLY.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS compare_items_at_path
      IMPORTING
        path          TYPE string OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zcl_zip_diff_item.

    METHODS get_items_at_path
      IMPORTING
        items             TYPE cl_abap_zip=>t_files
        path              TYPE string OPTIONAL
      RETURNING
        VALUE(path_items) TYPE ty_items.

    METHODS compare_two_items
      IMPORTING
        zip_1         TYPE REF TO cl_abap_zip
        zip_2         TYPE REF TO cl_abap_zip
        name_1        TYPE string
        name_2        TYPE string
      RETURNING
        VALUE(result) TYPE ty_diff_item-diff_state.

    DATA: zip_1 TYPE REF TO cl_abap_zip,
          zip_2 TYPE REF TO cl_abap_zip.

ENDCLASS.



CLASS zcl_zip_diff_item IMPLEMENTATION.


  METHOD compare_items_at_path.

    DATA: diff_item   TYPE ty_diff_item,
          folder_diff TYPE REF TO zcl_zip_diff_item,
          item_1 TYPE REF TO zcl_zip_diff_item=>ty_item,
          item_2 TYPE REF TO zcl_zip_diff_item=>ty_item.

    result = NEW zcl_zip_diff_item( ).
    result->path = path.

    DATA(zip_1_items) = get_items_at_path( items = zip_1->files path = path ).
    DATA(zip_2_items) = get_items_at_path( items = zip_2->files path = path ).

    DATA(zip_1_index) = 0.
    DATA(zip_2_index) = 0.
    DATA(zip_1_read) = abap_true.
    DATA(zip_2_read) = abap_true.

    WHILE zip_1_read = abap_true OR zip_2_read = abap_true.

      IF zip_1_read = abap_true.
        zip_1_index = zip_1_index + 1.
        IF zip_1_index > lines( zip_1_items ).
          zip_1_read = abap_false.
          CLEAR item_1.
        ELSE.
          item_1 = REF #( zip_1_items[ zip_1_index ] OPTIONAL ).
        ENDIF.
      ENDIF.
      IF zip_2_read = abap_true.
        zip_2_index = zip_2_index + 1.
        IF zip_2_index > lines( zip_2_items ).
          zip_2_read = abap_false.
          CLEAR item_2.
        ELSE.
          item_2 = REF #( zip_2_items[ zip_2_index ] OPTIONAL ).
        ENDIF.
      ENDIF.

      CLEAR folder_diff.
      DATA(diff_state) = state-undefined.
      zip_1_read = abap_false.
      zip_2_read = abap_false.
      IF zip_1_index <= lines( zip_1_items ) AND zip_2_index <= lines( zip_2_items ).
        IF item_1->local_name < item_2->local_name.
          diff_state = state-deleted.
          zip_1_read = abap_true.
        ELSEIF item_1->local_name > item_2->local_name.
          diff_state = state-added.
          zip_2_read = abap_true.
        ELSE.
          " same item -> compare the contents of the 2 items
          zip_1_read = abap_true.
          zip_2_read = abap_true.
          IF contains( val = item_1->local_name end = '/' ).
            " folder
            folder_diff = compare_items_at_path( path = |{ path }{ item_1->local_name }| ).
            LOOP AT folder_diff->items TRANSPORTING NO FIELDS WHERE diff_state <> state-same.
              EXIT.
            ENDLOOP.
            IF sy-subrc = 0.
              diff_state = state-changed.
            ELSE.
              diff_state = state-same.
            ENDIF.
          ELSE.
            diff_state = compare_two_items( zip_1 = zip_1 zip_2 = zip_2 name_1 = |{ path }{ item_1->local_name }| name_2 = |{ path }{ item_2->local_name }| ).
          ENDIF.
        ENDIF.
      ELSEIF zip_1_index <= lines( zip_1_items ).
        diff_state = state-deleted.
        zip_1_read = abap_true.
      ELSEIF zip_2_index <= lines( zip_2_items ).
        diff_state = state-added.
        zip_2_read = abap_true.
      ELSE.
        " no more file to compare
        EXIT.
      ENDIF.

      diff_item = VALUE #(
          LET item = SWITCH #( diff_state WHEN state-added THEN item_2 ELSE item_1 ) IN
          local_name  = item->local_name
          full_path   = path && item->local_name
          attr_1      = COND #( WHEN diff_state <> state-added THEN VALUE #(
                          date = item_1->date
                          time = item_1->time
                          size = item_1->size ) )
          attr_2      = COND #( WHEN diff_state <> state-deleted THEN VALUE #(
                          date = item_2->date
                          time = item_2->time
                          size = item_2->size ) )
          diff_state  = diff_state
          folder_diff = folder_diff ).

      INSERT diff_item INTO TABLE result->items.

    ENDWHILE.

  ENDMETHOD.


  METHOD compare_two_items.

    zip_1->get(
      EXPORTING
        name                    = name_1
      IMPORTING
        content                 = DATA(content_1)
      EXCEPTIONS
        zip_index_error         = 1
        zip_decompression_error = 2
        OTHERS                  = 3 ).
    IF sy-subrc <> 0.
      " todo
    ENDIF.
    zip_2->get(
      EXPORTING
        name                    = name_2
      IMPORTING
        content                 = DATA(content_2)
      EXCEPTIONS
        zip_index_error         = 1
        zip_decompression_error = 2
        OTHERS                  = 3 ).
    IF sy-subrc <> 0.
      " todo
    ENDIF.

    result = COND #( WHEN content_1 <> content_2 THEN state-only_content_changed ELSE state-same ).

  ENDMETHOD.


  METHOD get_diff.

    DATA(engine) = NEW zcl_zip_diff_item( ).
    engine->zip_1 = zip_1.
    engine->zip_2 = zip_2.
    result = engine->compare_items_at_path( ).

  ENDMETHOD.


  METHOD get_items_at_path.

    DATA: starter        TYPE string,
          item_or_folder TYPE string VALUE '^[^/]*(?:/|$)'.

    CLEAR path_items.

    LOOP AT items ASSIGNING FIELD-SYMBOL(<item>).
      DATA(local_name) = COND #( WHEN path = `` THEN substring_to( val = <item>-name regex = item_or_folder )
                                 WHEN contains( val = <item>-name start = path ) THEN substring_to( val = substring( val = <item>-name off = strlen( path ) ) regex = item_or_folder ) ).
      IF local_name IS NOT INITIAL.
        IF NOT line_exists( path_items[ local_name = local_name ] ).
          INSERT COND ty_item(
                when not contains( val = local_name end = '/' ) then value #(
                  local_name = local_name
                  full_path  = <item>-name
                  date       = <item>-date
                  time       = <item>-time
                  size       = <item>-size )
                else value #(
                  local_name = local_name
                  full_path  = path && local_name ) )
              INTO TABLE path_items.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


ENDCLASS.
