*"* use this source file for your ABAP unit test classes

class ltc_get_items_at_path DEFINITION DEFERRED.
class zcl_zip_diff_item DEFINITION LOCAL FRIENDS ltc_get_items_at_path.

CLASS ltc_compare_zip_items DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS only_in_one FOR TESTING.
    METHODS only_in_two FOR TESTING.
    METHODS identical_file FOR TESTING.
    METHODS different_file FOR TESTING.
    METHODS folder FOR TESTING RAISING cx_static_check.
    METHODS one_more_in_one FOR TESTING RAISING cx_static_check.
    TYPES ty_diff_items TYPE zcl_zip_diff_item=>ty_diff_items.
    constants state like zcl_zip_diff_item=>state VALUE zcl_zip_diff_item=>state.
ENDCLASS.

CLASS ltc_compare_zip_items IMPLEMENTATION.

  METHOD only_in_one.
    DATA(zip_1) = NEW cl_abap_zip( ).
    zip_1->add( name = 'file1.txt' content = cl_abap_codepage=>convert_to( 'abcdef' ) ).
    zip_1->add( name = 'file2.txt' content = cl_abap_codepage=>convert_to( 'abcdef' ) ).
    DATA(zip_2) = NEW cl_abap_zip( ).
    DATA(diff) = zcl_zip_diff_item=>get_diff( zip_1 = zip_1 zip_2 = zip_2 ).
    cl_abap_unit_assert=>assert_equals( act = diff->items exp = VALUE ty_diff_items(
        ( local_name = 'file1.txt' full_path = 'file1.txt' diff_state = state-deleted )
        ( local_name = 'file2.txt' full_path = 'file2.txt' diff_state = state-deleted ) ) ).
  ENDMETHOD.

  METHOD only_in_two.
    DATA(zip_1) = NEW cl_abap_zip( ).
    DATA(zip_2) = NEW cl_abap_zip( ).
    zip_2->add( name = 'file1.txt' content = cl_abap_codepage=>convert_to( 'abcdef' ) ).
    zip_2->add( name = 'file2.txt' content = cl_abap_codepage=>convert_to( 'abcdef' ) ).
    DATA(diff) = zcl_zip_diff_item=>get_diff( zip_1 = zip_1 zip_2 = zip_2 ).
    cl_abap_unit_assert=>assert_equals( act = diff->items exp = VALUE ty_diff_items(
        ( local_name = 'file1.txt' full_path = 'file1.txt' diff_state = state-added )
        ( local_name = 'file2.txt' full_path = 'file2.txt' diff_state = state-added ) ) ).
  ENDMETHOD.

  METHOD identical_file.
    DATA(zip_1) = NEW cl_abap_zip( ).
    zip_1->add( name = 'file.txt' content = cl_abap_codepage=>convert_to( 'abcdef' ) ).
    DATA(zip_2) = NEW cl_abap_zip( ).
    zip_2->add( name = 'file.txt' content = cl_abap_codepage=>convert_to( 'abcdef' ) ).
    DATA(diff) = zcl_zip_diff_item=>get_diff( zip_1 = zip_1 zip_2 = zip_2 ).
    cl_abap_unit_assert=>assert_equals( act = diff->items exp = VALUE ty_diff_items(
        ( local_name = 'file.txt' full_path = 'file.txt' diff_state = state-same ) ) ).
  ENDMETHOD.
  METHOD different_file.
    DATA(zip_1) = NEW cl_abap_zip( ).
    zip_1->add( name = 'file.txt' content = cl_abap_codepage=>convert_to( 'abcdef' ) ).
    DATA(zip_2) = NEW cl_abap_zip( ).
    zip_2->add( name = 'file.txt' content = cl_abap_codepage=>convert_to( 'abcdefghij' ) ).
    DATA(diff) = zcl_zip_diff_item=>get_diff( zip_1 = zip_1 zip_2 = zip_2 ).
    cl_abap_unit_assert=>assert_equals( act = diff->items exp = VALUE ty_diff_items(
        ( local_name = 'file.txt' full_path = 'file.txt' diff_state = state-only_content_changed ) ) ).
  ENDMETHOD.
  METHOD one_more_in_one.
    DATA(zip_1) = NEW cl_abap_zip( ).
    zip_1->add( name = 'file1.txt' content = cl_abap_codepage=>convert_to( 'abcdef' ) ).
    zip_1->add( name = 'file2.txt' content = cl_abap_codepage=>convert_to( 'abcdef' ) ).
    zip_1->add( name = 'file3.txt' content = cl_abap_codepage=>convert_to( 'abcdef' ) ).
    DATA(zip_2) = NEW cl_abap_zip( ).
    zip_2->add( name = 'file1.txt' content = cl_abap_codepage=>convert_to( 'abcdef' ) ).
    zip_2->add( name = 'file3.txt' content = cl_abap_codepage=>convert_to( 'abcdef' ) ).
    DATA(diff) = zcl_zip_diff_item=>get_diff( zip_1 = zip_1 zip_2 = zip_2 ).
    cl_abap_unit_assert=>assert_equals( act = diff->items exp = VALUE ty_diff_items(
        ( local_name = 'file1.txt' full_path = 'file1.txt' diff_state = state-same )
        ( local_name = 'file2.txt' full_path = 'file2.txt' diff_state = state-deleted )
        ( local_name = 'file3.txt' full_path = 'file3.txt' diff_state = state-same ) ) ).
  ENDMETHOD.
  METHOD folder.
    DATA(zip_1) = NEW cl_abap_zip( ).
    zip_1->add( name = 'test/file.txt' content = cl_abap_codepage=>convert_to( 'abcdef' ) ).
    DATA(zip_2) = NEW cl_abap_zip( ).
    zip_2->add( name = 'test/file.txt' content = cl_abap_codepage=>convert_to( 'abcdefghij' ) ).
    DATA(diff) = zcl_zip_diff_item=>get_diff( zip_1 = zip_1 zip_2 = zip_2 ).
    cl_abap_unit_assert=>assert_equals(
        act = CORRESPONDING ty_diff_items( diff->items EXCEPT folder_diff )
        exp = VALUE ty_diff_items(
            ( local_name = 'test/' full_path = 'test/' diff_state = state-changed ) ) ).
    data(sub_items) = diff->items[ local_name = 'test/' ]-folder_diff.
    cl_abap_unit_assert=>assert_equals(
        act = sub_items->items
        exp = VALUE ty_diff_itemS(
            ( local_name = 'file.txt' full_path = 'test/file.txt' diff_state = state-only_content_changed ) ) ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_get_items_at_path DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS level_1 FOR TESTING.
    METHODS level_2 FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltc_get_items_at_path IMPLEMENTATION.

  METHOD level_1.
    DATA(diff) = NEW zcl_zip_diff_item( ).
    DATA(path_files) = diff->get_items_at_path( path = || items = VALUE #(
        ( name = |zfile.txt| )
        ( name = |test/file1.txt| )
        ( name = |test/file2.txt| ) ) ).
    cl_abap_unit_assert=>assert_equals( act = path_files exp = VALUE string_table(
        ( |test/| )
        ( |zfile.txt| ) ) ).
  ENDMETHOD.

  METHOD level_2.
    DATA(diff) = NEW zcl_zip_diff_item( ).
    DATA(path_files) = diff->get_items_at_path( path = |test/| items = VALUE #(
        ( name = |zfile.txt| )
        ( name = |test/test/file.txt| )
        ( name = |test/file1.txt| )
        ( name = |test/file2.txt| ) ) ).
    cl_abap_unit_assert=>assert_equals( act = path_files exp = VALUE string_table(
        ( |file1.txt| )
        ( |file2.txt| )
        ( |test/| ) ) ).
  ENDMETHOD.

ENDCLASS.
