CLASS zcl_fm_params_rtts DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES :
      BEGIN OF ty_param_rtts,
        name               TYPE string,
        call_function_kind TYPE i,
        function_kind      TYPE i,
        type               TYPE REF TO cl_abap_datadescr,
      END OF ty_param_rtts,
      ty_params_rtts TYPE STANDARD TABLE OF ty_param_rtts WITH DEFAULT KEY.

    CLASS-METHODS get
      IMPORTING
        funcname           TYPE funcname
      RETURNING
        VALUE(params_rtts) TYPE ty_params_rtts
      RAISING
        zcx_fm_params_rtts.

    CLASS-METHODS class_constructor.

    CLASS-DATA:
      any_table      TYPE REF TO cl_abap_tabledescr READ-ONLY,
      index_table    TYPE REF TO cl_abap_tabledescr READ-ONLY,
      standard_table TYPE REF TO cl_abap_tabledescr READ-ONLY,
      sorted_table   TYPE REF TO cl_abap_tabledescr READ-ONLY,
      hashed_table   TYPE REF TO cl_abap_tabledescr READ-ONLY.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_method TYPE string VALUE 'DUMMY_RTTI_GENERIC_TABLE_TYPES'.

    CLASS-METHODS get_generic_table_type
      IMPORTING
        parameter_name    TYPE csequence
      RETURNING
        VALUE(rtti_table) TYPE REF TO cl_abap_tabledescr.

    "! This method doesn't do anything, but its signature is used
    "! to get the type descriptions (RTTS) of generic table types.
    CLASS-METHODS dummy_rtti_generic_table_types ##NEEDED
      IMPORTING
        any_table      TYPE ANY TABLE
        index_table    TYPE INDEX TABLE
        standard_table TYPE STANDARD TABLE
        sorted_table   TYPE SORTED TABLE
        hashed_table   TYPE HASHED TABLE.

    CLASS-DATA rtti_myself TYPE REF TO cl_abap_classdescr.

ENDCLASS.



CLASS zcl_fm_params_rtts IMPLEMENTATION.

  METHOD get.

    DATA:
      lt_fupararef TYPE TABLE OF fupararef,
      param_rtts   TYPE ty_param_rtts,
      lo_type      TYPE REF TO cl_abap_typedescr,
      lo_data      TYPE REF TO cl_abap_datadescr.
    FIELD-SYMBOLS:
      <ls_fupararef> TYPE fupararef.


    SELECT COUNT(*) FROM tfdir UP TO 1 ROWS
          WHERE funcname = funcname.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_fm_params_rtts.
    ENDIF.

    SELECT * FROM fupararef INTO TABLE lt_fupararef
          WHERE funcname = funcname.

    SORT lt_fupararef BY paramtype pposition.

    DATA(params_rtts_i) = VALUE zcl_fm_params_rtts=>ty_params_rtts( ).
    DATA(params_rtts_e) = VALUE zcl_fm_params_rtts=>ty_params_rtts( ).
    DATA(params_rtts_t) = VALUE zcl_fm_params_rtts=>ty_params_rtts( ).
    DATA(params_rtts_c) = VALUE zcl_fm_params_rtts=>ty_params_rtts( ).

    LOOP AT lt_fupararef ASSIGNING <ls_fupararef> WHERE paramtype <> 'X'.

      CLEAR param_rtts.
      param_rtts-name = <ls_fupararef>-parameter.

      " The KIND is reversed for I and E so that the direction
      " is seen from the perspective of CALL FUNCTION.
      param_rtts-call_function_kind = SWITCH #( <ls_fupararef>-paramtype
            WHEN 'I' THEN abap_func_exporting
            WHEN 'E' THEN abap_func_importing
            WHEN 'T' THEN abap_func_tables
            WHEN 'C' THEN abap_func_changing ).

      param_rtts-function_kind = SWITCH #( <ls_fupararef>-paramtype
            WHEN 'I' THEN abap_func_importing
            WHEN 'E' THEN abap_func_exporting
            WHEN 'T' THEN abap_func_tables
            WHEN 'C' THEN abap_func_changing ).

      CASE <ls_fupararef>-structure.

        WHEN 'TABLE' OR 'ANY TABLE'.
          param_rtts-type = any_table.
        WHEN 'STANDARD TABLE'.
          param_rtts-type = standard_table.
        WHEN 'SORTED TABLE'.
          param_rtts-type = sorted_table.
        WHEN 'INDEX TABLE'.
          param_rtts-type = index_table.
        WHEN 'HASHED TABLE'.
          param_rtts-type = hashed_table.

        WHEN OTHERS.

          IF <ls_fupararef>-paramtype = 'T'.
            CALL METHOD cl_abap_classdescr=>describe_by_name
              EXPORTING
                p_name         = <ls_fupararef>-structure
              RECEIVING
                p_descr_ref    = lo_type
              EXCEPTIONS
                type_not_found = 1
                OTHERS         = 2.
            IF sy-subrc <> 0.
              RAISE EXCEPTION TYPE zcx_fm_params_rtts.
            ENDIF.
            lo_data ?= lo_type.
            IF lo_data->kind = cl_abap_typedescr=>kind_table.
              param_rtts-type = lo_data.
            ELSE.
              param_rtts-type = cl_abap_tabledescr=>create(
                                              p_line_type   = lo_data
                                              p_table_kind  = cl_abap_tabledescr=>tablekind_std
                                              p_unique      = abap_false
                                              p_key_kind    = cl_abap_tabledescr=>keydefkind_default
                                          ).
            ENDIF.
          ELSEIF <ls_fupararef>-ref_class = 'X'.
            CALL METHOD cl_abap_classdescr=>describe_by_name
              EXPORTING
                p_name         = <ls_fupararef>-structure
              RECEIVING
                p_descr_ref    = lo_type
              EXCEPTIONS
                type_not_found = 1
                OTHERS         = 2.
            IF sy-subrc <> 0.
              RAISE EXCEPTION TYPE zcx_fm_params_rtts.
            ENDIF.
            param_rtts-type = cl_abap_refdescr=>create( lo_type ).
          ELSEIF <ls_fupararef>-structure IS NOT INITIAL.
            CALL METHOD cl_abap_classdescr=>describe_by_name
              EXPORTING
                p_name         = <ls_fupararef>-structure
              RECEIVING
                p_descr_ref    = lo_type
              EXCEPTIONS
                type_not_found = 1
                OTHERS         = 2.
            IF sy-subrc <> 0.
              RAISE EXCEPTION TYPE zcx_fm_params_rtts.
            ENDIF.
            param_rtts-type ?= lo_type.
          ELSE.
            CALL METHOD cl_abap_classdescr=>describe_by_name
              EXPORTING
                p_name         = 'ANY'
              RECEIVING
                p_descr_ref    = lo_type
              EXCEPTIONS
                type_not_found = 1
                OTHERS         = 2.
            IF sy-subrc <> 0.
              RAISE EXCEPTION TYPE zcx_fm_params_rtts.
            ENDIF.
            param_rtts-type ?= lo_type.
          ENDIF.

      ENDCASE.

      CASE <ls_fupararef>-paramtype.
        WHEN 'I'.
          APPEND param_rtts TO params_rtts_i.
        WHEN 'E'.
          APPEND param_rtts TO params_rtts_e.
        WHEN 'T'.
          APPEND param_rtts TO params_rtts_t.
        WHEN 'C'.
          APPEND param_rtts TO params_rtts_c.
      ENDCASE.

    ENDLOOP.

    " Parameters are returned ordered by kind of parameter
    params_rtts = VALUE #(
        ( LINES OF params_rtts_i )
        ( LINES OF params_rtts_c )
        ( LINES OF params_rtts_e )
        ( LINES OF params_rtts_t ) ).

  ENDMETHOD.

  METHOD class_constructor.

    rtti_myself ?= cl_abap_typedescr=>describe_by_name(
        '\CLASS=' && replace( val = sy-repid regex = '=*CP$' with = || ) ).

    any_table = get_generic_table_type( 'ANY_TABLE' ).
    index_table = get_generic_table_type( 'INDEX_TABLE' ).
    standard_table = get_generic_table_type( 'STANDARD_TABLE' ).
    sorted_table = get_generic_table_type( 'SORTED_TABLE' ).
    hashed_table = get_generic_table_type( 'HASHED_TABLE' ).

  ENDMETHOD.

  METHOD get_generic_table_type.

    CALL METHOD rtti_myself->get_method_parameter_type
      EXPORTING
        p_method_name       = c_method
        p_parameter_name    = parameter_name
      RECEIVING
        p_descr_ref         = DATA(lo_type)
      EXCEPTIONS
        parameter_not_found = 1
        method_not_found    = 2
        OTHERS              = 3.
    IF sy-subrc = 0.
      rtti_table ?= lo_type.
    ENDIF.

  ENDMETHOD.

  METHOD dummy_rtti_generic_table_types ##NEEDED.
  ENDMETHOD.

ENDCLASS.
