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
        VALUE(params_rtts) TYPE ty_params_rtts.

  PROTECTED SECTION.
  PRIVATE SECTION.
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

      IF <ls_fupararef>-paramtype = 'T'.
        CALL METHOD cl_abap_classdescr=>describe_by_name
          EXPORTING
            p_name         = <ls_fupararef>-structure
          RECEIVING
            p_descr_ref    = lo_type
          EXCEPTIONS
            type_not_found = 1
            OTHERS         = 2.
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
        param_rtts-type ?= lo_type.
      ENDIF.

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

    params_rtts = VALUE #(
        ( LINES OF params_rtts_i )
        ( LINES OF params_rtts_c )
        ( LINES OF params_rtts_e )
        ( LINES OF params_rtts_t ) ).

  ENDMETHOD.

ENDCLASS.
