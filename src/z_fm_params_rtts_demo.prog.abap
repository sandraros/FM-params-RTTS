*&---------------------------------------------------------------------*
*& Report z_fm_params_rtts_demo
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_fm_params_rtts_demo.

DATA dummy_fm_name TYPE tfdir-funcname.
SELECT-OPTIONS fm_names FOR dummy_fm_name DEFAULT 'Z_FM_PARAMS_RTTS_TEST'.

START-OF-SELECTION.

  DATA: ref_parameter TYPE REF TO data.

  SELECT funcname
      FROM tfdir
      WHERE funcname IN @fm_names
      INTO TABLE @DATA(tfdir_lines).

  LOOP AT tfdir_lines INTO DATA(tfdir).

    TRY.
        DATA(params_rtts) = zcl_fm_params_rtts=>get( funcname = tfdir-funcname ).
      CATCH zcx_fm_params_rtts INTO DATA(lx_fm_params_rtts).
        MESSAGE lx_fm_params_rtts TYPE 'I' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    DATA(param_bindings) = VALUE abap_func_parmbind_tab( ).
    LOOP AT params_rtts REFERENCE INTO DATA(param_rtts).
      IF NOT param_rtts->type->is_instantiatable( ).
        " The type of the parameter of the function module is generic.
        " A data object is instantiated with an arbitrary compatible type.
        CASE param_rtts->type->type_kind.
          WHEN param_rtts->type->typekind_any.
            CREATE DATA ref_parameter TYPE c LENGTH 20.
          WHEN param_rtts->type->typekind_data.
            CREATE DATA ref_parameter TYPE c LENGTH 20.
          WHEN param_rtts->type->typekind_char.
            CREATE DATA ref_parameter TYPE c LENGTH 20.
          WHEN param_rtts->type->typekind_num.
            CREATE DATA ref_parameter TYPE n LENGTH 10.
          WHEN param_rtts->type->typekind_packed.
            CREATE DATA ref_parameter TYPE p LENGTH 8 DECIMALS 2.
          WHEN param_rtts->type->typekind_hex.
            CREATE DATA ref_parameter TYPE x LENGTH 16.
          WHEN param_rtts->type->typekind_decfloat.
            CREATE DATA ref_parameter TYPE decfloat16.
          WHEN param_rtts->type->typekind_numeric.
            CREATE DATA ref_parameter TYPE decfloat16.
          WHEN param_rtts->type->typekind_simple.
            CREATE DATA ref_parameter TYPE c LENGTH 20.
          WHEN param_rtts->type->typekind_clike.
            CREATE DATA ref_parameter TYPE c LENGTH 20.
          WHEN param_rtts->type->typekind_csequence.
            CREATE DATA ref_parameter TYPE c LENGTH 20.
          WHEN param_rtts->type->typekind_xsequence.
            CREATE DATA ref_parameter TYPE x LENGTH 20.
          WHEN param_rtts->type->typekind_table.
            DATA(rtti_table) = CAST cl_abap_tabledescr( param_rtts->type ).
            CASE rtti_table->table_kind.
              WHEN rtti_table->tablekind_any.
                CREATE DATA ref_parameter TYPE TABLE OF i.
              WHEN rtti_table->tablekind_std.
                CREATE DATA ref_parameter TYPE TABLE OF i.
              WHEN rtti_table->tablekind_sorted.
                CREATE DATA ref_parameter TYPE SORTED TABLE OF i WITH NON-UNIQUE KEY table_line.
              WHEN rtti_table->tablekind_index.
                CREATE DATA ref_parameter TYPE TABLE OF i.
              WHEN rtti_table->tablekind_hashed.
                CREATE DATA ref_parameter TYPE HASHED TABLE OF i WITH UNIQUE KEY table_line.
              WHEN OTHERS.
                MESSAGE 'Type not supported' TYPE 'I' DISPLAY LIKE 'E'.
                RETURN.
            ENDCASE.
          WHEN OTHERS.
            MESSAGE 'Type not supported' TYPE 'I' DISPLAY LIKE 'E'.
            RETURN.
        ENDCASE.
      ELSE.
        CREATE DATA ref_parameter TYPE HANDLE param_rtts->type.
      ENDIF.
      param_bindings = VALUE #( BASE param_bindings
          ( name  = param_rtts->name
            kind  = param_rtts->call_function_kind
            value = ref_parameter ) ).
    ENDLOOP.

    " Don't call if one parameter is generic
    CHECK lines( param_bindings ) = lines( params_rtts ).

    DATA(exceptions) = VALUE abap_func_excpbind_tab( ( name = 'OTHERS' value = 1 ) ).

    " The call should not trigger an exception CX_SY_DYN_CALL_ILLEGAL_TYPE
    " or CX_SY_DYN_CALL_PARAM_MISSING.
    CALL FUNCTION tfdir-funcname
      PARAMETER-TABLE
      param_bindings
      EXCEPTION-TABLE
      exceptions.

    WRITE : / tfdir-funcname, sy-subrc.

  ENDLOOP.
