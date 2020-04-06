CLASS zcl_fm_params_rtts_apack DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_apack_manifest.

    METHODS: constructor.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_fm_params_rtts_apack IMPLEMENTATION.

  METHOD constructor.
    zif_apack_manifest~descriptor = VALUE #(
        group_id     = 'github.com/sandraros'
        artifact_id  = 'FM-params-RTTS'
        version      = '0.2'
        repository_type = 'abapGit'
        git_url      = 'https://github.com/sandraros/FM-params-RTTS.git'
        dependencies = VALUE #( ) ).
  ENDMETHOD.

ENDCLASS.
