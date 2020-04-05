INTERFACE zif_fm_params_rtts_test
  PUBLIC .

  TYPES: ty_big_c                   TYPE c LENGTH 262143,
         ty_big_n                   TYPE n LENGTH 262143,
         ty_big_x                   TYPE x LENGTH 262143,
         ty_big_p                   TYPE p LENGTH 16 DECIMALS 14,
         ty_standard_table_complete TYPE STANDARD TABLE OF i WITH EMPTY KEY,
         ty_any_table_no_key        TYPE ANY TABLE OF i,
         ty_standard_table_no_key   TYPE STANDARD TABLE OF i,
         ty_sorted_table_no_key     TYPE SORTED TABLE OF i,
         ty_index_table_no_key      TYPE INDEX TABLE OF i,
         ty_hashed_table_no_key     TYPE HASHED TABLE OF i.
ENDINTERFACE.
