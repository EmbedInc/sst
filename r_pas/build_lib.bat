@echo off
rem
rem   BUILD_LIB
rem
rem   Build the R_PAS library.
rem
setlocal
call build_pasinit

call src_syo pas

call src_pas %srcdir% %libname%_comblock
call src_pas %srcdir% %libname%_data_type
call src_pas %srcdir% %libname%_doit
call src_pas %srcdir% %libname%_dtype_record
call src_pas %srcdir% %libname%_exp
call src_pas %srcdir% %libname%_exp_eval
call src_pas %srcdir% %libname%_exp_sym
call src_pas %srcdir% %libname%_exp_term
call src_pas %srcdir% %libname%_ifunc
call src_pas %srcdir% %libname%_init
call src_pas %srcdir% %libname%_integer
call src_pas %srcdir% %libname%_item
call src_pas %srcdir% %libname%_item_eval
call src_pas %srcdir% %libname%_lit_string
call src_pas %srcdir% %libname%_preproc
call src_pas %srcdir% %libname%_proc_args
call src_pas %srcdir% %libname%_raw_sment
call src_pas %srcdir% %libname%_routine
call src_pas %srcdir% %libname%_sment_case
call src_pas %srcdir% %libname%_sment_const
call src_pas %srcdir% %libname%_sment_define
call src_pas %srcdir% %libname%_sment_label
call src_pas %srcdir% %libname%_sment_module
call src_pas %srcdir% %libname%_sment_prog
call src_pas %srcdir% %libname%_sment_rout
call src_pas %srcdir% %libname%_sment_type
call src_pas %srcdir% %libname%_sment_var
call src_pas %srcdir% %libname%_statement
call src_pas %srcdir% %libname%_statements
call src_pas %srcdir% %libname%_syn_pad
call src_pas %srcdir% %libname%_variable
call src_pas %srcdir% %libname%_var_init
call src_pas %srcdir% %libname%_vparam
call src_pas %srcdir% %libname%_write

call src_lib %srcdir% %libname%
call src_msg %srcdir% sst_pas_read
