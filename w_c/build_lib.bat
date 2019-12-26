@echo off
rem
rem   BUILD_LIB
rem
rem   Build the SST_W_C library.
rem
setlocal
call build_pasinit

call src_pas %srcdir% %libname%_arg
call src_pas %srcdir% %libname%_armode_pop
call src_pas %srcdir% %libname%_armode_push
call src_pas %srcdir% %libname%_assign
call src_pas %srcdir% %libname%_cont_def
call src_pas %srcdir% %libname%_declare
call src_pas %srcdir% %libname%_decl_sym_dtype
call src_pas %srcdir% %libname%_decl_sym_exp
call src_pas %srcdir% %libname%_decl_sym_rout
call src_pas %srcdir% %libname%_doit
call src_pas %srcdir% %libname%_dtype
call src_pas %srcdir% %libname%_dtype_simple
call src_pas %srcdir% %libname%_dtype_vrec
call src_pas %srcdir% %libname%_exec
call src_pas %srcdir% %libname%_exp
call src_pas %srcdir% %libname%_exp2
call src_pas %srcdir% %libname%_exp_adrable
call src_pas %srcdir% %libname%_exp_array
call src_pas %srcdir% %libname%_exp_const
call src_pas %srcdir% %libname%_exp_explicit
call src_pas %srcdir% %libname%_exp_implicit
call src_pas %srcdir% %libname%_exp_rec
call src_pas %srcdir% %libname%_header_decl
call src_pas %srcdir% %libname%_implicit_const
call src_pas %srcdir% %libname%_implicit_var
call src_pas %srcdir% %libname%_init
call src_pas %srcdir% %libname%_intrinsic
call src_pas %srcdir% %libname%_ival_unspec
call src_pas %srcdir% %libname%_name_com_var
call src_pas %srcdir% %libname%_opcodes
call src_pas %srcdir% %libname%_pos_pop
call src_pas %srcdir% %libname%_pos_push
call src_pas %srcdir% %libname%_rearrange
call src_pas %srcdir% %libname%_scope_pop
call src_pas %srcdir% %libname%_scope_push
call src_pas %srcdir% %libname%_sment_end
call src_pas %srcdir% %libname%_sment_end_nclose
call src_pas %srcdir% %libname%_sment_start
call src_pas %srcdir% %libname%_symbol
call src_pas %srcdir% %libname%_symbols
call src_pas %srcdir% %libname%_term
call src_pas %srcdir% %libname%_terms_implicit
call src_pas %srcdir% %libname%_value
call src_pas %srcdir% %libname%_var

call src_lib %srcdir% %libname%
call src_msg %srcdir% sst_c_write
