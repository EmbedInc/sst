@echo off
rem
rem   BUILD_LIB
rem
rem   Build the SST library.
rem
setlocal
call build_pasinit

call src_pas %srcdir% %libname%_call
call src_pas %srcdir% %libname%_call_arg
call src_pas %srcdir% %libname%_charh_info
call src_pas %srcdir% %libname%_char_from_ins
call src_pas %srcdir% %libname%_comblock
call src_pas %srcdir% %libname%_config_out
call src_pas %srcdir% %libname%_dtype_align
call src_pas %srcdir% %libname%_dtype_convertable
call src_pas %srcdir% %libname%_dtype_new
call src_pas %srcdir% %libname%_dtype_new_string
call src_pas %srcdir% %libname%_dtype_new_subrange
call src_pas %srcdir% %libname%_dtype_resolve
call src_pas %srcdir% %libname%_dtype_size
call src_pas %srcdir% %libname%_exp_const
call src_pas %srcdir% %libname%_exp_eval
call src_pas %srcdir% %libname%_exp_make
call src_pas %srcdir% %libname%_exp_simple
call src_pas %srcdir% %libname%_exp_useage_check
call src_pas %srcdir% %libname%_flag_used_dtype
call src_pas %srcdir% %libname%_flag_used_exp
call src_pas %srcdir% %libname%_flag_used_opcodes
call src_pas %srcdir% %libname%_flag_used_rout
call src_pas %srcdir% %libname%_flag_used_symbol
call src_pas %srcdir% %libname%_flag_used_var
call src_pas %srcdir% %libname%_init
call src_pas %srcdir% %libname%_intrinsic_dtype
call src_pas %srcdir% %libname%_mem_alloc_namesp
call src_pas %srcdir% %libname%_mem_alloc_scope
call src_pas %srcdir% %libname%_name_new_out
call src_pas %srcdir% %libname%_opcode_new
call src_pas %srcdir% %libname%_opcode_pos_pop
call src_pas %srcdir% %libname%_opcode_pos_push
call src_pas %srcdir% %libname%_ordval
call src_pas %srcdir% %libname%_out
call src_pas %srcdir% %libname%_rec_variant
call src_pas %srcdir% %libname%_routines_match
call src_pas %srcdir% %libname%_rwcheck
call src_pas %srcdir% %libname%_scope_new
call src_pas %srcdir% %libname%_scope_old
call src_pas %srcdir% %libname%_scope_unused_show
call src_pas %srcdir% %libname%_set_dtypes_combine
call src_pas %srcdir% %libname%_set_ele_find
call src_pas %srcdir% %libname%_set_val_convert
call src_pas %srcdir% %libname%_strh_info
call src_pas %srcdir% %libname%_symbol_lookup
call src_pas %srcdir% %libname%_symbol_lookup_name
call src_pas %srcdir% %libname%_symbol_new
call src_pas %srcdir% %libname%_symbol_new_name
call src_pas %srcdir% %libname%_sym_dtype_new_out
call src_pas %srcdir% %libname%_sym_var_new_out
call src_pas %srcdir% %libname%_term_eval
call src_pas %srcdir% %libname%_term_simple
call src_pas %srcdir% %libname%_var_funcname

call src_lib %srcdir% %libname%
call src_msg %srcdir% %libname%
