@echo off
rem
rem   BUILD_LIB
rem
rem   Build the SST_R_SYO library.
rem
setlocal
call build_pasinit

call src_syn syn

call src_pas %srcdir% %libname%_comblock
call src_pas %srcdir% %libname%_command
call src_pas %srcdir% %libname%_compare
call src_pas %srcdir% %libname%_declare
call src_pas %srcdir% %libname%_define
call src_pas %srcdir% %libname%_doit
call src_pas %srcdir% %libname%_expression
call src_pas %srcdir% %libname%_goto
call src_pas %srcdir% %libname%_init
call src_pas %srcdir% %libname%_int
call src_pas %srcdir% %libname%_item
call src_pas %srcdir% %libname%_jtargets_done
call src_pas %srcdir% %libname%_jtargets_make
call src_pas %srcdir% %libname%_jtarget_sym
call src_pas %srcdir% %libname%_opc
call src_pas %srcdir% %libname%_utitem

call src_lib %srcdir% %libname%
call src_msg %srcdir% sst_syo_read
