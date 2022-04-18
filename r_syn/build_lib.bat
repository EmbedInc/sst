@echo off
rem
rem   BUILD_LIB
rem
rem   Build the SST_R_SYN library.
rem
setlocal
call build_pasinit

call src_pas %srcdir% %libname%_assign
call src_pas %srcdir% %libname%_comblock
call src_pas %srcdir% %libname%_command
call src_pas %srcdir% %libname%_comp
call src_pas %srcdir% %libname%_declare
call src_pas %srcdir% %libname%_define
call src_pas %srcdir% %libname%_doit
call src_pas %srcdir% %libname%_err
call src_pas %srcdir% %libname%_expression
call src_pas %srcdir% %libname%_init
call src_pas %srcdir% %libname%_int
call src_pas %srcdir% %libname%_item
call src_pas %srcdir% %libname%_jtarg
call src_pas %srcdir% %libname%_utitem

call src_lib %srcdir% %libname%
call src_msg %srcdir% sst_syn_read
