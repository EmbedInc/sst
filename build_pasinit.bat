@echo off
rem
rem   Set up for building a Pascal module.
rem
call build_vars

call src_get %srcdir% %libname%.ins.pas
call src_get %srcdir% %libname%2.ins.pas

call src_getbase
call src_getfrom syo syo.ins.pas

make_debug debug_switches.ins.pas
call src_builddate "%srcdir%"
