@echo off
rem
rem   Set up for building a Pascal module.
rem
call build_vars

call src_get %srcdir% %libname%.ins.pas

call src_getbase
call src_getfrom fline fline.ins.pas
call src_getfrom syn syn.ins.pas
call src_getfrom syo syo.ins.pas
call src_getfrom sst sst.ins.pas

make_debug debug_switches.ins.pas
call src_builddate "%srcdir%"
