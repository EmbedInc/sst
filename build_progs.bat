@echo off
rem
rem   BUILD_PROGS
rem
rem   Build the executable programs from this source directory.
rem
setlocal
call build_pasinit

rem call src_prog %srcdir% sst
call src_prog %srcdir% syn
