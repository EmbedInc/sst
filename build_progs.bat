@echo off
rem
rem   BUILD_PROGS
rem
rem   Build the executable programs from this source directory.
rem
setlocal
call build_pasinit

call src_prog %srcdir% sst
