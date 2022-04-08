@echo off
rem
rem   Build everything from this source directory.
rem
setlocal
rem call godir "(cog)source/sst/r_pas"
rem call build
call godir "(cog)source/sst/r_syn"
call build
rem call godir "(cog)source/sst/w_c"
rem call build

call godir "(cog)source/sst"
rem call build_lib
call build_progs
