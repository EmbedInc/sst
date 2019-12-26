@echo off
rem
rem   Build everything from this source directory.
rem
setlocal
call godir "(cog)source/sst/r_pas"
call build
call godir "(cog)source/sst/r_syn"
call build
call godir "(cog)source/sst/w_c"
call build

call godir "(cog)source/sst"
call build_lib
call build_progs
