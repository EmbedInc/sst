@echo off
rem
rem   Define the variables for running builds from this source library.
rem
set srcdir=sst
set buildname=w_c
call treename_var "(cog)source/sst/w_c" sourcedir
set libname=sst_w_c
set fwname=
call treename_var "(cog)src/%srcdir%/debug_%fwname%.bat" tnam
make_debug "%tnam%"
call "%tnam%"
