@echo off
rem
rem   Define the variables for running builds from this source library.
rem
set srcdir=sst
set buildname=r_syo
call treename_var "(cog)source/sst/r_syo" sourcedir
set libname=sst_r_syo
set fwname=
call treename_var "(cog)src/%srcdir%/debug_%fwname%.bat" tnam
make_debug "%tnam%"
call "%tnam%"
