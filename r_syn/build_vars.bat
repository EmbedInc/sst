@echo off
rem
rem   Define the variables for running builds from this source library.
rem
set srcdir=sst
set buildname=r_syn
call treename_var "(cog)source/sst/r_syn" sourcedir
set libname=sst_r_syn
set fwname=
call treename_var "(cog)src/%srcdir%/debug_%fwname%.bat" tnam
make_debug "%tnam%"
call "%tnam%"
