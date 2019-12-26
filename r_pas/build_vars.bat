@echo off
rem
rem   Define the variables for running builds from this source library.
rem
set srcdir=sst
set buildname=r_pas
call treename_var "(cog)source/sst/r_pas" sourcedir
set libname=sst_r_pas
set fwname=
call treename_var "(cog)src/%srcdir%/debug_%fwname%.bat" tnam
make_debug "%tnam%"
call "%tnam%"
