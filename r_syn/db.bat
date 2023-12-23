@echo off
rem
rem   DB
rem
rem   Build the SYN program with the SST R_SYN library and the SYN library in
rem   debug mode.  The SYN program is then run in the debugger on the T.SYN file
rem   in this directory.
rem
setlocal
set debug=true
call treename_var "(cog)source/syn" tnam
cd /d "%tnam%"
call build_lib
endlocal

setlocal
set debug=true
cd ..
call build

call treename_var "(cog)src/sst" tnam
cd /d "%tnam%"
call extpath_var msvc/debugger.exe tnam
server "%tnam%" /DebugExe syn.exe "(cog)source/sst/r_syn/t.syn" -debug 10 -time %2 %3 %4 %5 %6 %7 %8 %9
