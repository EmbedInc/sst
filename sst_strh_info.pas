{   Subroutine SST_STRH_INFO (STR_H,FNAM,LNUM)
*
*   Get the source file name and line number of the first character indicated
*   by the source string handle STR_H.  FNAM is the returned file name, and LNUM
*   is the returned line number within that file.
}
module sst_STRH_INFO;
define sst_strh_info;
%include 'sst2.ins.pas';

procedure sst_strh_info (              {get info from handle to source string}
  in      str_h: syo_string_t;         {handle to source characters}
  in out  fnam: univ string_var_arg_t; {name of source file for first char}
  out     lnum: sys_int_machine_t);    {source file line number of first char}

begin
  sst_charh_info (str_h.first_char, fnam, lnum);
  end;
