{   Subroutine SST_CHARH_INFO (CHAR_H,FNAM,LNUM)
*
*   Return the source file name and line number for a particular source
*   character.  CHAR_H is the handle to the source character.  This originally
*   came from the syntaxer.
}
module sst_CHARH_INFO;
define sst_charh_info;
%include 'sst2.ins.pas';

procedure sst_charh_info (             {get info from handle to source character}
  in      char_h: syo_char_t;          {handle to source character}
  in out  fnam: univ string_var_arg_t; {name of source file for first char}
  out     lnum: sys_int_machine_t);    {source file line number of first char}

var
  ichar: sys_int_machine_t;            {char number within line (unused)}
  cnum: sys_int_machine_t;             {char index into source line (unused)}
  sline: string_var80_t;               {raw source line (unused)}

begin
  sline.max := sizeof(sline.str);      {init local var string}
  if char_h.crange_p = nil then begin  {there is no character here ?}
    fnam.len := 0;
    lnum := 0;
    return;
    end;

  syo_get_char_line (                  {get info about this source character}
    char_h,                            {handle to source character}
    ichar, cnum, sline,                {unused returned arguments}
    lnum,                              {line number within source file}
    fnam);                             {name of source file}
  end;
