{   Subroutine SST_CHAR_FROM_INS (CHAR_H)
*
*   Determine whether a character came from the special include file specified
*   with the -INS command line option.  CHAR_H is the handle to the source
*   stream character.  This routine always returns FALSE when not doing a
*   special include file translation.
}
module sst_CHAR_FROM_INS;
define sst_char_from_ins;
%include 'sst2.ins.pas';

function sst_char_from_ins (           {check for char is from special include file}
  in      char_h: syo_char_t)          {handle to source character to check}
  :boolean;                            {TRUE is character is from the include file}
  val_param;

var
  file_p: syo_file_p_t;                {points to descriptor for one input file}

begin
  sst_char_from_ins := false;          {init to char is not from the include file}
  if not sst_ins then return;          {not doing include file translate ?}

  if char_h.crange_p = nil             {no pointer to source line char range ?}
    then return;
  if char_h.crange_p^.line_p = nil     {no pointer to source line ?}
    then return;
  file_p := char_h.crange_p^.line_p^.file_p; {get pointer to file descriptor}

  while file_p <> nil do begin         {loop thru file nesting hierarchy}
    if                                 {this is the right source file ?}
        string_equal(file_p^.conn_p^.tnam, sst_ins_tnam)
        then begin
      sst_char_from_ins := true;
      return;
      end;
    file_p := file_p^.parent_p;        {pop one level in file nesting hierarchy}
    end;                               {back and check this parent file name}
  end;
