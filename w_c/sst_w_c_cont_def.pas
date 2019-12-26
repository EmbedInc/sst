{   Subroutine SST_W_C_CONT_DEF
*
*   This routine is called when a continuation line has just been created
*   within a DEFINE statement.  When installed, this routine is called by
*   using SST_W.LINE_NEW_CONT^.
*
*   DEFINE statements are continued by writing "\" to the end of the
*   line being continued.
}
module sst_w_c_CONT_DEF;
define sst_w_c_cont_def;
%include 'sst_w_c.ins.pas';

procedure sst_w_c_cont_def;            {does continuation line in DEFINE statement}

begin
  string_append1 (sst_out.dyn_p^.str_p^.s, '\'); {write continuation char}
  sst_w.line_insert^;                  {create the new line}
  end;
