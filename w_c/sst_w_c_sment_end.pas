{   Subroutine SST_W_C_SMENT_END
*
*   Write the end of the statement.  This will cause the semicolon statement
*   terminator to be written.  The indentation level will be restored by
*   decreasing it one level.  This is compatible with what was done in
*   subroutine SST_W_C_SMENT_START.
*
*   The state for the parent statement will be popped from the stack.
}
module sst_w_c_SMENT_END;
define sst_w_c_sment_end;
%include 'sst_w_c.ins.pas';

procedure sst_w_c_sment_end;           {indicate done writing a statement}

begin
  sst_w_c_sment_end_nclose;            {end the statement and restore/pop state}
  sst_w.appendn^ (';', 1);             {write semicolon statement terminator}
  sst_w.line_close^;                   {end the current output line}
  end;
