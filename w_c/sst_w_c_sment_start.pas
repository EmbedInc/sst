{   Subroutine SST_W_C_SMENT_START
*
*   Set up for writing the start of a new statement.  The write pointer will be
*   tabbed to the current indentation level, and the level will be incremented
*   by one.  This way any continuation lines for this statement will automatically
*   be indented one level more than the first line of the statement.
*
*   The current statement state is pushed onto the stack and a new state
*   created that will be used for any nested statements.
*
*   This subroutine is intended to be used together with SST_W_C_SMENT_END.
}
module sst_w_c_SMENT_START;
define sst_w_c_sment_start;
%include 'sst_w_c.ins.pas';

procedure sst_w_c_sment_start;         {set for writing start of a new statement}

var
  frame_p: frame_sment_p_t;            {points to new stack frame}

begin
  util_stack_push (                    {create stack frame for this statement}
    sst_stack, sizeof(frame_p^), frame_p);
  frame_p^.pos_before := sst_out.dyn_p^; {save position at start of statement}
  frame_p^.prev_p := frame_sment_p;    {save pointer to previous sment stack frame}
  frame_sment_p := frame_p;            {set new stack frame as current}

  sst_w.tab_indent^;                   {go to current indentation level}
  sst_w.indent^;                       {set indentation level for continuation lines}
  end;
