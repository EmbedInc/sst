{   Subroutine SST_W_C_SMENT_END_NCLOSE
*
*   This routine is called to indicate that the "statement" started with
*   subroutine SST_W_C_SMENT_START is finished.  The appropriate state
*   is popped/restored.  The indentation level is decreased by one, since
*   is was incremented by one in SST_W_C_SMENT_START.
}
module sst_w_c_SMENT_END_NCLOSE;
define sst_w_c_sment_end_nclose;
%include 'sst_w_c.ins.pas';

procedure sst_w_c_sment_end_nclose;    {done writing statement, leave line as is}

var
  frame_p: frame_sment_p_t;            {pointer to previous statement stack frame}

begin
  sst_w.undent^;                       {restore indentation level to statement start}

  util_stack_last_frame (              {get pointer to last frame on stack}
    sst_stack, sizeof(frame_sment_p^), frame_p);
  if frame_p <> frame_sment_p then begin {our frame isn't last on stack ?}
    sys_message_bomb ('sst_c_write', 'stack_sment_frame_err', nil, 0);
    end;
  frame_p := frame_sment_p^.prev_p;    {save pointer to previous sment stack frame}
  util_stack_pop (sst_stack, sizeof(frame_sment_p^)); {remove old stack frame}
  frame_sment_p := frame_p;            {pop current frame back to previous frame}
  end;
