{   Subroutine SST_W_C_POS_POP
*
*   Pop old writing position from stack and restore it as the current writing
*   position.  This subroutine is a matched pair with SST_W_POS_PUSH.
}
module sst_w_c_POS_POP;
define sst_w_c_pos_pop;
%include 'sst_w_c.ins.pas';

procedure sst_w_c_pos_pop;             {pop old position from stack and restore}

var
  frame_p: frame_posp_p_t;             {pointer to this stack frame}

begin
  util_stack_last_frame (              {make pointer to last stack frame}
    sst_stack, sizeof(frame_p^), frame_p);
  sst_out.dyn_p := frame_p^.dyn_p;     {restore to old position handle}
  frame_scope_p^.sment_type := frame_p^.sment_type; {restore statement type}
  util_stack_pop (sst_stack, sizeof(frame_p^)); {remove frame from stack}
%debug; write (sst_stack^.last_p^.curr_adr, ' ');
%debug; writeln ('POS POP');
  end;
