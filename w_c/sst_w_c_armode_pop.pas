{   Subroutine SST_W_C_ARMODE_POP
*
*   Restore the current array identifier interpretation mode to what it was before
*   the last call to SST_W_C_ARMODE_PUSH.  This also pops the stack frame from the
*   stack.
}
module sst_w_c_ARMODE_POP;
define sst_w_c_armode_pop;
%include 'sst_w_c.ins.pas';

procedure sst_w_c_armode_pop;          {restore previous array interpretation mode}

var
  f_p: frame_array_p_t;                {points to stack frame with old data}

begin
  util_stack_last_frame (sst_stack, sizeof(f_p^), f_p); {get pointer to old state}
  addr_cnt_ar := f_p^.addr_cnt;        {restore state from stack frame}
  array_mode := f_p^.mode;
  util_stack_pop (sst_stack, sizeof(f_p^)); {remove old frame from stack}
%debug; write (sst_stack^.last_p^.curr_adr, ' ');
%debug; writeln ('ARMODE POP');
  end;
