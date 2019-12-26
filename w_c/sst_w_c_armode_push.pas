{   Subroutine SST_W_C_ARMODE_PUSH (MODE)
*
*   Set the new mode for interpreting array identifiers.  The old array identifier
*   interpretation state is saved on the stack.
}
module sst_w_c_ARMODE_PUSH;
define sst_w_c_armode_push;
%include 'sst_w_c.ins.pas';

procedure sst_w_c_armode_push (        {set new array interpret mode, save old}
  in      mode: array_k_t);            {new array identifier interpretation mode}

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  f_p: frame_array_p_t;                {points to new stack frame}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

begin
%debug; write (sst_stack^.last_p^.curr_adr, ' ');
%debug; writeln ('ARMODE PUSH');

  util_stack_push (sst_stack, sizeof(f_p^), f_p); {create new stack frame}
  f_p^.addr_cnt := addr_cnt_ar;        {save current state on stack frame}
  f_p^.mode := array_mode;

  array_mode := mode;                  {set new array interpretation state}
  case array_mode of                   {what does raw array name represent ?}
array_whole_k: begin                   {name represents data in the whole array}
      addr_cnt_ar := 0;                {this is what we always assume}
      end;
array_pnt_whole_k,                     {name represents pointer to whole array data}
array_pnt_first_k: begin               {name represents pointer to first element}
      addr_cnt_ar := -1;               {dereference pointer once to get array value}
      end;
otherwise
    sys_msg_parm_int (msg_parm[1], ord(array_mode));
    sys_message_bomb ('sst_c_write', 'array_mode_unexpected', msg_parm, 1);
    end;
  end;
