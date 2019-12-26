{   Subroutine SST_W_C_SCOPE_POP
*
*   Restore the previous scope by popping it from the stack.  This undoes what
*   subroutine SST_W_C_SCOPE_PUSH does.
}
module sst_w_c_SCOPE_POP;
define sst_w_c_scope_pop;
%include 'sst_w_c.ins.pas';

procedure sst_w_c_scope_pop;           {restore previous scope as current scope}

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  dyn: sst_out_dyn_t;                  {saved current writing position}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

begin
  dyn := sst_out.dyn_p^;               {save current writing position data}

  frame_scope_p := frame_scope_p^.prev_p; {set previous scope frame as current}
  util_stack_pop (sst_stack, sizeof(frame_scope_p^)); {remove old stack frame}
%debug; write (sst_stack^.last_p^.curr_adr, ' ');
%debug; writeln ('SCOPE POP');

  sst_scope_p := frame_scope_p^.scope_p; {restore to scope of old stack frame}
  sst_names_p := sst_scope_p;

  case frame_scope_p^.sment_type of    {what kind of statement now writing ?}
sment_type_declg_k: begin              {new statement type is global declarations}
      sst_out.dyn_p := addr(pos_declg);
      end;
sment_type_decll_k: begin              {new statement type is local declarations}
      sst_out.dyn_p := addr(frame_scope_p^.pos_decll);
      end;
sment_type_exec_k: begin               {new statement type is executable}
      sst_out.dyn_p := addr(frame_scope_p^.pos_exec);
      end;
otherwise
    sys_msg_parm_int (msg_parm[1], ord(frame_scope_p^.sment_type));
    sys_message_bomb ('sst_c_write', 'statement_type_curr_bad', msg_parm, 1);
    end;
  sst_out.dyn_p^ := dyn;               {update new position of new statement to here}
  end;
