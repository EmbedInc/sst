{   Subroutine SST_W_C_POS_PUSH (STYPE)
*
*   Push the state for the current statement on the stack, and set up the
*   output writing state for a previous position.  STYPE is the type of statement
*   that will be written at the new position.  It is used to decide where to
*   re-position to.  Supported statement types are global declaration, local
*   declaration, and executable.
}
module sst_w_c_POS_PUSH;
define sst_w_c_pos_push;
%include 'sst_w_c.ins.pas';

procedure sst_w_c_pos_push (           {push curr pos, set to a previous position}
  in      stype: sment_type_k_t);      {statement type that will be written}

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  frame_p: frame_posp_p_t;             {pointer to this stack frame}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

label
  leave;

begin
%debug; write (sst_stack^.last_p^.curr_adr, ' ');
%debug; writeln ('POS PUSH');
{
*   Create new stack frame and push the old state on it.
}
  util_stack_push (sst_stack, sizeof(frame_p^), frame_p); {make stack frame}
  frame_p^.dyn_p := sst_out.dyn_p;     {save old pointer to current position}
  frame_p^.sment_type := frame_scope_p^.sment_type; {save statement type ID}
{
*   If we are currently inside the same statement type requested by STYPE, then
*   use the position of the start of the statement.
}
  if
      (frame_sment_p <> nil) and then  {we are inside a statement ?}
      (frame_scope_p^.sment_type = stype) {same sment type we want to switch to ?}
      then begin
    sst_out.dyn_p := addr(frame_sment_p^.pos_before); {go to before curr statement}
    goto leave;
    end;
{
*   We have to switch to the last known location of the requested statement type.
}
  case stype of
sment_type_declg_k: sst_out.dyn_p := addr(pos_declg); {global declaration}
sment_type_decll_k: sst_out.dyn_p := addr(frame_scope_p^.pos_decll); {local decl}
otherwise
    sys_msg_parm_int (msg_parm[1], ord(stype));
    sys_message_bomb ('sst_c_write', 'statement_type_bad', msg_parm, 1);
    end;
  frame_scope_p^.sment_type := stype;  {set new statement type as current}

leave:                                 {common exit point}
  if sst_out.dyn_p^.str_p = nil then begin {not pointing to any line ?}
    sst_out.dyn_p^.str_p := sst_out.first_str_p; {init to before first line}
    end;
  end;
