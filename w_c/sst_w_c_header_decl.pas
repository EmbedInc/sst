{   Subroutine SST_W_C_HEADER_DECL (STYPE,POP_NEEDED)
*
*   Set up for a declaration statement.  Nothing is done if this
*   is already the current statement type.  POP_NEEDED is returned TRUE, if the
*   caller must call SST_W_C_POS_POP when done writing the statement.
*   This will be true if called while writing executable code.  In that case,
*   the position will be temporarily set back to the requested declaration
*   statement.
}
module sst_w_c_HEADER_DECL;
define sst_w_c_header_decl;
%include 'sst_w_c.ins.pas';

procedure sst_w_c_header_decl (        {if needed, set up for declaration statement}
  in      stype: sment_type_k_t;       {desired type of declaration statement}
  out     pop_needed: boolean);        {TRUE if caller must pop position when done}

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  st: sment_type_k_t;                  {statement type actually switching to}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

begin
  pop_needed := false;                 {init to no POP needed later by caller}
  st := stype;                         {init new statement type to desired type}
  if                                   {will be global declaration anyway ?}
      (frame_scope_p^.scope_type = scope_type_global_k) or {at global scope ?}
      (frame_scope_p^.sment_type = sment_type_declg_k) {already at global level ?}
      then begin
    st := sment_type_declg_k;          {only global declarations possible here}
    end;
{
*   Handle cases where we need to push the current position on the stack
*   and temporarily switch back to the requested statement type.  POP_NEEDED
*   will be set to TRUE to indicate that the caller must pop the state later.
*   Any of the following conditions require state to be saved before switching:
*
*   1) Currently inside a continuous statement.
*
*   2) Currently inside executable code.
*
*   3) The requested statement type is GLOBAL DECLARATION, but the current
*      position is not at the global level.
}
  if
      (frame_sment_p <> nil) or        {currently inside a statement ?}
      (frame_scope_p^.sment_type = sment_type_exec_k) or {in executable code ?}
      ( (st = sment_type_declg_k) and  {want global declaration from elsewhere ?}
        (frame_scope_p^.sment_type <> st)
        )
      then begin
    sst_w_c_pos_push (st);             {push curr state and go to statement start}
    pop_needed := true;                {caller will have to pop position later}
    return;
    end;
{
*   All done if already in this statement type anyway.
}
  if st = frame_scope_p^.sment_type    {already doing this kind of statement ?}
    then return;
{
*   We are not inside the start/end of a continuous statement or within
*   executable code, but are not writing the desired statement type either.
*   In this case, just switch to the new statement type since there won't be
*   any need to restore to the current position.
}
  case st of                           {what kind of statement do we want ?}
sment_type_decll_k: begin
      frame_scope_p^.pos_decll := sst_out.dyn_p^; {init position to here}
      sst_out.dyn_p := addr(frame_scope_p^.pos_decll); {use pos for this sment type}
      end;
otherwise
    sys_msg_parm_int (msg_parm[1], ord(st));
    sys_message_bomb ('sst_c_write', 'decl_statement_type_bad', msg_parm, 1);
    end;
  frame_scope_p^.sment_type := st;     {indicate we are now in new statement type}
  end;
