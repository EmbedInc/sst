{   Subroutine SST_OPCODE_POS_POP
*
*   Pop the current opcode end of chain position from the stack.  This restores
*   what SST_OPCODE_POP_PUSH did.
}
module sst_OPCODE_POS_POP;
define sst_opcode_pos_pop;
%include 'sst2.ins.pas';

procedure sst_opcode_pos_pop;          {pop curr opcode position from stack}

var
  frame_p: sst_frame_opc_pos_p_t;      {pointer to stack frame}

begin
  util_stack_last_frame (              {get pointer to our stack frame}
    sst_stack,                         {handle to stack}
    sizeof(frame_p^),                  {size of stack frame}
    frame_p);                          {returned pointer to stack frame}
  sst_opc_p := frame_p^.opc_p;         {restore pointer to current opcode}
  sst_opc_next_pp := frame_p^.opc_pp;  {restore current end of chain pointer}
  util_stack_pop (sst_stack, sizeof(frame_p^)); {remove frame from stack}
  end;
