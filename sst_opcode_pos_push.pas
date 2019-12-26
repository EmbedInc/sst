{   Subroutine SST_OPCODE_POS_PUSH (OPC_P)
*
*   Push the current opcode chain position onto the stack, and set a new
*   position.  OPC_P will be the new start of chain pointer.  The new chain
*   will be initialized to empty.  Subsequent calls to SST_OPCODE_NEW will
*   automatically cause new opcodes to be appended onto the new chain.
}
module sst_OPCODE_POS_PUSH;
define sst_opcode_pos_push;
%include 'sst2.ins.pas';

procedure sst_opcode_pos_push (        {push curr opc position, start new chain}
  in out  opc_p: sst_opc_p_t);         {pointer to new chain, will be set to NIL}

var
  frame_p: sst_frame_opc_pos_p_t;      {points to new stack frame}

begin
  util_stack_push (                    {create opcode position stack frame}
    sst_stack,                         {handle to the stack}
    sizeof(frame_p^),                  {size of stack frame}
    frame_p);                          {returned pointer to new stack frame}
  frame_p^.opc_p := sst_opc_p;         {save pointer to current opcode}
  frame_p^.opc_pp := sst_opc_next_pp;  {save old next chain start pointer}
  sst_opc_next_pp := addr(opc_p);      {set new chain start pointer}
  opc_p := nil;                        {init new chain to empty}
  end;
