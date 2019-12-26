{   Subroutine SST_OPCODE_NEW
*
*   Create a new opcode descriptor at the current end of chain.
}
module sst_OPCODE_NEW;
define sst_opcode_new;
%include 'sst2.ins.pas';

procedure sst_opcode_new;              {create new empty opcode, make current}

begin
  util_mem_grab (                      {allocate memory for new opcode descriptor}
    sizeof(sst_opc_p^),                {amount of memory to allocate}
    sst_scope_p^.mem_p^,               {parent memory context}
    false,                             {won't need to separately deallocate this}
    sst_opc_p);                        {returned pointer to new memory}
  sst_opc_next_pp^ := sst_opc_p;       {link new opcode to end of chain}
  sst_opc_next_pp := addr(sst_opc_p^.next_p); {update end of chain pointer}
  sst_opc_p^.next_p := nil;            {init to new opcode is last in chain}
  sst_opc_p^.str_h.first_char.crange_p := nil;
  sst_opc_p^.str_h.first_char.ofs := 0;
  sst_opc_p^.str_h.last_char.crange_p := nil;
  sst_opc_p^.str_h.last_char.ofs := 0;
  end;
