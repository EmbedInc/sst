{   Subroutine SST_CALL (SYM)
*
*   Create an opcode for a call to the subroutine is indicated by the
*   symbol SYM.
}
module sst_call;
define sst_call;
%include 'sst2.ins.pas';

procedure sst_call (                   {create subroutine call opcode}
  in      sym: sst_symbol_t);          {symbol for name of subroutine to call}
  val_param;

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

begin
  if sym.symtype <> sst_symtype_proc_k then begin {symbol not a subroutine ?}
    sys_msg_parm_vstr (msg_parm[1], sym.name_in_p^);
    sys_message_bomb ('sst', 'symbol_not_subr_in_call', msg_parm, 1);
    end;

  sst_opcode_new;                      {create new opcode for subroutine call}
  sst_opc_p^.opcode := sst_opc_call_k;
  sst_opc_p^.call_proct_p := addr(sym.proc);
{
*   Create "variable" reference to subroutine entry point and link
*   in to opcode.
}
  sst_mem_alloc_scope (sizeof(sst_opc_p^.call_var_p^), sst_opc_p^.call_var_p);
  sst_opc_p^.call_var_p^.mod1.next_p := nil;
  sst_opc_p^.call_var_p^.mod1.modtyp := sst_var_modtyp_top_k;
  sst_opc_p^.call_var_p^.mod1.top_str_h := sst_opc_p^.str_h;
  sst_opc_p^.call_var_p^.mod1.top_sym_p := addr(sym);
  sst_opc_p^.call_var_p^.dtype_p := nil;
  sst_opc_p^.call_var_p^.rwflag := [sst_rwflag_read_k];
  sst_opc_p^.call_var_p^.vtype := sst_vtype_rout_k;
  sst_opc_p^.call_var_p^.rout_proc_p := addr(sym.proc);
{
*   Create initial called routine descriptor and link into opcode.
*   The descriptor will be initialized with no arguments.
}
  sst_mem_alloc_scope (sizeof(sst_opc_p^.call_proc_p^), sst_opc_p^.call_proc_p);
  sst_opc_p^.call_proc_p^.sym_p := addr(sym);
  sst_opc_p^.call_proc_p^.dtype_func_p := nil;
  sst_opc_p^.call_proc_p^.n_args := 0;
  sst_opc_p^.call_proc_p^.flags := [];
  sst_opc_p^.call_proc_p^.first_arg_p := nil;
  end;
