{   Routines to add call arguments to the current call opcode.
}
module sst_r_syn_arg;
define sst_r_syn_arg_syn;
define sst_r_syn_arg_match;
%include 'sst_r_syn.ins.pas';
{
********************************************************************************
*
*   Subroutine SST_R_SYN_ARG_SYN
*
*   Add SYN (the SYN library use state) as the next call argument to the current
*   opcode.
}
procedure sst_r_syn_arg_syn;           {add SYN as next call argument}
  val_param;

begin
  sst_call_arg_var (                   {add SYN argument}
    sst_opc_p^,                        {opcode to add call argument to}
    def_syn_p^.sym_p^.proc.first_arg_p^.sym_p^); {variable being passed}
  end;
{
********************************************************************************
*
*   Subroutine SST_R_SYN_ARG_MATCH
*
*   Add the local MATCH variable as the next call argument to the current
*   opcode.
}
procedure sst_r_syn_arg_match;         {add MATCH as next call argument}
  val_param;

begin
  sst_call_arg_var (                   {add SYN argument}
    sst_opc_p^,                        {opcode to add call argument to}
    match_var_p^.mod1.top_sym_p^);     {variable being passed}
  end;
