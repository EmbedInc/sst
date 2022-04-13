{   Routines that create assignment statement opcodes.
}
module sst_r_syn_assign;
define sst_r_syn_assign_exp;
define sst_r_syn_assign_match;
%include 'sst_r_syn.ins.pas';
{
********************************************************************************
*
*   Subroutine SST_R_SYN_ASSIGN_EXP (V, EXP)
*
*   Write SST opcode to assign the expression EXP to the variable SYM.
}
procedure sst_r_syn_assign_exp (       {make opcode to assign expression to variable}
  in      var v: sst_var_t;            {variable to assign value to}
  in      var exp: sst_exp_t);         {expression to assign to the variable}
  val_param;

begin
  sst_opcode_new;                      {create new blank opcode, make it current}

  sst_opc_p^.opcode := sst_opc_assign_k; {set opcode type}
  sst_opc_p^.assign_var_p := addr(v);  {save pointer to the variable}
  sst_opc_p^.assign_exp_p := addr(exp); {save pointer to the expression}
  end;
{
********************************************************************************
*
*   Subroutine SST_R_SYN_ASSIGN_MATCH (TF)
*
*   Write SST opcode to assign the value TF to the local variable MATCH of the
*   subroutine being currently defined.
}
procedure sst_r_syn_assign_match (     {make opcode to assign true/false to MATCH}
  in      tf: boolean);                {the value to assign to MATCH}
  val_param;

begin
  if tf
    then begin
      sst_r_syn_assign_exp (match_var_p^, exp_true_p^);
      end
    else begin
      sst_r_syn_assign_exp (match_var_p^, exp_false_p^);
      end
    ;
  end;
