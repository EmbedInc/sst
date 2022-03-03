{   Module of routines to create opcodes.
}
module sst_r_syn_opc;
define sst_r_syn_opc_assign;
%include 'sst_r_syn.ins.pas';
{
*********************************************************
}
procedure sst_r_syn_opc_assign (       {make opcode to assign symbol to variable}
  in      sym1: sst_symbol_t;          {variable to assign value to}
  in      sym2: sst_symbol_t);         {symbol with value to assign}

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  var_p: sst_var_p_t;                  {scratch pointer to var descriptor}
  exp_p: sst_exp_p_t;                  {scratch pointer to exp descriptor}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

begin
  sst_mem_alloc_scope (sizeof(var_p^), var_p); {create variable and expression desc}
  sst_mem_alloc_scope (sizeof(exp_p^), exp_p);

  sst_opcode_new;                      {create assignment opcode}
  sst_opc_p^.opcode := sst_opc_assign_k;
  sst_opc_p^.assign_var_p := var_p;
  sst_opc_p^.assign_exp_p := exp_p;
{
*   Fill in variable reference descriptor for the assignment variable.
}
  var_p^.mod1.next_p := nil;
  var_p^.mod1.modtyp := sst_var_modtyp_top_k;
  var_p^.mod1.top_str_h := sst_opc_p^.str_h;
  var_p^.mod1.top_sym_p := addr(sym1);

  var_p^.dtype_p := sym1.var_dtype_p;
  if sym1.var_arg_p = nil
    then begin                         {variable is not dummy argument}
      var_p^.rwflag := [sst_rwflag_read_k, sst_rwflag_write_k];
      end
    else begin
      var_p^.rwflag := sym1.var_arg_p^.rwflag_int;
      end
    ;
  var_p^.vtype := sst_vtype_var_k;
{
*   Fill in expression descriptor assignment value.
}
  exp_p^.term1.next_p := nil;          {init static parts of term desciptor}
  exp_p^.term1.op2 := sst_op2_none_k;
  exp_p^.term1.op1 := sst_op1_none_k;
  exp_p^.term1.str_h := sst_opc_p^.str_h;
  exp_p^.term1.dtype_hard := true;
  exp_p^.term1.val_eval := false;
  exp_p^.term1.val_fnd := false;
  exp_p^.term1.rwflag := [sst_rwflag_read_k];

  case sym2.symtype of

sst_symtype_const_k: begin             {symbol 2 is a mnemonic constant}
      exp_p^.term1.ttype := sst_term_const_k;
      exp_p^.term1.dtype_p := sym2.const_exp_p^.dtype_p;
      exp_p^.term1.dtype_hard := sym2.const_exp_p^.dtype_hard;
      exp_p^.term1.val_eval := true;
      exp_p^.term1.val_fnd := true;
      exp_p^.term1.val := sym2.const_exp_p^.val;
      end;

sst_symtype_enum_k: begin              {symbol 2 is enumerated constant}
      exp_p^.term1.ttype := sst_term_const_k;
      exp_p^.term1.dtype_p := sym2.enum_dtype_p;
      exp_p^.term1.dtype_hard := true;
      exp_p^.term1.val_eval := true;
      exp_p^.term1.val_fnd := true;
      exp_p^.term1.val.dtype := sst_dtype_enum_k;
      exp_p^.term1.val.enum_p := addr(sym2);
      end;

otherwise
    sys_msg_parm_int (msg_parm[1], ord(sym2.symtype));
    sys_message_bomb ('sst', 'symbol_type_unknown', msg_parm, 1);
    end;

  exp_p^.str_h := sst_opc_p^.str_h;    {fill in rest of expression descriptor}
  exp_p^.dtype_p := nil;
  exp_p^.dtype_hard := true;
  exp_p^.val_eval := false;
  exp_p^.val_fnd := false;
  exp_p^.rwflag := exp_p^.term1.rwflag;
  sst_exp_eval (exp_p^, false);        {fully evaluate the expression}
  end;
