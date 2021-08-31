{   Subroutine SST_FLAG_USED_EXP (EXP)
*
*   Flag all symbols eventually referenced by the expression EXP as used.
}
module sst_FLAG_USED_EXP;
define sst_flag_used_exp;
%include 'sst2.ins.pas';

procedure sst_flag_used_exp (          {flag symbols eventually used from expression}
  in      exp: sst_exp_t);             {expression that may reference symbols}

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  term_p: sst_exp_term_p_t;            {points to current term in expression}
  ele_p: sst_ele_exp_p_t;              {points to current set elements descriptor}
  ifarg_p: sst_exp_chain_p_t;          {points to current itrinsic function argument}
  msg_parm:                            {references to paramters for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

begin
  if exp.dtype_p <> nil then begin     {this expression has a data type ?}
    sst_flag_used_dtype (exp.dtype_p^); {flag symbols used by data type}
    end;

  term_p := addr(exp.term1);           {init current term to first in expression}

  while term_p <> nil do begin         {once for each term in expression}
    if exp.term1.next_p <> nil then begin {expression has more than one term ?}
      sst_flag_used_dtype (term_p^.dtype_p^); {declare dtype syms for this term}
      end;
    case term_p^.ttype of              {what kind of term is this ?}
{
*   Term is a constant.
}
sst_term_const_k: ;
{
*   Term is a variable reference.
}
sst_term_var_k: begin
  sst_flag_used_var (term_p^.var_var_p^);
  end;
{
*   Term is a function reference.
}
sst_term_func_k: begin
  sst_flag_used_var (term_p^.func_var_p^);
  sst_flag_used_rout (term_p^.func_proc_p^);
  end;
{
*   Term is an intrinsic function reference.
}
sst_term_ifunc_k: begin
  ifarg_p := term_p^.ifunc_args_p;     {init current ifunc argument to first arg}
  while ifarg_p <> nil do begin        {once for each argument}
    sst_flag_used_exp (ifarg_p^.exp_p^);
    ifarg_p := ifarg_p^.next_p;        {advance to next argument}
    end;                               {back and process this new argument}
  end;
{
*   Term is explicit type-casting function.
}
sst_term_type_k: begin
  sst_flag_used_dtype (term_p^.type_dtype_p^);
  sst_flag_used_exp (term_p^.type_exp_p^);
  end;
{
*   Term is a SET value.
}
sst_term_set_k: begin
  ele_p := term_p^.set_first_p;        {init first elements descriptor to current}
  while ele_p <> nil do begin          {once for each elements descriptor in set}
    sst_flag_used_exp (ele_p^.first_p^); {flag ele range start value exp as used}
    if ele_p^.last_p <> nil then begin {ele end range expression exists ?}
      sst_flag_used_exp (ele_p^.last_p^);
      end;
    ele_p := ele_p^.next_p;            {advance to next elements descriptor}
    end;                               {back and process this new elements descriptor}
  end;
{
*   Term is nested expression.
}
sst_term_exp_k: begin
  sst_flag_used_exp (term_p^.exp_exp_p^); {process nested expression}
  end;
{
*   Term is the value of a field in a record.
}
sst_term_field_k: begin
  sst_flag_used_symbol (term_p^.field_sym_p^);
  sst_flag_used_exp (term_p^.field_exp_p^);
  end;
{
*   Term is the value of a range of array subscripts.
}
sst_term_arele_k: begin
  sst_flag_used_exp (term_p^.arele_exp_p^);
  end;
{
*   Unrecognized or illegal term type.
}
otherwise
      sys_msg_parm_int (msg_parm[1], ord(term_p^.ttype));
      syo_error (term_p^.str_h, 'sst', 'term_type_unknown', msg_parm, 1);
      end;                             {end of term type cases we needed to handle}
    term_p := term_p^.next_p;          {advance to next term in expression}
    end;                               {back and process this new term in expression}
  end;
