{   Function SST_W_C_EXP_ADRABLE (EXP)
*
*   Returns TRUE if the "address of" operator (&) may be used in front of the
*   indicated expression directly.
}
module sst_w_c_EXP_ADRABLE;
define sst_w_c_exp_adrable;
%include 'sst_w_c.ins.pas';

function sst_w_c_exp_adrable (         {determine whether "address of" exp is legal}
  in      exp: sst_exp_t)              {descriptor for expression to examine}
  : boolean;                           {TRUE if "address of" expression is legal}

begin
  sst_w_c_exp_adrable := false;        {init to "address of" is not legal}
  if not sst_exp_simple(exp) then return; {expression is not simple ?}

  if exp.term1.ttype = sst_term_const_k {term is numeric constant ?}
    then return;

  if                                   {term is symbolic constant ?}
      (exp.term1.ttype = sst_term_var_k) and
      (exp.term1.var_var_p^.vtype = sst_vtype_const_k)
    then return;

  sst_w_c_exp_adrable := true;
  end;
