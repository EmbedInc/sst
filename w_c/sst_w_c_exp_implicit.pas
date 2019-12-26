{   Subroutine SST_W_C_EXP_IMPLICIT (EXP,SYM_P)
*
*   Create an implicit variable for the value of expression EXP, if EXP is
*   not a "simple" expression.  If an implicit variable is created, then SYM_P
*   will point to its symbol descriptor.  If not, then SYM_P will be returned
*   NIL.
}
module sst_w_c_EXP_IMPLICIT;
define sst_w_c_exp_implicit;
%include 'sst_w_c.ins.pas';

procedure sst_w_c_exp_implicit (       {create implicit var for exp value, if needed}
  in      exp: sst_exp_t;              {descriptor for expression}
  out     sym_p: sst_symbol_p_t);      {will pnt to implicit var, or NIL for unused}

begin
  if sst_exp_simple (exp) then begin   {no need for an implicit variable ?}
    sym_p := nil;                      {indicate no implicit variable created}
    return;
    end;
  sst_w_c_exp_explicit (exp, exp.dtype_p^, sym_p);
  end;
