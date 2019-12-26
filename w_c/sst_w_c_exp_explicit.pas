{   Subroutine SST_W_C_EXP_EXPLICIT (EXP, DTYPE, SYM_P)
*
*   Create a variable that will have the value of the expression EXP.
*   The variable will be declared and set to the expression value.
*   DTYPE is the descriptor for the data type the variable is to have.
*   SYM_P is returned pointing to the variable's symbol descriptor.
}
module sst_w_c_EXP_EXPLICIT;
define sst_w_c_exp_explicit;
%include 'sst_w_c.ins.pas';

procedure sst_w_c_exp_explicit (       {always create variable for expression value}
  in      exp: sst_exp_t;              {descriptor for expression}
  in      dtype: sst_dtype_t;          {data type descriptor for variable}
  out     sym_p: sst_symbol_p_t);      {will point to variable's symbol descriptor}

var
  v: sst_var_t;                        {descriptor for implicit var reference}

begin
  if exp.val_fnd
{
*   The expression has a known constant value.  The implicit variable will
*   be declared STATIC, with the expression as its initial value.  If a
*   static variable was previously declared with the same data type and value,
*   then the existing static variable will be reused.
}
    then begin
      sst_w_c_implicit_const (         {create/reuse static constant variable}
        dtype,                         {data type of variable}
        exp.val,                       {value of variable}
        sym_p);                        {returned pointer to new/created variable}
      end
{
*   The expression does not have a known constant value.  The expression value
*   will be assigned to the implicit value at run time right before the current
*   statement.
}
    else begin
      sst_w_c_implicit_var (           {create and declare implicit variable}
        dtype,                         {data type for new variable}
        v);                            {filled in variable reference descriptor}
      sst_w_c_pos_push (sment_type_exec_k); {position for write before curr statement}
      sst_w_c_assign (v, exp);         {assign expression value to new variable}
      sst_w_c_pos_pop;                 {restore original writing position}
      sym_p := v.mod1.top_sym_p;       {return pointer to new symbol descriptor}
      end
    ;
  end;
