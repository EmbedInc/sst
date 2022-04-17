{   Routines that create comparison expressions.
}
module sst_r_syn_compare;
define sst_r_syn_comp_var_int;
define sst_r_syn_comp_var_sym;
define sst_r_syn_set_mflag;
%include 'sst_r_syn.ins.pas';
{
********************************************************************************
*
*   Subroutine SST_R_SYN_COMP_VAR_INT (SYM, IVAL, OP, EXP_P)
*
*   Create the expression that compares the variable with symbol SYM to the
*   integer value IVAL.  OP is the comparison operator.  EXP_P is returned
*   pointing to the new expression.
}
procedure sst_r_syn_comp_var_int (     {create exp comparing var to integer constant}
  in      sym: sst_symbol_t;           {variable for first term in comparison}
  in      ival: sys_int_machine_t;     {integer value to compare against}
  in      op: sst_op2_k_t;             {comparison operator}
  out     exp_p: sst_exp_p_t);         {returned pointer to new expression}
  val_param;

var
  term_p: sst_exp_term_p_t;            {pointer to second term in expression}

begin
  exp_p := sst_exp_make_var (sym);     {init expression with just variable}

  sst_mem_alloc_scope (sizeof(term_p^), term_p); {get mem for second term}
  exp_p^.term1.next_p := term_p;       {link new term as second term in exp}

  term_p^.next_p := nil;               {init term descriptor}
  term_p^.op2 := op;
  term_p^.op1 := sst_op1_none_k;
  term_p^.str_h.first_char.crange_p := nil;
  term_p^.str_h.first_char.ofs := 0;
  term_p^.str_h.last_char := term_p^.str_h.first_char;
  term_p^.rwflag := [sst_rwflag_read_k];
  term_p^.ttype := sst_term_const_k;
  term_p^.dtype_p := sym_int_p^.dtype_dtype_p;
  term_p^.dtype_hard := false;
  term_p^.val_eval := true;
  term_p^.val_fnd := true;
  term_p^.val.dtype := sst_dtype_int_k;
  term_p^.val.int_val := ival;

  exp_p^.val_eval := false;            {reset expression to not evaluated}
  sst_exp_eval (exp_p^, false);        {re-evaluate compound expression}
  end;
