{   Subroutine SST_W_C_TERMS_IMPLICIT (TERM1,N_TERMS,DTYPE,SYM_P)
*
*   Create an implicit var, if neccessary, from the expression formed by the
*   list of terms starting at TERM1.  N_TERMS is the number of terms that are
*   to be considered in the expression.  SYM_P is returned pointing to the
*   symbol descriptor of the implicit variable if one is created.  It will be
*   returned NIL when no implicit variable is created.  DTYPE is the descriptor
*   for the data type to make the implicit variable if one is created.
}
module sst_w_c_TERMS_IMPLICIT;
define sst_w_c_terms_implicit;
%include 'sst_w_c.ins.pas';

procedure sst_w_c_terms_implicit (     {make implicit var from terms list if needed}
  in      term1: sst_exp_term_t;       {descriptor for first term in chain}
  in      n_terms: sys_int_machine_t;  {number of terms in chain to consider}
  in      dtype: sst_dtype_t;          {descriptor for data type of implicit var}
  out     sym_p: sst_symbol_p_t);      {will pnt to implicit var, or NIL for unused}

var
  term_p: sst_exp_term_p_t;            {points to current term in terms chain}
  i: sys_int_machine_t;                {scratch integer and loop counter}
  v: sst_var_t;                        {descriptor for implicit variable reference}

begin
  if                                   {don't need implicit variable ?}
      (n_terms = 1) and then           {only one term, not a compound expression ?}
      sst_term_simple(term1)           {the one and only term is "simple" ?}
      then begin
    sym_p := nil;
    return;
    end;
{
*   An implicit variable needs to be created.
}
  sst_w_c_implicit_var (dtype, v);     {create and declare implicit variable}
  sym_p := v.mod1.top_sym_p;           {extract and pass back pointer to variable}
  sst_w_c_pos_push (sment_type_exec_k); {position for write before curr statement}
  sst_w_c_sment_start;                 {start assignment statement}
  sst_w.append_sym_name^ (sym_p^);     {write name of implicit variable}
  sst_w.delimit^;
  sst_w.appendn^ ('=', 1);             {assignment operator}
  sst_w.delimit^;
  if n_terms <= 1
    then begin                         {expression is only one term}
      sst_w_c_term (term1, 0, enclose_no_k); {write value of term}
      end
    else begin                         {expression is list of terms with operators}
      term_p := term1.next_p;          {init curr term to second in chain}
      for i := 3 to n_terms do begin   {loop until TERM_P points to last term}
        term_p := term_p^.next_p;      {advance current term to next in chain}
        end;
      sst_w_c_exp2(                    {write the compound expression}
        term1,                         {first term in expression before operator}
        n_terms - 1,                   {number of terms in exp before operator}
        term_p^,                       {first term in expression after operator}
        1,                             {number of terms in exp after operator}
        term_p^.op2,                   {operator between expressions}
        enclose_no_k);                 {no need to enclose expression in parentheses}
      end
    ;                                  {done writing expression value}
  sst_w_c_sment_end;
  sst_w_c_pos_pop;                     {restore original writing position}
  end;
