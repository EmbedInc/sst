{   Subroutine SST_R_SYN_UTITEM (JTARG)
*
*   Process UNTAGGED_ITEM syntax.
}
module sst_r_syn_utitem;
define sst_r_syn_utitem;
%include 'sst_r_syn.ins.pas';

procedure sst_r_syn_utitem (           {process UNTAGGED_ITEM syntax}
  in out  jtarg: jump_targets_t);      {execution block jump targets info}
  val_param;

var
  tag: sys_int_machine_t;              {tag from syntax tree}
  sym_p: sst_symbol_p_t;               {scratch pointer to SST symbol}
  exp_p: sst_exp_p_t;                  {scratch pointer to SST expression}
  term_p: sst_exp_term_p_t;            {scratch pointer to SST term in expression}
  token: string_var8192_t;             {scratch token or string}

label
  comp_char_sym,
  trerr;

begin
  token.max := sizeof(token.str);      {init local var string}

  if not syn_trav_next_down (syn_p^)   {down into UNTAGGED_ITEM syntax}
    then goto trerr;
  tag := syn_trav_next_tag (syn_p^);   {get tag indicating the type of item}
  case tag of                          {what kind of item is it ?}
{
********************************************************************************
*
*   Item is .EOL
}
1: begin
  sym_p := sym_ichar_eol_p;            {get pointer to constant to compare char to}
{
*   Common code to compare the next input character to the constant pointed to
*   by SYM_P.  MATCH is set to TRUE iff the two match.
}
comp_char_sym:                         {common code to compare next char to SYM_P^}
  sst_call (sym_cpos_push_p^);         {save current input position on stack}
  sst_r_syn_arg_syn;                   {pass SYN}
{
*   Make expression comparing the next input character to the constant at SYM_P.
}
  sst_mem_alloc_scope (                {create expression descriptor}
    sizeof(exp_p^), exp_p);

  exp_p^.str_h.first_char.crange_p := nil;
  exp_p^.dtype_p := sst_dtype_bool_p;  {data type of whole expression}
  exp_p^.dtype_hard := true;           {data type is known and fixed}
  exp_p^.val_eval := true;             {attempted to resolve value}
  exp_p^.val_fnd := false;             {fixed value not found}
  exp_p^.rwflag := [sst_rwflag_read_k]; {expression is read-only}

  exp_p^.term1.op2 := sst_op2_none_k;  {no operator with previous term}
  exp_p^.term1.op1 := sst_op1_none_k;  {no unary operator applying to this term}
  exp_p^.term1.ttype := sst_term_func_k; {term is the value of a function}
  exp_p^.term1.str_h.first_char.crange_p := nil;
  exp_p^.term1.dtype_p := sym_int_p^.dtype_dtype_p; {term is machine integer}
  exp_p^.term1.dtype_hard := true;     {data type is known and fixed}
  exp_p^.term1.val_eval := true;       {tried to resolve value}
  exp_p^.term1.val_fnd := false;       {not a known fixed value}
  exp_p^.term1.rwflag := [sst_rwflag_read_k]; {term is read-only}
  exp_p^.term1.func_var_p := var_ichar_p; {"variable" reference for calling the function}
  exp_p^.term1.func_proc_p := exp_ichar_p^.term1.func_proc_p; {function call}
  exp_p^.term1.func_proct_p := exp_ichar_p^.term1.func_proct_p; {function template}

  sst_mem_alloc_scope (sizeof(term_p^), term_p); {get mem for second term}
  exp_p^.term1.next_p := term_p;       {link new term as second term in exp}

  term_p^.next_p := nil;               {no additional terms in expression}
  term_p^.op2 := sst_op2_eq_k;         {operator with previous term}
  term_p^.op1 := sst_op1_none_k;       {no unary operation on this term}
  term_p^.ttype := sst_term_const_k;   {this term is a constant}
  term_p^.str_h.first_char.crange_p := nil;
  term_p^.dtype_p := sym_int_p^.dtype_dtype_p; {data type is machine integer}
  term_p^.dtype_hard := true;          {data type is known and fixed}
  term_p^.val_eval := true;            {tried to resolve value}
  term_p^.val_fnd := true;             {found known fixed value}
  term_p^.val := sym_p^.const_exp_p^.val; {copy value from the symbol to compare against}
  term_p^.rwflag := [sst_rwflag_read_k]; {term is read-only}
{
*   Set MATCH to the result of comparing the next input character to the
*   selected constant.
}
  sst_r_syn_assign_exp (               {assign expression result to a variable}
    match_var_p^,                      {variable to assign to}
    exp_p^);                           {expression to assign to the variable}
  sst_r_syn_err_check;                 {abort on end of error re-parse}

  sst_call (sym_cpos_pop_p^);          {restore input position if no match}
  sst_r_syn_arg_syn;                   {pass SYN}
  sst_r_syn_arg_match;                 {pass MATCH}

  sst_r_syn_jtarg_goto (               {jump according to MATCH}
    jtarg, [jtarg_yes_k, jtarg_no_k]);
  end;
{
********************************************************************************
*
*   Item is .EOF
}
2: begin
  sym_p := sym_ichar_eof_p;            {constant next input char must match}
  goto comp_char_sym;
  end;
{
********************************************************************************
*
*   Item is symbol reference
}
3: begin
  end;
{
********************************************************************************
*
*   Item is string constant
}
4: begin
  end;
{
********************************************************************************
*
*   Item is .RANGE
}
5: begin
  end;
{
********************************************************************************
*
*   Item is .OCCURS
}
6: begin
  end;
{
********************************************************************************
*
*   Item is nested expression in parenthesis.
}
7: begin
  end;
{
********************************************************************************
*
*   Item is .CHARCASE
}
8: begin
  end;
{
********************************************************************************
*
*   Item is .NULL
}
9: begin
  end;
{
********************************************************************************
*
*   Item is .UPTO
}
10: begin
  end;
{
********************************************************************************
*
*   Item is .NOT
}
11: begin
  end;
{
********************************************************************************
*
*   Item is .EOD
}
12: begin
  sym_p := sym_ichar_eod_p;            {constant next input char must match}
  goto comp_char_sym;
  end;
{
********************************************************************************
*
*   Item is .OPTIONAL
}
13: begin
  end;
{
********************************************************************************
*
*   Unexpected tag value.
}
otherwise
    syn_msg_tag_bomb (syn_p^, 'sst_syn_read', 'syerr_utitem', nil, 0);
    end;                               {end of item format cases}

  if not syn_trav_up(syn_p^)           {back up from UNTAGGED_ITEM syntax}
    then goto trerr;
  return;
{
*   The syntax tree is not as expected.  We assume this is due to a syntax
*   error.
}
trerr:
  sys_message ('sst_syn_read', 'syerr_utitem');
  syn_parse_err_show (syn_p^);
  sys_bomb;
  end;


(*
var
  sym_p: sst_symbol_p_t;               {scratch symbol pointer}
  var_p: sst_var_p_t;                  {scratch variable pointer}
  exp_p: sst_exp_p_t;                  {scratch expression pointer}

begin
  sst_r_syn_int (sym_p);               {create integer variable}
  sst_sym_var (sym_p^, var_p);         {create reference to the variable}
  sst_exp_const_int (44, exp_p);       {create constant integer expression}
  sst_r_syn_assign_exp (var_p^, exp_p^); {assign the expression to the variable}
  sst_r_syn_jtarg_goto (jtarg, [jtarg_yes_k, jtarg_no_k]);
  end;
*)
