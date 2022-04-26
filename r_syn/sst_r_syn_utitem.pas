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
  exp_p, exp2_p, exp3_p: sst_exp_p_t;  {scratch pointers to SST expressions}
  arg_p: sst_exp_p_t;                  {scratch pointer to call argument expression}
  var_p: sst_var_p_t;                  {scratch pointer to variable reference}
  term_p: sst_exp_term_p_t;            {scratch pointer to SST term in expression}
  data_p: symbol_data_p_t;             {pointer to symbol data in our symbol table}
  ran1, ran2: sys_int_machine_t;       {start and end of range character codes}
  jt: jump_targets_t;                  {jump targets for subordinate syntax}
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
  syn_trav_tag_string (syn_p^, token); {get the symbol name string}
  sst_r_syn_sym_lookup (token, data_p); {get data for this called syntax}
  sst_r_syn_sym_called (token);        {make this syntax as called}

  exp_p := sst_r_syn_exp_pfunc (data_p^.sym_p^); {get expression for parse func value}
  sst_r_syn_assign_exp (               {assign expression to variable}
    match_var_p^,                      {assign to local MATCH variable}
    exp_p^);                           {expression to assign to the variable}

  sst_r_syn_err_check;                 {abort parsing on end of error re-parse}
  sst_r_syn_jtarg_goto (               {jump according to MATCH}
    jtarg, [jtarg_yes_k, jtarg_no_k]);
  end;
{
********************************************************************************
*
*   Item is string constant
}
4: begin
  if not syn_trav_next_down (syn_p^)   {down into STRING syntax}
    then goto trerr;
  if syn_trav_next_tag (syn_p^) <> 1   {get tag for raw string}
    then goto trerr;
  syn_trav_tag_string (syn_p^, token); {get the raw string}

  exp_p := sst_func_exp (sym_test_string_p^); {init SYN_P_TEST_STRING function call}
  sst_func_arg (exp_p^, exp_syn_p^);   {add SYN call argument}
  sst_exp_const_vstr (token, arg_p);   {create string constant expression}
  sst_func_arg (exp_p^, arg_p^);       {pass it}
  sst_exp_const_int (token.len, arg_p); {create string length expression}
  sst_func_arg (exp_p^, arg_p^);       {pass it}

  sst_r_syn_assign_exp (               {assign SYN_P_TEST_STRING result to MATCH}
    match_var_p^,                      {variable to assign to}
    exp_p^);                           {expression to assign to it}

  sst_r_syn_err_check;                 {abort parsing on end of error re-parse}
  sst_r_syn_jtarg_goto (               {jump according to MATCH}
    jtarg, [jtarg_yes_k, jtarg_no_k]);
  if not syn_trav_up (syn_p^)          {back up from STRING syntax}
    then goto trerr;
  end;
{
********************************************************************************
*
*   Item is .RANGE
}
5: begin
  if syn_trav_next_tag (syn_p^) <> 1   {get tag for start of range character}
    then goto trerr;
  if not sst_r_syn_char_get (ran1)     {get start of range character code}
    then goto trerr;

  if syn_trav_next_tag (syn_p^) <> 1   {get tag for end of range character}
    then goto trerr;
  if not sst_r_syn_char_get (ran2)     {get end of range character code}
    then goto trerr;

  sst_call (sym_cpos_push_p^);         {save current input position on stack}
  sst_r_syn_arg_syn;                   {pass SYN}

  exp_p := sst_func_exp (sym_ichar_p^); {init SYN_P_ICHAR result value expression}
  sst_func_arg (exp_p^, exp_syn_p^);   {add SYN call argument}

  sst_r_syn_int (sym_p);               {create new integer variable}
  sst_sym_var (sym_p^, var_p);         {make variable descriptor for new var}
  sst_r_syn_assign_exp (var_p^, exp_p^); {assign function value to the new var}
  sst_r_syn_err_check;                 {abort on end of error re-parse}
{
*   Set MATCH according to whether the character code is within the range or
*   not.  EXP_P is set pointing to the expression:
*
*     (VAR >= ran1) and (VAR <= ran2)
*
*   The first sub-expression is pointed to by EXP2_P, and the second by EXP3_P.
}
  sst_r_syn_comp_var_int (             {create first sub-expression}
    sym_p^,                            {symbol of variable to compare}
    ran1,                              {integer value to compare against}
    sst_op2_ge_k,                      {compare operator}
    exp2_p);                           {returned pointer to the new expresion}

  sst_r_syn_comp_var_int (             {create second sub-expression}
    sym_p^,                            {symbol of variable to compare}
    ran2,                              {integer value to compare against}
    sst_op2_le_k,                      {compare operator}
    exp3_p);                           {returned pointer to the new expresion}
  {
  *   Create the high level expression, which is the AND of the two sub
  *   expressions.
  }
  sst_mem_alloc_scope (                {create expression descriptor}
    sizeof(exp_p^), exp_p);

  exp_p^.str_h.first_char.crange_p := nil;
  exp_p^.dtype_p := sst_dtype_bool_p;  {data type of whole expression}
  exp_p^.dtype_hard := true;           {data type is known and fixed}
  exp_p^.val_eval := true;             {indicated attempted to evaluate}
  exp_p^.val_fnd := false;             {no fixed value found}
  exp_p^.rwflag := [sst_rwflag_read_k]; {expression is read-only}

  exp_p^.term1.op2 := sst_op2_none_k;  {no operation with previous term}
  exp_p^.term1.op1 := sst_op1_none_k;  {no unary operation on this term}
  exp_p^.term1.ttype := sst_term_exp_k; {this term is an expression}
  exp_p^.term1.str_h.first_char.crange_p := nil;
  exp_p^.term1.dtype_p := exp2_p^.dtype_p; {data type of this term}
  exp_p^.term1.dtype_hard := true;     {data type is known and fixed}
  exp_p^.term1.val_eval := true;       {tried to evaluate fixed value}
  exp_p^.term1.val_fnd := false;       {no fixed value}
  exp_p^.term1.rwflag := [sst_rwflag_read_k]; {term is read-only}
  exp_p^.term1.exp_exp_p := exp2_p;    {the expression that is this term}

  sst_mem_alloc_scope (                {create descriptor for second term}
    sizeof(term_p^), term_p);
  exp_p^.term1.next_p := term_p;       {link second term to after first}

  term_p^.next_p := nil;               {this is last term in expression}
  term_p^.op2 := sst_op2_and_k;        {AND operation between prev term and this}
  term_p^.op1 := sst_op1_none_k;       {no unarty operation on this term}
  term_p^.ttype := sst_term_exp_k;     {this term is an expression}
  term_p^.str_h.first_char.crange_p := nil;
  term_p^.dtype_p := exp3_p^.dtype_p;  {data type of this term}
  term_p^.dtype_hard := true;          {data type is known and fixed}
  term_p^.val_eval := true;            {tried to evaluate fixed value}
  term_p^.val_fnd := false;            {no fixed value}
  term_p^.rwflag := [sst_rwflag_read_k]; {term is read-only}
  term_p^.exp_exp_p := exp3_p;         {the expression that is this term}
  {
  *   Assign the value of the top level expression to the local MATCH variable.
  }
  sst_r_syn_assign_exp (               {assign expression value to a variable}
    match_var_p^,                      {variable to assign to}
    exp_p^);                           {expression to assign to it}
{
*   MATCH is all set.  Handle the yes/no cases accordingly>
}
  sst_call (sym_cpos_pop_p^);          {restore input position if no match}
  sst_r_syn_arg_syn;                   {pass SYN}
  sst_r_syn_arg_match;                 {pass MATCH}

  sst_r_syn_jtarg_goto (               {jump according to MATCH}
    jtarg, [jtarg_yes_k, jtarg_no_k]);
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
  sst_r_syn_expression (jtarg);        {process the subordinate expression}
  end;
{
********************************************************************************
*
*   Item is .CHARCASE
}
8: begin
  tag := syn_trav_next_tag (syn_p^);   {get tag identifying the character case}
  case tag of                          {which character case handling is it ?}
1:  sym_p := sym_charcase_up_p;
2:  sym_p := sym_charcase_down_p;
3:  sym_p := sym_charcase_asis_p;
otherwise                              {unexpected tag value}
    syn_msg_tag_bomb (syn_p^, 'sst_syn_read', 'charcase_bad', nil, 0);
    end;

  sst_call (sym_charcase_p^);          {start call to SYN_P_CHARCASE}
  sst_r_syn_arg_syn;                   {add SYN call argument}
  sst_call_arg_enum (sst_opc_p^, sym_p^); {add char case handling choice argument}

  sst_r_syn_assign_match (true);       {this item always matches}
  sst_r_syn_jtarg_goto (               {jump according to MATCH}
    jtarg, [jtarg_yes_k, jtarg_no_k]);
  end;
{
********************************************************************************
*
*   Item is .NULL
}
9: begin
  sst_r_syn_assign_match (true);       {this item always matches}
  sst_r_syn_jtarg_goto (               {jump according to MATCH}
    jtarg, [jtarg_yes_k, jtarg_no_k]);
  end;
{
********************************************************************************
*
*   Item is .UPTO
}
10: begin
  sst_call (sym_cpos_push_p^);         {save current input position on stack}
  sst_r_syn_arg_syn;                   {pass SYN}

  sst_r_syn_jtarg_sub (                {make subordinate jump targets for ITEM}
    jtarg,                             {parent jump targets}
    jt,                                {new subordinate targets}
    lab_fall_k,                        {fall thru on YES}
    lab_fall_k);                       {fall thru on NO}
  sst_r_syn_item (jt);                 {process the UPTO item}
  sst_r_syn_jtarg_here (jt);           {define jump target labels here}

  sst_call (sym_cpos_pop_p^);          {restore input position if no match}
  sst_r_syn_arg_syn;                   {pass SYN}
  sst_call_arg_exp (sst_opc_p^, exp_false_p^); {pass FALSE to always restore position}

  sst_r_syn_jtarg_goto (               {jump according to MATCH}
    jtarg, [jtarg_yes_k, jtarg_no_k]);
  end;
{
********************************************************************************
*
*   Item is .NOT
}
11: begin
  sst_r_syn_jtarg_sub (                {make subordinate jump targets for ITEM}
    jtarg,                             {parent jump targets}
    jt,                                {new subordinate targets}
    lab_fall_k,                        {fall thru on YES}
    lab_fall_k);                       {fall thru on NO}
  sst_r_syn_item (jt);                 {process the item that will be negated}
  sst_r_syn_jtarg_here (jt);           {define jump target labels here}

  sst_r_syn_assign_exp (               {negate the local MATCH variable}
    match_var_p^,                      {variable to assign to}
    match_not_exp_p^);                 {expression to assign to the variable}

  sst_r_syn_jtarg_goto (               {jump according to MATCH}
    jtarg, [jtarg_yes_k, jtarg_no_k]);
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
