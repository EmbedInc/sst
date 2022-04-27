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
  lab_loop_p: sst_symbol_p_t;          {pointer to label at start of loop}
  exp_p, exp2_p, exp3_p: sst_exp_p_t;  {scratch pointers to SST expressions}
  arg_p: sst_exp_p_t;                  {scratch pointer to call argument expression}
  var_p: sst_var_p_t;                  {scratch pointer to variable reference}
  term_p: sst_exp_term_p_t;            {scratch pointer to SST term in expression}
  data_p: symbol_data_p_t;             {pointer to symbol data in our symbol table}
  ran1, ran2: sys_int_machine_t;       {start and end of range character codes}
  occ1, occ2: sys_int_machine_t;       {min and max occurance limits}
  occinf: boolean;                     {max occur limit infinite, OCC2 irrelevant}
  jt: jump_targets_t;                  {jump targets for subordinate syntax}
  token: string_var8192_t;             {scratch token or string}
  stat: sys_err_t;                     {completion status}

label
  comp_char_sym, occur_n, occur_dnmatch,
  done_item, trerr;

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

  sst_r_syn_exp_exp2 (                 {create top level expression}
    exp2_p^, exp3_p^,                  {the two subexpressions}
    sst_op2_and_k,                     {operator to combine subexpressions}
    exp_p);                            {returned pointer to combined expression}
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
{
*   Read the min/max occurance limits and set OCC1, OCC2, and OCCINF
*   accordingly.
}
  {
  *   Get the minimum number of occurance into OCC1.
  }
  if syn_trav_next_tag (syn_p^) <> 1   {get tag for first value integer}
    then goto trerr;
  syn_trav_tag_string (syn_p^, token); {get the raw string}
  string_t_int (token, occ1, stat);    {get min occurance number}
  syn_error_bomb (syn_p^, stat, 'sst_syn_read', 'occurs_limit_low_bad', nil, 0);
  {
  *   Get the maximum number of occurances into OCC2, or set OCCINF to indicate
  *   there is no upper limit.  The value of OCC2 is irrelevant when OCCINF is
  *   TRUE.
  }
  if not syn_trav_next_down (syn_p^)   {down into END_RANGE syntax}
    then goto trerr;

  case syn_trav_next_tag(syn_p^) of    {which type of end of range is it ?}
1:  begin                              {explicit integer value}
      syn_trav_tag_string (syn_p^, token); {get the raw string}
      string_t_int (token, occ2, stat); {get max occurance number}
      syn_error_bomb (syn_p^, stat, 'sst_syn_read', 'integer_bad', nil, 0);
      occinf := false;                 {OCC2 contains the finite upper limit}
      end;
2:  begin                              {inifinite}
      occ2 := 0;                       {set to fixed value, unused}
      occinf := true;                  {indicate no upper limit on occurances}
      end;
otherwise
    syn_msg_tag_bomb (                 {unexpected tag encountered}
      syn_p^, 'sst_syn_read', 'occurs_limit_high_bad', nil, 0);
    end;

  if not syn_trav_up (syn_p^)          {back up from END_RANGE syntax}
    then goto trerr;
  {
  *   Check for the upper limit is less than the lower limit.  The .OCCURS
  *   condition is always FALSE then.
  }
  if                                   {check for impossible condition}
      (not occinf) and                 {upper limit exists ?}
      (occ2 < occ1)                    {both limits can't be met ?}
      then begin
    sst_r_syn_assign_match (false);    {indicate syntax doesn't match}
    sst_r_syn_jtarg_goto (             {jump according to MATCH}
      jtarg, [jtarg_yes_k, jtarg_no_k]);
    goto done_item;                    {done procession this .OCCURS item}
    end;
  {
  *   Check for the upper limit is 0 or less.  The .OCCURS condition is
  *   always TRUE then.
  }
  if                                   {0 upper limit ?}
      (not occinf) and                 {upper limit exists ?}
      (occ2 <= 0)                      {limit is always met ?}
      then begin
    sst_r_syn_assign_match (true);     {indicate syntax matched}
    sst_r_syn_jtarg_goto (             {jump according to MATCH}
      jtarg, [jtarg_yes_k, jtarg_no_k]);
    goto done_item;                    {done procession this .OCCURS item}
    end;
{
*   The range for valid number of occurrances is known.  The following state is
*   set:
*
*     OCC1  -  Minimum required number of occurances.
*
*     OCC2  -  Maximum allowed number of occurances.
*
*     OCCINF  -  There is no upper bound on the maximum number of occurances.
*       When OOCINF is TRUE, then the OCC2 value is unused and irrelevant.
*
*   The above conditions have been verified to be possible to meet, and that the
*   item must be run at least once.
*
*   The next syntax tree entry is for the subordinate item that is to be
*   repeated.
}
occur_n:                               {OCC1, OCC2, OCCINF all set and valid}
{
*   Create the occurrance counter and initialize it to 0.
}
  sst_r_syn_int (sym_p);               {create new integer variable}
  sst_sym_var (sym_p^, var_p);         {make reference to new variable}
  sst_exp_const_int (0, exp_p);        {make constant 0 expression}
  sst_r_syn_assign_exp (               {assign expression to variable}
    var_p^,                            {variable}
    exp_p^);                           {expression}
{
*   Create the label to jump to for repeating the loop.
}
  sst_r_syn_jtarg_label_here (lab_loop_p); {create and define top of loop label}
{
*   Process the subordinate ITEM syntax.
}
  sst_r_syn_jtarg_sub (                {make jump targets for subordinate item}
    jtarg,                             {parent jump targets}
    jt,                                {new subordinate targets}
    lab_fall_k,                        {fall thru on YES}
    lab_fall_k);                       {fall thru on NO}

  sst_r_syn_item (jt);                 {process the item, set MATCH accordingly}

  sst_r_syn_jtarg_here (jt);           {define jump target labels here}
{
*   Handle the case of the item not matching the syntax template.  In that case,
*   the final MATCH answer is whether the number of iterations is within the
*   min/max limits.  Since the loop is aborted whenever the number of iterations
*   matches the upper limit, there is no need to check the upper limit.  The
*   iteration count can't exceed the upper limit here.
}
  sst_opcode_new;                      {create new opcode for IF}
  sst_opc_p^.opcode := sst_opc_if_k;
  sst_opc_p^.if_exp_p := match_not_exp_p; {conditional expression is NOT MATCH}
  sst_opc_p^.if_false_p := nil;        {there is no FALSE case code}
  sst_opcode_pos_push (sst_opc_p^.if_true_p); {set up for writing TRUE code}
  {
  *   The item did not match.  Now set MATCH according to whether the number of
  *   iterations meets the lower limit.
  }
  if occ1 <= 0
    then begin                         {no lower limit, the result is always YES}
      sst_r_syn_assign_match (true);   {indicate syntax matched}
      sst_r_syn_jtarg_sym (            {make sure YES case has a label}
        jtarg.yes, sym_p);
      sst_r_syn_jtarg_label_goto (     {unconditionally jump to the label for YES}
        jtarg.yes.lab_p^);
      goto occur_dnmatch;              {done handling didn't match case}
      end
    else begin                         {need to check against the lower limit}
      sst_r_syn_comp_var_int (         {make exp comparing count to lower limit}
        var_p^.mod1.top_sym_p^,        {the variable to compare value of}
        occ1,                          {integer to compare it with}
        sst_op2_ge_k,                  {comparison operator}
        exp_p);                        {returned pointer to the comparison expression}
      sst_r_syn_assign_exp (           {set MATCH to the comparison result}
        match_var_p^,                  {the variable to assign to}
        exp_p^);                       {the expression to assign to it}
      end
    ;
  {
  *   Done with this item, jump according to MATCH.
  }
  sst_r_syn_jtarg_sub (                {make jump targets for subordinate item}
    jtarg,                             {parent jump targets}
    jt,                                {new subordinate targets}
    lab_same_k,                        {to same place as parent}
    lab_same_k);                       {to same place as parent}
  sst_r_syn_jtarg_goto (               {jump to yes/no labels according to MATCH}
    jt, [jtarg_yes_k, jtarg_no_k]);

occur_dnmatch:                         {done with didn't match case}
  sst_opcode_pos_pop;                  {done writing TRUE case opcodes}
{
*   Increment the number of occurences.
}
  exp_p := sst_exp_make_var (          {init expression to be ref to the counter}
    var_p^.mod1.top_sym_p^);

  sst_mem_alloc_scope (sizeof(term_p^), term_p); {get mem for second term}
  exp_p^.term1.next_p := term_p;       {link new term as second term in exp}

  term_p^.next_p := nil;               {no additional terms in expression}
  term_p^.op2 := sst_op2_add_k;        {operator with previous term}
  term_p^.op1 := sst_op1_none_k;       {no unary operation on this term}
  term_p^.ttype := sst_term_const_k;   {this term is a constant}
  term_p^.str_h.first_char.crange_p := nil;
  term_p^.dtype_p := sym_int_p^.dtype_dtype_p; {data type is machine integer}
  term_p^.dtype_hard := true;          {data type is known and fixed}
  term_p^.val_eval := true;            {tried to resolve value}
  term_p^.val_fnd := true;             {found known fixed value}
  term_p^.val.dtype := sst_dtype_int_k; {constant data type is integer}
  term_p^.val.int_val := 1;            {the constant value}
  term_p^.rwflag := [sst_rwflag_read_k]; {term is read-only}

  sst_r_syn_assign_exp (               {assign incremented value to the counter}
    var_p^,                            {the variable to assign to}
    exp_p^);                           {the expression to assign to it}
{
*   Stop processing and return TRUE if the maximum number of occurences has been
*   met.  It was previously verified to be at least the minimum number of
*   allowed occurences.
}
  if not occinf then begin             {there is an upper limit ?}
    sst_r_syn_comp_var_int (           {create expression comparing counter to limit}
      var_p^.mod1.top_sym_p^,          {the variable to compare value of}
      occ2,                            {integer to compare it with}
      sst_op2_ge_k,                    {comparison operator}
      exp_p);                          {returned pointer to the comparison expression}

    sst_opcode_new;                    {create new opcode for IF}
    sst_opc_p^.opcode := sst_opc_if_k;
    sst_opc_p^.if_exp_p := exp_p;      {conditional expression}
    sst_opc_p^.if_false_p := nil;      {there is no FALSE case code}

    sst_opcode_pos_push (sst_opc_p^.if_true_p); {set up for writing TRUE code}
    sst_r_syn_jtarg_sym (              {get label symbol for TRUE case}
      jtarg.yes,                       {jump target for TRUE case}
      sym_p);                          {returned pointer to TRUE case label}
    sst_r_syn_jtarg_label_goto (sym_p^); {go to syntax matched location}
    sst_opcode_pos_pop;                {done writing TRUE case opcodes}
    end;                               {end of check for upper limit case}
{
*   Back to try another iteration.
}
  sst_r_syn_jtarg_label_goto (lab_loop_p^); {jump back to start of loop}
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

done_item:                             {done processing the item}
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
