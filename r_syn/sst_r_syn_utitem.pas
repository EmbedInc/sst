{   Subroutine SST_R_SYN_UTITEM (TARG, SYM_MFLAG)
*
*   Process UNTAGGED_ITEM syntax.
}
module sst_r_syn_utitem;
define sst_r_syn_utitem;
%include 'sst_r_syn.ins.pas';

procedure sst_r_syn_utitem (           {process UTITEM syntax}
  in out  jtarg: jump_targets_t;       {execution block jump targets info}
  in      sym_mflag: sst_symbol_t);    {desc of parent MFLAG variable symbol}
  val_param;

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  tag: sys_int_machine_t;              {tag from syntax tree}
  str_h: syo_string_t;                 {handle to string from input file}
  sym_p, sym2_p: sst_symbol_p_t;       {scratch pointers to symbol descriptors}
  name_p: string_var_p_t;              {pointer to name in hash table entry}
  data_p: symbol_data_p_t;             {points to data in hash table entry}
  jt, jt2: jump_targets_t;             {scratch jump targets for lower routines}
  token: string_var8192_t;             {scratch token or string}
  i, j, k: sys_int_machine_t;          {scratch integers}
  var_p: sst_var_p_t;                  {scratch pointer to var descriptor}
  exp_p, exp2_p, exp3_p: sst_exp_p_t;  {scratch pointers to exp descriptors}
  term_p: sst_exp_term_p_t;            {scratch pointer to term in expression}
  call_p: call_p_t;                    {pointer to data about called syntax}
  call_pp: ^call_p_t;                  {points to end of called syntaxes chain pnt}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;
  stat: sys_err_t;

label
  comp_char_sym2, in_list, done_occurs, always_match, optional;

begin
  token.max := sizeof(token.str);      {init local var string}
  syo_level_down;                      {down into UNTAGGED_ITEM syntax}

  syo_get_tag_msg (                    {get tag to identify type of item}
    tag, str_h, 'sst_syn_read', 'syerr_define', nil, 0);
  case tag of
{
**************************************
*
*   Item is .EOL
}
1: begin
  sym2_p := sym_ichar_eol_p;           {symbol to compare character value to}

comp_char_sym2:                        {compare char value to SYM2_P^}
  sst_call (sym_cpos_push_p^);         {write call to SYO_P_CPOS_PUSH}

  sst_call (sym_get_ichar_p^);         {write call to SYO_P_GET_ICHAR}
  sst_r_syn_int (sym_p);               {make temporary integer variable}
  sst_call_arg_var (sst_opc_p^, sym_p^); {add integer variable call argument}

  sst_r_syn_set_mflag (                {set MFLAG based on comparison}
    sym_p^,                            {first term of comparison}
    sym2_p^,                           {second term of comparison}
    sst_op2_eq_k,                      {comparison operator}
    true,                              {indicate to take error flag into account}
    sym_mflag);                        {handle to MFLAG variable to set}
{
*   Pop parsing state saved earlier.
}
  sst_call (sym_cpos_pop_p^);          {write call to SYO_P_CPOS_POP}
  sst_call_arg_var (sst_opc_p^, sym_mflag); {add MFLAG variable as call argument}
{
*   Handle the jump targets.
}
  sst_r_syn_goto (                     {goto, as needed, to jump targets}
    jtarg,                             {jump targets data}
    [jtarg_yes_k, jtarg_no_k, jtarg_err_k], {which targets to process}
    sym_mflag);                        {handle to MFLAG variable}
  end;
{
**************************************
*
*   Item is .EOF
}
2: begin
  sym2_p := sym_ichar_eof_p;           {symbol to compare character value to}
  goto comp_char_sym2;                 {to common code}
  end;
{
**************************************
*
*   Item is symbol reference
}
3: begin
  syo_get_tag_string (str_h, token);   {get name of SYN file symbol}
  string_upcase (token);
  string_hash_ent_lookup (table_sym, token, name_p, data_p); {look up name in table}
  if data_p = nil then begin           {no such entry in table ?}
    sys_msg_parm_vstr (msg_parm[1], token);
    syo_error (str_h, 'sst_syn_read', 'symbol_not_declared', msg_parm, 1);
    end;
{
*   Add called syntax to list of syntaxes called by this parent, if not
*   already in list.
}
  call_pp := addr(def_syo_p^.call_p);  {init end of called syntaxes chain pointer}
  call_p := call_pp^;                  {init to first called syntax in chain}
  while call_p <> nil do begin         {loop thru called syntaxes chain}
    if call_p^.data_p = data_p then begin {this called syntax already in list ?}
      goto in_list;
      end;
    call_pp := addr(call_p^.next_p);   {update pointer to end of chain}
    call_p := call_pp^;                {advance to next called syntax entry}
    end;                               {back to process next called syntax entry}
  string_hash_mem_alloc_ndel (         {allocate memory for new called syntax entry}
    table_sym, sizeof(call_p^), call_p);
  call_pp^ := call_p;                  {add new entry to end of chain}
  call_p^.next_p := nil;               {this is now new end of chain}
  call_p^.data_p := data_p;            {save pointer to data about called syntax}
in_list:                               {jump here if called syntax already in list}

  sst_call (data_p^.sym_p^);           {create call to syntax subroutine}
  sst_call_arg_var (sst_opc_p^, sym_mflag); {pass MFLAG variable as call argument}

  sst_r_syn_goto (                     {go to jump targets, as required}
    jtarg,                             {jump targets data}
    [jtarg_yes_k, jtarg_no_k, jtarg_err_k], {which targets to process}
    sym_mflag);                        {handle to MFLAG variable}
  end;
{
**************************************
*
*   Item is string constant
}
4: begin
  syo_level_down;                      {down into STRING syntax}
  syo_get_tag_msg (                    {get tag for string contents}
    tag, str_h, 'sst_syn_read', 'syerr_string', nil, 0);
  syo_get_tag_string (str_h, token);   {get string value}
  syo_level_up;                        {back up from STRING syntax}

  sst_call (sym_test_string_p^);       {create call to SYM_P_TEST_STRING}
  sst_call_arg_var (sst_opc_p^, sym_mflag); {MFLAG variable is argument 1}
  sst_call_arg_str (                   {string characters are argument 2}
    sst_opc_p^,                        {opcode to add call argument to}
    token.str,                         {string value}
    token.len);                        {number of characters in string}
  sst_call_arg_int (sst_opc_p^, token.len); {number of characters is argument 3}

  sst_r_syn_goto (                     {go to jump targets, as required}
    jtarg,                             {jump targets data}
    [jtarg_yes_k, jtarg_no_k, jtarg_err_k], {which targets to process}
    sym_mflag);                        {handle to MFLAG variable}
  end;
{
**************************************
*
*   Item is .RANGE
}
5: begin
  syo_get_tag_msg (                    {get tag for range start character}
    tag, str_h, 'sst_syn_read', 'syerr_string', nil, 0);
  if tag <> 1 then begin
    syo_error_tag_unexp (tag, str_h);
    end;
  syo_get_tag_string (str_h, token);   {get string value}
  i := ord(token.str[2]) & 127;        {get character value for start of range}

  syo_get_tag_msg (                    {get tag for range start character}
    tag, str_h, 'sst_syn_read', 'syerr_string', nil, 0);
  if tag <> 1 then begin
    syo_error_tag_unexp (tag, str_h);
    end;
  syo_get_tag_string (str_h, token);   {get string value}
  j := ord(token.str[2]) & 127;        {get character value for start of range}
{
*   I contains the 0-127 character value for the start of the range, and
*   J contains the 0-127 character value for the end of the range.
}
  sst_call (sym_cpos_push_p^);         {write call to SYO_P_CPOS_PUSH}

  sst_call (sym_get_ichar_p^);         {write call to SYO_P_GET_ICHAR}
  sst_r_syn_int (sym_p);               {make temporary integer variable}
  sst_call_arg_var (sst_opc_p^, sym_p^); {add integer variable call argument}
{
*   Code has been written so that the integer variable at SYM_P contains
*   the character code of the character to check the range of.  The MFLAG
*   variable will be set as the result of a comparison.  Our local pointers
*   to handle the comparison expression will be:
*
*   EXP_P   -  Pointer to top level expression.  First term will be the
*              range check, second term will be ERROR test.
*   EXP2_P  -  Pointer to first term of EXP_P expression.  First term
*              will be low end of range check, second term high end of
*              range check.
*   EXP3_P  -  Pointers to nested expression for each term in range check.
*   TERM_P  -  Temporarily used to point to second terms of various
*              expressions.
*   VAR_P   -  Var descriptor referencing ERROR variable in common block.
}
  sst_mem_alloc_scope (sizeof(exp_p^), exp_p); {alloc mem for top expression desc}
  sst_mem_alloc_scope (sizeof(exp2_p^), exp2_p); {alloc mem for range check exp desc}
  sst_r_syn_comp_var_int (             {make exp comparing var to integer value}
    sym_p^,                            {variable for first term of comparison}
    i,                                 {integer value for second term}
    sst_op2_ge_k,                      {comparison operator}
    exp3_p);                           {returned pointer to new expression}

  sst_opcode_new;                      {create comparison opcode}
  sst_opc_p^.opcode := sst_opc_if_k;
  sst_opc_p^.if_exp_p := exp_p;        {set pointer to top level decision expression}

  sst_mem_alloc_scope (sizeof(var_p^), var_p); {alloc mem for var desc to ERROR}
  sst_mem_alloc_scope (sizeof(term_p^), term_p); {alloc mem for second EXP_P term}

  var_p^.dtype_p := sym_error_p^.var_dtype_p; {fill in var desc referencing ERROR}
  var_p^.rwflag := [sst_rwflag_read_k, sst_rwflag_write_k];
  var_p^.vtype := sst_vtype_var_k;
  var_p^.mod1.next_p := nil;
  var_p^.mod1.modtyp := sst_var_modtyp_top_k;
  var_p^.mod1.top_str_h := sst_opc_p^.str_h;
  var_p^.mod1.top_sym_p := sym_error_p;

  exp_p^.str_h := sst_opc_p^.str_h;    {top level expression descriptor}
  exp_p^.dtype_p := nil;
  exp_p^.dtype_hard := false;
  exp_p^.val_eval := false;
  exp_p^.val_fnd := false;
  exp_p^.rwflag := [sst_rwflag_read_k];

  exp_p^.term1.next_p := term_p;       {first term of top level expression}
  exp_p^.term1.op2 := sst_op2_none_k;
  exp_p^.term1.op1 := sst_op1_none_k;
  exp_p^.term1.ttype := sst_term_exp_k;
  exp_p^.term1.str_h := sst_opc_p^.str_h;
  exp_p^.term1.dtype_p := nil;
  exp_p^.term1.dtype_hard := false;
  exp_p^.term1.val_eval := false;
  exp_p^.term1.val_fnd := false;
  exp_p^.term1.rwflag := [sst_rwflag_read_k];
  exp_p^.term1.exp_exp_p := exp2_p;

  term_p^.next_p := nil;               {second term of top level expression}
  term_p^.op2 := sst_op2_or_k;
  term_p^.op1 := sst_op1_none_k;
  term_p^.ttype := sst_term_var_k;
  term_p^.str_h := sst_opc_p^.str_h;
  term_p^.dtype_p := nil;
  term_p^.dtype_hard := true;
  term_p^.val_eval := false;
  term_p^.val_fnd := false;
  term_p^.rwflag := [sst_rwflag_read_k, sst_rwflag_write_k];
  term_p^.var_var_p := var_p;

  sst_mem_alloc_scope (sizeof(term_p^), term_p); {alloc mem for second EXP2_P term}

  exp2_p^.str_h := sst_opc_p^.str_h;   {range check expression descriptor}
  exp2_p^.dtype_p := nil;
  exp2_p^.dtype_hard := false;
  exp2_p^.val_eval := false;
  exp2_p^.val_fnd := false;
  exp2_p^.rwflag := [sst_rwflag_read_k];

  exp2_p^.term1.next_p := term_p;      {low end of range check term}
  exp2_p^.term1.op2 := sst_op2_none_k;
  exp2_p^.term1.op1 := sst_op1_none_k;
  exp2_p^.term1.ttype := sst_term_exp_k;
  exp2_p^.term1.str_h := sst_opc_p^.str_h;
  exp2_p^.term1.dtype_p := nil;
  exp2_p^.term1.dtype_hard := false;
  exp2_p^.term1.val_eval := false;
  exp2_p^.term1.val_fnd := false;
  exp2_p^.term1.rwflag := [sst_rwflag_read_k];
  exp2_p^.term1.exp_exp_p := exp3_p;

  sst_r_syn_comp_var_int (             {make exp comparing var to integer value}
    sym_p^,                            {variable for first term of comparison}
    j,                                 {integer value for second term}
    sst_op2_le_k,                      {comparison operator}
    exp3_p);                           {returned pointer to new expression}

  term_p^.next_p := nil;               {high end of range check term}
  term_p^.op2 := sst_op2_and_k;
  term_p^.op1 := sst_op1_none_k;
  term_p^.ttype := sst_term_exp_k;
  term_p^.str_h := sst_opc_p^.str_h;
  term_p^.dtype_p := nil;
  term_p^.dtype_hard := false;
  term_p^.val_eval := false;
  term_p^.val_fnd := false;
  term_p^.rwflag := [sst_rwflag_read_k];
  term_p^.exp_exp_p := exp3_p;

  sst_exp_eval (exp_p^, false);        {fully evaluate top level expression}
{
*   Write code for YES case.
}
  sst_opcode_pos_push (sst_opc_p^.if_true_p); {set up for writing TRUE code}
  sst_r_syn_opc_assign (sym_mflag, sym_mflag_yes_p^);
  sst_opcode_pos_pop;                  {pop back to parent opcode chain}
{
*   Write code for NO case.
}
  sst_opcode_pos_push (sst_opc_p^.if_false_p); {set up for writing FALSE code}
  sst_r_syn_opc_assign (sym_mflag, sym_mflag_no_p^);
  sst_opcode_pos_pop;                  {pop back to parent opcode chain}
{
*   Pop parsing state saved earlier.
}
  sst_call (sym_cpos_pop_p^);          {write call to SYO_P_CPOS_POP}
  sst_call_arg_var (sst_opc_p^, sym_mflag); {add MFLAG variable as call argument}
{
*   Handle the jump targets.
}
  sst_r_syn_goto (                     {goto, as needed, to jump targets}
    jtarg,                             {jump targets data}
    [jtarg_yes_k, jtarg_no_k, jtarg_err_k], {which targets to process}
    sym_mflag);                        {handle to MFLAG variable}
  end;
{
**************************************
*
*   Item is .OCCURS
}
6: begin
  syo_get_tag_msg (                    {get tag for min occurrence number}
    tag, str_h, 'sst_syn_read', 'syerr_string', nil, 0);
  if tag <> 1 then begin
    syo_error_tag_unexp (tag, str_h);
    end;
  syo_get_tag_string (str_h, token);   {get string value}
  string_t_int (token, i, stat);       {get min occurrence number}
  syo_error_abort (stat, str_h, '', '', nil, 0);
  if i < 0 then begin                  {negative min occurrence limit not allowed}
    syo_error (str_h, 'sst_syn_read', 'occurs_limit_low_bad', nil, 0);
    end;

  syo_level_down;                      {down into END_RANGE syntax}
  syo_get_tag_msg (                    {get tag for min occurrence number}
    tag, str_h, 'sst_syn_read', 'syerr_string', nil, 0);
  case tag of
1:  begin                              {tag was for max occurrence integer value}
      syo_get_tag_string (str_h, token); {get string value}
      string_t_int (token, j, stat);   {get max occurrence number}
      syo_error_abort (stat, str_h, '', '', nil, 0);
      if j < i then begin
        syo_error (str_h, 'sst_syn_read', 'occurs_limit_high_below', nil, 0);
        end;
      end;
2:  begin                              {tag was for infinite occurrences}
      j := -1;                         {flag that infinite occurrences allowed}
      end;
otherwise
    syo_error_tag_unexp (tag, str_h);
    end;
  syo_level_up;                        {back up from END_RANGE syntax}
{
*   I is set to the minimum allowable occurrence count and J is the maximum
*   allowable occurrence count.  J is negative to indicate that no upper
*   occurrence limit is being imposed.  Some special cases will be handled
*   separately.  K will be set to indicate which combinations of upper/lower
*   limits need to be checked.  K will have the following values:
*
*     0 - No limits need checking
*     1 - Lower limit needs checking
*     2 - Upper limit needs checking
*     3 - Both limits need checking
}
  k := 0;                              {init limit checks flag}
  if i > 0 then k := k + 1;            {lower limit needs checking ?}
  if j >= 0 then k := k + 2;           {upper limit needs checking ?}
{
*   Handle special case where upper and lower limits are both 1.  This means
*   there is really nothing to do.
}
  if (i = 1) and (j = 1) then begin    {OCCURS is really a NOP ?}
    sst_r_syn_item (jtarg, sym_mflag); {process subject item directly}
    goto done_occurs;
    end;
{
*   Handle special case where item is allowed 0 or one times.  This
*   is the same logic as handled by the OPTIONAL item.
}
  if (i = 0) and (j = 1) then begin    {item allowed 0 or 1 times ?}
    goto optional;
    end;
{
*******************
*
*   The min and max limits don't match any special case.  Handle in general
*   way by counting the number of occurrences, then checking against the
*   limits at the end.
}
  sst_r_syn_int (sym_p);               {get handle to occurrence counting variable}
{
*   Make opcode to initialize the occurrence counter variable to 0.
}
  if k <> 0 then begin                 {some occurrence limit needs checking ?}
    sst_opcode_new;                    {create initial assignment opcode}
    sst_opc_p^.opcode := sst_opc_assign_k;
    sst_mem_alloc_scope (sizeof(var_p^), var_p); {grab mem for assignment var desc}
    sst_opc_p^.assign_var_p := var_p;  {point opcode to assignment var desc}
    sst_exp_const_int (-1, sst_opc_p^.assign_exp_p); {assignment value is -1}

    var_p^.mod1.next_p := nil;
    var_p^.mod1.modtyp := sst_var_modtyp_top_k;
    var_p^.mod1.top_str_h := sst_opc_p^.str_h;
    var_p^.mod1.top_sym_p := sym_p;
    var_p^.dtype_p := sym_p^.var_dtype_p;
    var_p^.rwflag := [sst_rwflag_read_k, sst_rwflag_write_k];
    var_p^.vtype := sst_vtype_var_k;
    end;
{
*   Insert call to SYO_P_CPOS_PUSH if there is any chance the final
*   syntax matched answer might be NO.
}
  if k <> 0 then begin                 {some limit will have to be checked ?}
    sst_call (sym_cpos_push_p^);       {create call to SYO_P_CPOS_PUSH}
    end;
{
*   Make opcode for counted loop with decision at the bottom.  We will
*   exit the loop after the first time the item fails.
}
  sst_opcode_new;                      {make loop with decision at bottom opcode}
  sst_opc_p^.opcode := sst_opc_loop_tbot_k;
  sst_r_syn_comp_var_sym (             {create loop continuation expression}
    sym_mflag,                         {variable for first term in expression}
    sym_mflag_no_p^,                   {symbol to compare variable to}
    sst_op2_eq_k,                      {comparison operator}
    sst_opc_p^.lpbt_exp_p);            {returned pointer to expression descriptor}
{
*   Write opcodes for loop body.
}
  sst_opcode_pos_push (sst_opc_p^.lpbt_code_p); {set up for writing loop body code}
{
*   Increment the occurrence counter.  This was initialized to -1, so it will
*   be the number of successful occurrences upon loop exit.
}
  if k <> 0 then begin                 {some occurrence limit needs checking ?}
    sst_opcode_new;                    {create counter increment opcode}
    sst_opc_p^.opcode := sst_opc_assign_k;
    sst_opc_p^.assign_var_p := var_p;  {point opcode to assignment var desc}
    sst_mem_alloc_scope (sizeof(exp_p^), exp_p); {alloc mem for expression desc}
    sst_opc_p^.assign_exp_p := exp_p;  {point opcode to assignment value expression}

    sst_mem_alloc_scope (sizeof(term_p^), term_p); {get mem for second term}

    exp_p^.term1.next_p := term_p;
    exp_p^.term1.op2 := sst_op2_none_k;
    exp_p^.term1.op1 := sst_op1_none_k;
    exp_p^.term1.ttype := sst_term_var_k;
    exp_p^.term1.str_h := sst_opc_p^.str_h;
    exp_p^.term1.dtype_p := var_p^.dtype_p;
    exp_p^.term1.dtype_hard := true;
    exp_p^.term1.val_eval := false;
    exp_p^.term1.val_fnd := false;
    exp_p^.term1.rwflag := var_p^.rwflag;
    exp_p^.term1.var_var_p := var_p;

    exp_p^.str_h := sst_opc_p^.str_h;
    exp_p^.dtype_p := exp_p^.term1.dtype_p;
    exp_p^.dtype_hard := false;
    exp_p^.val_eval := false;
    exp_p^.val_fnd := false;
    exp_p^.rwflag := [sst_rwflag_read_k];

    term_p^.next_p := nil;
    term_p^.op2 := sst_op2_add_k;
    term_p^.op1 := sst_op1_none_k;
    term_p^.str_h := sst_opc_p^.str_h;
    term_p^.val_eval := false;
    term_p^.val_fnd := false;
    term_p^.rwflag := [sst_rwflag_read_k];
    term_p^.ttype := sst_term_const_k;
    term_p^.dtype_p := sym_int_machine_t_p^.dtype_dtype_p;
    term_p^.dtype_hard := false;
    term_p^.val_eval := true;
    term_p^.val_fnd := true;
    term_p^.val.dtype := sst_dtype_int_k;
    term_p^.val.int_val := 1;

    sst_exp_eval (exp_p^, false);
    end;
{
*   Write code to process the item.
*
*   An additional layer of jump targets is used if we must always
*   do a SYO_P_CPOS_POP before passing back to the parent.
}
  if k = 0
    then begin                         {OK to go directly to parent on error}
      sst_r_syn_jtargets_make (        {create jump targets for nested item}
        jtarg,                         {template jump targets}
        jt,                            {output jump targets}
        lab_fall_k,                    {fall thru on YES}
        lab_fall_k,                    {fall thru on NO}
        lab_same_k);                   {go to parent target on ERROR}
      end
    else begin                         {must always do call to SYO_P_CPOS_POP}
      sst_r_syn_jtargets_make (        {create private jump targets layer}
        jtarg,                         {template jump targets}
        jt2,                           {output jump targets}
        lab_fall_k,                    {fall thru on YES}
        lab_fall_k,                    {fall thru on NO}
        lab_fall_k);                   {fall thru on ERROR}
      sst_r_syn_jtargets_make (        {create nested jump targets for ITEM}
        jt2,                           {template jump targets}
        jt,                            {output jump targets}
        lab_fall_k,                    {fall thru on YES}
        lab_fall_k,                    {fall thru on NO}
        lab_same_k);                   {go to parent target on ERROR}
      end
    ;
  sst_r_syn_item (jt, sym_mflag);      {process nested item syntax}
  sst_r_syn_jtargets_done (jt);        {tag implicit labels as being here}
{
****************
*
*   All done with loop body code.
}
  sst_opcode_pos_pop;                  {pop opcode position back to after loop}
  case k of
{
****************
*
*   Neither upper nor lower loop limit need checking.
}
0: begin
  goto always_match;
  end;
{
****************
*
*   Only lower limit needs to be checked.
}
1: begin
  sst_mem_alloc_scope (sizeof(exp_p^), exp_p);
  sst_mem_alloc_scope (sizeof(term_p^), term_p);

  sst_opcode_new;                      {create opcode for IF}
  sst_opc_p^.opcode := sst_opc_if_k;
  sst_r_syn_comp_var_int (             {make exp to compare var with int value}
    sym_p^,                            {variable for first term in compare}
    i,                                 {value for second term of compare}
    sst_op2_ge_k,                      {comparison operator}
    sst_opc_p^.if_exp_p);              {returned pointer to decision expression}
  end;
{
****************
*
*   Only upper limit needs to be checked.
}
2: begin
  sst_mem_alloc_scope (sizeof(exp_p^), exp_p);
  sst_mem_alloc_scope (sizeof(term_p^), term_p);

  sst_opcode_new;                      {create opcode for IF}
  sst_opc_p^.opcode := sst_opc_if_k;
  sst_r_syn_comp_var_int (             {make exp to compare var with int value}
    sym_p^,                            {variable for first term in compare}
    j,                                 {value for second term of compare}
    sst_op2_le_k,                      {comparison operator}
    sst_opc_p^.if_exp_p);              {returned pointer to decision expression}
  end;
{
****************
*
*   Both upper and lower limit need to be checked.
}
3: begin
  sst_opcode_new;                      {create opcode for IF}
  sst_opc_p^.opcode := sst_opc_if_k;
  sst_mem_alloc_scope (sizeof(exp_p^), exp_p);
  sst_opc_p^.if_exp_p := exp_p;        {point to boolean expression for decision}

  sst_r_syn_comp_var_int (             {make compare exp for lower limit}
    sym_p^,                            {variable for first term in compare}
    i,                                 {value for second term of compare}
    sst_op2_ge_k,                      {comparison operator}
    exp2_p);                           {returned pointer to decision expression}

  sst_r_syn_comp_var_int (             {make compare exp for upper limit}
    sym_p^,                            {variable for first term in compare}
    j,                                 {value for second term of compare}
    sst_op2_le_k,                      {comparison operator}
    exp3_p);                           {returned pointer to decision expression}

  sst_mem_alloc_scope (sizeof(term_p^), term_p); {get mem for second term}

  exp_p^.term1.next_p := term_p;       {fill in first term in top expression}
  exp_p^.term1.op2 := sst_op2_none_k;
  exp_p^.term1.op1 := sst_op1_none_k;
  exp_p^.term1.ttype := sst_term_exp_k;
  exp_p^.term1.str_h := sst_opc_p^.str_h;
  exp_p^.term1.dtype_p := nil;
  exp_p^.term1.dtype_hard := false;
  exp_p^.term1.val_eval := false;
  exp_p^.term1.val_fnd := false;
  exp_p^.term1.rwflag := [sst_rwflag_read_k];
  exp_p^.term1.exp_exp_p := exp2_p;

  term_p^.next_p := nil;               {fill in second term in top expression}
  term_p^.op2 := sst_op2_and_k;        {operator between the two terms}
  term_p^.op1 := sst_op1_none_k;
  term_p^.ttype := sst_term_exp_k;
  term_p^.str_h := sst_opc_p^.str_h;
  term_p^.dtype_p := nil;
  term_p^.dtype_hard := false;
  term_p^.val_eval := false;
  term_p^.val_fnd := false;
  term_p^.rwflag := [sst_rwflag_read_k];
  term_p^.exp_exp_p := exp3_p;

  exp_p^.str_h := sst_opc_p^.str_h;    {fill in top expression descriptor}
  exp_p^.dtype_p := nil;
  exp_p^.dtype_hard := false;
  exp_p^.val_eval := false;
  exp_p^.val_fnd := false;
  exp_p^.rwflag := [sst_rwflag_read_k];

  sst_exp_eval (exp_p^, false);        {evaluate whole expression}
  end;

    end;                               {end of limits check cases}
{
*   Write code for YES case.
}
  sst_opcode_pos_push (sst_opc_p^.if_true_p); {set up for writing TRUE code}
  sst_r_syn_opc_assign (sym_mflag, sym_mflag_yes_p^);
  sst_opcode_pos_pop;                  {pop back to parent opcode chain}
{
*   Write code for NO case.
}
  sst_opcode_pos_push (sst_opc_p^.if_false_p); {set up for writing FALSE code}
  sst_r_syn_opc_assign (sym_mflag, sym_mflag_no_p^);
  sst_opcode_pos_pop;                  {pop back to parent opcode chain}

  sst_r_syn_jtargets_done (jt2);       {ITEM ERROR case comes here}

  sst_call (sym_cpos_pop_p^);          {create call to SYO_P_CPOS_POP}
  sst_call_arg_var (sst_opc_p^, sym_mflag); {add MFLAG variable as call argument}

  sst_r_syn_goto (                     {go to jump targets, as required}
    jtarg,                             {jump targets data}
    [jtarg_yes_k, jtarg_no_k, jtarg_err_k], {which targets to process}
    sym_mflag);                        {handle to MFLAG variable}

done_occurs:                           {all done processing OCCURS item}
  end;
{
**************************************
*
*   Item is nested expression in parenthesis.
}
7: begin
  sst_call (sym_cpos_push_p^);         {save parse state before expression}

  sst_r_syn_jtargets_make (            {make new jump targets for nested ITEM}
    jtarg, jt,                         {input and output jump targets}
    lab_fall_k, lab_fall_k, lab_fall_k); {fall thru in all cases}
  sst_r_syn_expression (jt, sym_mflag); {process nested expression}
  sst_r_syn_jtargets_done (jt);        {tag implicit labels as being here}

  sst_call (sym_cpos_pop_p^);          {conditionally restore parse state after exp}
  sst_call_arg_var (sst_opc_p^, sym_mflag); {add MFLAG variable as call argument}

  sst_r_syn_goto (                     {go to jump targets, as required}
    jtarg,                             {jump targets data}
    [jtarg_yes_k, jtarg_no_k, jtarg_err_k], {which targets to process}
    sym_mflag);                        {handle to MFLAG variable}
  end;
{
**************************************
*
*   Item is .CHARCASE
}
8: begin
  syo_get_tag_msg (                    {get tag for character case select}
    tag, str_h, 'sst_syn_read', 'syerr_string', nil, 0);
  case tag of
1:  begin                              {upper case}
      sym_p := sym_charcase_up_p;
      end;
2:  begin                              {lower case}
      sym_p := sym_charcase_down_p;
      end;
3:  begin                              {off}
      sym_p := sym_charcase_asis_p;
      end;
    end;                               {end of character case cases}

  sst_call (sym_charcase_p^);          {create call to SYO_P_CHARCASE}
  sst_call_arg_enum (sst_opc_p^, sym_p^); {pass new character case as argument 1}

  goto always_match;                   {make sure MFLAG is set to YES}
  end;
{
**************************************
*
*   Item is .NULL
}
9: begin
always_match:                          {jump here to set MFLAG to YES}
  sst_r_syn_opc_assign (sym_mflag, sym_mflag_yes_p^); {set MFLAG to YES}

  if                                   {need to jump somewhere ?}
      (not (jflag_fall_k in jtarg.yes.flags)) or {not fall thru ?}
      (jflag_indir_k in jtarg.yes.flags) {indirect reference ?}
      then begin
    sst_opcode_new;                    {create GOTO opcode}
    sst_opc_p^.opcode := sst_opc_goto_k;
    sst_opc_p^.str_h.first_char.crange_p := nil;
    sst_opc_p^.str_h.first_char.ofs := 0;
    sst_opc_p^.str_h.last_char := sst_opc_p^.str_h.first_char;
    sst_r_syn_jtarget_sym (            {get or make jump target symbol}
      jtarg.yes,                       {jump target descriptor}
      sst_opc_p^.goto_sym_p);          {returned pointer to label symbol}
    end;
  end;
{
**************************************
*
*   Item is .UPTO
}
10: begin
  sst_call (sym_cpos_push_p^);         {create call to SYO_P_CPOS_PUSH}

  sst_r_syn_jtargets_make (            {make new jump targets for nested ITEM}
    jtarg, jt,                         {input and output jump targets}
    lab_fall_k, lab_fall_k, lab_fall_k); {fall thru in all cases}
  sst_r_syn_item (jt, sym_mflag);      {process nested ITEM}
  sst_r_syn_jtargets_done (jt);        {tag implicit labels as being here}

  sst_call (sym_cpos_pop_p^);          {create call to SYO_P_CPOS_POP}
  sst_call_arg_enum (sst_opc_p^, sym_mflag_no_p^); {always restores state}

  sst_r_syn_goto (                     {go to jump targets, as required}
    jtarg,                             {jump targets data}
    [jtarg_yes_k, jtarg_no_k, jtarg_err_k], {which targets to process}
    sym_mflag);                        {handle to MFLAG variable}
  end;
{
**************************************
*
*   Item is .NOT
}
11: begin
  sst_r_syn_jtargets_make (            {make new jump targets for nested ITEM}
    jtarg, jt,                         {input and output jump targets}
    lab_fall_k, lab_fall_k, lab_fall_k); {fall thru in all cases}
  sst_r_syn_item (jt, sym_mflag);      {process nested ITEM}
  sst_r_syn_jtargets_done (jt);        {tag implicit labels as being here}

  sst_r_syn_set_mflag (                {write code to flip sense of MFLAG}
    sym_mflag,                         {variable for first term in comparison}
    sym_mflag_yes_p^,                  {value to compare against}
    sst_op2_ne_k,                      {comparision operator}
    true,                              {always set to YES on error condition}
    sym_mflag);                        {handle to MFLAG variable to set}

  sst_r_syn_goto (                     {go to jump targets, as required}
    jtarg,                             {jump targets data}
    [jtarg_yes_k, jtarg_no_k, jtarg_err_k], {which targets to process}
    sym_mflag);                        {handle to MFLAG variable}
  end;
{
**************************************
*
*   Item is .EOD
}
12: begin
  sym2_p := sym_ichar_eod_p;           {symbol to compare character value to}
  goto comp_char_sym2;                 {to common code}
  end;
{
**************************************
*
*   Item is .OPTIONAL
}
13: begin
optional:                              {jump here from .OCCURS [0 TO 1]}
  sst_r_syn_jtargets_make (            {make new jump targets for nested ITEM}
    jtarg, jt,                         {input and output jump targets}
    lab_fall_k, lab_fall_k, lab_fall_k); {fall thru in all cases}
  sst_r_syn_item (jt, sym_mflag);      {process nested ITEM}
  sst_r_syn_jtargets_done (jt);        {tag implicit labels as being here}
  goto always_match;                   {.OPTIONAL always returns YES}
  end;
{
**************************************
*
*   Unexpected expression format tag value.
}
otherwise
    syo_error_tag_unexp (tag, str_h);
    end;                               {end of item format cases}

  syo_level_up;                        {back up from ITEM syntax}
  end;
