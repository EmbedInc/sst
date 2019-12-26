{   Subroutine SST_R_PAS_RAW_SMENT
*
*   Create new opcodes from the RAW_STATEMENT syntax.  All opcodes will be of
*   "executable" type.
}
module sst_r_pas_RAW_SMENT;
define sst_r_pas_raw_sment;
%include 'sst_r_pas.ins.pas';

procedure sst_r_pas_raw_sment;         {build opcodes from RAW_STATEMENT syntax}

const
  max_msg_parms = 3;                   {max parameters we can pass to a message}

var
  tag: sys_int_machine_t;              {syntax tag ID}
  str_h: syn_string_t;                 {handle to string associated with TAG}
  tag2: sys_int_machine_t;             {extra syntax tag to avoid corrupting TAG}
  str2_h: syn_string_t;                {handle to string associated with TAG2}
  var_p: sst_var_p_t;                  {points to variable descriptor}
  sym_p: sst_symbol_p_t;               {pointer to scratch symbol descriptor}
  sym_prev_p: sst_symbol_p_t;          {points to previous symbol in linked list}
  args_here: boolean;                  {TRUE if subroutine has call arguments}
  s: string_var80_t;                   {scratch var string for error messages, etc}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;
  stat: sys_err_t;                     {completion status code}

label
  tag_loop, loop_tag6, loop_tag8, do_call, leave;
{
*************************************************************************
*
*   Local subroutine GET_LABEL_SYM_PNT (STR_H,SYM_P)
*
*   STR_H is the SYN string handle for a label name.  SYM_P will be returned
*   pointing to the symbol descriptor for the label.  Errors will be checked.
}
procedure get_label_sym_pnt (
  in      str_h: syn_string_t;         {string handle to lable name}
  out     sym_p: sst_symbol_p_t);      {returned pointing to symbol descriptor}

var
  fnam: string_treename_t;             {file name passed to a message}
  lnum: sys_int_machine_t;             {line number passed to a message}

begin
  fnam.max := sizeof(fnam.str);

  sst_symbol_lookup (str_h, sym_p, stat); {get pointer to label symbol}
  syn_error_abort (stat, str_h, '', '', nil, 0);
  if sym_p^.symtype <> sst_symtype_label_k then begin {symbol not a label ?}
    sst_charh_info (sym_p^.char_h, fnam, lnum);
    sys_msg_parm_vstr (msg_parm[1], sym_p^.name_in_p^);
    sys_msg_parm_int (msg_parm[2], lnum);
    sys_msg_parm_vstr (msg_parm[3], fnam);
    syn_error (str_h, 'sst_pas_read', 'symbol_not_label', msg_parm, 3);
    end;
  end;
{
*************************************************************************
*
*   Start of main routine.
}
begin
  s.max := sizeof(s.str);              {init local var string}
  syn_level_down;                      {down into RAW_STATMENT syntax}

tag_loop:                              {back here each new top level synax tag}
  syn_get_tag_msg (tag, str_h, 'sst_pas_read', 'statement_exec_bad', nil, 0);
  case tag of
{
***************************
*
*   No more tags in this RAW_STATEMENT syntax.
}
syn_tag_end_k: begin
  goto leave;                          {go to normal exit code}
  end;
{
***************************
*
*   Tag indicates a null statement.  This happens in empty BEGIN ... END blocks,
*   and consecutive ";".  This is harmless and ignored.
}
1: begin
  end;
{
***************************
*
*   Tag is label name of a GOTO statement.
}
2: begin
  sst_opcode_new;                      {make new opcode descriptor}
  sst_opc_p^.opcode := sst_opc_goto_k; {set opcode type}
  sst_opc_p^.str_h := str_h;           {save handle to source characters}
  get_label_sym_pnt (str_h, sst_opc_p^.label_sym_p); {get pointer to label symbol}
  end;
{
***************************
*
*   Tag is the conditional logical expression of an IF statement.
}
3: begin
  sst_opcode_new;                      {make new opcode descriptor}
  sst_opc_p^.opcode := sst_opc_if_k;   {set opcode type}
  sst_opc_p^.str_h := str_h;           {save handle to source characters}
  sst_r_pas_exp (str_h, false, sst_opc_p^.if_exp_p); {process conditional expression}
  sst_exp_useage_check (               {check expression attributes for this useage}
    sst_opc_p^.if_exp_p^,              {expression to check}
    [sst_rwflag_read_k],               {read/write access needed to expression value}
    sst_dtype_bool_p^);                {data type value must be compatible with}
  syn_get_tag_msg (tag, str_h, 'sst_pas_read', 'statement_exec_bad', nil, 0);
  sst_opcode_pos_push (sst_opc_p^.if_true_p); {new opcodes get chained here}
  sst_r_pas_raw_sment;                 {build opcodes for TRUE case}
  sst_opcode_pos_pop;                  {back to regular opcode chain}
  syn_get_tag_msg (tag, str_h, 'sst_pas_read', 'statement_exec_bad', nil, 0);
  if tag = syn_tag_end_k
    then begin                         {no ELSE clause}
      sst_opc_p^.if_false_p := nil;    {indicate no opcodes for FALSE case}
      end
    else begin                         {ELSE clause exists}
      sst_opcode_pos_push (sst_opc_p^.if_false_p); {new opcodes get chained here}
      sst_r_pas_raw_sment;             {build opcodes for FALSE case}
      sst_opcode_pos_pop;              {back to regular opcode chain}
      end
    ;
  end;
{
***************************
*
*   Tag is for a CASE statement.
}
4: begin
  sst_r_pas_sment_case (str_h);        {process CASE statement}
  end;
{
***************************
*
*   Tag is for a FOR statement.
}
5: begin
  sst_opcode_new;                      {make new opcode descriptor}
  sst_opc_p^.opcode := sst_opc_loop_cnt_k; {opcode is for a counted loop}
  sst_opc_p^.str_h := str_h;           {save handle to source characters}
{
*   Process counting variable.
}
  syn_get_tag_msg (tag, str_h, 'sst_pas_read', 'statement_exec_bad', nil, 0);
  if tag <> 1 then syn_error_tag_unexp (tag, str_h);
  sst_r_pas_variable (sst_opc_p^.lpcn_var_p); {get descriptor for counting var}
  sst_var_funcname (sst_opc_p^.lpcn_var_p^); {call func instead of stuff return val}
  if sst_opc_p^.lpcn_var_p^.vtype <> sst_vtype_var_k then begin {not a variable ?}
    syn_get_tag_string (str_h, s);     {get "variable" reference string}
    sys_msg_parm_vstr (msg_parm[1], s);
    syn_error (str_h, 'sst_pas_read', 'not_a_variable', msg_parm, 1);
    end;
  if not (sst_rwflag_write_k in sst_opc_p^.lpcn_var_p^.rwflag) then begin
    syn_error (str_h, 'sst', 'var_not_writeable', nil, 0);
    end;
{
*   Process starting value expression.
}
  syn_get_tag_msg (tag, str_h, 'sst_pas_read', 'statement_exec_bad', nil, 0);
  if tag <> 1 then syn_error_tag_unexp (tag, str_h);
  sst_r_pas_exp (str_h, false, sst_opc_p^.lpcn_exp_start_p); {build exp descriptor}
  sst_exp_useage_check (               {check expression attributes for this useage}
    sst_opc_p^.lpcn_exp_start_p^,      {expression to check}
    [sst_rwflag_read_k],               {read/write access needed to expression value}
    sst_opc_p^.lpcn_var_p^.dtype_p^);  {data type value must be compatible with}
{
*   Process TO/DOWNTO keywords.
}
  syn_get_tag_msg (tag, str_h, 'sst_pas_read', 'statement_exec_bad', nil, 0);
  case tag of
1: begin                               {keyword is TO}
      sst_opc_p^.lpcn_inc_dir := sst_incdir_up_k;
      end;
2: begin                               {keyword is DOWNTO}
      sst_opc_p^.lpcn_inc_dir := sst_incdir_down_k;
      end;
otherwise
    syn_error_tag_unexp (tag, str_h);
    end;                               {end of TO/DOWNTO tag cases}
{
*   Process ending value expression.
}
  syn_get_tag_msg (tag, str_h, 'sst_pas_read', 'statement_exec_bad', nil, 0);
  if tag <> 1 then syn_error_tag_unexp (tag, str_h);
  sst_r_pas_exp (str_h, false, sst_opc_p^.lpcn_exp_end_p); {build exp descriptor}
  sst_exp_useage_check (               {check expression attributes for this useage}
    sst_opc_p^.lpcn_exp_end_p^,        {expression to check}
    [sst_rwflag_read_k],               {read/write access needed to expression value}
    sst_opc_p^.lpcn_var_p^.dtype_p^);  {data type value must be compatible with}
{
*   Set up the loop increment value expression.  This is either -1 or +1,
*   or an explicit value if the BY keyword is present.
}
  syn_get_tag_msg (tag, str_h, 'sst_pas_read', 'statement_exec_bad', nil, 0);
  case tag of

1: begin                               {implicit value is either -1 or +1}
      sst_mem_alloc_namesp (           {allocate mem for increment value expression}
        sizeof(sst_opc_p^.lpcn_exp_inc_p^), sst_opc_p^.lpcn_exp_inc_p);
      with sst_opc_p^.lpcn_exp_inc_p^: exp do begin {EXP is increment expression desc}
        exp.str_h := str_h;            {source is TO/DOWNTO keyword}
        exp.dtype_p := sst_config.int_machine_p;
        exp.dtype_hard := false;
        exp.val_eval := true;
        exp.val_fnd := true;
        exp.val.dtype := sst_dtype_int_k;
        case sst_opc_p^.lpcn_inc_dir of
sst_incdir_up_k: exp.val.int_val := 1;
sst_incdir_down_k: exp.val.int_val := -1;
          end;
        exp.rwflag := [sst_rwflag_read_k];
        exp.term1.next_p := nil;
        exp.term1.op2 := sst_op2_none_k;
        exp.term1.op1 := sst_op1_none_k;
        exp.term1.ttype := sst_term_const_k;
        exp.term1.str_h := exp.str_h;
        exp.term1.dtype_p := exp.dtype_p;
        exp.term1.dtype_hard := false;
        exp.term1.val_eval := true;
        exp.term1.val_fnd := true;
        exp.term1.val := exp.val;
        exp.term1.rwflag := exp.rwflag;
        end;                           {done with EXP abbreviation}
      end;

2: begin                               {loop increment value is given explicitly}
      sst_r_pas_exp (str_h, false, sst_opc_p^.lpcn_exp_inc_p); {build exp descriptor}
      sst_exp_useage_check (           {check expression attributes for this useage}
        sst_opc_p^.lpcn_exp_inc_p^,    {expression to check}
        [sst_rwflag_read_k],           {read/write access needed to expression value}
        sst_opc_p^.lpcn_var_p^.dtype_p^); {data type value must be compatible with}
      end;

otherwise
    syn_error_tag_unexp (tag, str_h);
    end;
{
*   Process the loop statement.
}
  syn_get_tag_msg (tag, str_h, 'sst_pas_read', 'statement_exec_bad', nil, 0);
  if tag <> 1 then syn_error_tag_unexp (tag, str_h);
  sst_opcode_pos_push (sst_opc_p^.lpcn_code_p); {new opcodes get chained here}
  sst_r_pas_raw_sment;                 {build opcodes for body of loop}
  sst_opcode_pos_pop;                  {back to regular opcode chain}
  end;                                 {end of FOR statement case}
{
***************************
*
*   Tag is for a REPEAT ... UNTIL loop.
}
6: begin
  sst_opcode_new;                      {make new opcode descriptor}
  sst_opc_p^.opcode := sst_opc_loop_tbot_k; {set opcode type}
  sst_opc_p^.str_h := str_h;           {save handle to source characters}
  sst_opcode_pos_push (sst_opc_p^.lpbt_code_p); {new opcodes get chained here}

loop_tag6:                             {back here each new syntax tag}
  syn_get_tag_msg (tag, str_h, 'sst_pas_read', 'statement_exec_bad', nil, 0);
  case tag of
1: begin                               {tag is for more statements in loop body}
      sst_r_pas_raw_sment;             {add more statements to body of loop}
      goto loop_tag6;                  {back for next tag}
      end;
2: begin                               {tag is for loop termination expression}
      sst_opcode_pos_pop;              {back to regular opcode chain}
      sst_r_pas_exp (str_h, false, sst_opc_p^.lpbt_exp_p); {make ending expression}
      sst_exp_useage_check (           {check expression attributes for this useage}
        sst_opc_p^.lpbt_exp_p^,        {expression to check}
        [sst_rwflag_read_k],           {read/write access needed to expression value}
        sst_dtype_bool_p^);            {data type value must be compatible with}
      end;
otherwise
    syn_error_tag_unexp (tag, str_h);
    end;
  end;
{
***************************
*
*   Tag is for a WHILE statement.
}
7: begin
  sst_opcode_new;                      {make new opcode descriptor}
  sst_opc_p^.opcode := sst_opc_loop_ttop_k; {set opcode type}
  sst_opc_p^.str_h := str_h;           {save handle to source characters}
{
*   Process loop conditional expression.
}
  syn_get_tag_msg (tag, str_h, 'sst_pas_read', 'statement_exec_bad', nil, 0);
  if tag <> 1 then syn_error_tag_unexp (tag, str_h);
  sst_r_pas_exp (str_h, false, sst_opc_p^.lptp_exp_p); {make conditional expression}
  sst_exp_useage_check (               {check expression attributes for this useage}
    sst_opc_p^.lptp_exp_p^,            {expression to check}
    [sst_rwflag_read_k],               {read/write access needed to expression value}
    sst_dtype_bool_p^);                {data type value must be compatible with}
{
*   Process loop body code.
}
  syn_get_tag_msg (tag, str_h, 'sst_pas_read', 'statement_exec_bad', nil, 0);
  if tag <> 1 then syn_error_tag_unexp (tag, str_h);
  sst_opcode_pos_push (sst_opc_p^.lptp_code_p); {new opcodes get chained here}
  sst_r_pas_raw_sment;                 {build opcodes for loop body}
  sst_opcode_pos_pop;                  {back to regular opcode chain}
  end;
{
***************************
*
*   Tag is for a WITH statement.
}
8: begin
  sst_opcode_new;                      {make new opcode descriptor}
  sst_opc_p^.opcode := sst_opc_abbrev_k; {set opcode type}
  sst_opc_p^.str_h := str_h;           {save handle to source characters}
  sst_scope_new;                       {create new scope for abbreviations}
  sst_scope_p^.flag_ref_used := true;  {flag referenced symbols as used}
  sst_opc_p^.abbrev_scope_p := sst_scope_p; {save scope of abbreviation symbols}
  sst_opc_p^.abbrev_sym_first_p := nil; {init to no symbols in linked list}
  sym_prev_p := nil;                   {init to no previous symbol created yet}

loop_tag8:                             {back here each new tag in WITH statement}
  syn_get_tag_msg (tag, str_h, 'sst_pas_read', 'statement_exec_bad', nil, 0);
  case tag of
1: begin                               {tag is for another WITH_ABBREV syntax}
      syn_level_down;                  {down into WITH_ABBREV syntax}
      syn_get_tag_msg (                {get tag for abbreviation expansion "variable"}
        tag, str_h, 'sst_pas_read', 'statement_with_bad', nil, 0);
      if tag <> 1 then syn_error_tag_unexp (tag, str_h);
      sst_r_pas_variable (var_p);      {create var descriptor for abbrev expansion}
      sst_var_funcname (var_p^);       {call func instead of stuff return value}
      syn_get_tag_msg (                {get tag for abbreviation name}
        tag2, str2_h, 'sst_pas_read', 'statement_with_bad', nil, 0);
      if tag <> 1 then syn_error_tag_unexp (tag, str_h);
      syn_level_up;                    {back up from WITH_ABBREV syntax}
      sst_symbol_new                   {create initial symbol for abbrev name}
        (str2_h, syn_charcase_down_k, sym_p, stat);
      syn_error_abort (stat, str2_h, 'sst_pas_read', 'statement_with_bad', nil, 0);
      sym_p^.symtype := sst_symtype_abbrev_k; {new symbol is an abbreviation}
      sym_p^.abbrev_var_p := var_p;    {point abbreviation to its expansion}
      sym_p^.next_p := nil;            {init this symbol to be end of chain}
      if sym_prev_p = nil
        then begin                     {this is the first symbol in the chain}
          sst_opc_p^.abbrev_sym_first_p := sym_p; {save pointer to start of chain}
          end
        else begin
          sym_prev_p^.next_p := sym_p; {create link from previous symbol to new sym}
          end
        ;
      sym_prev_p := sym_p;             {update pointer to last symbol in chain}
      goto loop_tag8;                  {back for next tag in WITH statement}
      end;
2: begin                               {tag is for statements covered by abbrevs}
      sst_opcode_pos_push (sst_opc_p^.abbrev_code_p); {new opcodes get chained here}
      sst_r_pas_raw_sment;             {build opcodes for code using abbrevs}
      sst_opcode_pos_pop;              {back to regular opcode chain}
      sst_scope_old;                   {restore to previous scope}
      end;
otherwise
    syn_error_tag_unexp (tag, str_h);
    end;
  end;
{
***************************
*
*   Tag is for EXIT statement.
}
9: begin
  sst_opcode_new;
  sst_opc_p^.opcode := sst_opc_loop_exit_k;
  sst_opc_p^.str_h := str_h;
  end;
{
***************************
*
*   Tag is for NEXT statement.
}
10: begin
  sst_opcode_new;
  sst_opc_p^.opcode := sst_opc_loop_next_k;
  sst_opc_p^.str_h := str_h;
  end;
{
***************************
*
*   Tag is for RETURN statement.
}
11: begin
  sst_opcode_new;
  sst_opc_p^.opcode := sst_opc_return_k;
  sst_opc_p^.str_h := str_h;
  end;
{
***************************
*
*   Tag is for a variable or other symbol.  This could be an assignment
*   statement or a procecure call.
}
12: begin
  sst_opcode_new;                      {make new opcode descriptor}
  sst_opc_p^.str_h := str_h;           {save handle to source characters}
  sst_r_pas_variable (var_p);          {read VARIABLE and build variable descriptor}
  syn_get_tag_msg (tag, str_h, 'sst_pas_read', 'statement_exec_bad', nil, 0);
  case tag of
{
*   Statement is a procedure call with no arguments.
}
syn_tag_end_k: begin
  args_here := false;                  {no subroutine arguments exist}
  goto do_call;                        {to common code for to handle subroutine call}
  end;
{
*   Statement is a procedure call with arguments.
}
1: begin
  sst_var_funcname (var_p^);           {call function instead of stuff return value}
  args_here := true;                   {subroutine arguments do exist}

do_call:                               {start common code to handle subroutine call}
  if                                   {is this a function instead of subroutine ?}
      (var_p^.vtype = sst_vtype_rout_k) and then
      (var_p^.rout_proc_p^.dtype_func_p <> nil)
      then begin
    syn_error (str_h, 'sst_pas_read', 'routine_is_function', nil, 0);
    end;
  sst_opc_p^.opcode := sst_opc_call_k; {opcode is for subroutine call}
  sst_opc_p^.call_var_p := var_p;      {point opcode to variable referencing subr}
  sst_r_pas_routine (                  {build and check subroutine call descriptor}
    sst_opc_p^.str_h, var_p^, args_here, sst_opc_p^.call_proc_p);
  sst_opc_p^.call_proct_p := var_p^.rout_proc_p; {save pointer to routine template}
  end;
{
*   Statement is an assignment statement.  The tag is for the expression to the
*   right of the ":=".
}
2: begin
  sst_rwcheck (                        {check that variable is writeable}
    [sst_rwflag_write_k],              {access actually used}
    var_p^.rwflag,                     {allowed access to this variable}
    stat);
  syn_error_abort (stat, sst_opc_p^.str_h, '', '', nil, 0);
  sst_opc_p^.opcode := sst_opc_assign_k; {opcode indicates assignment statement}
  sst_opc_p^.assign_var_p := var_p;    {set pointer to variable being assigned to}
  sst_r_pas_exp (str_h, false, sst_opc_p^.assign_exp_p); {build expression descriptor}
  sst_exp_useage_check (               {check expression attributes for this useage}
    sst_opc_p^.assign_exp_p^,          {expression to check}
    [sst_rwflag_read_k],               {read/write access needed to expression value}
    var_p^.dtype_p^);                  {data type value must be compatible with}
  syn_error_abort (stat, sst_opc_p^.assign_exp_p^.str_h, '', '', nil, 0);
  end;
{
*   Unexpected tag value.
}
otherwise
    syn_error_tag_unexp (tag, str_h);
    end;                               {end of TAG cases within call/assign sment}
  end;
{
***************************
*
*   Tag is block of statements within BEGIN ... END.
}
13: begin
  sst_r_pas_statements;                {process nested BEGIN ... END block}
  end;
{
***************************
*
*   Tag is label name for any upcoming statements.
}
14: begin
  sst_opcode_new;                      {make new opcode descriptor}
  sst_opc_p^.opcode := sst_opc_label_k; {set opcode type}
  sst_opc_p^.str_h := str_h;           {save handle to source characters}
  sst_scope_p^.flag_ref_used := false; {putting label in code is not "use"}
  get_label_sym_pnt (str_h, sst_opc_p^.label_sym_p); {get pointer to label symbol}
  sst_scope_p^.flag_ref_used := true;  {restore to symbol references are "uses"}
  goto tag_loop;
  end;
{
***************************
*
*   Tag is for WRITE or WRITELN statement.
}
15: begin
  sst_r_pas_write;                     {process WRITE/WRITELN statement}
  end;
{
***************************
*
*   Tag is for DISCARD intrinsic procedure.
}
16: begin
  sst_opcode_new;                      {make new opcode descriptor}
  sst_opc_p^.str_h := str_h;           {save handle to source characters}
  sst_opc_p^.opcode := sst_opc_discard_k; {indicate the type of this new opcode}
  syn_get_tag_msg (                    {get tag to function reference expression}
    tag, str_h, 'sst_pas_read', 'statement_exec_bad', nil, 0);
  if tag <> 1 then begin
    syn_error_tag_unexp (tag, str_h);
    end;
  sst_r_pas_exp (str_h, false, sst_opc_p^.discard_exp_p); {get function reference exp}
  with sst_opc_p^.discard_exp_p^: exp do begin {EXP is func ref expression descriptor}
    if                                 {invalid expression for DISCARD ?}
        (exp.term1.next_p <> nil) or   {more than one term in expression ?}
        (exp.term1.op1 <> sst_op1_none_k) or {unadic operator in front of exp ?}
        ( (exp.term1.ttype <> sst_term_func_k) and {not some kind of function ?}
          (exp.term1.ttype <> sst_term_ifunc_k))
        then begin
      syn_error (str_h, 'sst_pas_read', 'discard_arg_bad', nil, 0);
      end;
    end;                               {done with EXP abbreviation}
  end;
{
***************************
*
*   Unrecognized or unimplemented TAG value.
}
otherwise
    syn_error_tag_unexp (tag, str_h);
    end;                               {end of tag cases}
{
*   Make sure there are no more tags left in this RAW_STATEMENT syntax.
}
  syn_get_tag_msg (tag, str_h, 'sst_pas_read', 'statement_exec_bad', nil, 0);
  if tag <> syn_tag_end_k then begin
    syn_error_tag_unexp (tag, str_h);  {complain if not hit end of syntax}
    end;

leave:                                 {common exit point}
  syn_level_up;                        {up from RAW_STATEMENT syntax level}
  end;
