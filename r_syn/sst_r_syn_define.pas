{   Subroutine SST_R_SYN_DEFINE
*
*   Process DEFINE syntax.
}
module sst_r_syn_define;
define sst_r_syn_define;
%include 'sst_r_syn.ins.pas';

procedure sst_r_syn_define;            {process DEFINE syntax}

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  tag: sys_int_machine_t;              {tag from syntax tree}
  syname: string_var32_t;              {name of syntax symbol being defined}
  sym_p: sst_symbol_p_t;               {scratch pointer to SST symbol}
  fname: string_var32_t;               {name of field looking for}
  field_p: sst_symbol_p_t;             {scratch pointer to field in record}
  mod_p: sst_var_mod_p_t;              {scratch pointer to variable modifier}
  var_p: sst_var_p_t;                  {scratch SST variable pointer}
  name_p: string_var_p_t;              {pnt to name in hash table entry}
  data_p: symbol_data_p_t;             {pnt to hash table data for syn constr symbol}
  func_p: sst_symbol_p_t;              {pnt to SST symbol for the syntax parsing function}
  scope_old_p: sst_scope_p_t;          {saved pointer to scope before subroutine}
  names_old_p: sst_scope_p_t;          {saved pointer to names space before subr}
  jtarg: jump_targets_t;               {jump targets for subordinate syntax routines}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;
  stat: sys_err_t;                     {completion status}

label
  trerr;

begin
  syname.max := size_char(syname.str); {init local var strings}
  fname.max := size_char(fname.str);

  if not syn_trav_next_down (syn_p^)   {down into DEFINE syntax}
    then goto trerr;
{
*   Get the name of the syntax being defined into SYNAME.  Set DATA_P pointing
*   to the SYN symbol data.
}
  tag := syn_trav_next_tag (syn_p^);   {get symbol name tag}
  if tag <> 1 then begin               {unexpected tag ?}
    syn_msg_tag_bomb (syn_p^, 'sst_syn_read', 'syerr_define', nil, 0);
    end;

  syn_trav_tag_string (syn_p^, syname); {get name of symbol being defined}
  string_upcase (syname);              {SYN symbol names are case-insensitive}
  if sst_level_debug >= 1 then begin
    writeln ('Defining ', syname.str:syname.len);
    end;

  string_hash_ent_lookup (             {look up name in SYN symbol table}
    table_sym, syname, name_p, data_p);
  if data_p = nil then begin           {symbol not previously declared ?}
    sys_msg_parm_vstr (msg_parm[1], syname);
    syn_msg_pos_bomb (syn_p^, 'sst_syn_read', 'symbol_not_declared', msg_parm, 1);
    end;
  if sst_symflag_extern_k in data_p^.sym_p^.flags then begin {externally defined ?}
    sys_msg_parm_vstr (msg_parm[1], syname);
    syn_msg_pos_bomb (syn_p^, 'sst_syn_read', 'symbol_external', msg_parm, 1);
    end;
  if sst_symflag_def_k in data_p^.sym_p^.flags then begin {already defined before}
    sys_msg_parm_vstr (msg_parm[1], syname);
    syn_msg_pos_bomb (syn_p^, 'sst_syn_read', 'symbol_already_defined', msg_parm, 1);
    end;

  data_p^.sym_p^.flags :=              {subroutine symbol will now be defined}
    data_p^.sym_p^.flags + [sst_symflag_def_k];
  if sst_symflag_global_k in data_p^.sym_p^.flags then begin {subr globally known ?}
    data_p^.sym_p^.flags :=            {explicitly flag this subroutine as used}
      data_p^.sym_p^.flags + [sst_symflag_used_k];
    end;

  def_syn_p := data_p;                 {save pnt to data about syntax being defined}
{
*   Set up for writing the syntax parsing subroutine.
}
  func_p := data_p^.sym_p;             {save pointer to function being written}

  sst_opcode_new;                      {create opcode for this routine definition}
  sst_opc_p^.opcode := sst_opc_rout_k;
  sst_opc_p^.str_h.first_char := func_p^.char_h;
  sst_opc_p^.str_h.last_char := sst_opc_p^.str_h.first_char;
  sst_opc_p^.rout_sym_p := func_p;

  scope_old_p := sst_scope_p;          {save current scope and namespace}
  names_old_p := sst_names_p;

  sst_scope_p := func_p^.proc_scope_p; {switch to scope and namespace of the function}
  sst_names_p := sst_scope_p;

  sst_opcode_pos_push (sst_opc_p^.rout_p); {switch to opcodes chain of the function}

  sst_opcode_new;                      {create "start of executable" opcode}
  sst_opc_p^.opcode := sst_opc_exec_k;
  sst_opc_p^.str_h.first_char := func_p^.char_h;
  sst_opc_p^.str_h.last_char := sst_opc_p^.str_h.first_char;
  sst_opcode_pos_push (sst_opc_p^.exec_p); {future opcodes will be on exec list}

  seq_label := 1;
  seq_int := 1;
{
*   Create local boolean variable MATCH.  Since it is a local variable, MATCH is
*   created separately for each subroutine.  The MATCH variable is also set up
*   to automatically supply the function return value.
}
  sst_symbol_new_name (                {create the local variable MATCH symbol}
    string_v('match'(0)),              {name of symbol to create}
    sym_p,                             {returned pointer to the new symbol}
    stat);
  syn_error_bomb (syn_p^, stat, '', '', nil, 0);

  sym_p^.symtype := sst_symtype_var_k; {symbol looks like a variable inside routine}
  sym_p^.var_dtype_p := func_p^.proc.dtype_func_p; {same a func return val data type}
  sym_p^.var_val_p := nil;             {no initial value}
  sym_p^.var_arg_p := nil;             {no dummy argument descriptor}
  sym_p^.var_proc_p := addr(func_p^.proc); {point to the routine descriptor}
  sym_p^.var_com_p := nil;             {not in a common block}
  sym_p^.var_next_p := nil;            {no next var in common block}

  func_p^.proc_funcvar_p := sym_p;     {variable that is function return value inside routine}

  sst_mem_alloc_scope (                {allocate mem for variable descriptor}
    sizeof(match_var_p^), match_var_p);

  match_var_p^.mod1.next_p := nil;     {no additional modifiers}
  match_var_p^.mod1.modtyp := sst_var_modtyp_top_k; {this is top level modifier}
  match_var_p^.mod1.top_str_h.first_char.crange_p := nil;
  match_var_p^.mod1.top_sym_p := sym_p; {symbol being referenced}

  match_var_p^.dtype_p := sst_dtype_bool_p; {data type is boolean}
  match_var_p^.rwflag :=               {variable can be read and written}
    [sst_rwflag_read_k, sst_rwflag_write_k];
  match_var_p^.vtype := sst_vtype_var_k; {this var reference is to a regular variable}
{
*   Create expressions for the value of MATCH and NOT MATCH.
}
  match_exp_p := sst_exp_make_var (sym_p^); {make expression for MATCH value}

  match_not_exp_p := sst_exp_make_var (sym_p^); {init expression for NOT MATCH}
  match_not_exp_p^.rwflag := [sst_rwflag_read_k]; {expression is only readable}
  match_not_exp_p^.term1.op1 := sst_op1_not_k; {apply boolean NOT to this term}
  match_not_exp_p^.term1.rwflag := [sst_rwflag_read_k]; {term is only readable}
{
*   Create expression for the SYN call argument value.
}
  exp_syn_p := sst_exp_make_var (func_p^.proc.first_arg_p^.sym_p^);
{
*   Make SST structures for using the SYN_P_ICHAR function.  The variable
*   reference is created first because it is needed to create the function
*   value expression.
}
  sst_mem_alloc_scope (                {create var reference to SYN_P_ICHAR function}
    sizeof(var_ichar_p^), var_ichar_p);
  var_ichar_p^.mod1.next_p := nil;     {no second modifier}
  var_ichar_p^.mod1.modtyp := sst_var_modtyp_top_k; {this is first (and only) modifier}
  var_ichar_p^.mod1.top_str_h.first_char.crange_p := nil;
  var_ichar_p^.mod1.top_sym_p := sym_ichar_p; {pointer to symbol being referenced}
  var_ichar_p^.dtype_p := sym_ichar_p^.proc_dtype_p; {procedure data type}
  var_ichar_p^.rwflag := [sst_rwflag_read_k]; {read-only}
  var_ichar_p^.vtype := sst_vtype_rout_k; {this var reference is to procedure}
  var_ichar_p^.rout_proc_p := addr(sym_ichar_p^.proc); {points to procedure definition}

  exp_ichar_p := sst_r_syn_exp_ichar;  {expression for SYN_P_ICHAR function value}
{
*   Create the expression that is the value of SYN.ERR_END in the parsing
*   function being built.  SYM_ERROR_P is set pointing to the expression.
}
  string_vstring (fname, 'err_end'(0), -1); {make name of field looking for}
  sym_p := func_p^.proc.first_arg_p^.sym_p; {get pointer to SYN call argument}

  field_p := sym_p^.dtype_dtype_p^.rec_first_p; {init to first field in SYN}
  while field_p <> nil do begin        {look for the ERR_END field}
    if string_equal (field_p^.name_in_p^, fname) then exit;
    field_p := field_p^.field_next_p;
    end;
  if field_p = nil then begin          {didn't find the field ?}
    writeln ('INTERNAL ERROR: Unable to find field "', fname.str:fname.len, '" in SST_R_SYN_DEFINE.');
    sys_bomb;
    end;

  sst_mem_alloc_scope (                {create top level variable descriptor}
    sizeof(var_p^), var_p);
  var_p^.mod1.modtyp := sst_var_modtyp_top_k; {top modifier in chain}
  var_p^.mod1.top_str_h.first_char.crange_p := nil;
  var_p^.mod1.top_sym_p := sym_p;      {set pointer to root symbol}
  var_p^.dtype_p := sym_p^.var_dtype_p; {data type of the record}
  var_p^.rwflag :=                     {only allowed to read the variable}
    [sst_rwflag_read_k];
  var_p^.vtype := sst_vtype_var_k;     {this var reference is to a regular variable}

  sst_mem_alloc_scope (                {create second modifier, field within record}
    sizeof(mod_p^), mod_p);
  var_p^.mod1.next_p := mod_p;         {link to after the root modifier}
  mod_p^.next_p := nil;                {no following modifier}
  mod_p^.modtyp := sst_var_modtyp_field_k; {this mod is for field within record}
  mod_p^.field_str_h.first_char.crange_p := nil;
  mod_p^.field_sym_p := field_p;       {point to symbol for the specific field}

  sst_mem_alloc_scope (                {create expression for SYN.ERR_END value}
    sizeof(sym_error_p^), sym_error_p);
  sym_error_p^.str_h.first_char.crange_p := nil;
  sym_error_p^.dtype_p := var_p^.dtype_p; {expression data type}
  sym_error_p^.dtype_hard := true;     {data type if known and fixed}
  sym_error_p^.val_eval := false;      {didn't attempt to resolve value}
  sym_error_p^.val_fnd := false;       {value is not set}
  sym_error_p^.rwflag := [sst_rwflag_read_k]; {only allowed to read the expression}

  sym_error_p^.term1.next_p := nil;    {no subsequent term in this expression}
  sym_error_p^.term1.op2 := sst_op2_none_k; {no operator with next term}
  sym_error_p^.term1.op1 := sst_op1_none_k; {no unary operator on this term}
  sym_error_p^.term1.ttype := sst_term_var_k; {this term is a variable reference}
  sym_error_p^.term1.str_h.first_char.crange_p := nil;
  sym_error_p^.term1.dtype_p := var_p^.dtype_p; {data type of this term}
  sym_error_p^.term1.dtype_hard := true; {data type is known and fixed}
  sym_error_p^.term1.val_eval := false; {didn't attempt to resolve value}
  sym_error_p^.term1.val_fnd := false; {value not set}
  sym_error_p^.term1.rwflag := [sst_rwflag_read_k]; {only allowed to read}
  sym_error_p^.term1.var_var_p := var_p; {point to the variable being referenced}
{
*   Do other initialization before writing code (creating opcodes) in the
*   function being defined.
}
  sst_r_syn_jtarg_init (jtarg);        {init all jump targets to "fall thru"}
  label_err_p := nil;                  {init to no explicit check for error written}
{
**************************************
*
*   Write code for mandatory initialization before any syntax checking is done.
}
  {
  *   Call SYN_P_CONSTR_START.
  }
  sst_call (sym_constr_start_p^);      {create call to SYN_P_CONSTR_START}

  sst_call_arg_var (                   {add SYN argument}
    sst_opc_p^,                        {opcode to add call argument to}
    func_p^.proc.first_arg_p^.sym_p^); {variable being passed}

  sst_call_arg_str (sst_opc_p^, syname.str, syname.len); {add syntax name string arg}
  sst_call_arg_int (sst_opc_p^, syname.len); {add syntax name length arg}
  {
  *   Initialize MATCH to FALSE.
  }
  sst_r_syn_assign_match (false);      {init MATCH to false}
{
**************************************
*
*   Write syntax checking body of routine.
}
  {
  *   Process EXPRESSION syntax.
  }
  tag := syn_trav_next_tag (syn_p^);   {get tag for symbol expression}
  if tag <> 1 then begin
    syn_msg_tag_bomb (syn_p^, '', '', nil, 0);
    end;

  sst_r_syn_expression (jtarg);        {process EXPRESSION syntax}

  sst_r_syn_jtarg_here (jtarg);        {create any neccessary labels here}
{
**************************************
*
*   Write common ending code.
}
  {
  *   End this syntax construction.
  }
  sst_call (sym_constr_end_p^);        {create call to SYN_P_CONSTR_END}

  sst_call_arg_var (                   {add SYN argument}
    sst_opc_p^,                        {opcode to add call argument to}
    func_p^.proc.first_arg_p^.sym_p^); {variable being passed}

  sst_call_arg_var (                   {pass the MATCH value resulting from this syntax}
    sst_opc_p^,                        {opcode to add call argument to}
    match_var_p^.mod1.top_sym_p^);     {variable being passed}
  {
  *   Write error exit code if an error check was done at least once.
  }
  if label_err_p <> nil then begin     {error jump target symbol was created ?}
    sst_opcode_new;                    {add RETURN to end of previous code}
    sst_opc_p^.opcode := sst_opc_return_k;

    sst_opcode_new;                    {create opcode for the label}
    sst_opc_p^.opcode := sst_opc_label_k;
    sst_opc_p^.label_sym_p := label_err_p; {point to label to appear here}
    label_err_p^.label_opc_p := sst_opc_p; {link label symbol to target opcode}

    sst_r_syn_assign_match (false);    {set function return value to FALSE}
    end;
{
**************************************
*
*   Restore state to before subroutine definition.
}
  sst_opcode_pos_pop;                  {back from executable opcodes in subroutine}
  sst_opcode_pos_pop;                  {back from subroutine definition opcode}
  sst_scope_p := scope_old_p;          {restore old scope and name space}
  sst_names_p := names_old_p;
  if not syn_trav_up (syn_p^)          {back up from DEFINE syntax}
    then goto trerr;
  def_syn_p := nil;                    {no syntax currently being defined}
  if sst_level_debug >= 2 then begin
    writeln ('  returning from DEFINE');
    end;
{
*   Invalidate globally-visible reference to the local scope of this syntax
*   parsing function.  This state is re-created for each new parsing function.
}
  def_syn_p := nil;
  exp_syn_p := nil;
  sym_error_p := nil;
  match_var_p := nil;
  match_exp_p := nil;
  match_not_exp_p := nil;
  label_err_p := nil;
  var_ichar_p := nil;
  exp_ichar_p := nil;

  return;
{
*   The syntax tree is not as expected.  We assume this is due to a syntax
*   error.
}
trerr:
  sys_message ('sst_syn_read', 'syerr_define');
  syn_parse_err_show (syn_p^);
  sys_bomb;
  end;
