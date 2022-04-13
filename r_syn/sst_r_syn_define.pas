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
  name_p: string_var_p_t;              {pnt to name in hash table entry}
  data_p: symbol_data_p_t;             {pnt to hash table data for syn constr symbol}
  func_p: sst_symbol_p_t;              {pnt to SST symbol for the syntax parsing function}
  scope_old_p: sst_scope_p_t;          {saved pointer to scope before subroutine}
  names_old_p: sst_scope_p_t;          {saved pointer to names space before subr}
  jtarg: jump_targets_t;               {jump targets for subordinate syntax routines}
  var_p: sst_var_p_t;                  {scratch pointer to SST variable}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;
  stat: sys_err_t;                     {completion status}

label
  trerr;

begin
  syname.max := sizeof(syname.str);    {init local var strings}

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

  seq_mflag := 1;                      {init seq numbers for making unique symbols}
  seq_label := 1;
  seq_int := 1;
{
**************************************
*
*   Write code for mandatory initialization before any syntax checking is done.
}
  {
  *   Create local boolean variable MATCH.  Since it is a local variable, MATCH
  *   is created separately for each subroutine.
  }
  sst_symbol_new_name (                {create the local variable MATCH symbol}
    string_v('match'(0)),              {name of symbol to create}
    sym_p,                             {returned pointer to the new symbol}
    stat);
  syn_error_bomb (syn_p^, stat, '', '', nil, 0);

  sym_p^.symtype := sst_symtype_var_k; {this symbol is a variable}
  sym_p^.var_dtype_p := sst_dtype_bool_p; {set pointer to the data type}
  sym_p^.var_val_p := nil;             {no initial value expression}
  sym_p^.var_arg_p := nil;             {this variable is not a dummy argument}
  sym_p^.var_proc_p := nil;
  sym_p^.var_com_p := nil;             {not in a common block}
  sym_p^.var_next_p := nil;

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

  match_exp_p := sst_exp_make_var (sym_p^); {make expression for MATCH value}
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
*
*   Set up root jump targets.  These will all be "fall thru" with MATCH
*   properly set.
}
  jtarg.yes.flags := [jflag_fall_k, jflag_mset_k];
  jtarg.yes.lab_p := nil;
  jtarg.no.flags := [jflag_fall_k, jflag_mset_k];
  jtarg.no.lab_p := nil;
  jtarg.err.flags := [jflag_fall_k, jflag_mset_k];
  jtarg.err.lab_p := nil;
{
*   Process EXPRESSION syntax.
}
  tag := syn_trav_next_tag (syn_p^);   {get tag for symbol expression}
  if tag <> 1 then begin
    syn_msg_tag_bomb (syn_p^, '', '', nil, 0);
    end;

  sst_r_syn_expression (jtarg);        {process EXPRESSION syntax}
  sst_r_syn_jtargets_done (jtarg);     {create any neccessary labels here}
{
**************************************
*
*   End this syntax construction.  This is done by adding a call to
*   SYN_P_CONSTR_END.
}
  sst_call (sym_constr_end_p^);        {create call to SYN_P_CONSTR_END}

  sst_call_arg_var (                   {add SYN argument}
    sst_opc_p^,                        {opcode to add call argument to}
    func_p^.proc.first_arg_p^.sym_p^); {variable being passed}

  sst_call_arg_var (                   {pass the MATCH value resulting from this syntax}
    sst_opc_p^,                        {opcode to add call argument to}
    match_var_p^.mod1.top_sym_p^);     {variable being passed}
{
**************************************
*
*   Set the syntax construction function return value.  This is the same as
*   the value of MATCH at this point.
}
  sst_mem_alloc_scope (                {allocate mem for variable descriptor}
    sizeof(var_p^), var_p);

  var_p^.mod1.next_p := nil;           {no subsequent modifier in chaing}
  var_p^.mod1.modtyp := sst_var_modtyp_top_k; {this is top modifier}
  var_p^.mod1.top_str_h.first_char.crange_p := nil;
  var_p^.mod1.top_sym_p := func_p^.proc_funcvar_p; {symbol being referenced}

  var_p^.dtype_p := var_p^.mod1.top_sym_p^.var_dtype_p; {data type}
  var_p^.rwflag := [sst_rwflag_write_k];
  var_p^.vtype := sst_vtype_var_k;

  sst_r_syn_assign_exp (var_p^, match_exp_p^); {write the assignment}
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
