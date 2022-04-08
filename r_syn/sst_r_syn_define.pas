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
  name_p: string_var_p_t;              {pointer to name in hash table entry}
  data_p: symbol_data_p_t;             {pointer to user data in hash table entry}
  scope_old_p: sst_scope_p_t;          {saved pointer to scope before subroutine}
  names_old_p: sst_scope_p_t;          {saved pointer to names space before subr}
  jtarg: jump_targets_t;               {jump targets for subordinate syntax routines}
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
**************************************
*
*   Find syntax construction symbol being defined and set up state for defining
*   it.  DATA_P set set pointing to the data for this symbol in our private
*   symbol table.
}
  tag := syn_trav_next_tag (syn_p^);   {get symbol name tag}
  if tag <> 1 then begin               {unexpected tag ?}
    syn_msg_tag_bomb (syn_p^, 'sst_syn_read', 'syerr_define', nil, 0);
    end;
  syn_trav_tag_string (syn_p^, syname); {get name of symbol being defined}
  string_upcase (syname);              {SYN symbol names are case-insensitive}

  string_hash_ent_lookup (             {look up name in SYN symbol table}
    table_sym, syname, name_p, data_p);
  if data_p = nil then begin           {symbol not previously declared ?}
    sys_msg_parm_vstr (msg_parm[1], syname);
    syn_msg_pos_bomb (syn_p^, 'sst_syn_read', 'symbol_not_declared', msg_parm, 1);
    end;
  if sst_symflag_extern_k in data_p^.sym_p^.flags then begin
    sys_msg_parm_vstr (msg_parm[1], syname);
    syn_msg_pos_bomb (syn_p^, 'sst_syn_read', 'symbol_external', msg_parm, 1);
    end;
  if sst_symflag_def_k in data_p^.sym_p^.flags then begin
    sys_msg_parm_vstr (msg_parm[1], syname);
    syn_msg_pos_bomb (syn_p^, 'sst_syn_read', 'symbol_already_defined', msg_parm, 1);
    end;
  def_syn_p := data_p;                 {save pnt to data about syntax being defined}
  if debug >= 1 then begin
    writeln ('Defining ', syname.str:syname.len);
    end;
{
*   Set up for writing the syntax parsing subroutine.
}
  sst_opcode_new;                      {create opcode for this routine definition}
  sst_opc_p^.opcode := sst_opc_rout_k;
  sst_opc_p^.str_h.first_char := data_p^.sym_p^.char_h;
  sst_opc_p^.str_h.last_char := sst_opc_p^.str_h.first_char;
  sst_opc_p^.rout_sym_p := data_p^.sym_p;
  data_p^.sym_p^.flags :=              {subroutine symbol will now be defined}
    data_p^.sym_p^.flags + [sst_symflag_def_k];
  if sst_symflag_global_k in data_p^.sym_p^.flags then begin {subr globally known ?}
    data_p^.sym_p^.flags :=            {explicitly flag this subroutine as used}
      data_p^.sym_p^.flags + [sst_symflag_used_k];
    end;

  scope_old_p := sst_scope_p;          {save current scope pointers}
  names_old_p := sst_names_p;

  sst_scope_p := data_p^.sym_p^.proc_scope_p; {switch to scope of subroutine}
  sst_names_p := sst_scope_p;

  sst_opcode_pos_push (sst_opc_p^.rout_p); {init opcodes chain for this routine}

  sst_opcode_new;                      {create "start of executable" opcode}
  sst_opc_p^.opcode := sst_opc_exec_k;
  sst_opc_p^.str_h.first_char := data_p^.sym_p^.char_h;
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
  *   is created separately for each subroutine.  However, the variable
  *   descriptor, MATCH_VAR, is static in the main common block, and is re-used
  *   to be the descriptor for the variable in the subroutine being currently
  *   defined.  The static state in MATCH_VAR was initialized once, with only
  *   the pointer to the symbol changing for each use.
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

  match_var.mod1.top_sym_p := sym_p;   {set symbol for curr local MATCH variable}
  {
  *   Call SYN_P_CONSTR_START.
  }
  sst_call (sym_constr_start_p^);      {create call to SYN_P_CONSTR_START}

  sst_call_arg_var (                   {add SYN argument}
    sst_opc_p^,                        {opcode to add call argument to}
    data_p^.sym_p^.proc.first_arg_p^.sym_p^); {variable being passed}

  sst_call_arg_str (sst_opc_p^, syname.str, syname.len); {add syntax name argument}
  sst_call_arg_int (sst_opc_p^, syname.len); {add syntax name length argument}
  {
  *   Initialize MATCH to FALSE.
  }
  sst_opcode_new;                      {create new empty opcode, make current}
  sst_opc_p^.opcode := sst_opc_assign_k; {opcode is assignment to variable}
  sst_opc_p^.assign_var_p := addr(match_var); {the variable to assign to}
  sst_opc_p^.assign_exp_p := exp_false_p; {the expression to assign to it}
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
  sst_call (sym_constr_start_p^);      {create call to SYN_P_CONSTR_START}

  sst_call_arg_var (                   {add SYN argument}
    sst_opc_p^,                        {opcode to add call argument to}
    data_p^.sym_p^.proc.first_arg_p^.sym_p^); {variable being passed}

  sst_call_arg_var (                   {pass the MATCH value resulting from this syntax}
    sst_opc_p^,                        {opcode to add call argument to}
    match_var.mod1.top_sym_p^);        {variable being passed}
{
**************************************
*
*   Restore state to before subroutine definition.
}
  sst_opcode_pos_pop;                  {back from executable opcodes in subroutine}
  sst_opcode_pos_pop;                  {back from subroutine definition opcode}
  sst_scope_p := scope_old_p;          {restore old scope and name space}
  sst_names_p := names_old_p;
  syo_level_up;                        {back up from DEFINE syntax}
  def_syn_p := nil;                    {no syntax currently being defined}
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
