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
  str_h: syn_string_t;                 {handle to string from input file}
  syname: string_var32_t;              {name of syntax symbol being defined}
  name_p: string_var_p_t;              {pointer to name in hash table entry}
  data_p: symbol_data_p_t;             {pointer to user data in hash table entry}
  scope_old_p: sst_scope_p_t;          {saved pointer to scope before subroutine}
  names_old_p: sst_scope_p_t;          {saved pointer to names space before subr}
  mflag_p: sst_symbol_p_t;             {points to MFLAG dummy argument symbol}
  jtarg: jump_targets_t;               {jump targets for subordinate syntax routines}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

begin
  syname.max := sizeof(syname.str);    {init local var strings}

  syn_level_down;                      {down into DEFINE syntax}
{
**************************************
*
*   Get name of symbol being defined, and set up state for writing code
*   to the symbol subroutine.
}
  syn_get_tag_msg (                    {get symbol name tag}
    tag, str_h, 'sst_syn_read', 'syerr_define', nil, 0);
  if tag <> 1 then syn_error_tag_unexp (tag, str_h);
  syn_get_tag_string (str_h, syname);  {get name of symbol being defined}
  string_upcase (syname);              {SYN symbol names are case-insensitive}

  string_hash_ent_lookup (             {look up name in SYN symbol table}
    table_sym, syname, name_p, data_p);
  if data_p = nil then begin           {symbol not previously declared ?}
    sys_msg_parm_vstr (msg_parm[1], syname);
    syn_error (str_h, 'sst_syn_read', 'symbol_not_declared', msg_parm, 1);
    end;
  if sst_symflag_extern_k in data_p^.sym_p^.flags then begin
    sys_msg_parm_vstr (msg_parm[1], syname);
    syn_error (str_h, 'sst_syn_read', 'symbol_external', msg_parm, 1);
    end;
  if sst_symflag_def_k in data_p^.sym_p^.flags then begin
    sys_msg_parm_vstr (msg_parm[1], syname);
    syn_error (str_h, 'sst_syn_read', 'symbol_already_defined', msg_parm, 1);
    end;

  %debug; writeln ('Defining ', syname.str:syname.len);

  def_syn_p := data_p;                 {save pnt to data about syntax being defined}

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

  seq_mflag := 1;                      {init SYN common block state}
  seq_label := 1;
  seq_int := 1;

  mflag_p :=                           {get pointer to MFLAG dummy argument}
    data_p^.sym_p^.proc.first_arg_p^.sym_p;
{
**************************************
*
*   Write code for mandatory initialization before any syntax checking is done.
}
  sst_call (sym_start_routine_p^);     {create call to SYN_P_START_ROUTINE}
  sst_call_arg_str (sst_opc_p^, syname.str, syname.len); {add syntax name argument}
  sst_call_arg_int (sst_opc_p^, syname.len); {add syntax name length argument}
{
**************************************
*
*   Write syntax checking body of routine.
*
*   Set up root jump targets.  These will all be "fall thru" with MFLAG
*   properly set.
}
  jtarg.yes.flags := [jflag_fall_k, jflag_mfset_k];
  jtarg.yes.lab_p := nil;
  jtarg.no.flags := [jflag_fall_k, jflag_mfset_k];
  jtarg.no.lab_p := nil;
  jtarg.err.flags := [jflag_fall_k, jflag_mfset_k];
  jtarg.err.lab_p := nil;
{
*   Process EXPRESSION syntax.
}
  syn_get_tag_msg (                    {get tag for symbol expression}
    tag, str_h, 'sst_syn_read', 'syerr_define', nil, 0);
  if tag <> 1 then syn_error_tag_unexp (tag, str_h);

  sst_r_syn_expression (               {process EXPRESSION syntax}
    jtarg,                             {jump targets}
    mflag_p^);                         {descriptor for MFLAG variable to set}

  sst_r_syn_jtargets_done (jtarg);     {create any neccessary labels here}
{
**************************************
*
*   Create opcodes for the syntax routine exit code.  This will be a call
*   to SYN_P_END_ROUTINE with the MFLAG variable.
}
  sst_call (sym_end_routine_p^);       {create call to SYN_P_END_ROUTINE}
  sst_call_arg_var (sst_opc_p^, mflag_p^); {add call argument for MFLAG variable}
{
**************************************
*
*   Restore state to before subroutine definition.
}
  sst_opcode_pos_pop;                  {back from executable opcodes in subroutine}
  sst_opcode_pos_pop;                  {back from subroutine definition opcode}
  sst_scope_p := scope_old_p;          {restore old scope and name space}
  sst_names_p := names_old_p;
  syn_level_up;                        {back up from DEFINE syntax}
  def_syn_p := nil;                    {no syntax currently being defined}
  end;
