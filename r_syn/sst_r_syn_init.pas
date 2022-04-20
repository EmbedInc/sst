{   Subroutine SST_R_SYN_INIT
*
*   Init the SST front end for reading SYN files.
*
*   The internal state of the SYN front end is initialized, and system resources
*   allocated.  The module that will contain the syntax parsing code is created.
*   Subsequent calls to the DOIT entry point for this SST front end will add to
*   this module.
}
module sst_r_syn_init;
define sst_r_syn_init;
%include 'sst_r_syn.ins.pas';

procedure sst_r_syn_init;              {init front end state for reading .syn files}

var
  gnam: string_leafname_t;             {generic name of input file}
  fnam_synstart: string_treename_t;    {pathname to SYO_SYN.INS.PAS file}
  sym_p: sst_symbol_p_t;               {pointer to module name symbol}
  stat: sys_err_t;                     {completion status code}
{
**********************************************
*
*   Local function LOOKUP_SYMBOL (NAME)
*
*   Look up symbol in symbol table.  The function returns the pointer to the
*   symbol descriptor with the name NAME.  It is an error if the symbol does not
*   exist.
}
function lookup_symbol (
  in      name: string)                {name of symbol to look up}
  :sst_symbol_p_t;                     {returned pointer to symbol descriptor}

var
  vname: string_var80_t;               {var string symbol name}
  sym_p: sst_symbol_p_t;               {pointer to symbol descriptor}
  stat: sys_err_t;                     {completion status code}

begin
  vname.max := sizeof(vname.str);      {init local var string}

  string_vstring (vname, name, sizeof(name)); {make var string symbol name}
  sst_symbol_lookup_name (vname, sym_p, stat); {try to look up name in symbol table}
  sys_error_abort (stat, 'sst_syn_read', 'symbol_predef_not_found', nil, 0);
  lookup_symbol := sym_p;              {return pointer to symbol descriptor}
  end;
{
**********************************************
*
*   Start of main routine.
}
begin
  gnam.max := sizeof(gnam.str);        {init local var strings}
  fnam_synstart.max := sizeof(fnam_synstart.str);
{
*   Read the SYN_SYN.INS.PAS file.  This declares all the routines and other
*   symbols that the syntax parsing code we will generate needs to reference.
}
  sys_cognivis_dir ('lib', fnam_synstart); {make pathname of SYN_SYN.INS.PAS}
  string_appends (fnam_synstart, '/syn_syn.ins.pas');

  sst_r_pas_init;                      {init for reading Pascal syntax}
  sst_r.doit^ (                        {run Pascal front end}
    fnam_synstart,                     {name of Pascal file to read}
    gnam,                              {returned generic name of input file}
    stat);                             {returned completion status code}
  if sys_stat_match (sst_subsys_k, sst_stat_err_handled_k, stat) then begin
    sys_exit_error;                    {exit quietly with error condition}
    end;
  sys_error_abort (stat, 'sst', 'readin', nil, 0);
{
*   Set up the SST front end for reading the user's syntax definition file.
}
  syo_preproc_set (nil);               {de-install any Pascal front end preprocessor}
  sst_r.doit := addr(sst_r_syn_doit);  {set up front end call table}
{
*   Save pointers to all the pre-defined symbols we might need later.
}
  {
  *   Routines to call from syntax parsing code.
  }
  sym_constr_start_p := lookup_symbol ('syn_p_constr_start');
  sym_constr_end_p := lookup_symbol ('syn_p_constr_end');
  sym_cpos_push_p := lookup_symbol ('syn_p_cpos_push');
  sym_cpos_pop_p := lookup_symbol ('syn_p_cpos_pop');
  sym_cpos_get_p := lookup_symbol ('syn_p_cpos_get');
  sym_cpos_set_p := lookup_symbol ('syn_p_cpos_set');
  sym_tag_start_p := lookup_symbol ('syn_p_tag_start');
  sym_tag_end_p := lookup_symbol ('syn_p_tag_end');
  sym_ichar_p := lookup_symbol ('syn_p_ichar');
  sym_test_string_p := lookup_symbol ('syn_p_test_string');
  sym_test_eol_p := lookup_symbol ('syn_p_test_eol');
  sym_test_eof_p := lookup_symbol ('syn_p_test_eof');
  sym_test_eod_p := lookup_symbol ('syn_p_test_eod');
  sym_charcase_p := lookup_symbol ('syn_p_charcase');
  {
  *   Data types.
  }
  sym_syn_t_p := lookup_symbol ('syn_t');
  sym_charcase_t_p := lookup_symbol ('syn_charcase_k_t');
  sym_int_p := lookup_symbol ('sys_int_machine_t');
  {
  *   Constants.
  }
  sym_charcase_down_p := lookup_symbol ('syn_charcase_down_k');
  sym_charcase_up_p := lookup_symbol ('syn_charcase_up_k');
  sym_charcase_asis_p := lookup_symbol ('syn_charcase_asis_k');
  sym_ichar_eol_p := lookup_symbol ('syn_ichar_eol_k');
  sym_ichar_eof_p := lookup_symbol ('syn_ichar_eof_k');
  sym_ichar_eod_p := lookup_symbol ('syn_ichar_eod_k');
  {
  *   Expressions with fixed value.
  }
  sst_exp_const_bool (true, exp_true_p);
  sst_exp_const_bool (false, exp_false_p);
{
*   Initialize the rest of the SYN front end static state (SST_R_SYN common
*   block).
}
  syn_lib_new (                        {start new use of the SYN library}
    sst_scope_root_p^.mem_p^,          {parent memory context}
    syn_p);                            {return pointer to new library use state}

  sst_r_syn_sym_init;                  {create and init our symbol table}

  prefix.max := size_char(prefix.str); {init subroutines prefix to empty}
  prefix.len := 0;
  def_syn_p := nil;                    {init to not currently defining a symbol}
  seq_subr := 1;                       {init sequence numbers to make unique names}
  seq_label := 1;
  seq_int := 1;
  lab_fall_k := univ_ptr(addr(lab_fall_k));
  lab_same_k := univ_ptr(addr(lab_same_k));
  sym_error_p := nil;
  match_var_p := nil;
  match_exp_p := nil;
{
*   Create the module for all the routines generated from the SYN file.
}
  sst_symbol_new_name (                {create module name symbol}
    string_v('module_syn'(0)), sym_p, stat);
  sys_error_abort (stat, 'sst_syn_read', 'module_symbol_create', nil, 0);

  sst_scope_new;                       {create new subordinate scope for module}
  sst_scope_p^.symbol_p := sym_p;

  sym_p^.symtype := sst_symtype_module_k; {fill in module symbol descriptor}
  sym_p^.flags := [sst_symflag_def_k, sst_symflag_used_k];
  sym_p^.module_scope_p := sst_scope_p;

  sst_opcode_new;                      {create MODULE opcode}
  sst_opc_p^.opcode := sst_opc_module_k;
  sst_opc_p^.module_sym_p := sym_p;
  sst_opcode_pos_push (sst_opc_p^.module_p); {switch to defining module contents}
  end;
