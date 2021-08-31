{   Subroutine SST_R_SYN_DECLARE
*
*   Process DECLARE syntax.
}
module sst_r_syn_declare;
define sst_r_syn_declare;
%include 'sst_r_syn.ins.pas';

procedure sst_r_syn_declare;           {process DECLARE syntax}

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  tag: sys_int_machine_t;              {tag from syntax tree}
  str_h, str2_h: syo_string_t;         {handles to strings from input file}
  syname: string_var32_t;              {name of syntax symbol being declared}
  prname: string_var32_t;              {name of procedure to run syntax}
  token: string_var16_t;               {scratch string for number conversion}
  hpos: string_hash_pos_t;             {hash table position handle}
  found: boolean;                      {TRUE if name found in hash table}
  flags: sst_symflag_t;                {flags for SST subroutine name symbol}
  name_p: string_var_p_t;              {pointer to name in hash table entry}
  data_p: symbol_data_p_t;             {pointer to user data in hash table entry}
  arg_p: sst_proc_arg_p_t;             {pointer to info about subroutine argument}
  dt_p: sst_dtype_p_t;                 {scratch pointer to data type descriptor}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;
  stat: sys_err_t;

label
  opt_tag, done_opt_tags;

begin
  syname.max := sizeof(syname.str);    {init local var strings}
  prname.max := sizeof(prname.str);
  token.max := sizeof(token.str);

  syo_level_down;                      {down into DECLARE syntax}

  syo_get_tag_msg (                    {get symbol name tag}
    tag, str_h, 'sst_syo_read', 'syerr_declare', nil, 0);
  if tag <> 1 then syo_error_tag_unexp (tag, str_h);
  syo_get_tag_string (str_h, syname);  {get name of symbol being declared}
{
*   Init to defaults before processing options.
}
  prname.len := 0;                     {subroutine name not explicitly set}
  flags := [];                         {syntax subroutine will be local to this file}
{
*   Back here to get each optional tag.
}
opt_tag:
  syo_get_tag_msg (                    {get tag for next option, if any}
    tag, str2_h, 'sst_syo_read', 'syerr_declare', nil, 0);
  case tag of
1:  begin                              {tag is for subroutine name}
      syo_get_tag_string (str2_h, prname);
      flags := flags + [sst_symflag_global_k]; {subroutine will be globally visible}
      end;
2:  begin                              {tag is EXTERNAL option}
      flags := flags + [sst_symflag_extern_k]; {subroutine not defined here}
      end;
syo_tag_end_k: begin                   {done processing all the optional tags}
      if prname.len = 0 then begin     {need to make subroutine name ?}
        string_copy (prefix, prname);  {init subroutine name with prefix}
        prname.len := min(prname.len, prname.max - 6); {leave room for suffix}
        string_appendn (prname, '_sy', 3);
        string_f_int_max_base (        {make subroutine sequence number string}
          token,                       {output string}
          seq_subr,                    {input number}
          10,                          {base}
          3,                           {field width}
          [string_fi_leadz_k, string_fi_unsig_k], {write leading zeros, no sign}
          stat);
        syo_error_abort (stat, str2_h, 'sst_syo_read', 'seq_subr_err', nil, 0);
        string_append (prname, token); {make full subroutine name}
        seq_subr := seq_subr + 1;      {update sequence number for next time}
        string_downcase (prname);      {default subroutine names are lower case}
        end;
      goto done_opt_tags;              {all done processing tags}
      end;
otherwise
    syo_error_tag_unexp (tag, str2_h);
    end;                               {end of optional tag type cases}
  goto opt_tag;                        {back for next optional tag}
done_opt_tags:                         {done processing optional tags}
  syo_level_up;                        {back up from DECLARE syntax}

  string_upcase (syname);              {SYN file symbols are case-insensitive}
{
*   All done reading the input stream for this SYMBOL command.
*   The SYN file symbol name is in SYNAME, and the subroutine name is
*   in PRNAME.
}
  %debug; writeln ('Declaring "', syname.str:syname.len,
  %debug;   '", subroutine "', prname.str:prname.len, '"');

  string_hash_pos_lookup (             {get hash table position for new name}
    table_sym, syname, hpos, found);
  if found then begin                  {this symbol already declared ?}
    sys_msg_parm_vstr (msg_parm[1], syname);
    syo_error (str_h, 'sst_syo_read', 'symbol_already_used', msg_parm, 1);
    end;
  string_hash_ent_add (                {add new symbol to the SYN symbol table}
    hpos,                              {handle to hash table position}
    name_p,                            {points to name in hash table entry}
    data_p);                           {points to data area in hash table entry}
  sst_symbol_new_name (                {add subroutine name to SST symbol table}
    prname, data_p^.sym_p, stat);
  syo_error_abort (stat, str_h, '', '', nil, 0);

  data_p^.name_p := name_p;            {save pointer to hash entry name}
  data_p^.call_p := nil;               {init list of called syntaxes to empty}

  sst_scope_new;                       {create private scope for new subroutine}
  sst_scope_p^.symbol_p := data_p^.sym_p;

  sst_mem_alloc_scope (sizeof(arg_p^), arg_p); {alloc memory for subr arg descriptor}
{
*   Fill in descriptor for subroutine name symbol.
}
  sst_dtype_new (dt_p);                {create PROC data type descriptor}
  dt_p^.dtype := sst_dtype_proc_k;
  dt_p^.proc_p := addr(data_p^.sym_p^.proc);

  data_p^.sym_p^.symtype := sst_symtype_proc_k; {fill in SST symbol descriptor}
  data_p^.sym_p^.flags := flags;
  data_p^.sym_p^.proc.sym_p := data_p^.sym_p;
  data_p^.sym_p^.proc.dtype_func_p := nil;
  data_p^.sym_p^.proc.n_args := 1;
  data_p^.sym_p^.proc.flags := [];
  data_p^.sym_p^.proc.first_arg_p := arg_p;
  data_p^.sym_p^.proc_scope_p := sst_scope_p;
  data_p^.sym_p^.proc_dtype_p := dt_p;
  data_p^.sym_p^.proc_funcvar_p := nil;
{
*   Fill in descriptor for MFLAG subroutine argument.
}
  arg_p^.next_p := nil;
  sst_symbol_new_name (                {create symbol for subroutine argument}
    string_v('mflag'(0)), arg_p^.sym_p, stat);
  syo_error_abort (stat, str_h, '', '', nil, 0);
  arg_p^.name_p := nil;
  arg_p^.exp_p := nil;
  arg_p^.dtype_p := sym_mflag_t_p^.dtype_dtype_p;
  arg_p^.pass := sst_pass_ref_k;
  arg_p^.rwflag_int := [sst_rwflag_read_k, sst_rwflag_write_k];
  arg_p^.rwflag_ext := [sst_rwflag_write_k];
  arg_p^.univ := false;
{
*   Fill in symbol descriptor for MFLAG dummy variable.
}
  arg_p^.sym_p^.symtype := sst_symtype_var_k;
  arg_p^.sym_p^.flags := [sst_symflag_def_k];
  arg_p^.sym_p^.var_dtype_p := arg_p^.dtype_p;
  arg_p^.sym_p^.var_val_p := nil;
  arg_p^.sym_p^.var_arg_p := arg_p;
  arg_p^.sym_p^.var_proc_p := addr(data_p^.sym_p^.proc);
  arg_p^.sym_p^.var_com_p := nil;
  arg_p^.sym_p^.var_next_p := nil;

  sst_scope_old;                       {pop back from subroutine's scope}
  end;
