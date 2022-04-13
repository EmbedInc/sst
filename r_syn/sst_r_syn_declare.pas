{   Subroutine SST_R_SYN_DECLARE
*
*   Process DECLARE syntax, which declares the existance of a symbol.
}
module sst_r_syn_declare;
define sst_r_syn_declare;
%include 'sst_r_syn.ins.pas';

procedure sst_r_syn_declare;           {process DECLARE syntax}

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  tag: sys_int_machine_t;              {tag from syntax tree}
  syname: string_var32_t;              {name of syntax symbol being declared}
  prname: string_var32_t;              {name of procedure to run syntax}
  token: string_var16_t;               {scratch string for number conversion}
  hpos: string_hash_pos_t;             {hash table position handle}
  found: boolean;                      {TRUE if name found in hash table}
  flags: sst_symflag_t;                {flags for SST subroutine name symbol}
  name_p: string_var_p_t;              {pointer to name in hash table entry}
  data_p: symbol_data_p_t;             {pointer to user data in hash table entry}
  func_p: sst_symbol_p_t;              {pnt to syntax parsing function being defined}
  fret_p: sst_symbol_p_t;              {pnt to local var that is function return value}
  sym_p: sst_symbol_p_t;               {scratch pointer to a SST symbol}
  arg_p: sst_proc_arg_p_t;             {pointer to info about subroutine argument}
  dt_p: sst_dtype_p_t;                 {scratch pointer to data type descriptor}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;
  stat: sys_err_t;

label
  opt_tag, done_opt_tags, trerr;

begin
  syname.max := sizeof(syname.str);    {init local var strings}
  prname.max := sizeof(prname.str);
  token.max := sizeof(token.str);

  if not syn_trav_next_down (syn_p^)   {down into DECLARE syntax}
    then goto trerr;

  tag := syn_trav_next_tag (syn_p^);   {get symbol name tag}
  if tag <> 1 then begin
    syn_msg_tag_bomb (syn_p^, 'sst_syn_read', 'syerr_declare', nil, 0);
    end;
  syn_trav_tag_string (syn_p^, syname); {get the symbol name}
{
*   Init to defaults before processing options.
}
  prname.len := 0;                     {subroutine name not explicitly set}
  flags := [];                         {syntax subroutine will be local to this file}
{
*   Back here to get each optional tag.
}
opt_tag:
  tag := syn_trav_next_tag (syn_p^);   {get tag for next option, if any}
  case tag of
1:  begin                              {tag is for subroutine name}
      syn_trav_tag_string (syn_p^, prname); {get the subroutine name}
      flags := flags + [sst_symflag_global_k]; {subroutine will be globally visible}
      end;
2:  begin                              {tag is EXTERNAL option}
      flags := flags + [sst_symflag_extern_k]; {subroutine not defined here}
      end;
syn_tag_end_k: begin                   {done processing all the optional tags}
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
        if sys_error(stat) then begin
          sys_msg_parm_vstr (msg_parm[1], syname);
          syn_error_bomb (syn_p^, stat,
            'sst_syn_read', 'seq_subr_err', msg_parm, 1);
          end;
        string_append (prname, token); {make full subroutine name}
        seq_subr := seq_subr + 1;      {update sequence number for next time}
        string_downcase (prname);      {default subroutine names are lower case}
        end;
      goto done_opt_tags;              {all done processing tags}
      end;
otherwise
    syn_msg_tag_bomb (syn_p^, 'sst_syn_read', 'syerr_declare', nil, 0);
    sys_bomb;
    end;                               {end of optional tag type cases}
  goto opt_tag;                        {back for next optional tag}

done_opt_tags:                         {done processing optional tags}
  if not syn_trav_up (syn_p^)
    then goto trerr;

  string_upcase (syname);              {SYN file symbols are case-insensitive}
{
*   All done reading the input stream for this SYMBOL command.  The SYN file
*   symbol name is in SYNAME, and the subroutine name is in PRNAME.
*
*   Create the new SYN symbol.
}
  string_hash_pos_lookup (             {get hash table position for new name}
    table_sym, syname, hpos, found);
  if found then begin                  {this symbol already declared ?}
    sys_msg_parm_vstr (msg_parm[1], syname);
    syn_msg_pos_bomb (syn_p^, 'sst_syn_read', 'symbol_already_used', msg_parm, 1);
    end;
  string_hash_ent_add (                {add new symbol to the SYN symbol table}
    hpos,                              {handle to hash table position}
    name_p,                            {points to name in hash table entry}
    data_p);                           {points to data area in hash table entry}

  data_p^.name_p := name_p;            {save pointer to hash entry name}
  data_p^.call_p := nil;               {init list of called syntaxes to empty}
{
*   Create the SST procedure.  FUNC_P will be left pointing to the procedure
*   descriptor.
}
  sst_symbol_new_name (                {add function name to SST symbol table}
    prname, func_p, stat);
  syn_error_bomb (syn_p^, stat, '', '', nil, 0);

  data_p^.sym_p := func_p;             {point SYN symbol to the SST symbol}

  sst_scope_new;                       {create private scope for new function}
  sst_scope_p^.symbol_p := func_p;     {set pointer to top symbol for this scope}
  {
  *   Create the SYN dummy argument.  ARG_P will be left pointing to the
  *   argument descriptor.
  }
  sst_symbol_new_name (                {create symbol for arg inside routine}
    string_v('syn'(0)), sym_p, stat);
  syn_error_bomb (syn_p^, stat, '', '', nil, 0);

  sst_mem_alloc_scope (sizeof(arg_p^), arg_p); {alloc memory for subr arg descriptor}

  sym_p^.symtype := sst_symtype_var_k; {symbol is a variable}
  sym_p^.flags := [sst_symflag_def_k]; {symbol is defined}
  sym_p^.var_dtype_p := sym_syn_t_p^.dtype_dtype_p; {variable's data type}
  sym_p^.var_val_p := nil;             {no initial value}
  sym_p^.var_arg_p := arg_p;           {to arg descriptor when dummy arg}
  sym_p^.var_proc_p := addr(func_p^.proc); {procedure this is dummy arg of}
  sym_p^.var_com_p := nil;             {not in a common block}
  sym_p^.var_next_p := nil;            {no next var in common block}

  arg_p^.next_p := nil;                {init to no following argument in list}
  arg_p^.sym_p := sym_p;               {point to symbol for the function argument}
  arg_p^.name_p := nil;                {arg name when arg in routine template}
  arg_p^.exp_p := nil;                 {expression when routine is called}
  arg_p^.dtype_p := sym_p^.var_dtype_p; {data type of the argument}
  arg_p^.pass := sst_pass_ref_k;       {argument is passed by reference}
  arg_p^.rwflag_int :=                 {access permission inside the routine}
    [sst_rwflag_read_k, sst_rwflag_write_k];
  arg_p^.rwflag_ext :=                 {access permission of routine in called code}
    [sst_rwflag_read_k, sst_rwflag_write_k];
  arg_p^.univ := false;                {not allowed to match any data type}
  {
  *   Create the data type descriptor for this routine.  DT_P will be left
  *   pointing to the data type descriptor.
  }
  sst_dtype_new (dt_p);                {create PROC data type descriptor}

  dt_p^.symbol_p := func_p;            {point to symbol representing this data type}
  dt_p^.dtype := sst_dtype_proc_k;     {this dtype is a procedure}
  dt_p^.bits_min := 0;                 {fill in benign data size and alignment values}
  dt_p^.align_nat := 1;
  dt_p^.align := 1;
  dt_p^.size_used := 0;
  dt_p^.size_align := 0;
  dt_p^.proc_p := addr(func_p^.proc);  {set pointer to procedure descriptor}
  {
  *   Create the "variable" that is the function return value as seen inside the
  *   routine.  FRET_P will be left pointing to the variable symbol.
  }
  sst_symbol_new_name (                {create symbol for function return "variable}
    func_p^.name_in_p^,                {same name as the function name}
    fret_p,                            {returned pointer to symbol descriptor}
    stat);
  sys_error_abort (stat, '', '', nil, 0);

  fret_p^.symtype := sst_symtype_var_k; {symbol looks like a variable inside routine}
  fret_p^.var_dtype_p := sst_dtype_bool_p; {set the data type}
  fret_p^.var_val_p := nil;            {no initial value}
  fret_p^.var_arg_p := nil;            {no dummy argument descriptor}
  fret_p^.var_proc_p := addr(func_p^.proc); {point to the routine descriptor}
  fret_p^.var_com_p := nil;            {not in a common block}
  fret_p^.var_next_p := nil;           {no next var in common block}
  {
  *   Fill in the procedure descriptor for the syntax parsing function that will
  *   be associated with the SYN symbol being declared.  FUNC_P is pointing to
  *   the procedure descriptor to fill in.
  }
  func_p^.proc_scope_p := sst_scope_p; {save pointer to routine's scope}
  func_p^.symtype := sst_symtype_proc_k; {symbol is a routine}
  func_p^.flags := flags;              {init EXTERN, etc, as known now}

  func_p^.proc.sym_p := func_p;        {point routine descriptor to name symbol}
  func_p^.proc.dtype_func_p := fret_p^.var_dtype_p; {function value data type}
  func_p^.proc.n_args := 1;            {number of arguments the routine takes}
  func_p^.proc.flags := [];            {init separate flags}
  func_p^.proc.first_arg_p := arg_p;   {point to first argument in list}

  func_p^.proc_scope_p := sst_scope_p; {scope inside the routine}
  func_p^.proc_dtype_p := dt_p;        {routine's "data type"}
  func_p^.proc_funcvar_p := fret_p;    {variable that is function return value inside routine}

  sst_scope_old;                       {pop back from subroutine's scope}
  return;
{
*   The syntax tree is not as expected.  We assume this is due to a syntax
*   error.
}
trerr:
  sys_message ('sst_syn_read', 'syerr_declare');
  syn_parse_err_show (syn_p^);
  sys_bomb;
  end;
