{   Subroutine SST_SCOPE_UNUSED_SHOW (SCOPE)
*
*   List the unused symbols in the indicated scope.  Only those symbols will
*   be listed that are unused, obey the -SHOW_UNUSED command line option,
*   and are not intrinsic to the front end.
}
module sst_SCOPE_UNUSED_SHOW;
define sst_scope_unused_show;
%include 'sst2.ins.pas';

procedure sst_scope_unused_show (      {write names of unused symbols in a scope}
  in      scope: sst_scope_t);         {scope to list unused symbols in}

const
  max_msg_parms = 3;                   {max parameters we can pass to a message}

var
  pos: string_hash_pos_t;              {position handle into hash table}
  found: boolean;                      {TRUE if hash table entry was found}
  name_p: univ_ptr;                    {unused subroutine argument}
  sym_p: sst_symbol_p_t;               {pointer to to current symbol to write out}
  sym_pp: sst_symbol_pp_t;             {pointer to hash table user data area}
  dt_p: sst_dtype_p_t;                 {scratch pointer to data type descriptor}
  level: sys_int_machine_t;            {source file nesting level of symbol}
  fnam: string_treename_t;             {file name passed to a message}
  lnum: sys_int_machine_t;             {line number passed to a message}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

label
  next_sym;

begin
  if sst_level_unused = 0 then return; {not supposed to write any unused symbols ?}
  fnam.max := sizeof(fnam.str);        {init local var string}

  string_hash_pos_first (sst_scope_p^.hash_h, pos, found); {get pos for first symbol}
  while found do begin                 {once for each symbol at this scope}
    string_hash_ent_atpos (pos, name_p, sym_pp); {get pointer to this hash entry}
    sym_p := sym_pp^;                  {get pointer to symbol descriptor}
    if sst_symflag_used_k in sym_p^.flags {symbol is used ?}
      then goto next_sym;
    if sst_symflag_intrinsic_in_k in sym_p^.flags {intrinsic to front end ?}
      then goto next_sym;
    if sym_p^.char_h.crange_p = nil    {didn't come from input stream ?}
      then goto next_sym;
    if sym_p^.symtype = sst_symtype_dtype_k then begin {could be undefined dtype ?}
      dt_p := sym_p^.dtype_dtype_p;    {resolve base data type}
      while dt_p^.dtype = sst_dtype_copy_k do dt_p := dt_p^.copy_dtype_p;
      if dt_p^.dtype = sst_dtype_undef_k {data type is undefined ?}
        then goto next_sym;
      end;
    if                                 {variable in common block declared here ?}
        (sym_p^.symtype = sst_symtype_var_k) and {symbol is a variable ?}
        (sym_p^.var_com_p <> nil) and then {variable is in a common block ?}
        (not (sst_symflag_extern_k in sym_p^.var_com_p^.flags)) {com defined here ?}
      then goto next_sym;
    if sst_level_unused <> sst_level_unused_all_k then begin {don't list some syms ?}
      syn_get_char_flevel (sym_p^.char_h, level); {get source file nesting level}
      if level >= sst_level_unused     {symbol declared too deep in nested files ?}
        then goto next_sym;
      end;
{
*   This symbol needs to be listed.
}
    if
        (sym_p^.symtype = sst_symtype_var_k) and
        (sym_p^.var_arg_p <> nil)
      then begin                       {unused symbol is routine argument}
        sys_msg_parm_vstr (msg_parm[1], sym_p^.name_in_p^);
        sys_msg_parm_vstr (msg_parm[2], sym_p^.var_proc_p^.sym_p^.name_in_p^);
        sys_message_parms ('sst', 'symbol_arg_unused', msg_parm, 2);
        end
      else begin                       {unused symbol is not arg to this routine}
        sst_charh_info (sym_p^.char_h, fnam, lnum);
        sys_msg_parm_vstr (msg_parm[1], sym_p^.name_in_p^);
        sys_msg_parm_int (msg_parm[2], lnum);
        sys_msg_parm_vstr (msg_parm[3], fnam);
        sys_message_parms ('sst', 'symbol_unused', msg_parm, 3);
        end
      ;

next_sym:                              {jump here to advance to next symbol in scope}
    string_hash_pos_next (pos, found); {advance to next hash table entry}
    end;                               {back and do this new hash table entry}
  end;
