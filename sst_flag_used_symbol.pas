{   Subroutine SST_FLAG_USED_SYMBOL (SYM)
*
*   Flag the indicated symbol as used.  Also, follow all symbols it eventually
*   references and flag them all as used.  SYM is the symbol descriptor of the
*   top level symbol to follow.
}
module sst_FLAG_USED_SYMBOL;
define sst_flag_used_symbol;
%include 'sst2.ins.pas';

procedure sst_flag_used_symbol (       {flag symbols eventually used from this sym}
  in out  sym: sst_symbol_t);          {descriptor for top symbol to follow}

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  sym_p: sst_symbol_p_t;               {scratch pointer to a symbol}
  pos: string_hash_pos_t;              {position handle into hash table}
  name_p: univ_ptr;                    {unused subroutine argument}
  sym_pp: sst_symbol_pp_t;             {pointer to hash table user data area}
  found: boolean;                      {TRUE if hash table entry was found}
  msg_parm:                            {references to paramters for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

begin
  sym.flags := sym.flags +             {flag this symbol as used}
    [sst_symflag_used_k];
  if                                   {don't process this symbol any further ?}
      (sst_symflag_followed_k in sym.flags) or {already did this symbol ?}
      (sst_symflag_following_k in sym.flags) {already working on this symbol ?}
    then return;
  sym.flags := sym.flags + [sst_symflag_following_k]; {now working on this symbol}
  case sym.symtype of                  {what kind of symbol is this ?}
{
*************************************
*
*   Symbol is a constant.
}
sst_symtype_const_k: begin
  sst_flag_used_exp (sym.const_exp_p^); {declare nested symbols in expression}
  end;
{
*************************************
*
*   Symbol is the value of an enumerated type.
}
sst_symtype_enum_k: begin
  sst_flag_used_dtype (sym.enum_dtype_p^); {make sure parent data type is used}
  end;
{
*************************************
*
*   Symbol is a data type.
}
sst_symtype_dtype_k: begin
  sst_flag_used_dtype (sym.dtype_dtype_p^); {declare nested symbols in dtype}
  end;                                 {end of symbol is data type case}
{
*************************************
*
*   Symbol is a field of a record.
}
sst_symtype_field_k: begin
  sst_flag_used_dtype (sym.field_parent_p^); {flag symbols used by whole record}
  sst_flag_used_dtype (sym.field_dtype_p^); {flag symbols used by this field}
  end;
{
*************************************
*
*   Symbol is a variable.
}
sst_symtype_var_k: begin
  if sym.var_com_p <> nil then begin   {variable is in a common block ?}
    if                                 {not currently working on the common block ?}
        (not (sst_symflag_following_k in sym.var_com_p^.flags))
        then begin
      sym.flags := sym.flags - [sst_symflag_following_k]; {not working on this sym}
      sst_flag_used_symbol (sym.var_com_p^); {process the common block}
      return;
      end;
    end;                               {done handling variable is in common block}
  sst_flag_used_dtype (sym.var_dtype_p^); {declare nested symbols in data type}
  if sym.var_val_p <> nil then begin   {initial value expression exists ?}
    sst_flag_used_exp (sym.var_val_p^); {flag symbols in initial value exp as used}
    end;
  end;
{
*************************************
*
*   Symbol is an abbreviation.
}
sst_symtype_abbrev_k: begin
  sst_flag_used_var (sym.abbrev_var_p^);
  end;
{
*************************************
*
*   Symbol is a routine name.
}
sst_symtype_proc_k: begin
  sst_flag_used_rout (sym.proc);       {declare nested symbols in routine}
  end;
{
*************************************
*
*   Symbol is a common block name.
}
sst_symtype_com_k: begin
  sym_p := sym.com_first_p;            {init current symbol to first in com block}
  while sym_p <> nil do begin          {once for each symbol in common block}
    sst_flag_used_symbol (sym_p^);     {flag this variable as used}
    sym_p := sym_p^.var_next_p;        {advance to next variable in common block}
    end;                               {back and process this new variable}
  end;
{
*************************************
*
*   Symbol is a module name.
}
sst_symtype_module_k: begin
  string_hash_pos_first (sym.module_scope_p^.hash_h, pos, found); {get starting pos}
  while found do begin                 {once for each symbol in module's scope}
    string_hash_ent_atpos (pos, name_p, sym_pp); {get pointer to user data area}
    sym_p := sym_pp^;                  {get pointer to symbol descriptor}
    if
        (sst_symflag_global_k in sym_p^.flags) and {symbol is globally known ?}
        (sst_symflag_def_k in sym_p^.flags) and {actually defined ?}
        (not (sst_symflag_extern_k in sym_p^.flags)) {comes from this module ?}
        then begin
      sst_flag_used_symbol (sym_p^);   {flag symbol as used}
      end;
    string_hash_pos_next (pos, found); {advance to next symbol in this scope}
    end;
  end;
{
*************************************
*
*   Symbol types that require no special processing.
}
sst_symtype_prog_k: ;
sst_symtype_label_k: ;
otherwise
    sys_msg_parm_int (msg_parm[1], ord(sym.symtype));
    sys_message_bomb ('sst', 'symbol_type_unknown', msg_parm, 1);
    end;                               {end of symbol type cases}

  sym.flags := sym.flags +             {all done following this symbol}
    [sst_symflag_followed_k];
  sym.flags := sym.flags -             {this symbol no longer in process}
    [sst_symflag_following_k, sst_symflag_following_dt_k];
  end;
