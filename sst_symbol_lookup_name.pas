{   Subroutine SST_SYMBOL_LOOKUP_NAME (NAME, SYM_P, STAT)
*
*   Look up an existing symbol in the currently active name spaces.
*   SYM_P is returned pointing to the symbol data block.
*   STAT is returned with an error if the symbol was not found, in which
*   case SYM_P is returned NIL.
}
module sst_symbol_lookup_name;
define sst_symbol_lookup_name;
%include 'sst2.ins.pas';

procedure sst_symbol_lookup_name (     {look up symbol in visible name spaces}
  in      name: univ string_var_arg_t; {name of symbol to look up}
  out     sym_p: sst_symbol_p_t;       {returned pointer to symbol descriptor}
  out     stat: sys_err_t);            {completion status code}

var
  uname: string_var132_t;              {upcased symbol name string}
  name_p: string_var_p_t;              {points to symbol name in hash table entry}
  scope_p: sst_scope_p_t;              {points to curr scope looking up name in}
  sym_pp: sst_symbol_pp_t;             {points to hash table user data area}

label
  found;

begin
  uname.max := sizeof(uname.str);      {init local var string}

  sys_error_none (stat);               {init STAT to indicate no error}
  sym_p := nil;                        {init pointer to indicate symbol not found}

  string_copy (name, uname);           {make upper case copy of symbol name}
  string_upcase (uname);

  scope_p := sst_names_p;              {init current scope to most local name space}
  while scope_p <> nil do begin        {keep looping until reach top name space}
    string_hash_ent_lookup (           {look for raw name in this name space}
      scope_p^.hash_h,                 {hash table handle at this name space}
      name,                            {the name to look up}
      name_p,                          {returned pointer to stored name string}
      sym_pp);                         {returned pointer to hash entry user data}
    if sym_pp <> nil then goto found;  {found symbol ?}
    string_hash_ent_lookup (           {look for upcased name in this name space}
      scope_p^.hash_h,                 {hash table handle at this name space}
      uname,                           {the name to look up}
      name_p,                          {returned pointer to stored name string}
      sym_pp);                         {returned pointer to hash entry user data}
    if sym_pp <> nil then goto found;  {found symbol ?}
    scope_p := scope_p^.parent_p;      {switch to next more global name space}
    end;                               {back and look in new name space}
{
*   The symbol was not found in any of the currently active name spaces.
}
  sys_stat_set (sst_subsys_k, sst_stat_sym_not_found_k, stat); {set error code}
  sys_stat_parm_vstr (name, stat);     {pass symbol name to error code}
  return;
{
*   The symbol was found.  SYM_PP points to the user data pointer in the
*   hash table entry for this symbol.
}
found:
  sym_p := sym_pp^;                    {get pointer to symbol descriptor block}
  if sst_scope_p^.flag_ref_used then begin {flag referenced symbols as used ?}
    sym_p^.flags :=                    {indicate symbol was "used"}
      sym_p^.flags + [sst_symflag_used_k];
    end;
  end;
