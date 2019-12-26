{   Subroutine SST_NAME_NEW_OUT (NAME,NAME_P,SYM_PP,STAT)
*
*   Add a new name to the output symbol table at the current scope.
*
*   NAME is the name to add.  The current character case setting for output symbol
*     names will be applied before adding NAME to the symbol table.
*
*   NAME_P is returned pointing to the symbol name string stored in the hash
*     table entry.
*
*   SYM_PP is returned pointing to the start of the user data area in the
*     hash table entry.  The only thing stored there is the pointer to the symbol
*     descriptor.  This need not exist if the name is added to the symbol table
*     only to prevent use of the name later.
*
*   STAT is the returned completion status code.  It will be set to the
*     SST_STAT_SYM_FOUND_K status if the symbol name was already in the symbol
*     table at or above the current scope.
}
module sst_NAME_NEW_OUT;
define sst_name_new_out;
%include 'sst2.ins.pas';

procedure sst_name_new_out (           {add symbol to output table at curr scope}
  in      name: univ string_var_arg_t; {name of new symbol}
  out     name_p: univ string_var_p_t; {will point to name stored in hash table}
  out     sym_pp: sst_symbol_pp_t;     {points to hash table entry user data area}
  out     stat: sys_err_t);            {completion status code}

var
  namec: string_var132_t;              {name after case rules applied}
  pos_curr: string_hash_pos_t;         {position handle for hash table at curr scope}
  pos: string_hash_pos_t;              {position handle for other hash tables}
  scope_p: sst_scope_p_t;              {points to scope looking for name at}
  found: boolean;                      {TRUE if name found in hash table}

begin
  namec.max := sizeof(namec.str);      {init local var string}
  sys_error_none (stat);               {init to no error occurred}

  string_copy (name, namec);           {make local copy of name supplied by caller}
  case sst_config.charcase of          {apply current output name case rule}
syn_charcase_down_k: string_downcase (namec);
syn_charcase_up_k: string_upcase (namec);
    end;                               {end of character case rule cases}

  scope_p := sst_scope_p;              {set first scope to look for name in}
  while scope_p <> nil do begin        {once for each scope up to root scope}
    if scope_p = sst_scope_p
      then begin                       {we are looking for name in most local scope}
        string_hash_pos_lookup (       {look up name in output symbol table}
          scope_p^.hash_out_h,         {hash table handle}
          namec,                       {symbol name}
          pos_curr,                    {save position handle for most local scope}
          found);                      {returned TRUE if name existed here}
        end
      else begin                       {not looking in table where symbol goes later}
        string_hash_pos_lookup (       {look up name in output symbol table}
          scope_p^.hash_out_h,         {hash table handle}
          namec,                       {symbol name}
          pos,                         {returned hash table position handle}
          found);                      {returned TRUE if name existed here}
        end
      ;
    if found then begin                {name already exists ?}
      sys_stat_set (                   {set STAT to indicate name already exists}
        sst_subsys_k, sst_stat_sym_found_k, stat);
      sys_stat_parm_vstr (namec, stat); {pass symbol name to error code}
      return;                          {return with SYMBOL ALREADY EXISTS status}
      end;
    scope_p := scope_p^.parent_p;      {advance to next most global scope}
    end;                               {back and look for symbol in new scope}
{
*   The symbol exists in none of the currently active scopes.  POS_CURR is the
*   hash table position handle to where the symbol goes in the current scope.
}
  string_hash_ent_add (                {add name to hash table}
    pos_curr,                          {position handle of where to add name}
    name_p,                            {returned pointing to name stored in table}
    sym_pp);                           {returned pointing to start of user data area}
  sym_pp^ := nil;                      {init to this name has no symbol descriptor}
  end;
