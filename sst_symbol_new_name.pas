{   Subroutine SST_SYMBOL_NEW_NAME (NAME,SYMBOL_P,STAT)
*
*   Add a new symbol to the current scope.  It is an error if the symbol
*   already exists in that name space, although it may exists in more global
*   name spaces.  NAME is the name of the new symbol to create.
*   SYMBOL_P is returned pointing to the newly created data block for the symbol.
*   STAT is returned with an error if the symbol already exists in the current
*   scope.
*
*   As a special case, an intrinsic symbol may be re-used if the current scope
*   is the root scope.  This is to support include file translations.
*
*   Whether the symbol name existed previously or not, SYMBOL_P is still returned
*   pointing to the symbol descriptor.
}
module sst_SYMBOL_NEW_NAME;
define sst_symbol_new_name;
%include 'sst2.ins.pas';

procedure sst_symbol_new_name (        {add symbol to curr scope given symbol name}
  in      name: univ string_var_arg_t; {name of new symbol}
  out     symbol_p: sst_symbol_p_t;    {points to new symbol descriptor}
  out     stat: sys_err_t);            {completion status code}

var
  pos: string_hash_pos_t;              {handle to position in hash table}
  name_p: string_var_p_t;              {points to symbol name in hash table entry}
  found: boolean;                      {TRUE if symbol previously existed}
  sym_pp: sst_symbol_pp_t;             {points to hash entry user data area}
  fnam: string_treename_t;             {file name passed to a message}
  lnum: sys_int_machine_t;             {line number passed to a message}

label
  init_sym;

begin
  fnam.max := sizeof(fnam.str);
  sys_error_none (stat);               {init STAT to indicate no error}

  string_hash_pos_lookup (             {get position for name in hash table}
    sst_scope_p^.hash_h,               {handle to hash table for current scope}
    name,                              {name to find position for}
    pos,                               {returned position for this name}
    found);                            {TRUE if symbol already exists}

  if found then begin                  {symbol is already here ?}
    string_hash_ent_atpos (pos, name_p, sym_pp); {get pointer to symbol data}
    symbol_p := sym_pp^;               {get pointer to symbol descriptor}
    if symbol_p^.symtype = sst_symtype_illegal_k {previously made by not defined ?}
      then return;
    if                                 {intrinsic symbol in global scope ?}
        (sst_symflag_intrinsic_in_k in symbol_p^.flags) and
        (sst_scope_p = sst_scope_root_p)
      then goto init_sym;              {re-use this symbol descriptor}
    sys_stat_set (sst_subsys_k, sst_stat_sym_prev_def_k, stat); {set error code}
    sst_charh_info (symbol_p^.char_h, fnam, lnum);
    sys_stat_parm_vstr (name, stat);
    sys_stat_parm_int (lnum, stat);
    sys_stat_parm_vstr (fnam, stat);
    return;                            {return with error}
    end;

  string_hash_ent_add (pos, name_p, sym_pp); {create hash table entry for new symbol}

  util_mem_grab (                      {allocate memory for symbol descriptor}
    sizeof(symbol_p^),                 {amount of memory to allocate}
    sst_scope_p^.mem_p^,               {parent memory context}
    false,                             {we won't need to individually release mem}
    symbol_p);                         {returned pointer to new memory area}
  sym_pp^ := symbol_p;                 {save pointer to symbol block in hash entry}

init_sym:                              {jump here to reset old symbol descriptor}
  with symbol_p^: sym do begin         {SYM is symbol data block}
    sym.name_in_p := name_p;           {init as much of symbol data as we can}
    sym.name_out_p := nil;             {indicate output name not chosen yet}
    sym.next_p := nil;
    sym.char_h.crange_p := nil;
    sym.scope_p := sst_scope_p;
    sym.symtype := sst_symtype_illegal_k; {init to illegal symbol}
    sym.flags := [];
    end;                               {done with SYM abbreviation}
  end;
