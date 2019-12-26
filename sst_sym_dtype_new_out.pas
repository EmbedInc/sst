{   Subroutine SST_SYM_DTYPE_NEW_OUT (NAME,RENAME,DTYPE,SIZE,DT_P,STAT)
*
*   Create a new symbol that is a data type.  The symbol will go into the output
*   symbol table at the current scope.
*
*   NAME is the raw symbol name.  It may be altered to create a unique name if
*     MAKE_UNIQUE is set to true.  In either case, the current character case
*     rule will be applied to it.
*
*   RENAME  -  Rename stategy flag.
*
*   DTYPE is the base data type for the new data type descriptor.
*
*   SIZE is the storage size of the new data type.  It will be initialized
*     assuming natural alignment.
*
*   DT_P will be returned pointing to the new data type descriptor.  The
*     descriptor will be initialized as far as possible.
*
*   STAT is the returned completion status code.  It will be set to the
*     SST_STAT_SYM_PREV_DEF_K status if the symbol name was already in the symbol
*     table at or above the current scope, and MAKE_UNIQUE was set to FALSE.
}
module sst_SYM_DTYPE_NEW_OUT;
define sst_sym_dtype_new_out;
%include 'sst2.ins.pas';

procedure sst_sym_dtype_new_out (      {create output symbol that is a data type}
  in      name: univ string_var_arg_t; {name of symbol to create}
  in      rename: sst_rename_k_t;      {re-name strategy flag}
  in      dtype: sst_dtype_k_t;        {which base data type it is}
  in      size: sys_int_adr_t;         {data type size in machine addresses}
  out     dt_p: sst_dtype_p_t);        {points to new data type descriptor}

var
  namef: string_var80_t;               {final name used after conversions}
  name_p: string_var_p_t;              {points to name stored in hash table}
  pos: string_hash_pos_t;              {hash table position handle}
  sym_pp: sst_symbol_pp_t;             {points to hash table user data area}
  sym_p: sst_symbol_p_t;               {points to symbol descriptor for this name}

begin
  namef.len := sizeof(namef.str);      {init local VAR string}

  sst_w.name^ (                        {get hash handle for new symbol name}
    name.str, name.len,                {raw input name}
    sst_config.suffix_dtype.str, sst_config.suffix_dtype.len, {suffix name}
    rename,                            {re-name strategy flag}
    namef,                             {final resulting name}
    pos);                              {hash table position handle where name goes}
  string_hash_ent_add (pos, name_p, sym_pp); {add name to hash table}
  sst_mem_alloc_scope (sizeof(sym_p^), sym_p); {alloc symbol descriptor}
  sym_pp^ := sym_p;                    {point hash table entry to symbol descriptor}
  sym_p^.name_in_p := nil;             {partially initialize new symbol descriptor}
  sym_p^.name_out_p := name_p;
  sym_p^.next_p := nil;
  sym_p^.char_h.crange_p := nil;
  sym_p^.scope_p := sst_scope_p;
  sym_p^.flags := [];
{
*   The symbol has been added to the symbol table.  SYM_P is pointing to the
*   new symbol descriptor.  The symbol descriptor has been partially initialized.
}
  sym_p^.symtype := sst_symtype_dtype_k; {symbol is a data type}
  sst_mem_alloc_scope (sizeof(dt_p^), dt_p); {allocate mem for new data type}
  sym_p^.dtype_dtype_p := dt_p;        {point symbol descriptor to data type desc}
  dt_p^.symbol_p := sym_p;             {init data type descriptor as far as possible}
  dt_p^.dtype := dtype;
  dt_p^.bits_min := sst_config.bits_adr * size;
  dt_p^.align_nat := size;
  dt_p^.align := size;
  dt_p^.size_used := size;
  dt_p^.size_align := size;
  end;
