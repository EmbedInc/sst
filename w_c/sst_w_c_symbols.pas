{   Subroutine SST_W_C_SYMBOLS (GLOBAL_ONLY)
*
*   Write the declaration for all symbols that need it in the current scope.
*
*   As much as possible, the symbols will be written in the order of
*   constants, data types, common blocks, procedures, and variables.
*
*   Only global symbols that are actually defined here (and any dependencies)
*   will be written when GLOBAL_ONLY is TRUE.
}
module sst_w_c_SYMBOLS;
define sst_w_c_symbols;
%include 'sst_w_c.ins.pas';

const
  n_symtypes = 8;                      {number of entries in SYMTYPE array}

var
  symtype:                             {the symbol types to declare, in order}
    array[1..n_symtypes] of sst_symtype_k_t :=
    [ sst_symtype_const_k,
      sst_symtype_dtype_k,
      sst_symtype_dtype_k,
      sst_symtype_com_k,
      sst_symtype_proc_k,
      sst_symtype_var_k,
      sst_symtype_abbrev_k,
      sst_symtype_label_k              {not declared, but need to be named}
      ]
    ;

procedure sst_w_c_symbols (            {declare all used symbols not decl before}
  in      global_only: boolean);       {declare only global symbols def here if TRUE}

var
  pos: string_hash_pos_t;              {position handle into hash table}
  name_p: univ_ptr;                    {unused subroutine argument}
  sym_p: sst_symbol_p_t;               {pointer to to current symbol to write out}
  sym_pp: sst_symbol_pp_t;             {pointer to hash table user data area}
  dt_p: sst_dtype_p_t;                 {pointer to base data type if sym is dtype}
  i: sys_int_machine_t;                {loop counter}
  found: boolean;                      {TRUE if hash table entry was found}
  pop: boolean;                        {TRUE if need to pop writing position}
  pnt_ok: boolean;                     {TRUE if OK to request pointer data types}

begin
  if not global_only then begin        {declaring all symbols in this scope ?}
    sst_scope_unused_show (sst_scope_p^); {complain about unused symbols}
    end;

  pop := false;                        {init to no position pop needed at end}
  if global_only then begin            {declare only global symbols defined here ?}
    sst_w_c_header_decl (sment_type_declg_k, pop);
    end;

  pnt_ok := false;                     {init to not do pointer data types yet}
{
*   Actually declare the used symbols at this level for real.  As much as
*   possible, the symbols will be declared in the order given in the
*   SYMTYPE array, above.
}
  for i := 1 to n_symtypes do begin    {once for each type of symbol to look for}
    sst_w.blank_line^;
    string_hash_pos_first (sst_scope_p^.hash_h, pos, found); {get pos for first sym}
    while found do begin               {once for each hash table entry}
      string_hash_ent_atpos (pos, name_p, sym_pp); {get pointer to user data area}
      sym_p := sym_pp^;                {get pointer to symbol descriptor}
      dt_p := nil;                     {init to symbol is not a data type}
      if sym_p^.symtype = sst_symtype_dtype_k then begin {symbol is a data type ?}
        dt_p := sym_p^.dtype_dtype_p;
        while ((dt_p <> nil) and then (dt_p^.dtype = sst_dtype_copy_k))
          do dt_p := dt_p^.copy_dtype_p;
        end;                           {done handling symbol is a data type}
      if                               {declare this symbol now ?}
          (sym_p^.symtype = symtype[i]) and {the kind of symbol we are looking for ?}
          (not (sst_symflag_written_k in sym_p^.flags)) and {not already written ?}
          ( (sst_symflag_used_k in sym_p^.flags) or {symbol is used ?}
            sst_writeall or            {supposed to write all symbols ?}
            sst_char_from_ins(sym_p^.char_h) or {from the special include file ?}
            (                          {global symbol defined here ?}
              (sst_symflag_global_k in sym_p^.flags) and
              (not (sst_symflag_extern_k in sym_p^.flags))
              )
            ) and
          ( (not global_only) or
            (sst_symflag_global_k in sym_p^.flags) {symbol globally known ?}
            ) and
          ( (sym_p^.symtype <> sst_symtype_dtype_k) or {suppressed pointer dtype ?}
            (dt_p = nil) or
            pnt_ok or else
            (dt_p^.dtype <> sst_dtype_pnt_k)
            )
          then begin
        sst_w_c_symbol (sym_p^);       {declare symbol}
        end;
      string_hash_pos_next (pos, found); {advance to next hash table entry}
      end;                             {back and do this new hash table entry}
    pnt_ok := pnt_ok or (symtype[i] = sst_symtype_dtype_k); {pointers OK next time ?}
    end;                               {back to look for next symbol type}

  if pop then begin                    {need to pop writing position ?}
    sst_w_c_pos_pop;
    end;
  end;
