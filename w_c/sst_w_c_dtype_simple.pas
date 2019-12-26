{   Subroutine SST_W_C_DTYPE_SIMPLE (DTYPE,NAME,PACK)
*
*   Write the data type definition for the data type descriptor DTYPE.
*   Only a "simple" data type will be written.  If necessary, a hidden
*   data type will be created, declared to the not-simple data type, and then
*   its name written here.  PACK is true to indicate that the data type is
*   a declaration for a field in a packed record.
*   NAME is the name of the symbol to declare with this data type.  The
*   symbol name will be written in the appropriate place in the data type
*   declaration.
*
*   This routine would be used when referencing the data type DTYPE.  It would
*   not be used when defining the data type DTYPE.
}
module sst_w_c_DTYPE_SIMPLE;
define sst_w_c_dtype_simple;
%include 'sst_w_c.ins.pas';

procedure sst_w_c_dtype_simple (       {write data type def, forced to be simple}
  in      dtype: sst_dtype_t;          {data type descriptor block}
  in      name: univ string_var_arg_t; {name of symbol to declare with this dtype}
  in      pack: boolean);              {TRUE if part of packed record}
  val_param;

var
  dt_p: sst_dtype_p_t;                 {points to base data type of DTYPE}
  dtp_p: sst_dtype_p_t;                {points to base pointed-to data type}
  sym_p: sst_symbol_p_t;               {points to name of data type, if any}
  scope_old_p: sst_scope_p_t;          {saved copy of current scope pointer}
  rename: string_var80_t;              {renamed NAME for recursive call}

label
  write_name, write_long;

begin
  rename.max := sizeof(rename.str);    {init local var string}

  sym_p := dtype.symbol_p;             {init pointer to data type's name}
  dt_p := addr(dtype);                 {init pointer to base data type descriptor}
  while dt_p^.dtype = sst_dtype_copy_k do begin {resolve base data type descriptor}
    if sym_p = nil                     {name not found yet ?}
      then sym_p := dt_p^.copy_symbol_p; {use copied name, if available}
    dt_p := dt_p^.copy_dtype_p;        {resolve one level of copy}
    if sym_p = nil                     {name not found yet ?}
      then sym_p := dt_p^.symbol_p;    {try to grab name at this level}
    end;                               {keep looping until DT_P^ is not a copy}
{
*   DT_P is pointing to the base data type descriptor.  SYM_P is pointing to
*   the highest level name symbol for the original data type, if one was found.
}
  if dt_p^.dtype = sst_dtype_undef_k then begin {data type is undefined ?}
    sst_w.appendn^ ('void', 4);
    goto write_name;
    end;
{
*   If this is a field in a packed record, and it is a simple data type that
*   will require a field width specifier, then call the general routine.
*   This routine contains the logic for writing the field width specifiers.
}
  if pack then begin                   {this is a field in a packed record ?}
    case dt_p^.dtype of                {what is base data type of field ?}
sst_dtype_int_k,
sst_dtype_enum_k,
sst_dtype_bool_k,
sst_dtype_char_k,
sst_dtype_set_k,
sst_dtype_range_k: goto write_long;    {these data types will require field widths}
      end;
    end;
{
*   Handle case where no name symbol was available anywhere.  If the data type
*   is a pointer, then add "*" to the front of NAME and call ourselves recursively
*   with the pointed-to data type.  Otherwise, give this data type a name,
*   declare it, and then use the new symbol name.
}
  if sym_p = nil then begin            {no name symbol available anywhere ?}
    if dt_p^.dtype = sst_dtype_proc_k  {DTYPE is data type for a routine ?}
      then goto write_long;            {write long form of routine data types}
    if dt_p^.dtype = sst_dtype_pnt_k then begin {data type is a pointer ?}
      dtp_p := dt_p^.pnt_dtype_p;      {resolve base pointed-to data type}
      while dtp_p^.dtype = sst_dtype_copy_k do dtp_p := dtp_p^.copy_dtype_p;
      if dtp_p^.dtype = sst_dtype_proc_k {DTYPE is pointer to routine ?}
        then goto write_long;          {write long form of routine pointer dtypes}
      rename.len := 0;
      string_append1 (rename, '*');
      string_append (rename, name);
      sst_w_c_dtype_simple (dt_p^.pnt_dtype_p^, rename, false); {do by recursive call}
      return;
      end;

    scope_old_p := sst_scope_p;        {save pointer to current scope}
    while sst_scope_p^.parent_p <> nil do begin {loop no further than top scope}
      if sst_scope_p^.symbol_p <> nil then begin {this scope has an owning symbol ?}
        case sst_scope_p^.symbol_p^.symtype of {what kind of symbol owns this scope ?}
sst_symtype_proc_k,
sst_symtype_prog_k,
sst_symtype_module_k: exit;            {this scope is global enough}
          end;
        end;
      sst_scope_p := sst_scope_p^.parent_p; {move to next most local scope}
      end;

    sst_mem_alloc_scope (sizeof(sym_p^), sym_p); {create new symbol descriptor}
    sym_p^.name_in_p := nil;
    sym_p^.name_out_p := nil;
    sym_p^.next_p := nil;
    sym_p^.char_h.crange_p := nil;
    sym_p^.char_h.ofs := 0;
    sym_p^.scope_p := sst_scope_p;
    sym_p^.symtype := sst_symtype_dtype_k;
    sym_p^.flags := [sst_symflag_def_k, sst_symflag_used_k, sst_symflag_created_k];
    sym_p^.dtype_dtype_p := dt_p;

    dt_p^.symbol_p := sym_p;           {link data type to its new symbol}
    sst_w_c_symbol (sym_p^);           {name and declare this data type symbol}
    sst_scope_p := scope_old_p;        {restore pointer to current scope}
    end;                               {SYM_P definately points to symbol descriptor}

  if                                   {need STRUCT or UNION keyword ?}
      (dt_p^.dtype = sst_dtype_rec_k) and {data type is record ?}
      (not (sst_symflag_written_k in sym_p^.flags)) {data type not fully declared ?}
      then begin
    if sst_rec_variant(dt_p^)
      then sst_w.appendn^ ('union', 5)
      else sst_w.appendn^ ('struct', 6);
    sst_w.delimit^;
    end;

  sst_w.append_sym_name^ (sym_p^);     {write data type's name}
write_name:                            {jump here to write just declare var name}
  if name.len > 0 then begin           {we have a name to declare ?}
    sst_w.delimit^;
    sst_w.append^ (name);              {write name of symbol being declared}
    end;
  return;

write_long:                            {jump here for "long" definition of dtype}
  sst_w_c_dtype (dt_p^, name, pack);   {write full data type declaration}
  end;
