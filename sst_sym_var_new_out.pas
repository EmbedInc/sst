{   Subroutine SST_SYM_VAR_NEW_OUT (DTYPE,SYM_P)
*
*   Create a new output symbol that is a variable.  The variable name will be
*   inferred from the data type, and adjusted as necessary to make it unique.
*   DTYPE is the descriptor for the data type of the new variable.  SYM_P
*   is returned pointing to the new symbol descriptor.  The symbol will be
*   installed in the output symbol table at the current scope.
}
module sst_SYM_VAR_NEW_OUT;
define sst_sym_var_new_out;
%include 'sst2.ins.pas';

procedure sst_sym_var_new_out (        {create output symbol that is a variable}
  in      dtype: sst_dtype_t;          {descriptor for variable's data type}
  out     sym_p: sst_symbol_p_t);      {points to newly created symbol descriptor}

var
  name: string_var80_t;                {orignal variable name attempt}
  namef: string_var80_t;               {variable name guaranteed to be unique}
  dt_p: sst_dtype_p_t;                 {points to base data type descriptor}
  dt: sst_dtype_k_t;                   {base data type ID}
  suffix: string_var4_t;               {suffix to append to variable name}
  pos: string_hash_pos_t;              {hash table position handle}
  sym_pp: sst_symbol_pp_t;             {points to hash table user data area}

begin
  name.max := sizeof(name.str);        {init local var strings}
  namef.max := sizeof(namef.str);
  suffix.max := sizeof(suffix.str);
  suffix.len := 0;
  name.len := 0;
{
*   Pick a name based on the data type of the implicit variable.
}
  sst_dtype_resolve (dtype, dt_p, dt); {get base data type from DTYPE}
  case dt of                           {what kind of data type is this ?}
sst_dtype_int_k: string_appendn (name, 'int', 3);
sst_dtype_enum_k: string_appendn (name, 'enum', 4);
sst_dtype_float_k: begin
      if dt_p^.size_used = sst_config.float_single_p^.size_used
        then string_appendn (name, 'sgl', 3)
        else if dt_p^.size_used = sst_config.float_double_p^.size_used
          then string_appendn (name, 'dbl', 3)
          else string_appendn (name, 'flt', 3)
          ;
        ;
      end;
sst_dtype_bool_k: string_appendn (name, 'bool', 4);
sst_dtype_char_k: string_appendn (name, 'chr', 3);
sst_dtype_rec_k: string_appendn (name, 'rec', 3);
sst_dtype_array_k: begin
      if dt_p^.ar_string
        then string_appendn (name, 'str', 3) {string, special case of an array}
        else string_appendn (name, 'arr', 3); {non-string array}
      end;
sst_dtype_set_k: string_appendn (name, 'set', 3);
sst_dtype_range_k: string_appendn (name, 'rng', 5);
sst_dtype_proc_k: string_appendn (name, 'proc', 4);
sst_dtype_pnt_k: string_appendn (name, 'pnt', 3);
otherwise
    string_appendn (name, 'xxx', 3);
    end;
{
*   NAME contains the raw name for this variable.
}
  string_appendn (name, '_', 1);       {this helps distinguish implicit variables}
  if dt = sst_dtype_pnt_k then begin   {variable is a pointer ?}
    string_appendn (suffix, '_p', 2);  {set variable name suffix}
    end;
  sst_w.name^ (                        {make final variable name}
    name.str, name.len,                {starting variable name}
    suffix.str, suffix.len,            {variable name suffix}
    sst_rename_all_k,                  {make unique name over all visible scopes}
    namef,                             {returned final variable name}
    pos);                              {returned hash table position for name}
  sst_mem_alloc_scope (sizeof(sym_p^), sym_p); {allocate symbol descriptor}
  string_hash_ent_add (pos, sym_p^.name_out_p, sym_pp); {add name to symbol table}
  sym_pp^ := sym_p;                    {point hash table entry to symbol descriptor}

  sym_p^.name_in_p := nil;             {set up symbol descriptor}
  sym_p^.next_p := nil;
  sym_p^.char_h.crange_p := nil;
  sym_p^.scope_p := sst_scope_p;
  sym_p^.symtype := sst_symtype_var_k;
  sym_p^.flags := [sst_symflag_created_k, sst_symflag_def_k];
  sym_p^.var_dtype_p := addr(dtype);
  sym_p^.var_val_p := nil;
  sym_p^.var_arg_p := nil;             {this variable is not a dummy argument}
  sym_p^.var_proc_p := nil;
  sym_p^.var_com_p := nil;
  end;
