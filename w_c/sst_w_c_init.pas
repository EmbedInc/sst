{   Subroutine SST_W_C_INIT
*
*   Set up the back end call table to install the routines for writing C.
}
module sst_w_c_INIT;
define sst_w_c_init;
%include 'sst_w_c.ins.pas';

procedure sst_w_c_init;                {init back end state for writing C}

var
  i: sys_int_machine_t;                {loop counter}
  ent_p: string_chain_ent_p_t;         {points to current reserved name chain entry}
  name_p: string_var_p_t;              {unused subroutine return argument}
  sym_pp: sst_symbol_pp_t;             {unused subroutine return argument}
  stat: sys_err_t;
{
*************************************
*
*   Local subroutine INSTALL_DTYPE_NAME (DTYPE)
*
*   Install the name of the data type DTYPE in the output symbol table.  It is
*   not an error if the name is already in the symbol table.  It is assumed that
*   the data type descriptor is properly filled in, and that it points to
*   a symbol descriptor.
}
procedure install_dtype_name (
  in      dtype: sst_dtype_t);         {descriptor for data type to add to sym table}

var
  pos: string_hash_pos_t;              {position handle into a hash table}
  sym_pp: sst_symbol_pp_t;             {pointer to data area in hash table entry}
  name_p: string_var_p_t;              {unused subroutine return argument}
  found: boolean;                      {TRUE if name found in hash table}

begin
  string_hash_pos_lookup (             {get position for this name in hash table}
    sst_scope_p^.hash_out_h,           {handle to base output symbol table}
    dtype.symbol_p^.name_out_p^,       {name to find hash table position for}
    pos,                               {returned hash table position handle}
    found);                            {TRUE if name already in hash table}
  if not found then begin              {name not already in symbol table ?}
    string_hash_ent_add (pos, name_p, sym_pp); {add name to symbol table}
    sym_pp^ := dtype.symbol_p;         {point hash entry to name descriptor}
    end;
  end;
{
*************************************
*
*   Start of main routine.
}
begin
  sst_w.doit := addr(sst_w_c_doit);    {install routine to "run" the back end}
{
*   Put the basic machine data types into the output symbol table.  These
*   data types were declared in the config file, and were all set up by the
*   routine SST_CONFIG_OUT.  Each data type has a data type descriptor and
*   a symbol descriptor, although the symbol has not been entered in any
*   symbol table.
}
  for i := 1 to sst_config.n_size_int do begin {once for each base integer type}
    with sst_config.size_int[i]: csize do begin {CSIZE is this SIZE_INT entry}
      install_dtype_name (csize.dtype_p^); {install data type in symbol table}
      end;                             {done with CSIZE abbreviation}
    end;                               {back for next integer size}

  for i := 1 to sst_config.n_size_float do begin {once for each base float type}
    with sst_config.size_float[i]: csize do begin {CSIZE is this SIZE_FLOAT entry}
      install_dtype_name (csize.dtype_p^); {install data type in symbol table}
      end;                             {done with CSIZE abbreviation}
    end;                               {back for next floating point size}

  install_dtype_name (sst_dtype_uptr_p^);
  install_dtype_name (sst_dtype_bool_p^);
  install_dtype_name (sst_dtype_char_p^);
{
*   Put the reserved symbols into the symbol table.  No symbol descriptor
*   will be connected, so these only serve to prevent other symbols names
*   from being reserved names.
*
*   It is not an error if the reserved name is already in the output symbol table.
*   This allows listing all the reserved names without having to delete the ones
*   explicitly given as data type names, etc.
}
  ent_p := sst_config.reserve_p;       {init current chain entry to first}
  while ent_p <> nil do begin          {once for each entry in reserved names chain}
    sst_name_new_out (                 {add just the name to the symbol table}
      ent_p^.s,                        {symbol name}
      name_p,                          {pnt to name in hash entry, UNUSED}
      sym_pp,                          {pnt to hash entry data area, UNUSED}
      stat);
    ent_p := ent_p^.next_p;            {advance to next name in chain}
    end;                               {back and process this new chain entry}
{
*   Set the default SET sizes if none were explicitly given.  The C language
*   has no native SET data type, so set the SIZE_SET list to all the available
*   integer data types, and set SIZE_SET_MULTIPLE to the preferred machine
*   integer.
}
  if sst_config.n_size_set = 0 then begin {not SIZE_SET commands were used ?}
    for i := 1 to sst_config.n_size_int do begin {once for each integer size}
      sst_config.size_set[i].size :=   {copy data from this integer size}
        sst_config.size_int[i].size;
      sst_config.size_set[i].dtype_p :=
        sst_config.size_int[i].dtype_p;
      end;                             {back for next integer size}
    sst_config.n_size_set := sst_config.n_size_int; {set number of SET sizes}
    end;                               {done with defaults for SIZE_SET commands}

  if sst_config.size_set_multiple.size = 0 then begin {no SIZE_SET_MULTIPLE cmd ?}
    sst_config.size_set_multiple.size := {copy data from preferred machine integer}
      sst_config.int_machine_p^.size_used;
    sst_config.size_set_multiple.dtype_p :=
      sst_config.int_machine_p;
    end;                               {done with default for SIZE_SET_MULTIPLE}
{
*   Set the length of output lines before wrapping is required.
}
  sst_out.wrap_len := 80;              {max allowed output line size}
  string_vstring (sst_out.comm_start, '/* '(0), -1);
  string_vstring (sst_out.comm_end, ' */'(0), -1);
  sst_out.comm_pos := 40;              {default column for end of line comment start}
{
*   Initialize other state.
}
  decl_done := [];                     {no explicit declarations written yet}
  addr_cnt_ar := -1;                   {init how array identifiers are interpreted}
  array_mode := array_pnt_first_k;
  no_ibm_str_kluge := false;           {init to use IBM string constant kluge}
  end;
