{   Subroutine SST_SYM_VAR (SYM, VAR_P)
*
*   Create a new variable descriptor that references the simple variable SYM.
*   SYM must be the symbol of a simple variable.
}
module sst_sym_var;
define sst_sym_var;
%include 'sst2.ins.pas';

procedure sst_sym_var (                {make variable descriptor from simple var symbol}
  in      var sym: sst_symbol_t;       {symbol of a simple variable}
  out     var_p: sst_var_p_t);         {returned pointer to new variable descriptor}
  val_param;

label
  notvar;

begin
{
*   Validate the symbol.  It must be a simple variable.
}
  if sym.symtype <> sst_symtype_var_k
    then goto notvar;
{
*   Create a new variable descriptor and fill it in.
}
  sst_mem_alloc_scope (                {allocate memory for the new var descriptor}
    sizeof(var_p^), var_p);

  var_p^.mod1.next_p := nil;           {no subsequent modifier}
  var_p^.mod1.modtyp := sst_var_modtyp_top_k; {this is top modifier}
  var_p^.mod1.top_str_h.first_char := sym.char_h;
  var_p^.mod1.top_str_h.last_char := sym.char_h;
  var_p^.mod1.top_sym_p := addr(sym);  {point to the symbol}

  var_p^.dtype_p := sym.var_dtype_p;   {data type}
  var_p^.rwflag :=                     {allow reading and writing of variable}
    [sst_rwflag_read_k, sst_rwflag_write_k];
  var_p^.vtype := sst_vtype_var_k;     {reference is to a variable}
  return;                              {success, normal return point}

notvar:                                {symbol is not var of the right type}
  writeln ('INTERNAL ERROR: Symbol "', sym.name_in_p^.str:sym.name_in_p^.len,
    '" is not simple var in SST_SYM_VAR.');
  sys_bomb;
  end;
