{   Subroutine SST_W_C_IMPLICIT_VAR (DTYPE,V)
*
*   Create an implicit variable.  DTYPE is the descriptor for the data type of
*   the new variable.  V is filled in to be the descriptor for the new variable.
*   The variable will be declared, and installed in the symbol table.
}
module sst_w_c_IMPLICIT_VAR;
define sst_w_c_implicit_var;
%include 'sst_w_c.ins.pas';

procedure sst_w_c_implicit_var (       {create an implicit variable}
  in      dtype: sst_dtype_t;          {data type descriptor for new variable}
  out     v: sst_var_t);               {filled in "variable" desc for new variable}

begin
  sst_sym_var_new_out (                {create var and install in symbol table}
    dtype, v.mod1.top_sym_p);
  sst_w_c_symbol (v.mod1.top_sym_p^);  {declare the variable}

  v.dtype_p := addr(dtype);            {fill in rest of var descriptor}
  v.rwflag := [sst_rwflag_read_k, sst_rwflag_write_k];
  v.vtype := sst_vtype_var_k;
  v.mod1.next_p := nil;
  v.mod1.modtyp := sst_var_modtyp_top_k;
  v.mod1.top_str_h.first_char.crange_p := nil;
  v.mod1.top_str_h.first_char.ofs := 0;
  v.mod1.top_str_h.last_char.crange_p := nil;
  v.mod1.top_str_h.last_char.ofs := 0;
  end;
