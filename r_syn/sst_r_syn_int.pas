{   Subroutine SST_R_SYN_INT (SYM_P)
*
*   Create new integer variable in the current scope.  SYM_P will be returned
*   pointing to the symbol descriptor for the new variable.
}
module sst_r_syn_int;
define sst_r_syn_int;
%include 'sst_r_syn.ins.pas';

procedure sst_r_syn_int (              {make new interger variable}
  out     sym_p: sst_symbol_p_t);      {pointer to symbol descriptor of new var}
  val_param;

var
  name: string_var32_t;                {name of new variable}
  token: string_var16_t;               {scratch token for number conversion}
  stat: sys_err_t;

begin
  name.max := sizeof(name.str);        {init local var strings}
  token.max := sizeof(token.str);

  string_vstring (name, 'i', 1);       {set static part of variable name}
  string_f_int (token, seq_int);       {make sequence number string}
  seq_int := seq_int + 1;              {update sequence number to use next time}
  string_append (name, token);         {add sequence number to variable name}

  sst_symbol_new_name (name, sym_p, stat);
  sys_error_abort (stat, '', '', nil, 0);

  sym_p^.symtype := sst_symtype_var_k;
  sym_p^.flags := [sst_symflag_def_k];
  sym_p^.var_dtype_p := sym_int_p^.dtype_dtype_p;
  sym_p^.var_val_p := nil;
  sym_p^.var_arg_p := nil;
  sym_p^.var_proc_p := nil;
  sym_p^.var_com_p := nil;
  sym_p^.var_next_p := nil;
  end;
