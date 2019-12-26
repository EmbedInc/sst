{   Module of wrapper routines that make various forms of common expressions.
}
module sst_exp_make;
define sst_exp_make_var;
%include 'sst2.ins.pas';
{
******************************************
}
function sst_exp_make_var (            {make expression that references a variable}
  in      sym: sst_symbol_t)           {symbol of variable to make expression from}
  :sst_exp_p_t;                        {returned pointer to new expression}

var
  var_p: sst_var_p_t;                  {scratch pointer to variable descriptor}
  exp_p: sst_exp_p_t;                  {pointer to returned expression}

begin
{
*   Create variable descriptor for reference to SYM.
}
  sst_mem_alloc_scope (sizeof(var_p^), var_p); {allocate mem for var descriptor}

  var_p^.mod1.next_p := nil;
  var_p^.mod1.modtyp := sst_var_modtyp_top_k;
  var_p^.mod1.top_str_h.first_char := sym.char_h;
  var_p^.mod1.top_str_h.last_char := sym.char_h;
  var_p^.mod1.top_sym_p := addr(sym);
  var_p^.dtype_p := sym.var_dtype_p;
  if sym.var_arg_p = nil
    then begin                         {variable is not a dummy argument}
      var_p^.rwflag := [sst_rwflag_read_k, sst_rwflag_write_k];
      end
    else begin                         {variable is a dummy argument}
      var_p^.rwflag := sym.var_arg_p^.rwflag_int;
      end
    ;
  var_p^.vtype := sst_vtype_var_k;
{
*   Create expression descriptor.
}
  sst_mem_alloc_scope (sizeof(exp_p^), exp_p); {allocate mem for exp descriptor}

  exp_p^.term1.next_p := nil;
  exp_p^.term1.op2 := sst_op2_none_k;
  exp_p^.term1.op1 := sst_op1_none_k;
  exp_p^.term1.ttype := sst_term_var_k;
  exp_p^.term1.str_h := var_p^.mod1.top_str_h;
  exp_p^.term1.dtype_p := var_p^.dtype_p;
  exp_p^.term1.dtype_hard := true;
  exp_p^.term1.val_eval := false;
  exp_p^.term1.val_fnd := false;
  exp_p^.term1.rwflag := var_p^.rwflag;
  exp_p^.term1.var_var_p := var_p;

  exp_p^.str_h := exp_p^.term1.str_h;
  exp_p^.dtype_p := exp_p^.term1.dtype_p;
  exp_p^.dtype_hard := exp_p^.term1.dtype_hard;
  exp_p^.val_eval := false;
  exp_p^.val_fnd := false;
  exp_p^.rwflag := exp_p^.term1.rwflag;

  sst_exp_eval (exp_p^, false);        {evaluate expression}

  sst_exp_make_var := exp_p;           {pass back expression pointer}
  end;
