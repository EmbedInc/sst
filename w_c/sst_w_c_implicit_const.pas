{   Subroutine SST_W_C_IMPLICIT_CONST (DTYPE,VAL,SYM_P)
*
*   Create or reuse an implicit variable with a constant value.  If an
*   implicit variable exists with the same data type and value, then it
*   is reused.  Otherwise, a new variable is created.
}
module sst_w_c_IMPLICIT_CONST;
define sst_w_c_implicit_const;
%include 'sst_w_c.ins.pas';

procedure sst_w_c_implicit_const (     {create/reuse implicit constant variable}
  in      dtype: sst_dtype_t;          {data type descriptor for variable}
  in      val: sst_var_value_t;        {descriptor for variable's value}
  out     sym_p: sst_symbol_p_t);      {returned pointing to reused/new variable}

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  dt_p: sst_dtype_p_t;                 {points to base data type}
  exp_p: sst_exp_p_t;                  {points to value expression for new variable}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

label
  next_sym;

begin
  dt_p := addr(dtype);                 {resolve base data type of variable}
  while dt_p^.dtype = sst_dtype_copy_k do dt_p := dt_p^.copy_dtype_p;

  sym_p := frame_scope_p^.const_p;     {init curr symbol to first const implicit var}
  while sym_p <> nil do begin          {look thru all the existing const impl vars}
    if sym_p^.var_dtype_p <> dt_p      {data types don't match ?}
      then goto next_sym;
    if sym_p^.var_val_p = nil          {doesn't have init val ? (shouldn't happen)}
      then goto next_sym;
    if not sym_p^.var_val_p^.val_fnd   {init val not const ? (shouldn't happen)}
      then goto next_sym;
    with sym_p^.var_val_p^.val: symval do begin {SYMVAL is symbol's value descriptor}
      if symval.dtype <> val.dtype     {const value data types don't match ?}
        then goto next_sym;
      case val.dtype of
sst_dtype_int_k: begin
          if symval.int_val <> val.int_val then goto next_sym;
          end;
sst_dtype_enum_k: begin
          if symval.enum_p <> val.enum_p then goto next_sym;
          end;
sst_dtype_float_k: begin
          if symval.float_val <> val.float_val then goto next_sym;
          end;
sst_dtype_bool_k: begin
          if symval.bool_val <> val.bool_val then goto next_sym;
          end;
sst_dtype_char_k: begin
          if symval.char_val <> val.char_val then goto next_sym;
          end;
sst_dtype_array_k: begin
          if not string_equal(symval.ar_str_p^, val.ar_str_p^) then goto next_sym;
          end;
sst_dtype_set_k: begin
          goto next_sym;               {set values not implemented yet}
          end;
sst_dtype_pnt_k: begin
          if symval.pnt_dtype_p <> val.pnt_dtype_p then goto next_sym;
          if symval.pnt_exp_p <> val.pnt_exp_p then goto next_sym;
          end;
otherwise
        sys_msg_parm_int (msg_parm[1], ord(val.dtype));
        sys_message_bomb ('sst', 'dtype_unexpected', msg_parm, 1);
        end;
      end;                             {done with SYMVAL abbreviation}
    return;                            {SYM_P is pointing to reused variable}

next_sym:                              {jump here if curr symbol not reusable}
    sym_p := sym_p^.next_p;            {advance to next symbol in chain}
    end;                               {back and test this new symbol in chain}
{
*   None of the existing symbols matched the requirements for the new symbol.
*   Now create a new implicit variable.
}
  sst_sym_var_new_out (                {create variable and install in symbol table}
    dt_p^,                             {data type of new variable}
    sym_p);                            {returned pointing to new var descriptor}
  sst_mem_alloc_scope (sizeof(exp_p^), exp_p); {allocate mem for exp descriptor}

  exp_p^.str_h.first_char.crange_p := nil; {fill in const val expression descriptor}
  exp_p^.str_h.first_char.ofs := 0;
  exp_p^.str_h.last_char.crange_p := nil;
  exp_p^.str_h.last_char.ofs := 0;
  exp_p^.dtype_p := dt_p;
  exp_p^.dtype_hard := true;
  exp_p^.val_eval := true;
  exp_p^.val_fnd := true;
  exp_p^.val := val;
  exp_p^.rwflag := [sst_rwflag_read_k];

  exp_p^.term1.next_p := nil;
  exp_p^.term1.op2 := sst_op2_none_k;
  exp_p^.term1.op1 := sst_op1_none_k;
  exp_p^.term1.ttype := sst_term_const_k;
  exp_p^.term1.str_h := exp_p^.str_h;
  exp_p^.term1.dtype_p := dt_p;
  exp_p^.term1.dtype_hard := true;
  exp_p^.term1.val_eval := true;
  exp_p^.term1.val_fnd := true;
  exp_p^.term1.val := val;
  exp_p^.term1.rwflag := [sst_rwflag_read_k];

  sym_p^.var_val_p := exp_p;           {set expression as variable's initial value}
  sym_p^.flags := sym_p^.flags + [sst_symflag_static_k]; {flag variable as static}
  sst_w_c_symbol (sym_p^);             {declare variable and set initial value}
  sym_p^.next_p := frame_scope_p^.const_p; {link new var onto const impl var chain}
  frame_scope_p^.const_p := sym_p;
  end;
