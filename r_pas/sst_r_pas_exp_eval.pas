{   Subroutine SST_R_PAS_EXP_EVAL (VAL)
*
*   Evaluate an EXPRESSION syntax.  It must be possible to evaluate the expression
*   at compile time.  VAL is the returned expression value.
}
module sst_r_pas_EXP_EVAL;
define sst_r_pas_exp_eval;
%include 'sst_r_pas.ins.pas';

procedure sst_r_pas_exp_eval (         {find constant value of EXPRESSION syntax}
  out     val: sst_var_value_t);       {value of EXPRESSION}

var
  tag: sys_int_machine_t;              {syntax tag ID}
  str_h: syo_string_t;                 {handle to string associated with TAG}
  tag_op: sys_int_machine_t;           {syntax tag for operator between items}
  str_h_op: syo_string_t;              {handle to string associated with TAG_OP}
  val2: sst_var_value_t;               {for ITEM after current operator}

label
  loop, do_float, dtype_mismatch, op_mismatch;

begin
  syo_level_down;                      {down to EXPRESSION syntax}
  syo_get_tag_msg (tag, str_h, 'sst_pas_read', 'constant_bad', nil, 0);
  case tag of
1: begin                               {first operand is EXPRESSION}
      sst_r_pas_exp_eval (val);
      end;
2: begin                               {first operand is ITEM}
      sst_r_pas_item_eval (val);
      end;
otherwise
    syo_error_tag_unexp (tag, str_h);
    end;
{
*   VAL has been initialized from the first (and mandatory) item
*   in the expression.  There may now be any number of operator/item pairs
*   following.  For each one, get the new item value, apply the operator, and
*   put the result back into VAL.  VAL will always be the result of all
*   previous work.
}
loop:                                  {back here each new operator/item pair}
  syo_get_tag_msg (tag_op, str_h_op, 'sst_pas_read', 'constant_bad', nil, 0); {get op tag}
  if tag_op = syo_tag_end_k then begin {no more tags in EXPRESSION ?}
    syo_level_up;                      {back up from EXPRESSION syntax}
    return;
    end;

  syo_get_tag_msg (tag, str_h, 'sst_pas_read', 'constant_bad', nil, 0); {get second ITEM tag}
  case tag of
1: begin                               {this operand is EXPRESSION}
      sst_r_pas_exp_eval (val2);
      end;
2: begin                               {this operand is ITEM}
      sst_r_pas_item_eval (val2);
      end;
otherwise
    syo_error_tag_unexp (tag, str_h);
    end;
{
*   VAL declares the value of the first operand, and VAL2 declares
*   the value of the second operand.  TAG_OP is set to the tag value of the
*   operand in between.
}
  case val.dtype of                    {what data type is the first operand ?}
{
****************************************
*
*   First operand is INTEGER.
}
sst_dtype_int_k: begin
  case val2.dtype of                   {what is data type of second operand ?}

sst_dtype_int_k: begin                 {INTEGER op INTEGER}
  case tag_op of                       {what operand was used indicated ?}
1: begin                               {+}
  val.int_val := val.int_val + val2.int_val;
  end;
2: begin                               {-}
  val.int_val := val.int_val - val2.int_val;
  end;
3: begin                               {**}
  val.int_val := val.int_val ** val2.int_val;
  end;
4: begin                               {*}
  val.int_val := val.int_val * val2.int_val;
  end;
5: begin                               {/}
  val.float_val := val.int_val / val2.int_val;
  val.dtype := sst_dtype_float_k;
  end;
6: begin                               {DIV}
  val.int_val := val.int_val div val2.int_val;
  end;
7: begin                               {MOD}
  val.int_val := val.int_val mod val2.int_val;
  end;
8: begin                               {&}
  val.int_val := val.int_val & val2.int_val;
  end;
9: begin                               {!}
  val.int_val := val.int_val ! val2.int_val;
  end;
10: begin                              {=}
  val.bool_val := val.int_val = val2.int_val;
  val.dtype := sst_dtype_bool_k;
  end;
11: begin                              {<>}
  val.bool_val := val.int_val <> val2.int_val;
  val.dtype := sst_dtype_bool_k;
  end;
12: begin                              {>=}
  val.bool_val := val.int_val >= val2.int_val;
  val.dtype := sst_dtype_bool_k;
  end;
13: begin                              {>}
  val.bool_val := val.int_val > val2.int_val;
  val.dtype := sst_dtype_bool_k;
  end;
14: begin                              {<=}
  val.bool_val := val.int_val <= val2.int_val;
  val.dtype := sst_dtype_bool_k;
  end;
15: begin                              {<}
  val.bool_val := val.int_val < val2.int_val;
  val.dtype := sst_dtype_bool_k;
  end;
otherwise
    goto op_mismatch;
    end;                               {end of INTEGER op INTEGER operator types}
  end;                                 {end of INTEGER op INTEGER}

sst_dtype_float_k: begin               {INTEGER op FLOAT}
  val.float_val := val.int_val;        {convert first operand to floating point}
  val.dtype := sst_dtype_float_k;
  goto do_float;                       {process two floating point operands}
  end;                                 {end of INTEGER op FLOAT case}

otherwise
    goto dtype_mismatch;
    end;                               {done with second operand data type cases}
  end;                                 {done with first operand is INTEGER case}
{
****************************************
*
*   First operand is floating point.
}
sst_dtype_float_k: begin
  case val2.dtype of                   {what is data type of second operand ?}

sst_dtype_int_k: begin                 {FLOAT op INTEGER}
  val2.float_val := val2.int_val;      {convert second operand to floating point}
  val2.dtype := sst_dtype_float_k;
  goto do_float;                       {process as two floating point operands}
  end;                                 {end of FLOAT op INTEGER}

sst_dtype_float_k: begin               {FLOAT op FLOAT}
  goto do_float;                       {handle floating point operations}
  end;                                 {end of FLOAT op FLOAT}

otherwise
    goto dtype_mismatch;
    end;                               {done with second operand data type cases}
  end;                                 {done with first operand is FLOAT case}
{
****************************************
*
*   First operand is BOOLEAN.
}
sst_dtype_bool_k: begin
  case val2.dtype of                   {what is data type of second operand ?}

sst_dtype_bool_k: begin                {BOOLEAN op BOOLEAN}
  case tag_op of                       {what operand was used indicated ?}
10: begin                              {=}
  val.bool_val := val.bool_val = val2.bool_val;
  end;
11: begin                              {<>}
  val.bool_val := val.bool_val <> val2.bool_val;
  end;
16: begin                              {AND}
  val.bool_val := val.bool_val AND val2.bool_val;
  end;
17: begin                              {OR}
  val.bool_val := val.bool_val OR val2.bool_val;
  end;
otherwise
    goto op_mismatch;
    end;                               {end of BOOLEAN op BOOLEAN operator types}
  end;                                 {end of BOOLEAN op BOOLEAN}

otherwise
    goto dtype_mismatch;
    end;                               {done with second operand data type cases}
  end;                                 {done with first operand is BOOLEAN case}
{
****************************************
*
*   Illegal or unsupported data type for first operand of diadic pair.
}
otherwise
    goto op_mismatch;
    end;                               {end of first operand data type cases}
  goto loop;

do_float:                              {jump here after convert to FLOAT op FLOAT}
  case tag_op of                       {what operand was used indicated ?}
1: begin                               {+}
  val.float_val := val.float_val + val2.float_val;
  end;
2: begin                               {-}
  val.float_val := val.float_val - val2.float_val;
  end;
3: begin                               {**}
  val.float_val := val.float_val ** val2.float_val;
  end;
4: begin                               {*}
  val.float_val := val.float_val * val2.float_val;
  end;
5: begin                               {/}
  val.float_val := val.float_val / val2.float_val;
  end;
10: begin                              {=}
  val.bool_val := val.float_val = val2.float_val;
  val.dtype := sst_dtype_bool_k;
  end;
11: begin                              {<>}
  val.bool_val := val.float_val <> val2.float_val;
  val.dtype := sst_dtype_bool_k;
  end;
12: begin                              {>=}
  val.bool_val := val.float_val >= val2.float_val;
  val.dtype := sst_dtype_bool_k;
  end;
13: begin                              {>}
  val.bool_val := val.float_val > val2.float_val;
  val.dtype := sst_dtype_bool_k;
  end;
14: begin                              {<=}
  val.bool_val := val.float_val <= val2.float_val;
  val.dtype := sst_dtype_bool_k;
  end;
15: begin                              {<}
  val.bool_val := val.float_val < val2.float_val;
  val.dtype := sst_dtype_bool_k;
  end;
otherwise
    goto op_mismatch;
    end;                               {end of FLOAT op FLOAT operator types}
  goto loop;                           {back and handle next operator/operand pair}

dtype_mismatch:                        {mismatched operand data types}
  syo_error (str_h, 'sst_pas_read', 'exp_dtype_mismatch', nil, 0);

op_mismatch:                           {operator is mismatched to operand data types}
  syo_error (str_h, 'sst_pas_read', 'exp_operand_mismatch', nil, 0);
  end;
