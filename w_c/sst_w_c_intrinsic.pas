{   Subroutine SST_W_C_INTRINSIC (INTR)
*
*   Write out one of the symbols the C back end treats as if are intrinsic,
*   but that really do need to be declared.  This routine first makes sure
*   the appropriate declarations is made, if needed, and then writes the
*   symbol at the current writing position.
}
module sst_w_c_INTRINSIC;
define sst_w_c_intrinsic;
%include 'sst_w_c.ins.pas';

procedure sst_w_c_intrinsic (          {write name of intrinsic symbol}
  in      intr: intr_k_t);             {selects symbol, will be declared if needed}

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

begin
  case intr of                         {which intrinsic symbol is requested ?}
intr_nil_k: begin                      {NIL pointer value}
      sst_w_c_declare (decl_nil_k);
      sst_w.appendn^ ('nil', 3);
      end;
intr_true_k: begin                     {logical TRUE}
      sst_w_c_declare (decl_true_k);
      sst_w.appendn^ ('true', 4);
      end;
intr_false_k: begin                    {logical FALSE}
      sst_w_c_declare (decl_false_k);
      sst_w.appendn^ ('false', 5);
      end;
intr_nullset_k: begin                  {empty set value}
      sst_w_c_declare (decl_nullset_k);
      sst_w.appendn^ ('nullset', 7);
      end;
intr_unspec_int_k: begin
      sst_w_c_declare (decl_unspec_int_k);
      sst_w.appendn^ ('unspec_int', 10);
      end;
intr_unspec_enum_k: begin
      sst_w_c_declare (decl_unspec_enum_k);
      sst_w.appendn^ ('unspec_enum', 11);
      end;
intr_unspec_float_k: begin
      sst_w_c_declare (decl_unspec_float_k);
      sst_w.appendn^ ('unspec_float', 12);
      end;
intr_unspec_bool_k: begin
      sst_w_c_declare (decl_unspec_bool_k);
      sst_w.appendn^ ('unspec_bool', 11);
      end;
intr_unspec_char_k: begin
      sst_w_c_declare (decl_unspec_char_k);
      sst_w.appendn^ ('unspec_char', 11);
      end;
intr_unspec_set_k: begin
      sst_w_c_declare (decl_unspec_set_k);
      sst_w.appendn^ ('unspec_set', 10);
      end;
intr_unspec_pnt_k: begin
      sst_w_c_declare (decl_unspec_pnt_k);
      sst_w.appendn^ ('unspec_pnt', 10);
      end;
{
*   Integer math functions.
}
intr_abs_k: begin                      {absolute value, integer}
      sst_w_c_declare (decl_stdlib_k);
      sst_w.appendn^ ('abs', 3);
      end;
{
*   Floating point math intrinsics.
}
intr_atan_k: begin                     {arctangent, one argument}
      sst_w_c_declare (decl_math_k);
      sst_w.appendn^ ('atan', 4);
      end;
intr_atan2_k: begin                    {arctangent, two arguments}
      sst_w_c_declare (decl_math_k);
      sst_w.appendn^ ('atan2', 5);
      end;
intr_ceil_k: begin                     {to integer, round toward +infinity}
      sst_w_c_declare (decl_math_k);
      sst_w.appendn^ ('ceil', 4);
      end;
intr_cos_k: begin                      {cosine}
      sst_w_c_declare (decl_math_k);
      sst_w.appendn^ ('cos', 3);
      end;
intr_exp_k: begin                      {E ** arg}
      sst_w_c_declare (decl_math_k);
      sst_w.appendn^ ('exp', 3);
      end;
intr_fabs_k: begin                     {absolute value, floating point}
      sst_w_c_declare (decl_math_k);
      sst_w.appendn^ ('fabs', 4);
      end;
intr_floor_k: begin                    {to integer, round toward -infinity}
      sst_w_c_declare (decl_math_k);
      sst_w.appendn^ ('floor', 5);
      end;
intr_log_k: begin                      {natural log}
      sst_w_c_declare (decl_math_k);
      sst_w.appendn^ ('log', 3);
      end;
intr_pow_k: begin                      {arg1 ** arg2}
      sst_w_c_declare (decl_math_k);
      sst_w.appendn^ ('pow', 3);
      end;
intr_sin_k: begin                      {sine}
      sst_w_c_declare (decl_math_k);
      sst_w.appendn^ ('sin', 3);
      end;
intr_sqrt_k: begin                     {square root}
      sst_w_c_declare (decl_math_k);
      sst_w.appendn^ ('sqrt', 4);
      end;
intr_tan_k: begin                      {tangent}
      sst_w_c_declare (decl_math_k);
      sst_w.appendn^ ('tan', 3);
      end;
otherwise
    sys_msg_parm_int (msg_parm[1], ord(intr));
    sys_message_bomb ('sst_c_write', 'intrinsic_symbol_id_bad', msg_parm, 1);
    end;
  end;
