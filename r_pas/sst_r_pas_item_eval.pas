{   Subroutine SST_R_PAS_ITEM_EVAL (VAL)
*
*   Evaluate an ITEM syntax.  The ITEM must resolve to a constant that can be
*   evaluated at compile time.  VAL is returned as the constant value.
}
module sst_r_pas_ITEM_EVAL;
define sst_r_pas_item_eval;
%include 'sst_r_pas.ins.pas';

procedure sst_r_pas_item_eval (        {find constant value of ITEM syntax}
  out     val: sst_var_value_t);       {returned value of ITEM}

var
  tag: sys_int_machine_t;              {syntax tag ID}
  str_h: syn_string_t;                 {handle to string associated with TAG}
  tag2: sys_int_machine_t;             {syntax tag to avoid corrupting TAG}
  str2_h: syn_string_t;                {string handle associated with TAG2}
  tag_unadic: sys_int_machine_t;       {tag for preceding unadic operator, if any}
  str_h_unadic: syn_string_t;          {handle to string associated with TAG_UNADIC}
  token: string_var80_t;               {scratch string for number conversion}
  str: string_var8192_t;               {for building string constant}
  size: sys_int_adr_t;                 {amount of memory needed}
  sym_p: sst_symbol_p_t;               {points to symbol from symbol table}
  msg_parm:                            {message parameter references}
    array[1..4] of sys_parm_msg_t;
  stat: sys_err_t;

label
  unadic_not_match;

begin
  token.max := sizeof(token.str);      {init local var strings}
  str.max := sizeof(str.str);

  syn_level_down;                      {down into ITEM syntax level}
  syn_get_tag_msg (tag_unadic, str_h_unadic, 'sst_pas_read', 'constant_bad', nil, 0);
  syn_get_tag_msg (tag, str_h, 'sst_pas_read', 'constant_bad', nil, 0);
  case tag of
{
********************************
*
*   Floating point number.
}
1: begin
  syn_get_tag_string (str_h, token);   {get string of real number}
  string_t_fp2 (token, val.float_val, stat); {make floating point from string}
  syn_error_abort (stat, str_h, 'sst_pas_read', 'constant_bad', nil, 0);
  val.dtype := sst_dtype_float_k;
  end;
{
********************************
*
*   Integer.
}
2: begin
  sst_r_pas_integer (val.int_val);     {get integer value}
  val.dtype := sst_dtype_int_k;        {set data type for this item}
  end;
{
********************************
*
*   Literal string.
}
3: begin
  sst_r_pas_lit_string (str);          {get value of literal string constant}
  if str.len = 1
    then begin                         {string has one char, make CHAR data type}
      val.char_val := str.str[1];
      val.dtype := sst_dtype_char_k;
      end
    else begin                         {not just one char, make STRING data type}
      size :=                          {amount of mem needed for string constant}
        sizeof(string_var4_t)          {size of string with 4 chars}
        - 4 + str.len;                 {adjust for size of our string}
      sst_mem_alloc_scope (size, val.ar_str_p); {allocate memory for returned string}
      val.ar_str_p^.max := str.len;    {set returned string}
      string_copy (str, val.ar_str_p^);
      val.dtype := sst_dtype_array_k;
      end
    ;
  end;
{
********************************
*
*   Nested expression in parenthesis.
}
4: begin
  sst_r_pas_exp_eval (val);            {return nested expression value}
  end;
{
********************************
*
*   SET value.
}
5: begin
  writeln ('SET constant expression have not been implemented yet.');
  syn_error (str_h, '', '', nil, 0);
  end;
{
********************************
*
*   Variable, CONST, function, or array.
}
6: begin
  syn_level_down;                      {down into VARIABLE syntax}
  syn_get_tag_msg (                    {get variable name tag}
    tag, str_h, 'sst_pas_read', 'constant_bad', nil, 0);
  syn_get_tag_msg (tag2, str2_h, 'sst_pas_read', 'constant_bad', nil, 0);
  if tag2 <> syn_tag_end_k then begin
    syn_error (str2_h, 'sst_pas_read', 'var_not_simple', nil, 0);
    end;
  syn_level_up;                        {back to ITEM syntax level from VARIABLE}

  sst_symbol_lookup (str_h, sym_p, stat); {look up symbol name in symbol table}
  syn_error_abort (stat, str_h, '', '', nil, 0);
  syn_get_tag_msg (tag, str_h, 'sst_pas_read', 'constant_bad', nil, 0);
  case tag of                          {what follows the symbol name ?}

1: begin                               {nothing follows the symbol name}
  case sym_p^.symtype of               {what kind of symbol is this ?}
sst_symtype_const_k: begin             {symbol is a constant}
  val := sym_p^.const_exp_p^.val;      {copy value of constant}
  end;
sst_symtype_enum_k: begin              {symbol is value of an enumerated type}
  val.enum_p := sym_p;                 {pass back pointer to symbol}
  val.dtype := sst_dtype_enum_k;       {indicate this is value of enumerated type}
  end;
otherwise                              {this symbol type can not have a constant val}
  sys_msg_parm_vstr (msg_parm[1], sym_p^.name_in_p^);
  syn_error (str_h, 'sst_pas_read', 'symbol_not_value', msg_parm, 1);
  end;                                 {end of symbol data type cases}
  end;                                 {end of symbol with nothing following}

2: begin                               {symbol is followed by function arguments}
  if sym_p^.symtype <> sst_symtype_front_k then begin {not intrinsic function ?}
    sys_msg_parm_vstr (msg_parm[1], sym_p^.name_in_p^);
    syn_error (str_h, 'sst_pas_read', 'func_not_intrinsic', msg_parm, 1);
    end;

  writeln ('"', sym_p^.name_in_p^.str:sym_p^.name_in_p^.len,
    '" is unimplemented intrinsic function in SST_R_PAS_ITEM_EVAL.');
  syn_error (str_h, '', '', nil, 0);
  end;                                 {end of ITEM is function with args case}

otherwise
    syn_error_tag_unexp (tag, str_h);
    end;                               {done with what follows symbol cases}
  end;                                 {end of VAR, CONST, function, or array case}
{
********************************
*
*   Boolean constant TRUE.
}
7: begin
  val.dtype := sst_dtype_bool_k;
  val.bool_val := true;
  end;
{
********************************
*
*   Boolean constant FALSE.
}
8: begin
  val.dtype := sst_dtype_bool_k;
  val.bool_val := false;
  end;
{
********************************
*
*   Pointer constant NIL.
}
9: begin
  val.dtype := sst_dtype_pnt_k;
  val.pnt_dtype_p := sst_dtype_uptr_p;
  val.pnt_exp_p := nil;
  end;
{
********************************
*
*   Unexpected TAG value for second tag in ITEM.
}
otherwise
    syn_error_tag_unexp (tag, str_h);
    end;                               {done with second ITEM tag cases}
{
*   VAL is all set except for handling the unadic operator, if any.
}
  if tag_unadic <> 1 then begin        {there was a preceeding unadic operator ?}
  case val.dtype of

sst_dtype_int_k: begin                 {integer}
  case tag_unadic of
2: ;                                   {+}
3: val.int_val := -val.int_val;        {-}
5: val.int_val := ~val.int_val;        {~}
otherwise goto unadic_not_match;
    end;
  end;

sst_dtype_float_k: begin               {floating point}
  case tag_unadic of
2: ;                                   {+}
3: val.float_val := -val.float_val;    {-}
otherwise goto unadic_not_match;
    end;
  end;

sst_dtype_bool_k: begin                {boolean}
  case tag_unadic of
4: val.bool_val := not val.bool_val;   {not}
otherwise goto unadic_not_match;
    end;
  end;

otherwise
    goto unadic_not_match;             {all other combinations are invalid}
    end;                               {end of data type cases}
  end;                                 {done handling preceeding unadic operator}

  syn_level_up;                        {up from ITEM syntax level}
  return;

unadic_not_match:                      {unadic operator not match item data type}
  syn_error (str_h_unadic, 'sst_pas_read', 'unadic_not_match', nil, 0);
  end;
