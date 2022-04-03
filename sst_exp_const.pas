{   Collection of routines that create various types of expression descriptors
*   that have a constant value.
}
module sst_exp_const;
define sst_exp_const_enum;
define sst_exp_const_float;
define sst_exp_const_int;
define sst_exp_const_bool;
define sst_exp_const_str;
define sst_exp_const_vstr;
%include 'sst2.ins.pas';
{
************************************************************
*
*   Local subroutine SST_EXP_CONST (EXP_P)
*
*   Create and initialize an expression descriptor to hold a constant value.
}
procedure sst_exp_const (              {create and partially init constant exp desc}
  out     exp_p: sst_exp_p_t);         {pointer to new expression descriptor}

begin
  sst_mem_alloc_scope (sizeof(exp_p^), exp_p); {alloc mem for expression descriptor}

  exp_p^.str_h.first_char.crange_p := nil;
  exp_p^.str_h.first_char.ofs := 0;
  exp_p^.str_h.last_char.crange_p := nil;
  exp_p^.str_h.last_char.ofs := 0;
  exp_p^.val_eval := true;
  exp_p^.val_fnd := true;
  exp_p^.rwflag := [sst_rwflag_read_k];

  exp_p^.term1.next_p := nil;
  exp_p^.term1.op2 := sst_op2_none_k;
  exp_p^.term1.op1 := sst_op1_none_k;
  exp_p^.term1.ttype := sst_term_const_k;
  exp_p^.term1.str_h := exp_p^.str_h;
  exp_p^.term1.val_eval := true;
  exp_p^.term1.val_fnd := true;
  exp_p^.term1.rwflag := [sst_rwflag_read_k];
  end;
{
************************************************************
*
*   Subroutine SST_EXP_CONST_ENUM (SYM, EXP_P)
*
*   Create an expression descriptor with a constant enumerated value.
*   SYM is the symbol descriptor for the enumerated value.
}
procedure sst_exp_const_enum (         {create const expression with ENUM value}
  in      sym: sst_symbol_t;           {symbol descriptor for enumerated value}
  out     exp_p: sst_exp_p_t);         {pointer to new expression descriptor}

begin
  sst_exp_const (exp_p);               {create and init expression descriptor}

  exp_p^.val.dtype := sst_dtype_enum_k; {set values for top expression}
  exp_p^.val.enum_p := addr(sym);
  exp_p^.dtype_p := sym.enum_dtype_p;

  exp_p^.term1.val := exp_p^.val;      {copy values to term descriptor}
  exp_p^.term1.dtype_p := exp_p^.dtype_p;
  end;
{
************************************************************
*
*   Subroutine SST_EXP_CONST_FLOAT (F, EXP_P)
*
*   Create an expression descriptor with the constant floating point value F.
}
procedure sst_exp_const_float (        {create const expression with FLOAT value}
  in      f: double;                   {floating point value}
  out     exp_p: sst_exp_p_t);         {pointer to new expression descriptor}
  val_param;

begin
  sst_exp_const (exp_p);               {create and init expression descriptor}

  exp_p^.val.dtype := sst_dtype_float_k; {set values for top expression}
  exp_p^.val.float_val := f;
  exp_p^.dtype_p := sst_dtype_float_max_p;

  exp_p^.term1.val := exp_p^.val;      {copy values to term descriptor}
  exp_p^.term1.dtype_p := exp_p^.dtype_p;
  end;
{
************************************************************
*
*   Subroutine SST_EXP_CONST_INT (I, EXP_P)
*
*   Create an expression descriptor with the constant integer value I.
}
procedure sst_exp_const_int (          {create const expression with INTEGER value}
  in      i: sys_int_max_t;            {integer value}
  out     exp_p: sst_exp_p_t);         {pointer to new expression descriptor}
  val_param;

begin
  sst_exp_const (exp_p);               {create and init expression descriptor}

  exp_p^.val.dtype := sst_dtype_int_k; {set values for top expression}
  exp_p^.val.int_val := i;
  exp_p^.dtype_p := sst_dtype_int_max_p;

  exp_p^.term1.val := exp_p^.val;      {copy values to term descriptor}
  exp_p^.term1.dtype_p := exp_p^.dtype_p;
  end;
{
************************************************************
*
*   Subroutine SST_EXP_CONST_BOOL (B, EXP_P)
*
*   Create an expression descriptor with the constant integer value I.
}
procedure sst_exp_const_bool (         {create const expression with BOOLEAN value}
  in      b: boolean;                  {boolean value}
  out     exp_p: sst_exp_p_t);         {pointer to new expression descriptor}
  val_param;

begin
  sst_exp_const (exp_p);               {create and init expression descriptor}

  exp_p^.dtype_p := sst_dtype_bool_p;  {point to expression data type}
  exp_p^.val.dtype := sst_dtype_bool_k; {set values for top expression}
  exp_p^.val.bool_val := b;            {set expression value}

  exp_p^.term1.val := exp_p^.val;      {copy values to term descriptor}
  exp_p^.term1.dtype_p := exp_p^.dtype_p;
  end;
{
************************************************************
*
*   Subroutine SST_EXP_CONST_STR (STR, LEN, DTLEN, EXP_P)
*
*   Create an expression descriptor with the constant string STR.  LEN
*   is the string length.
}
procedure sst_exp_const_str (          {create const expression with STRING value}
  in      str: univ string;            {string value}
  in      len: sys_int_machine_t;      {number of characters in the string}
  in      dtlen: sys_int_machine_t;    {max length of string data type}
  out     exp_p: sst_exp_p_t);         {pointer to new expression descriptor}
  val_param;

var
  dt_p: sst_dtype_p_t;                 {pointer to new string data type}
  vstr_p: string_var_p_t;              {pointer to memory to hold string value}
  l: sys_int_machine_t;                {string length actually used}
  i: sys_int_machine_t;                {scratch integer and loop counter}

begin
  l := min(len, dtlen);                {number of characters to actually store}
{
*   Create data type for string of length DTLEN.
}
  sst_dtype_new (dt_p);                {create new data type in this scope}
  dt_p^.dtype := sst_dtype_array_k;    {data type ID}
  dt_p^.bits_min :=                    {min bits storage required}
    sst_dtype_char_p^.bits_min * dtlen;
  dt_p^.align_nat := sst_dtype_char_p^.align_nat; {natural alignment of string}
  dt_p^.size_used :=                   {min machine addresses to hold hold string}
    (dt_p^.bits_min + sst_config.bits_adr - 1) div sst_config.bits_adr;

  dt_p^.ar_dtype_ele_p := sst_dtype_char_p; {data type of array elements}
  dt_p^.ar_dtype_rem_p := nil;
  sst_exp_const_int (1, dt_p^.ar_ind_first_p); {first subscript range expression}
  sst_exp_const_int (dtlen, dt_p^.ar_ind_last_p); {end subscript range expression}
  dt_p^.ar_ind_n := dtlen;             {number of elements in array}
  dt_p^.ar_n_subscr := 1;              {number of subscripts}
  dt_p^.ar_string := true;             {this is STRING special case}

  sst_dtype_align (dt_p^, sst_align_natural_k); {set final data type alignment}
{
*   Create string to hold the constant value.
}
  sst_mem_alloc_scope (                {allocate memory for var string}
    string_size (l),                   {amount of mem needed for var string}
    vstr_p);                           {returned pointer to var string}

  for i := 1 to l do begin             {once for each character to copy}
    vstr_p^.str[i] := str[i];          {copy this input character}
    end;
  vstr_p^.len := l;                    {set string lengths}
  vstr_p^.max := l;
{
*   Create expression descriptor for whole string constant.
}
  sst_exp_const (exp_p);               {create and init expression descriptor}

  exp_p^.val.dtype := sst_dtype_array_k; {set values for top expression}
  exp_p^.val.ar_str_p := vstr_p;
  exp_p^.dtype_p := dt_p;

  exp_p^.term1.val := exp_p^.val;      {copy values to term descriptor}
  exp_p^.term1.dtype_p := exp_p^.dtype_p;
  end;
{
************************************************************
*
*   Subroutine SST_EXP_CONST_VSTR (S, EXP_P)
*
*   Create an expression descriptor with the constant string value in S.
}
procedure sst_exp_const_vstr (         {create const expression with STRING value}
  in      s: univ string_var_arg_t;    {string value, described using VAR STRING}
  out     exp_p: sst_exp_p_t);         {pointer to new expression descriptor}

var
  sz: sys_int_adr_t;                   {amount of memory needed}
  s_p: string_var_p_t;                 {will point to new constant string}

begin
  sst_exp_const (exp_p);               {create and init expression descriptor}

  exp_p^.val.dtype := sst_dtype_array_k; {set values for top expression}
  sz := sizeof(s_p^) - sizeof(s_p^.str) + {mem needed for var string}
    (sizeof(char) * s.len);
  sst_mem_alloc_scope (sz, s_p);       {allocate memory for var string constant}
  s_p^.max := s.len;                   {copy string value to new string}
  string_copy (s, s_p^);
  exp_p^.val.ar_str_p := s_p;          {point value descriptor to new string}

  sst_dtype_new_string (s.len, exp_p^.dtype_p); {create dtype desc for the string}

  exp_p^.term1.val := exp_p^.val;      {copy values to term descriptor}
  exp_p^.term1.dtype_p := exp_p^.dtype_p;
  end;
