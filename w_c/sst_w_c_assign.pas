{   Subroutine SST_W_C_ASSIGN (V, EXP)
*
*   Write a complete assignment statement.  This may actually expand out into
*   several statements.  The assignment variable is specified by VAR.
*   EXP is the descriptor for the assignment value expression.
}
module sst_w_c_ASSIGN;
define sst_w_c_assign;
%include 'sst_w_c.ins.pas';

procedure sst_w_c_assign (             {write complete assignment statement}
  in      v: sst_var_t;                {descriptor for variable to assign to}
  in      exp: sst_exp_t);             {descriptor for assignment value expression}

var
  dt_p, dt2_p: sst_dtype_p_t;          {scratch pointers to data type descriptors}
  dtp_p: sst_dtype_p_t;                {pointer to pointed-to data type}
  i: sys_int_machine_t;                {scratch integer}
  acnt: sys_int_machine_t;             {number of times to take address of expression}
  token: string_var32_t;               {for number conversion}
  type_cast: boolean;                  {TRUE if need to type-cast expression}
  armode: array_k_t;                   {array symbol interpret mode for expression}

label
  cast, normal;

begin
  token.max := sizeof(token.str);      {init local var string}
  type_cast := false;                  {init to don't explicitly type-cast exp}
  armode := array_pnt_first_k;         {init to how C views array symbols}

  dt_p := v.dtype_p;                   {resolve base data type of variable}
  while dt_p^.dtype = sst_dtype_copy_k do dt_p := dt_p^.copy_dtype_p;
  if dt_p^.dtype = sst_dtype_array_k then begin
{
*   The data type of the assignment variable is ARRAY.  The C language
*   can not handle array assignments with an assignment statement.
*   We will call an intrinsic function that copies memory.  The form is:
*
*     memcpy (V, EXP, SIZE);
*
*   where V is the assignement array variable, EXP is the assignment expression,
*   and SIZE is the number of memory units to copy.  If the arrays are strings,
*   then the end of V must be cleared to blanks when EXP is smaller than V.
*   This is done with the statement:
*
*     memset (V+START, ' ', N);
}
    sst_w_c_armode_push (array_whole_k); {array identifiers represent whole array}
    sst_w_c_declare (decl_string_k);   {make sure MEMCPY and MEMSET declared}
    dt2_p := exp.dtype_p;              {resolve base data type of expression}
    while dt2_p^.dtype = sst_dtype_copy_k do dt2_p := dt2_p^.copy_dtype_p;
    i := min(dt_p^.size_used, dt2_p^.size_used); {number of mem units to copy}
    if i > 0 then begin                {there is something to copy ?}
      sst_w_c_sment_start;
      sst_w.appendn^ ('memcpy (', 8);
      sst_w_c_var (v, 0);
      sst_w.appendn^ (',', 1);
      sst_w.delimit^;
      acnt := 0;                       {init to pass source expression as is}
      if dt2_p^.dtype <> sst_dtype_array_k then begin {source not an array ?}
        acnt := 1;                     {pass address of source expression}
        end;
      sst_w_c_exp (exp, acnt, nil, enclose_no_k);
      sst_w.appendn^ (',', 1);
      sst_w.delimit^;
      string_f_int (token, i);         {make number of addresses to copy}
      sst_w.append^ (token);
      sst_w.appendn^ (')', 1);
      sst_w_c_sment_end;
      end;                             {done with code to copy the data}
    if                                 {string, needs padding with blanks ?}
        (dt_p^.ar_string) and          {data type is a string of characters ?}
        (i < dt_p^.size_used)          {not whole string got set ?}
        then begin
      sst_w_c_sment_start;
      sst_w.appendn^ ('memset (', 8);
      sst_w_c_var (v, 0);
      if i > 0 then begin
        sst_w.delimit^;
        sst_w.appendn^ ('+', 1);
        sst_w.delimit^;
        sst_w.append^ (token);
        end;
      sst_w.appendn^ (',', 1);
      sst_w.delimit^;
      sst_w.appendn^ (''' '',', 4);
      sst_w.delimit^;
      string_f_int (token, dt_p^.size_used - i); {number of chars to clear}
      sst_w.append^ (token);
      sst_w.appendn^ (')', 1);
      sst_w_c_sment_end;
      end;                             {done handling padding string with blanks}
    sst_w_c_armode_pop;                {restore old array name interpret mode}
    return;
    end;                               {done with data type is ARRAY}
{
*   Check for an assignment of one string pointer to another where the
*   string lengths differ.  The front end allows this.  The C compiler also
*   does, but will issue a warning.  In that case explicitly type-cast
*   the expression to the data type of the assignment variable.
}
  if dt_p^.dtype <> sst_dtype_pnt_k then goto normal;
  dtp_p := dt_p^.pnt_dtype_p;          {resolve base pointed-to data type}
  if dtp_p = nil then goto normal;     {assigning to a NIL pointer ?}
  while dtp_p^.dtype = sst_dtype_copy_k do dtp_p := dtp_p^.copy_dtype_p;
  if dtp_p^.dtype <> sst_dtype_array_k then goto normal;
  if not dtp_p^.ar_string then goto normal;
{
*   The assignment variable is a pointer to a string.
*   Now check the expression to see whether it is pointing to a different size
*   string.
}
  dt2_p := exp.dtype_p;                {resolve expression's base data type}
  while dt2_p^.dtype = sst_dtype_copy_k do dt2_p := dt2_p^.copy_dtype_p;
  if dt2_p^.dtype <> sst_dtype_pnt_k then goto normal;
  dt2_p := dt2_p^.pnt_dtype_p;         {get pointed-to data type}
  if dt2_p = nil then goto cast;
  while dt2_p^.dtype = sst_dtype_copy_k do dt2_p := dt2_p^.copy_dtype_p;
  if dt2_p^.dtype <> sst_dtype_array_k then goto normal;
  if not dt2_p^.ar_string then goto normal;
  if dt2_p^.ar_ind_n = dtp_p^.ar_ind_n then goto normal; {string lengths match}

cast:                                  {jump here if need to type-cast expression}
  type_cast := true;

normal:                                {skip to here if not need to type-cast exp}
  sst_w_c_sment_start;                 {start a new statement}

  sst_w_c_armode_push (array_whole_k); {array assign vars are the arrays}
  sst_w_c_var (v, 0);                  {write variable being assigned to}
  sst_w_c_armode_pop;

  sst_w.delimit^;
  sst_w.appendn^ ('=', 1);             {write assignment operator}
  sst_w.delimit^;

  if type_cast then begin              {need to write explicit type-cast}
    token.len := 0;
    sst_w.appendn^ ('(', 1);
    sst_w_c_dtype_simple (v.dtype_p^, token, false); {write desired data type}
    sst_w.appendn^ (')', 1);
    armode := array_pnt_whole_k;       {preven additional automatic type-casting}
    end;

  sst_w_c_armode_push (armode);        {set array symbol interpretation mode}
  sst_w_c_exp (exp, 0, nil, enclose_no_k); {write expression for assignment value}
  sst_w_c_armode_pop;                  {restore array symbol interpretation mode}

  sst_w_c_sment_end;                   {finish this statement}
  end;
