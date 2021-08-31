{   Subroutine SST_W_C_TERM (TERM, ADDR_CNT, ENCLOSE)
*
*   Write the term indicated by the term descriptor TERM.  ADDR_CNT indicates
*   how many times the "address of" should be taken of the term.  It may
*   be negative to indicate pointer dereferences.
*   ENCLOSE indicates whether the final expression should be enclosed in
*   parentheses.  Values of ENCLOSE can be:
*
*     ENCLOSE_YES_K  -  Enclose in parentheses, if neccessary, to make the
*       entire expression be one term.
*
*     ENCLOSE_NO_K  -  Don't enclose expression in parentheses, even if is is
*       written as more than one term with operators in between.
}
module sst_W_C_TERM;
define sst_w_c_term;
%include 'sst_w_c.ins.pas';

procedure sst_w_c_term (               {write a term in an expression}
  in      term: sst_exp_term_t;        {descriptor for term to write}
  in      addr_cnt: sys_int_machine_t; {number of times to take address of}
  in      enclose: enclose_k_t);       {enclose in () yes/no}

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

type
  permit_k_t = (permit_adr, permit_upt); {pointer diddle permission flags}

  permit_s_t = set of permit_k_t;

var
  r2: double;                          {for intermediate calculations}
  i: sys_int_machine_t;                {scratch integer and loop counter}
  imax: sys_int_max_t;                 {scratch integer for number conversion}
  ifarg_p: sst_exp_chain_p_t;          {points to curr argument of intrinsic func}
  exp_p: sst_exp_p_t;                  {scratch pointer to expression descriptor}
  arg_p: sst_proc_arg_p_t;             {points to current function argument desc}
  argt_p: sst_proc_arg_p_t;            {points to current function argument template}
  ele_p: sst_ele_exp_p_t;              {points to current set element descriptor}
  dt_p: sst_dtype_p_t;                 {scratch pointer to data type descriptor}
  sym_p: sst_symbol_p_t;               {scratch pointer to symbol descriptor}
  exp: sst_exp_t;                      {for enclosing term in whole expression}
  term_p: sst_exp_term_p_t;            {points to call argument for compiler bug}
  token: string_var32_t;               {scratch token for number conversion}
  op1: sst_op1_k_t;                    {saved copy of term's unary operator}
  enc: enclose_k_t;                    {internal ENCLOSE flag}
  perm: permit_s_t;                    {scratch permission flags}
  all_done: boolean;                   {TRUE if DO_ADDR_CNT finished whole term}
  set_empty: boolean;                  {TRUE if set expression is empty}
  pushed_armode: boolean;              {TRUE if pushed ARMODE in ifunc code}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;
  stat: sys_err_t;                     {error status code}

label
  write_const, const_done, ifunc_done;
{
***************************
*
*   Local subroutine DO_ADDR_CNT (CNT, PERMITTED)
*
*   Write the leading "*" or "&" symbols as neccesary.  CNT is the number of
*   times the "address of" operator needs to be written.  Negative values
*   indicate the number of times the "pointer dereference" operator needs to
*   be written.  If anything is written, ENC is set to indicate that the
*   whole expression must be enclosed, if neccessary.
*
*   PERMITTED is a set indicating which pointer/address operations are permitted
*   without the need for an implicit variable.  Valid values of the set are:
*
*     PERMIT_ADR  -  The "address of" operator is permitted one level.
*
*     PERMIT_UPT  -  Unpointer (dereference pointer) operations are allowed.
*
*   In cases where an implicit variable is created, this routine will handle
*   the entire term.  In that case ALL_DONE is set to TRUE to indicate that
*   the caller should just RETURN.
}
procedure do_addr_cnt (
  in      cnt: sys_int_machine_t;      {number of "address of" operations wanted}
  in      permitted: permit_s_t);      {set of pointer diddle permission flags}

var
  cnt_left: sys_int_machine_t;         {number of "address of"s left to do}
  v: sst_var_t;                        {descriptor for implicit variable}
  implicit_var: boolean;               {TRUE if implicit variable needed}

begin
  all_done := false;                   {init to whole term not handled here}
  if cnt = 0 then return;              {nothing needs to be done ?}

  cnt_left := cnt;                     {init number of "address of"s left to do}
  if cnt_left > 0
    then begin                         {"address of"s are requested}
      sst_w.appendn^ ('&', 1);         {do only one address of at a time}
      implicit_var := not (permit_adr in permitted);
      cnt_left := cnt - 1;             {one less "address of" left to do}
      end
    else begin                         {pointer dereferences requested}
      while cnt_left < 0 do begin      {once for each time to dereference pointer}
        sst_w.appendn^ ('*', 1);
        cnt_left := cnt_left + 1;      {one less pointer dereference left to do}
        end;
      implicit_var := not (permit_upt in permitted);
      end;
    ;

  if implicit_var or (cnt_left <> 0) then begin {create implicit variable for term ?}
    sst_w_c_implicit_var (             {create and declare implicit variable}
      term_p^.dtype_p^,                {data type for implicit variable}
      v);                              {filled in variable reference descriptor}
    sst_w_c_pos_push (sment_type_exec_k); {position for write before curr statement}
    sst_w_c_sment_start;               {start assignment statement}
    sst_w.append_sym_name^ (v.mod1.top_sym_p^); {write name of implicit variable}
    sst_w.delimit^;
    sst_w.appendn^ ('=', 1);
    sst_w.delimit^;
    sst_w_c_term (term_p^, cnt_left, enclose_no_k); {write the term's value}
    sst_w_c_sment_end;
    sst_w_c_pos_pop;                   {restore original writing position}

    sst_w.append_sym_name^ (v.mod1.top_sym_p^); {write var name for term value}
    all_done := true;                  {indicate that whole term finished}
    return;
    end;

  enc := enclose_yes_k;                {now needs to look like just one term}
  end;
{
***************************
*
*   Local subroutine MINMAX (ARG1, N_ARGS, DTYPE, IFUNC_ID)
*
*   Write out the equivalent of a MIN or MAX function.  ARG1 is the descriptor
*   for the first argument to the MIN/MAX function.  N_ARGS is the number of
*   arguments in the chain that need to be handled.  IFUNC_ID is the intrinsic
*   function ID, which indicates whether this is a MIN or MAX function.
*   It must be either SST_IFUNC_MAX_K or SST_IFUNC_MIN_K.  DTYPE must be the
*   data type descriptor for the result of the  whole MIN or MAX function.
}
procedure minmax (
  in      arg1: sst_exp_chain_t;       {descriptor for first intrinsic func argument}
  in      n_args: sys_int_machine_t;   {number of arguments to handle}
  in      dtype: sst_dtype_t;          {data type of whole MIN or MAX function}
  in      ifunc_id: sst_ifunc_k_t);    {identifies MIN or MAX function}

var
  sym1_p, sym2_p: sst_symbol_p_t;      {pointer to arg1,arg2 implicit vars, if any}
  exp1_p, exp2_p: sst_exp_p_t;         {pointers to arg1,arg2 expression descriptors}
  v: sst_var_t;                        {scratch descriptor for implicit variable}

begin
  exp1_p := arg1.exp_p;                {get pointer to argument 1 expression}
  exp2_p := arg1.next_p^.exp_p;        {get pointer to argument 2 expression}

  sst_w_c_exp_implicit (exp1_p^, sym1_p); {make implicit var for arg1, if needed}
  if n_args <= 2
    then begin                         {handle all call args directly}
      sst_w_c_exp_implicit (exp2_p^, sym2_p); {make implicit var for arg2, if needed}
      end
    else begin                         {too many args, handle remainder recursively}
      sst_w_c_implicit_var (dtype, v); {make implicit var for second term}
      sym2_p := v.mod1.top_sym_p;      {save symbol pointer to implicit variable}
      sst_w_c_pos_push (sment_type_exec_k); {position for write before curr statement}
      sst_w_c_sment_start;             {start assignment statement}
      sst_w.append_sym_name^ (sym2_p^); {write name of implicit variable}
      sst_w.delimit^;
      sst_w.appendn^ ('=', 1);
      sst_w.delimit^;
      minmax (                         {write value of arguments 2-N}
        arg1.next_p^, n_args-1, dtype, ifunc_id);
      sst_w_c_sment_end;               {close assignment to implicit var statement}
      sst_w_c_pos_pop;                 {restore original writing position}
      end
    ;
{
*   The function has now been resolved to 2 arguments.  An implicit variable
*   may have been created for each argument, which is indicated by the
*   SYMn_P pointer not being NIL.  When the pointer is NIL, then the argument
*   is to be referenced directly by the expression pointed to by EXPn_P.
*
*   A MAX expression has the form:
*
*     (arg1 > arg2) ? arg1 : arg2
*
*   The ">" is replaced by a "<" for a MIN expression.
}
  sst_w.appendn^ ('(', 1);
  if sym1_p = nil                      {write argument 1 value}
    then sst_w_c_exp (exp1_p^, 0, nil, enclose_yes_k)
    else sst_w.append_sym_name^ (sym1_p^);
  sst_w.delimit^;
  if ifunc_id = sst_ifunc_max_k        {write operator based on MIN/MAX choice}
    then sst_w.appendn^ ('>', 1)
    else sst_w.appendn^ ('<', 1);
  sst_w.delimit^;
  if sym2_p = nil                      {write argument 2 value}
    then sst_w_c_exp (exp2_p^, 0, nil, enclose_yes_k)
    else sst_w.append_sym_name^ (sym2_p^);
  sst_w.appendn^ (')', 1);
  sst_w.delimit^;
  sst_w.appendn^ ('?', 1);
  sst_w.delimit^;
  if sym1_p = nil                      {write argument 1 value}
    then sst_w_c_exp (exp1_p^, 0, nil, enclose_yes_k)
    else sst_w.append_sym_name^ (sym1_p^);
  sst_w.delimit^;
  sst_w.appendn^ (':', 1);
  sst_w.delimit^;
  if sym2_p = nil                      {write argument 2 value}
    then sst_w_c_exp (exp2_p^, 0, nil, enclose_yes_k)
    else sst_w.append_sym_name^ (sym2_p^);
  end;
{
***************************
*
*   Start of main routine.
}
begin
  token.max := sizeof(token.str);      {init local var string}
  term_p := addr(term);                {work around compiler bug}
  enc := enclose;                      {init internal ENCLOSE flag to caller's}
{
*   Handle unary operator applied to this term.  We will only deal with the
*   unary operator if the term is not a constant.  In that case, the constant
*   value will already reflect the unary operator.
}
  if                                   {need to deal with preceeding unary op ?}
      (term.op1 <> sst_op1_none_k) and
      (term.ttype <> sst_term_const_k)
      then begin
    do_addr_cnt (addr_cnt, []); if all_done then return;
    if enc = enclose_yes_k then sst_w.appendn^ ('(', 1); {open paren, if needed}
    case term.op1 of                   {which operator is it ?}
sst_op1_minus_k: sst_w.appendn^ ('-', 1); {arithmetic negation}
sst_op1_not_k: sst_w.appendn^ ('!', 1); {logical negation}
sst_op1_1comp_k: sst_w.appendn^ ('~', 1); {one's complement (flip all the bits)}
otherwise
      sys_msg_parm_int (msg_parm[1], ord(term.op1));
      sys_message_bomb ('sst', 'operator_unknown_unary', msg_parm, 1);
      end;                             {end of unary operator cases}
    term_p := addr(term);              {allow temp writing term although declared IN}
    op1 := term.op1;                   {save unary operator ID}
    term_p^.op1 := sst_op1_none_k;     {temporarily shut off unary operator}
    sst_w_c_term (term, 0, enclose_yes_k); {write the body of the term}
    term_p^.op1 := op1;                {restore unary operator ID}
    if enc = enclose_yes_k then sst_w.appendn^ (')', 1); {close paren, if needed}
    return;
    end;                               {done handling unary operator exists case}
{
*   This term has no unary operator we need to worry about.
}
  case term.ttype of                   {what kind of term is this ?}
{
*********************************************
*
*   Term is a constant.
}
sst_term_const_k: begin
  do_addr_cnt (addr_cnt, []); if all_done then return;
write_const:                           {jump here to just write constant value}
{
*   Check for FIRSTOF(integer) constant value.  Many C compilers have trouble
*   with this.  Instead of writing this value directly, we will set a static
*   variable to this value, and then use the variable.
}
  imax := lshft(1, sst_dtype_int_max_p^.bits_min - 1); {make FIRSTOF(integer)}
  if                                   {special integer compiler doesn't do right ?}
      (term.val.dtype = sst_dtype_int_k) and {constant is of integer type ?}
      (term.val.int_val = imax) and    {value is the nasty integer ?}
      (frame_scope_p^.sment_type = sment_type_exec_k) {in executable code ?}
      then begin
    exp.str_h := term.str_h;           {make expression of this term}
    exp.dtype_p := term.dtype_p;
    exp.dtype_hard := term.dtype_hard;
    exp.val_eval := term.val_eval;
    exp.val_fnd := term.val_fnd;
    exp.val := term.val;
    exp.rwflag := term.rwflag;
    exp.term1 := term;
    sst_w_c_exp_explicit (exp, exp.dtype_p^, sym_p); {create static variable}
    sst_w.append_sym_name^ (sym_p^);   {write name of var instead of const value}
    goto const_done;
    end;

  sst_w_c_value (                      {write term's constant value}
    term.val,                          {constant value descriptor}
    enc);                              {enclose in () yes/no flag}
const_done:                            {all done writing constant value}
  end;
{
*********************************************
*
*   Term is a variable reference.
}
sst_term_var_k: begin
  sst_w_c_var (term.var_var_p^, addr_cnt); {write variable reference}
  end;
{
*********************************************
*
*   Term is a function call.
}
sst_term_func_k: begin
  do_addr_cnt (addr_cnt, [permit_upt]); if all_done then return;
  sst_w_c_var (term.func_var_p^, 0);   {write function reference}
  sst_w.appendn^ ('(', 1);             {start function arguments}
  if term.func_proc_p^.n_args > 0 then begin {function has arguments ?}
    sst_w.allow_break^;
    arg_p := term.func_proc_p^.first_arg_p; {init curr argument to first in list}
    argt_p := term.func_proct_p^.first_arg_p; {init curr argument template}
    while (arg_p <> nil) or (argt_p <> nil) do begin {once for each function arg}
      sst_w_c_arg (arg_p, argt_p);     {write this call argument}
      if (arg_p <> nil) or (argt_p <> nil) then begin {another argument follows ?}
        sst_w.appendn^ (',', 1);
        sst_w.delimit^;
        end;
      end;                             {back to do this new call argument}
    end;                               {done handling call arguments}
  sst_w.appendn^ (')', 1);             {close argument list}
  end;
{
*********************************************
*
*   Term is an intrinsic function to the translator.
*
*   For the cases where the SST function maps directly to the target language
*   function, write the function and then fall thru to the end of the
*   case statement.
*
*   Cases that don't map directly to target language functions need to handle
*   everything directly, and then jump to IFUNC_DONE.
}
sst_term_ifunc_k: begin
  ifarg_p := term.ifunc_args_p;        {init current argument to first argument}
  pushed_armode := false;              {init to not pushed array interpret mode}
  case term.ifunc_id of
{
**********************
}
sst_ifunc_abs_k: begin                 {absolute value}
  do_addr_cnt (addr_cnt, []); if all_done then return;
  dt_p := ifarg_p^.exp_p^.dtype_p;     {get argument's data type}
  while dt_p^.dtype = sst_dtype_copy_k do dt_p := dt_p^.copy_dtype_p; {base dtype}
  if dt_p^.dtype = sst_dtype_float_k
    then begin                         {argument is floating point, use FABS}
      sst_w_c_intrinsic (intr_fabs_k);
      end
    else begin                         {argument is integer, use ABS}
      sst_w_c_intrinsic (intr_abs_k);
      end
    ;
  end;
{
**********************
}
sst_ifunc_addr_k: begin                {address of}
  sst_w_c_exp (ifarg_p^.exp_p^, addr_cnt + 1, nil, enc);
  goto ifunc_done;
  end;
{
**********************
}
sst_ifunc_align_k: begin               {minimum alignment needed for arg1}
  do_addr_cnt (addr_cnt, []); if all_done then return;
  goto write_const;
  end;
{
**********************
}
sst_ifunc_atan_k: begin                {arctangent given slope as ratio of 2 numbers}
  do_addr_cnt (addr_cnt, []); if all_done then return;
  exp_p := ifarg_p^.next_p^.exp_p;     {get pointer to second arg expression}
  r2 := 0;                             {init value so as to use ATAN2}
  if exp_p^.val_fnd then begin         {constant value known for second arg ?}
    case exp_p^.val.dtype of           {what data type is constant ?}
sst_dtype_int_k: r2 := exp_p^.val.int_val;
sst_dtype_float_k: r2 := exp_p^.val.float_val;
      end;                             {end of arg 2 constant data type cases}
    end;
  if r2 = 1.0
    then begin                         {second argument is 1.0, use ATAN}
      sst_w_c_intrinsic (intr_atan_k);
      sst_w.appendn^ ('(', 1);
      sst_w.allow_break^;
      sst_w_c_exp (ifarg_p^.exp_p^, 0, nil, enclose_no_k);
      sst_w.appendn^ (')', 1);
      goto ifunc_done;
      end
    else begin                         {second arg not known to be 1.0, use ATAN2}
      sst_w_c_intrinsic (intr_atan2_k);
      end
    ;
  end;
{
**********************
}
sst_ifunc_char_k: begin                {convert integer to char, ignore high bits}
  sst_w_c_exp (ifarg_p^.exp_p^, addr_cnt, sst_dtype_char_p, enc);
  goto ifunc_done;
  end;
{
**********************
}
sst_ifunc_cos_k: begin                 {cosine, argument in radians}
  do_addr_cnt (addr_cnt, []); if all_done then return;
  sst_w_c_intrinsic (intr_cos_k);
  end;
{
**********************
}
sst_ifunc_dec_k: begin                 {next smaller value of}
  do_addr_cnt (addr_cnt, []); if all_done then return;
  if enc = enclose_yes_k then sst_w.appendn^ ('(', 1); {open paren, if needed}
  sst_w_c_exp (ifarg_p^.exp_p^, 0, nil, enclose_yes_k);
  i := 1;                              {set delta amount value}
  string_f_int (token, i);             {make delta amount string}
  sst_w.delimit^;
  sst_w.appendn^ ('-', 1);
  sst_w.delimit^;
  sst_w.append^ (token);
  if enc = enclose_yes_k then sst_w.appendn^ (')', 1); {close paren, if needed}
  goto ifunc_done;
  end;
{
**********************
}
sst_ifunc_exp_k: begin                 {E to power of argument}
  do_addr_cnt (addr_cnt, []); if all_done then return;
  sst_w_c_intrinsic (intr_exp_k);
  end;
{
**********************
}
sst_ifunc_first_k: begin               {first possible value of}
  do_addr_cnt (addr_cnt, []); if all_done then return;
  goto write_const;
  end;
{
**********************
}
sst_ifunc_inc_k: begin                 {next greater value of}
  do_addr_cnt (addr_cnt, []); if all_done then return;
  if enc = enclose_yes_k then sst_w.appendn^ ('(', 1); {open paren, if needed}
  sst_w_c_exp (ifarg_p^.exp_p^, 0, nil, enclose_yes_k);
  i := 1;                              {set delta amount value}
  string_f_int (token, i);             {make delta amount string}
  sst_w.delimit^;
  sst_w.appendn^ ('+', 1);
  sst_w.delimit^;
  sst_w.append^ (token);
  if enc = enclose_yes_k then sst_w.appendn^ (')', 1); {close paren, if needed}
  goto ifunc_done;
  end;
{
**********************
}
sst_ifunc_int_near_k: begin            {convert to integer, round to nearest}
  do_addr_cnt (addr_cnt, []); if all_done then return;
  sst_w.appendn^ ('(int)', 5);
  sst_w_c_intrinsic (intr_floor_k);
  sst_w.appendn^ ('(', 1);
  sst_w_c_exp (ifarg_p^.exp_p^, 0, nil, enclose_yes_k);
  sst_w.delimit^;
  sst_w.appendn^ ('+', 1);
  sst_w.delimit^;
  sst_w.appendn^ ('0.5)', 4);
  goto ifunc_done;
  end;
{
**********************
}
sst_ifunc_int_zero_k: begin            {convert to integer, round towards zero}
  do_addr_cnt (addr_cnt, []); if all_done then return;
  sst_w.appendn^ ('(int)', 5);
  sst_w_c_exp (ifarg_p^.exp_p^, 0, nil, enclose_yes_k);
  goto ifunc_done;
  end;
{
**********************
}
sst_ifunc_last_k: begin                {last possible value of}
  do_addr_cnt (addr_cnt, []); if all_done then return;
  goto write_const;
  end;
{
**********************
}
sst_ifunc_ln_k: begin                  {logarithm base E}
  do_addr_cnt (addr_cnt, []); if all_done then return;
  sst_w_c_intrinsic (intr_log_k);
  end;
{
**********************
*
*   MIN and MAX functions.   These are handled similarly.
}
sst_ifunc_max_k,                       {maximum value of all arguments}
sst_ifunc_min_k: begin                 {minimum value of all arguments}
  do_addr_cnt (addr_cnt, []); if all_done then return;
  if enc = enclose_yes_k then sst_w.appendn^ ('(', 1); {open paren, if needed}
  i := 1;                              {init number of arguments counted}
  while ifarg_p^.next_p <> nil do begin {once for each argument after first}
    i := i + 1;                        {count one more argument}
    ifarg_p := ifarg_p^.next_p;        {advance to next argument in list}
    end;
{
*   I is the number of arguments to this function.
}
  minmax (                             {write this MIN or MAX function}
    term.ifunc_args_p^,                {descriptor for first argument}
    i,                                 {number of arguments}
    term.dtype_p^,                     {final resulting data type of intrinsic func}
    term.ifunc_id);                    {selects MIN or MAX function}
  if enc = enclose_yes_k then sst_w.appendn^ (')', 1); {close paren, if needed}
  goto ifunc_done;
  end;
{
**********************
}
sst_ifunc_offset_k: begin              {offset of field within record}
  do_addr_cnt (addr_cnt, []); if all_done then return;
  goto write_const;
  end;
{
**********************
}
sst_ifunc_ord_val_k: begin             {ordinal value of}
  sst_w_c_exp (ifarg_p^.exp_p^, addr_cnt, nil, enc);
  goto ifunc_done;
  end;
{
**********************
*
*   Arbitrary logical shift of ARG1 by ARG2 bits to the right.  ARG2 may
*   be negative to indicate a shift to the left.  This expression can take
*   three forms.  If ARG2 has a known value, and is positive:
*
*     (unsigned int)arg1 >> arg2
*
*   If ARG2 has a know value and is negative then:
*
*     arg1 << arg2
*
*   If ARG2 does not have a known value then:
*
*     (arg2 > 0) ? ((unsigned int)arg1 >> arg2) : (arg1 << -arg2)
*
*   In this case, ARG2 will have to be assigned to an implicit variable
*   if it is not a simple expression.
}
sst_ifunc_shift_lo_k: begin            {logical shift arg1 by arg2 bits right}
  do_addr_cnt (addr_cnt, []); if all_done then return;
  if enc = enclose_yes_k then sst_w.appendn^ ('(', 1); {open paren, if needed}
  if ifarg_p^.next_p^.exp_p^.val_fnd
    then begin                         {ARG2 has known constant value ?}
      if ifarg_p^.next_p^.exp_p^.val.int_val < 0
        then begin                     {ARG2 is negative, shift left}
          sst_w_c_exp (ifarg_p^.exp_p^, 0, nil, enclose_yes_k);
          sst_w.delimit^;
          sst_w.appendn^ ('<<', 2);
          sst_w.delimit^;
          sst_w_c_exp (ifarg_p^.next_p^.exp_p^, 0, nil, enclose_yes_k);
          end
        else begin                     {ARG2 is positive, shift right}
          sst_w.appendn^ ('(unsigned int)', 14);
          sst_w_c_exp (ifarg_p^.exp_p^, 0, nil, enclose_yes_k);
          sst_w.delimit^;
          sst_w.appendn^ ('>>', 2);
          sst_w.delimit^;
          sst_w_c_exp (ifarg_p^.next_p^.exp_p^, 0, nil, enclose_yes_k);
          end
        ;
      end
    else begin                         {value of ARG2 is not known}
      sst_w_c_exp_implicit (           {create implicit var for ARG2 if needed}
        ifarg_p^.next_p^.exp_p^, sym_p);
      sst_w.appendn^ ('(', 1);
      if sym_p = nil                   {write ARG2 value}
        then sst_w_c_exp (ifarg_p^.next_p^.exp_p^, 0, nil, enclose_yes_k)
        else sst_w.append_sym_name^ (sym_p^);
      sst_w.delimit^;
      sst_w.appendn^ ('>', 1);
      sst_w.delimit^;
      sst_w.appendn^ ('0', 1);
      sst_w.appendn^ (')', 1);
      sst_w.delimit^;
      sst_w.appendn^ ('?', 1);
      sst_w.delimit^;
      sst_w.appendn^ ('((unsigned int)', 15);
      sst_w_c_exp (ifarg_p^.exp_p^, 0, nil, enclose_yes_k);
      sst_w.delimit^;
      sst_w.appendn^ ('>>', 2);
      sst_w.delimit^;
      if sym_p = nil                   {write ARG2 value}
        then sst_w_c_exp (ifarg_p^.next_p^.exp_p^, 0, nil, enclose_yes_k)
        else sst_w.append_sym_name^ (sym_p^);
      sst_w.appendn^ (')', 1);
      sst_w.delimit^;
      sst_w.appendn^ (':', 1);
      sst_w.delimit^;
      sst_w.appendn^ ('(', 1);
      sst_w_c_exp (ifarg_p^.exp_p^, 0, nil, enclose_yes_k);
      sst_w.delimit^;
      sst_w.appendn^ ('<<', 2);
      sst_w.delimit^;
      sst_w.appendn^ ('-', 1);
      if sym_p = nil                   {write ARG2 value}
        then sst_w_c_exp (ifarg_p^.next_p^.exp_p^, 0, nil, enclose_yes_k)
        else sst_w.append_sym_name^ (sym_p^);
      sst_w.appendn^ (')', 1);
      end
    ;
  if enc = enclose_yes_k then sst_w.appendn^ (')', 1); {close paren, if needed}
  goto ifunc_done;
  end;
{
**********************
}
sst_ifunc_shiftl_lo_k: begin           {logical shift left arg1 by arg2 bits}
  do_addr_cnt (addr_cnt, []); if all_done then return;
  if enc = enclose_yes_k then sst_w.appendn^ ('(', 1); {open paren, if needed}
  sst_w_c_exp (ifarg_p^.exp_p^, 0, nil, enclose_yes_k);
  ifarg_p := ifarg_p^.next_p;          {advance to second argument}
  sst_w.delimit^;
  sst_w.appendn^ ('<<', 2);
  sst_w.delimit^;
  sst_w_c_exp (ifarg_p^.exp_p^, 0, nil, enclose_yes_k);
  if enc = enclose_yes_k then sst_w.appendn^ (')', 1); {close paren, if needed}
  goto ifunc_done;
  end;
{
**********************
}
sst_ifunc_shiftr_ar_k: begin           {arithmetic shift right arg1 by arg2 bits}
  do_addr_cnt (addr_cnt, []); if all_done then return;
  if enc = enclose_yes_k then sst_w.appendn^ ('(', 1); {open paren, if needed}
  sst_w.appendn^ ('(signed int)', 12);
  sst_w.allow_break^;
  sst_w_c_exp (ifarg_p^.exp_p^, 0, nil, enclose_yes_k);
  ifarg_p := ifarg_p^.next_p;          {advance to second argument}
  sst_w.delimit^;
  sst_w.appendn^ ('>>', 2);
  sst_w.delimit^;
  sst_w_c_exp (ifarg_p^.exp_p^, 0, nil, enclose_yes_k);
  if enc = enclose_yes_k then sst_w.appendn^ (')', 1); {close paren, if needed}
  goto ifunc_done;
  end;
{
**********************
}
sst_ifunc_shiftr_lo_k: begin           {logical shift right arg1 by arg2 bits}
  do_addr_cnt (addr_cnt, []); if all_done then return;
  if enc = enclose_yes_k then sst_w.appendn^ ('(', 1); {open paren, if needed}
  sst_w.appendn^ ('(unsigned int)', 14);
  sst_w.allow_break^;
  sst_w_c_exp (ifarg_p^.exp_p^, 0, nil, enclose_yes_k);
  ifarg_p := ifarg_p^.next_p;          {advance to second argument}
  sst_w.delimit^;
  sst_w.appendn^ ('>>', 2);
  sst_w.delimit^;
  sst_w_c_exp (ifarg_p^.exp_p^, 0, nil, enclose_yes_k);
  if enc = enclose_yes_k then sst_w.appendn^ (')', 1); {close paren, if needed}
  goto ifunc_done;
  end;
{
**********************
}
sst_ifunc_sin_k: begin                 {sine, arguments in radians}
  do_addr_cnt (addr_cnt, []); if all_done then return;
  sst_w_c_intrinsic (intr_sin_k);
  end;
{
**********************
}
sst_ifunc_size_align_k: begin          {align-padded size in machine addresses}
  do_addr_cnt (addr_cnt, []); if all_done then return;
  dt_p := ifarg_p^.exp_p^.dtype_p;     {resolve base argument data type}
  while dt_p^.dtype = sst_dtype_copy_k do dt_p := dt_p^.copy_dtype_p;
{
*   The C sizeof function doesn't work the way we want on arrays.  Since
*   array identifiers are actually implicit pointers, SIZEOF returns
*   the size of a pointer, not the array.  Dereferencing the pointer doesn't
*   work either, since it is a pointer to the first element, not the whole
*   array.
}
  if dt_p^.dtype = sst_dtype_array_k then begin {argument has ARRAY data type ?}
    goto write_const;
    end;

  if                                   {argument is element of array data type ?}
      (ifarg_p^.exp_p^.term1.ttype = sst_term_var_k) and then
      (ifarg_p^.exp_p^.term1.var_var_p^.vtype = sst_vtype_dtype_k) and
      (ifarg_p^.exp_p^.term1.var_var_p^.mod1.next_p <> nil)
    then goto write_const;

  sst_w_c_armode_push (array_whole_k); {array identifier represents whole array}
  pushed_armode := true;               {remember to pop mode later}
  sst_w.appendn^ ('sizeof', 6);
  end;
{
**********************
}
sst_ifunc_size_char_k: begin           {number of characters that can fit}
  do_addr_cnt (addr_cnt, []); if all_done then return;
  goto write_const;
  end;
{
**********************
}
sst_ifunc_size_min_k: begin            {minimum size of in machine addresses}
  do_addr_cnt (addr_cnt, []); if all_done then return;
  goto write_const;
  end;
{
**********************
*
*   The C language has no explicit function that takes the square of a value.
*   This will take the form:
*
*     arg * arg
*
*   An implicit variable will be created for ARG when it is not a simple
*   expression.
}
sst_ifunc_sqr_k: begin                 {square of}
  do_addr_cnt (addr_cnt, []); if all_done then return;
  if enc = enclose_yes_k then sst_w.appendn^ ('(', 1); {open paren, if needed}
  sst_w_c_exp_implicit (ifarg_p^.exp_p^, sym_p); {make implicit variable, if needed}
  if sym_p = nil
    then sst_w_c_exp (ifarg_p^.exp_p^, 0, nil, enclose_yes_k)
    else sst_w.append_sym_name^ (sym_p^);
  sst_w.delimit^;
  sst_w.appendn^ ('*', 1);
  sst_w.delimit^;
  if sym_p = nil
    then sst_w_c_exp (ifarg_p^.exp_p^, 0, nil, enclose_yes_k)
    else sst_w.append_sym_name^ (sym_p^);
  if enc = enclose_yes_k then sst_w.appendn^ (')', 1); {close paren, if needed}
  goto ifunc_done;
  end;
{
**********************
}
sst_ifunc_sqrt_k: begin                {square root of}
  do_addr_cnt (addr_cnt, []); if all_done then return;
  sst_w_c_intrinsic (intr_sqrt_k);
  end;
{
**********************
}
sst_ifunc_xor_k: begin                 {exclusive or}
  do_addr_cnt (addr_cnt, []); if all_done then return;
  if enc = enclose_yes_k then sst_w.appendn^ ('(', 1); {open paren, if needed}
  while ifarg_p <> nil do begin        {once for each argument to intrinsic function}
    sst_w_c_exp (ifarg_p^.exp_p^, 0, nil, enclose_yes_k);
    ifarg_p := ifarg_p^.next_p;        {advance to next argument in chain}
    if ifarg_p <> nil then begin       {an argument follows the one we just wrote}
      sst_w.delimit^;
      sst_w.appendn^ ('^', 1);
      sst_w.delimit^;
      end;
    end;                               {back to handle this new argument}
  if enc = enclose_yes_k then sst_w.appendn^ (')', 1); {close paren, if needed}
  goto ifunc_done;
  end;
{
**********************
*
*   Intrinsic function is set inversion (the existance of every set element
*   is flipped).  This will be written as the full set with the specified
*   elements removed.  This will have the form:
*
*     full_set ^ arg
*
*   when an argument expression exists.  Otherwise it will have the form:
*
*     full_set
*
*   FULL_SET will be a hexadecimal constant.
}
sst_ifunc_setinv_k: begin
  set_empty :=                         {true if ARG is definately the nullset}
    (ifarg_p^.exp_p^.term1.ttype = sst_term_set_k) and
    (ifarg_p^.exp_p^.term1.set_first_p = nil);
  if set_empty then begin              {we will just write FULL_SET value ?}
    enc := enclose_no_k;               {no need to enclose in parenthesis}
    end;
  do_addr_cnt (addr_cnt, []); if all_done then return;
  if enc = enclose_yes_k then sst_w.appendn^ ('(', 1); {open paren, if needed}
  imax := ~lshft(-1, term.dtype_p^.bits_min); {make set value for all elements exist}
  string_f_int_max_base (              {convert full set value to hexadecimal string}
    token,                             {output string}
    imax,                              {input number}
    16,                                {number base}
    (term.dtype_p^.bits_min + 3) div 4, {number of digits to create}
    [string_fi_leadz_k, string_fi_unsig_k], {write leading zeros, input is unsigned}
    stat);                             {error return status}
  sys_error_abort (stat, '', '', nil, 0);
  sst_w.appendn^ ('0x', 2);            {header for hexadecimal constant}
  sst_w.append^ (token);
  if not set_empty then begin          {argument exists ?}
    sst_w.delimit^;
    sst_w.appendn^ ('^', 1);
    sst_w.delimit^;
    sst_w_c_exp (ifarg_p^.exp_p^, 0, nil, enclose_yes_k);
    end;
  if enc = enclose_yes_k then sst_w.appendn^ (')', 1); {close paren, if needed}
  goto ifunc_done;
  end;
{
**********************
}
otherwise
    sys_msg_parm_int (msg_parm[1], ord(term.ifunc_id));
    syo_error (term.str_h, 'sst', 'func_intrinsic_unknown', msg_parm, 1);
    end;                               {end of intrinsic function ID cases}
{
**********************
*
*   All the intrinsic functions that fall thru here map directly to C
*   functions.  The function name has been written.  Now write the function
*   arguments list.
}
  sst_w.appendn^ ('(', 1);             {open parenthesis for argument list}
  while ifarg_p <> nil do begin        {once for each argument}
    sst_w_c_exp (ifarg_p^.exp_p^, 0, nil, enclose_no_k);
    ifarg_p := ifarg_p^.next_p;        {advance to next argument in chain}
    if ifarg_p <> nil then begin       {an argument follows the one we just wrote}
      sst_w.appendn^ (',', 1);
      sst_w.allow_break^;
      end;
    end;                               {back to handle this new argument}
  sst_w.appendn^ (')', 1);             {close parenthesis for argument list}

ifunc_done:                            {jump here on done writing intrinsic function}
  if pushed_armode then begin          {push ARMODE earlier ?}
    sst_w_c_armode_pop;                {restore original ARMODE}
    end;
  end;                                 {done with term is an intrinsic function}
{
*********************************************
*
*   Term is a type-casting function.
*
*   If EXP is the expresssion being type cast, and TYPE is the casting data type,
*   then this will take the form:
*
*     *(type *)&exp
*
*   The native C type casting functions CONVERT from one data type to another;
*   they don't RE-TYPE the same bit pattern.  Therefore, we have to get a pointer
*   to the original expression, cast it to be a pointer to the desired type, and
*   then dereference the result.
*
*   If the original expression already is a pointer, then it will be type-cast
*   directly:
*
*     (type)exp
}
sst_term_type_k: begin
  if sst_w_c_exp_adrable(term.type_exp_p^) {allowed to take address of argument ?}
    then perm := [permit_adr]
    else perm := [];
  do_addr_cnt (addr_cnt, perm); if all_done then return;

  dt_p := term.type_exp_p^.dtype_p;    {resolve base data type of expression}
  while dt_p^.dtype = sst_dtype_copy_k do dt_p := dt_p^.copy_dtype_p;
  if dt_p^.dtype = sst_dtype_pnt_k
    then begin                         {expression data type is POINTER}
      sst_w.appendn^ ('(', 1);
      sst_w.append_sym_name^ (term.type_dtype_p^.symbol_p^);
      sst_w.appendn^ (')', 1);
      sst_w_c_exp (term.type_exp_p^, 0, nil, enclose_yes_k);
      end
    else begin                         {expression is not a pointer}
      sst_w.appendn^ ('*(', 2);
      sst_w.append_sym_name^ (term.type_dtype_p^.symbol_p^);
      sst_w.delimit^;
      sst_w.appendn^ ('*)', 2);
      sst_w_c_exp (term.type_exp_p^, 1, nil, enclose_yes_k);
      end
    ;
  end;
{
*********************************************
*
*   Term is a SET.
}
sst_term_set_k: begin
  do_addr_cnt (addr_cnt, []); if all_done then return;
  ele_p := term.set_first_p;           {init current set element to first in list}

  if ele_p = nil then begin            {set expression is the null set ?}
    sst_w_c_intrinsic (intr_nullset_k);
    return;
    end;

  if ele_p^.next_p = nil then begin    {only one element in this set ?}
    enc := enclose_no_k;               {no need to enclose one element in parens}
    end;

  if enc = enclose_yes_k then sst_w.appendn^ ('(', 1); {open paren, if needed}

  while ele_p <> nil do begin          {once for each set element}
    if ele_p^.last_p <> nil
{
*   This element is actually a range of element values.  If ELE1 and ELE2
*   are the starting and ending element values of the range this will look like:
*
*   ((-1 << ele1) & ~(-2 << ele2))
}
      then begin
        sst_w.appendn^ ('((-1', 4);
        sst_w.delimit^;
        sst_w.appendn^ ('<<', 2);
        sst_w.delimit^;
        sst_w_c_exp (ele_p^.first_p^, 0, nil, enclose_yes_k);
        sst_w.appendn^ (')', 1);
        sst_w.delimit^;
        sst_w.appendn^ ('&', 1);
        sst_w.delimit^;
        sst_w.appendn^ ('~(-2', 4);
        sst_w.delimit^;
        sst_w.appendn^ ('<<', 2);
        sst_w.delimit^;
        sst_w_c_exp (ele_p^.last_p^, 0, nil, enclose_yes_k);
        sst_w.appendn^ ('))', 2);
        end
{
*   The expression for this element contains only one element.  It will look
*   like this:
*
*   (1 << ele)
}
      else begin
        sst_w.appendn^ ('(1', 2);
        sst_w.delimit^;
        sst_w.appendn^ ('<<', 2);
        sst_w.delimit^;
        sst_w_c_exp (ele_p^.first_p^, 0, nil, enclose_yes_k);
        sst_w.appendn^ (')', 1);
        end
      ;
    ele_p := ele_p^.next_p;            {advance to next element in this set}
    if ele_p <> nil then begin         {this was not last element in set ?}
      sst_w.delimit^;
      sst_w.appendn^ ('|', 1);         {all the elements are ORed together}
      sst_w.delimit^;
      end;
    end;                               {back and do next element in set}

  if enc = enclose_yes_k then sst_w.appendn^ (')', 1); {close paren, if needed}
  end;
{
*********************************************
*
*   Term is nested expression.
}
sst_term_exp_k: begin
  sst_w_c_exp (term.exp_exp_p^, addr_cnt, nil, enclose); {write nested expression}
  end;
{
*********************************************
*
*   Unexpected term type.
}
otherwise
    sys_msg_parm_int (msg_parm[1], ord(term.ttype));
    sys_message_bomb ('sst', 'term_type_unknown', msg_parm, 1);
    end;                               {end of term type cases}
  end;
