{   Subroutine SST_TERM_EVAL (TERM, NVAL_ERR)
*
*   Evaluate a compiled term.  This means determining its data type, and
*   value if known at compile time.  TERM is the term to evaluate.  If
*   NVAL_ERR is TRUE, then it will be considered an error if the term can
*   not be evaluated to a constant value.
}
module sst_TERM_EVAL;
define sst_term_eval;
%include 'sst2.ins.pas';

procedure sst_term_eval (              {evaluate compiled term in expression}
  in out  term: sst_exp_term_t;        {term, fills in value and data type}
  in      nval_err: boolean);          {unknown value at compile time is err if TRUE}

const
  args_n_any = 0;                      {CHECK_IFUNC_ARGS will allow any num of args}
  max_msg_parms = 2;                   {max parameters we can pass to a message}
  pi = 3.14159265358923846;
  pi_half = pi / 2.0;

var
  ord_min, ord_max: sys_int_max_t;     {ordinal value limits of subrange data type}
  ord_min_dt, ord_max_dt: sys_int_max_t; {min/max ordinal values of set ele dtype}
  ele_p: sst_ele_exp_p_t;              {pointer to current set element descriptor}
  dt_p: sst_dtype_p_t;                 {scratch pointer to data type descriptor}
  dt: sst_dtype_k_t;                   {scratch data type ID}
  ifarg_p: sst_exp_chain_p_t;          {pnt to curr link in intrinsic func arg chain}
  args_n: sys_int_machine_t;           {number of arguments to intrinsic function}
  args_dt: sst_dtype_set_t;            {set of all base dtype IDs of ifunc args}
  r1, r2: double;                      {scratch to compute result value}
  i1, i2: sys_int_max_t;               {scratch to compute reuslt value}
  mod_p: sst_var_mod_p_t;              {points to current modifier in var descriptor}
  field_last: boolean;                 {TRUE if last mod was field in record}
  stat: sys_err_t;                     {error status code}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

label
  arg_bad_offset, done_ifunc, done_term_type, leave, bad_op1;
{
*****************************************************************
*
*   Local subroutine DO_ELE_EXP (T, E)
*
*   Process an expression for the value of an element in a set.  E is the
*   expression descriptor.  T is the term descriptor that is a reference to
*   the set.  The data type of elements in the set so far is pointed to by
*   T.DTYPE_P^.SET_DTYPE_P.  It is set to NIL if no previous elements have
*   been processed.  It will be updated as necessary.  ORD_MIN and ORD_MAX
*   are the min/max ordinal values that set elements may take on.  These
*   are initialized for the first element and updated for subsequent elements.
}
procedure do_ele_exp (
  in out  t: sst_exp_term_t;           {term descriptor referencing SET}
  in out  e: sst_exp_t);               {expression descriptor for element value}

var
  dt2_p: sst_dtype_p_t;                {base data type of element expression}
  ord_ele_min, ord_ele_max: sys_int_max_t; {min/max ordinal values of this element}
  done_ord: boolean;                   {TRUE if ORD_ELE_MIN,ORD_ELE_MAX determined}

label
  resolve_dtype;

begin
  sst_exp_eval (e, false);             {make sure ele value expression is evaluated}

  done_ord := false;                   {init to not already found ORD_ELE_MIN/MAX}
  dt2_p := e.dtype_p;                  {init pointer to resolved data type}
resolve_dtype:                         {back here to re-try with next layer dtype}
  case dt2_p^.dtype of
sst_dtype_int_k: begin
      if not done_ord then begin
        if e.val_fnd
          then begin                   {expression has constant value}
            ord_ele_min := e.val.int_val;
            ord_ele_max := ord_ele_min;
            end
          else begin                   {expression has no known constant value}
            ord_ele_min := -100000;    {these values will prevent making subrange}
            ord_ele_max := 100000;
            end
          ;
        end;
      end;
sst_dtype_enum_k: begin
      if not done_ord then begin
        if e.val_fnd
          then begin                   {expression has constant value}
            ord_ele_min := e.val.enum_p^.enum_ordval;
            ord_ele_max := ord_ele_min;
            end
          else begin                   {expression has no known constant value}
            ord_ele_min := 0;
            ord_ele_max := dt2_p^.enum_last_p^.enum_ordval;
            end
          ;
        end;
      end;
sst_dtype_bool_k: begin
      if not done_ord then begin
        if e.val_fnd
          then begin                   {expression has constant value}
            if e.val.bool_val
              then ord_ele_min := 1
              else ord_ele_min := 0;
            ord_ele_max := ord_ele_min;
            end
          else begin                   {expression has no known constant value}
            ord_ele_min := 0;
            ord_ele_max := 1;
            end
          ;
        end;
      end;
sst_dtype_char_k: begin
      if not done_ord then begin
        if e.val_fnd
          then begin                   {expression has constant value}
            ord_ele_min := ord(e.val.char_val);
            ord_ele_max := ord_ele_min;
            end
          else begin                   {expression has no known constant value}
            ord_ele_min := 0;
            ord_ele_max := 255;
            end
          ;
        end;
      end;
sst_dtype_range_k: begin
      if not e.val_fnd then begin      {ord min/max is whole subrange ?}
        ord_ele_min := dt2_p^.range_ord_first;
        ord_ele_max := ord_ele_min + dt2_p^.range_n_vals - 1;
        done_ord := true;              {indicate ord min/max has been determined}
        end;
      dt2_p := dt2_p^.range_dtype_p;   {init subrange base data type pointer}
      goto resolve_dtype;              {go back to resolve real base data type}
      end;
sst_dtype_copy_k: begin
      dt2_p := dt2_p^.copy_dtype_p;
      goto resolve_dtype;
      end;
otherwise
    syn_error (e.str_h, 'sst', 'dtype_element_bad', nil, 0);
    end;                               {ORD_ELE_MIN, ORD_ELE_MAX all set}

  if t.dtype_p^.set_dtype_p = nil
    then begin                         {this is first element evaluated}
      t.dtype_p^.set_dtype_p := dt2_p; {init set elements data types}
      ord_min := ord_ele_min;          {init min/max possible ele ordinal values}
      ord_max := ord_ele_max;
      end
    else begin                         {previous terms have been evaluated}
      if dt2_p <> t.dtype_p^.set_dtype_p then begin
        syn_error (e.str_h, 'sst', 'dtype_element_mismatch', nil, 0);
        end;
      ord_min := min(ord_min, ord_ele_min); {update min/max ordinal value range}
      ord_max := max(ord_max, ord_ele_max);
      end
    ;
  end;
{
*****************************************************************
*
*   Local subroutine CHECK_IFUNC_ARGS (T, RW, N_ALLOWED, N, DT_ALLOWED, DT)
*
*   Process the arguments to an intrinsic function.  Call arguments are:
*
*     T  -  Term descriptor for the whole intrinsic function.
*
*     RW  -  Indicates the read/write access needed to the expression value.
*       RW is a SET.  The possible element values are:
*
*         SST_RWFLAG_READ_K  -  Expression value must be readable.
*         SST_RWFLAG_WRITE_K  -  Expression value must be writeable.
*
*     N_ALLOWED  -  The exact number of arguments allowed.  Will abort with error
*       if not match number of arguments, unless N_ALLOWED = ARGS_N_ANY.
*
*     N  -  Actual number of arguments found.
*
*     DT_ALLOWED  -  Set of all the base data type IDs allowed for all of the
*       arguments.  Will abort on error if an agument is encountered of a base data
*       type ID not in DT_ALLOWED, unless DT_ALLOWED is the empty set.
*
*     DT  -  Set of all the base data type IDs for all the arguments actually
*       encountered.
*
*   The expression for each argument will be evaluated.  IFARG_P will be set
*   pointing to the first link in the arguments chain for this intrinsic function.
*   T.VAL_FND will be set to TRUE if all the arguments have a known constant value,
*   otherwise T.VAL_FND will be set to FALSE.
}
procedure check_ifunc_args (           {get info about and check ifunc arguments}
  in out  t: sst_exp_term_t;           {term descriptor for whole intrinsic function}
  in      rw: sst_rwflag_t;            {read/write expression useage}
  in      n_allowed: sys_int_machine_t; {exact number of arguments allowed}
  out     n: sys_int_machine_t;        {number of arguments actually found}
  in      dt_allowed: sst_dtype_set_t; {set of all the legal arg base data type IDs}
  out     args_dt: sst_dtype_set_t);   {set of arg base data type IDs actually found}

begin
  n := 0;                              {init number of arguments found}
  args_dt := [];                       {init set of all base arg data types found}
  t.val_fnd := true;                   {init to all arguments have known value}
  ifarg_p := term.ifunc_args_p;        {init current argument to first in chain}

  while ifarg_p <> nil do begin        {once for each ifunc argument in chain}
    n := n + 1;                        {count one more argument to intrinsic func}
    if                                 {too many arguments ?}
        (n_allowed <> args_n_any) and
        (n > n_allowed)
        then begin
      sys_msg_parm_int (msg_parm[1], n_allowed);
      syn_error (
        ifarg_p^.exp_p^.str_h, 'sst', 'func_intrinsic_args_too_many', msg_parm, 1);
      end;
    sst_exp_eval (ifarg_p^.exp_p^, false); {evaluate this argument}

    sst_exp_useage_check (             {check that expression can be used this way}
      ifarg_p^.exp_p^,                 {expression descriptor}
      rw,                              {read/write access we require here}
      sst_dtype_none);                 {no type checking will be done}

    if                                 {data type mismatch ?}
        (dt_allowed <> []) and         {explicit types required ?}
        (not (ifarg_p^.exp_p^.val.dtype in dt_allowed)) {not match directly ?}
        then begin
      syn_error (ifarg_p^.exp_p^.str_h, 'sst', 'func_intrinsic_arg_dtype_bad', nil, 0);
      end;
    args_dt := args_dt + [ifarg_p^.exp_p^.val.dtype]; {accumulate dtype IDs found}
    t.val_fnd := t.val_fnd and ifarg_p^.exp_p^.val_fnd; {TRUE if all args const}
    ifarg_p := ifarg_p^.next_p;        {advance to next argument in chain}
    end;                               {back and process this next argument}

  if                                   {not enough arguments ?}
      (n_allowed <> args_n_any) and
      (n < n_allowed)
      then begin
    sys_msg_parm_int (msg_parm[1], n_allowed);
    sys_msg_parm_int (msg_parm[2], n);
    syn_error (term.str_h, 'sst', 'func_intrinsic_args_too_few', msg_parm, 2);
    end;

  ifarg_p := term.ifunc_args_p;        {reset current argument to first in chain}
  end;
{
*****************************************************************
*
*   Start of main routine.
}
begin
  if term.val_eval then goto leave;    {term already evaluated ?}
  term.val_eval := true;               {term will be evaluated}
  term.val_fnd := false;               {init to term has no constant value}
  term.dtype_hard := true;             {init to data type is absolutely definate}
  term.rwflag := [sst_rwflag_read_k];  {init to term is readable only}
  case term.ttype of                   {what kind of term is this ?}
{
******************************
*
*   Term is a constant.
}
sst_term_const_k: begin
  term.val_fnd := true;                {term definately has a constant value}
  case term.val.dtype of               {what kind of data type is constant}
sst_dtype_int_k: begin
      term.dtype_p := sst_dtype_int_max_p;
      term.dtype_hard := false;
      end;
sst_dtype_enum_k: begin
      term.dtype_p := term.val.enum_p^.enum_dtype_p;
      end;
sst_dtype_float_k: begin
      term.dtype_p := sst_dtype_float_max_p;
      term.dtype_hard := false;
      end;
sst_dtype_bool_k: begin
      term.dtype_p := sst_dtype_bool_p;
      end;
sst_dtype_char_k: begin
      term.dtype_p := sst_dtype_char_p;
      end;
sst_dtype_array_k: begin
      sst_dtype_new_string (           {create string data type for this term}
        term.val.ar_str_p^.len, term.dtype_p);
      end;
sst_dtype_pnt_k: begin
      term.dtype_p := term.val.pnt_dtype_p;
      end;
otherwise
    sys_msg_parm_int (msg_parm[1], ord(term.val.dtype));
    syn_error (term.str_h, 'sst', 'dtype_unexpected', msg_parm, 1);
    end;                               {end of data type cases}
  end;
{
******************************
*
*   Term is a variable.
}
sst_term_var_k: begin
  term.rwflag := term.var_var_p^.rwflag; {copy variable's read/write access flag}
  term.dtype_p := term.var_var_p^.dtype_p; {copy variable's data type}
  case term.var_var_p^.vtype of        {what kind of variable is this ?}
sst_vtype_var_k: ;                     {legal, but no special handling needed}
sst_vtype_dtype_k: ;
sst_vtype_const_k: begin               {"variable" is a named constant}
      term.val := term.var_var_p^.const_val_p^; {get constant's value}
      term.val_fnd := true;            {term has definate known value}
      end;
otherwise
    sys_msg_parm_int (msg_parm[1], ord(term.var_var_p^.vtype));
    syn_error (term.str_h, 'sst', 'vtype_unexpected', msg_parm, 1);
    end;                               {end of "variable" type cases}
  end;
{
******************************
*
*   Term is a function.
}
sst_term_func_k: begin
  term.rwflag := [sst_rwflag_read_k];  {function value is read-only}
  term.dtype_p := term.func_proc_p^.dtype_func_p; {get function's return data type}
  end;
{
******************************
*
*   Term is an intrinsic function.
}
sst_term_ifunc_k: begin
  case term.ifunc_id of                {which intrinsic function is it ?}
{
********
}
sst_ifunc_abs_k: begin                 {intrinsic function ABS}
  check_ifunc_args (                   {get info about and check ifunc arguments}
    term,                              {term descriptor for whole intrinsic function}
    [sst_rwflag_read_k],               {arguments must have readable values}
    1,                                 {number of arguments required}
    args_n,                            {number of arguments found}
    [sst_dtype_int_k, sst_dtype_float_k], {set of all the legal base data type IDs}
    args_dt);                          {set of all base data type IDs found}
  case ifarg_p^.exp_p^.val.dtype of    {what is base data type ID of term}
sst_dtype_int_k: begin                 {argument is integer}
      term.dtype_p := sst_dtype_int_max_p;
      if term.val_fnd then begin       {argument has known value ?}
        term.val.int_val := abs(ifarg_p^.exp_p^.val.int_val);
        end;
      end;
sst_dtype_float_k: begin               {argument is floating point}
      term.dtype_p := sst_dtype_float_max_p;
      if term.val_fnd then begin       {argument has known value ?}
        term.val.float_val := abs(ifarg_p^.exp_p^.val.float_val);
        end;
      end;
    end;                               {end of argument data type cases}
  term.dtype_hard := false;
  end;                                 {end of intrinsic function ABS}
{
********
}
sst_ifunc_addr_k: begin                {instrinsic function ADDR}
  check_ifunc_args (
    term,
    [],                                {no read/write access needed to arguments}
    1,
    args_n,
    [ sst_dtype_int_k,
      sst_dtype_enum_k,
      sst_dtype_float_k,
      sst_dtype_bool_k,
      sst_dtype_char_k,
      sst_dtype_rec_k,
      sst_dtype_array_k,
      sst_dtype_set_k,
      sst_dtype_proc_k,
      sst_dtype_pnt_k],
    args_dt);
  if not sst_exp_simple(ifarg_p^.exp_p^) then begin {arg not a simple expression ?}
    syn_error (ifarg_p^.exp_p^.str_h, 'sst', 'arg_addr_bad', nil, 0);
    end;

  sst_dtype_new (term.dtype_p);        {create and init new data type descriptor}
  term.dtype_p^.symbol_p := nil;       {make new data type pointer to arg data type}
  term.dtype_p^.dtype := sst_dtype_pnt_k;
  term.dtype_p^.bits_min := sst_dtype_uptr_p^.bits_min;
  term.dtype_p^.align_nat := sst_dtype_uptr_p^.align_nat;
  term.dtype_p^.align := sst_dtype_uptr_p^.align;
  term.dtype_p^.size_used := sst_dtype_uptr_p^.size_used;
  term.dtype_p^.size_align := sst_dtype_uptr_p^.size_align;
  term.dtype_p^.pnt_dtype_p := ifarg_p^.exp_p^.dtype_p;
{
*   The ADDR function is reported to have a constant value if the value
*   can be determined at bind time.  This means the argument must be a globally
*   known symbol or a static variable.
}
  term.val_fnd := false;               {init to ADDR function value is not known}
  if ifarg_p^.exp_p^.term1.ttype <> sst_term_var_k {arg not variable reference ?}
    then goto done_ifunc;
  with ifarg_p^.exp_p^.term1.var_var_p^.mod1.top_sym_p^: s do begin {S is arg sym}
    if                                 {ADDR does have constant value ?}
        (sst_symflag_global_k in s.flags) or {symbol is globally known ?}
        (sst_symflag_static_k in s.flags) {symbol is in static storage ?}
        then begin
      term.val.pnt_dtype_p := term.dtype_p; {data type being pointed to}
      term.val.pnt_exp_p := ifarg_p^.exp_p; {expression being pointed to}
      term.val_fnd := true;
      end;
    end;                               {done with S abbreviation}
  end;
{
********
}
sst_ifunc_align_k: begin               {min alignment needed by data type of arg}
  check_ifunc_args (
    term,
    [],                                {argument does not need to have a value}
    1,
    args_n,
    [ sst_dtype_int_k,
      sst_dtype_enum_k,
      sst_dtype_float_k,
      sst_dtype_bool_k,
      sst_dtype_char_k,
      sst_dtype_rec_k,
      sst_dtype_array_k,
      sst_dtype_set_k,
      sst_dtype_proc_k,
      sst_dtype_pnt_k],
    args_dt);
  term.dtype_p := sst_config.int_adr_p;
  term.val.int_val := ifarg_p^.exp_p^.dtype_p^.align;
  term.val_fnd := true;
  term.dtype_hard := false;
  end;
{
********
}
sst_ifunc_atan_k: begin                {intrinsic function ATAN}
  check_ifunc_args (
    term,
    [sst_rwflag_read_k],               {arguments must have readable values}
    2,                                 {number of arguments required}
    args_n,
    [sst_dtype_int_k, sst_dtype_float_k],
    args_dt);
  term.dtype_p := sst_dtype_float_max_p; {result will always be floating point}
  term.dtype_hard := false;
  if term.val_fnd then begin           {both arguments have known constant value ?}
    case ifarg_p^.exp_p^.val.dtype of  {what is base data type of arg 1 ?}
sst_dtype_int_k: r1 := ifarg_p^.exp_p^.val.int_val;
sst_dtype_float_k: r1 := ifarg_p^.exp_p^.val.float_val;
      end;
    ifarg_p := ifarg_p^.next_p;        {go to next argument}
    case ifarg_p^.exp_p^.val.dtype of  {what is base data type of arg 2 ?}
sst_dtype_int_k: r2 := ifarg_p^.exp_p^.val.int_val;
sst_dtype_float_k: r2 := ifarg_p^.exp_p^.val.float_val;
      end;
    if abs(r2) > 1.0E-100
      then begin                       {R2 is big enough to divide by}
        term.val.float_val := arctan(r1/r2);
        end
      else begin
        term.val.float_val := pi_half;
        if r1 < 0                      {take sign of arg 1 into account}
          then term.val.float_val := -term.val.float_val;
        if r2 < 0                      {take sign of arg 2 into account}
          then term.val.float_val := -term.val.float_val;
        end
      ;
    end;                               {done handling args have known constant value}
  end;
{
********
}
sst_ifunc_char_k: begin
  check_ifunc_args (
    term,
    [sst_rwflag_read_k],               {arguments must have readable values}
    1,                                 {number of arguments required}
    args_n,
    [sst_dtype_int_k],
    args_dt);
  term.dtype_p := sst_dtype_char_p;    {function data type is always CHARACTER}
  if term.val_fnd then begin           {argument has known constant value ?}
    term.val.char_val := chr(ifarg_p^.exp_p^.val.int_val);
    end;
  end;
{
********
}
sst_ifunc_cos_k: begin
  check_ifunc_args (
    term,
    [sst_rwflag_read_k],               {arguments must have readable values}
    1,                                 {number of arguments required}
    args_n,
    [sst_dtype_int_k, sst_dtype_float_k],
    args_dt);
  term.dtype_p := sst_dtype_float_max_p; {result will always be floating point}
  term.dtype_hard := false;
  if term.val_fnd then begin           {argument has known constant value ?}
    case ifarg_p^.exp_p^.val.dtype of
sst_dtype_int_k: term.val.float_val := cos(ifarg_p^.exp_p^.val.int_val);
sst_dtype_float_k: term.val.float_val := cos(ifarg_p^.exp_p^.val.float_val);
      end;
    end;                               {done handling arg has known constant value}
  end;
{
********
}
sst_ifunc_dec_k: begin
  check_ifunc_args (
    term,
    [sst_rwflag_read_k],               {arguments must have readable values}
    1,
    args_n,
    [ sst_dtype_int_k,                 {all the data types with ordinal values}
      sst_dtype_enum_k,
      sst_dtype_bool_k,
      sst_dtype_char_k,
      sst_dtype_pnt_k],
    args_dt);
  term.dtype_p := ifarg_p^.exp_p^.dtype_p;
  term.val_fnd := false;
  if sst_dtype_int_k in args_dt
    then term.dtype_hard := false;
  if not ifarg_p^.exp_p^.val_fnd       {input argument is not a known constant ?}
    then goto done_ifunc;

  term.val_fnd := true;                {init to value will be known}
  case ifarg_p^.exp_p^.val.dtype of    {what is data type of input arg's value ?}
sst_dtype_int_k: begin
      term.val.int_val := ifarg_p^.exp_p^.val.int_val - 1;
      end;
sst_dtype_enum_k: begin
      term.val.enum_p := ifarg_p^.exp_p^.val.enum_p^.enum_prev_p;
      if term.val.enum_p = nil then begin {no enumerated value here ?}
        syn_error (ifarg_p^.exp_p^.str_h, 'sst', 'ifunc_dec_min', nil, 0);
        end;
      end;
sst_dtype_char_k: begin
      if ord(ifarg_p^.exp_p^.val.char_val) = 0 then begin
        syn_error (ifarg_p^.exp_p^.str_h, 'sst', 'ifunc_dec_min', nil, 0);
        end;
      term.val.char_val := chr(ord(ifarg_p^.exp_p^.val.char_val) - 1);
      end;
otherwise
    term.val_fnd := false;
    end;
  end;
{
********
}
sst_ifunc_exp_k: begin
  check_ifunc_args (
    term,
    [sst_rwflag_read_k],               {arguments must have readable values}
    1,                                 {number of arguments required}
    args_n,
    [sst_dtype_int_k, sst_dtype_float_k],
    args_dt);
  term.dtype_p := sst_dtype_float_max_p; {result will always be floating point}
  term.dtype_hard := false;
  if term.val_fnd then begin           {argument has known constant value ?}
    case ifarg_p^.exp_p^.val.dtype of
sst_dtype_int_k: term.val.float_val := exp(ifarg_p^.exp_p^.val.int_val);
sst_dtype_float_k: term.val.float_val := exp(ifarg_p^.exp_p^.val.float_val);
      end;
    end;                               {done handling arg has known constant value}
  end;
{
********
}
sst_ifunc_first_k: begin
  check_ifunc_args (
    term,
    [],                                {argument does not need to have a value}
    1,
    args_n,
    [ sst_dtype_int_k,                 {all the data types with ordinal values}
      sst_dtype_enum_k,
      sst_dtype_bool_k,
      sst_dtype_char_k],
    args_dt);
  term.dtype_p := ifarg_p^.exp_p^.dtype_p;
  sst_dtype_resolve (term.dtype_p^, dt_p, dt); {resolve base data types}
  term.val_fnd := true;                {this function always has a value}
  case dt_p^.dtype of
sst_dtype_int_k: begin
      term.val.int_val := lshft(-1, dt_p^.bits_min-1);
      term.dtype_hard := false;
      end;
sst_dtype_enum_k: term.val.enum_p := dt_p^.enum_first_p;
sst_dtype_bool_k: term.val.bool_val := false;
sst_dtype_char_k: term.val.char_val := chr(0);
sst_dtype_range_k: begin
      term.val := dt_p^.range_first_p^.val;
      case dt of                       {what is base data type of subrange ?}
sst_dtype_int_k: term.dtype_hard := false;
        end;
      end;
otherwise
    syn_error (ifarg_p^.exp_p^.str_h, 'sst', 'func_intrinsic_arg_dtype_bad', nil, 0);
    end;
  end;
{
********
}
sst_ifunc_inc_k: begin
  check_ifunc_args (
    term,
    [sst_rwflag_read_k],               {arguments must have readable values}
    1,
    args_n,
    [ sst_dtype_int_k,                 {all the data types with ordinal values}
      sst_dtype_enum_k,
      sst_dtype_bool_k,
      sst_dtype_char_k,
      sst_dtype_pnt_k],
    args_dt);
  term.dtype_p := ifarg_p^.exp_p^.dtype_p;
  term.val_fnd := false;
  if sst_dtype_int_k in args_dt
    then term.dtype_hard := false;
  if not ifarg_p^.exp_p^.val_fnd       {input argument is not a known constant ?}
    then goto done_ifunc;

  term.val_fnd := true;                {init to value will be known}
  case ifarg_p^.exp_p^.val.dtype of    {what is data type of input arg's value ?}
sst_dtype_int_k: begin
      term.val.int_val := ifarg_p^.exp_p^.val.int_val + 1;
      end;
sst_dtype_enum_k: begin
      term.val.enum_p := ifarg_p^.exp_p^.val.enum_p^.enum_next_p;
      if term.val.enum_p = nil then begin {no enumerated value here ?}
        syn_error (ifarg_p^.exp_p^.str_h, 'sst', 'ifunc_inc_max', nil, 0);
        end;
      end;
sst_dtype_char_k: begin
      term.val.char_val := chr(ord(ifarg_p^.exp_p^.val.char_val) + 1);
      end;
otherwise
    term.val_fnd := false;
    end;
  end;
{
********
}
sst_ifunc_int_near_k: begin
  check_ifunc_args (
    term,
    [sst_rwflag_read_k],               {arguments must have readable values}
    1,                                 {number of arguments required}
    args_n,
    [sst_dtype_float_k],
    args_dt);
  term.dtype_p := sst_dtype_int_max_p;
  if term.val_fnd then begin           {argument has known constant value ?}
    term.val.int_val := round(ifarg_p^.exp_p^.val.float_val);
    end;
  term.dtype_hard := false;
  end;
{
********
}
sst_ifunc_int_zero_k: begin
  check_ifunc_args (
    term,
    [sst_rwflag_read_k],               {arguments must have readable values}
    1,                                 {number of arguments required}
    args_n,
    [sst_dtype_float_k],
    args_dt);
  term.dtype_p := sst_dtype_int_max_p;
  if term.val_fnd then begin           {argument has known constant value ?}
    term.val.int_val := trunc(ifarg_p^.exp_p^.val.float_val);
    end;
  term.dtype_hard := false;
  end;
{
********
}
sst_ifunc_last_k: begin
  check_ifunc_args (
    term,
    [],                                {argument does not need to have a value}
    1,
    args_n,
    [ sst_dtype_int_k,                 {all the data types with ordinal values}
      sst_dtype_enum_k,
      sst_dtype_bool_k,
      sst_dtype_char_k],
    args_dt);
  term.dtype_p := ifarg_p^.exp_p^.dtype_p;
  sst_dtype_resolve (term.dtype_p^, dt_p, dt); {resolve base data types}
  term.val_fnd := true;                {this function always has a value}
  case dt_p^.dtype of
sst_dtype_int_k: begin
      term.val.int_val := ~lshft(-1, dt_p^.bits_min-1);
      term.dtype_hard := false;
      end;
sst_dtype_enum_k: term.val.enum_p := dt_p^.enum_last_p;
sst_dtype_bool_k: term.val.bool_val := true;
sst_dtype_char_k: begin
      term.val.char_val := chr(255);
      end;
sst_dtype_range_k: begin
      term.val := dt_p^.range_last_p^.val;
      case dt of                       {what is base data type of subrange ?}
sst_dtype_int_k: term.dtype_hard := false;
        end;
      end;
otherwise
    syn_error (ifarg_p^.exp_p^.str_h, 'sst', 'func_intrinsic_arg_dtype_bad', nil, 0);
    end;
  end;
{
********
}
sst_ifunc_ln_k: begin
  check_ifunc_args (
    term,
    [sst_rwflag_read_k],               {arguments must have readable values}
    1,                                 {number of arguments required}
    args_n,
    [sst_dtype_int_k, sst_dtype_float_k],
    args_dt);
  term.dtype_p := sst_dtype_float_max_p; {result will always be floating point}
  term.dtype_hard := false;
  if term.val_fnd then begin           {argument has known constant value ?}
    case ifarg_p^.exp_p^.val.dtype of
sst_dtype_int_k: begin
        term.val.float_val := ln(ifarg_p^.exp_p^.val.int_val);
        end;
sst_dtype_float_k: begin
        term.val.float_val := ln(ifarg_p^.exp_p^.val.float_val);
        end;
      end;
    end;                               {done handling arg has known constant value}
  end;
{
********
}
sst_ifunc_max_k: begin
  check_ifunc_args (
    term,
    [sst_rwflag_read_k],               {arguments must have readable values}
    args_n_any,                        {number of arguments required}
    args_n,
    [sst_dtype_int_k, sst_dtype_float_k],
    args_dt);
  if args_n < 2 then begin             {too few arguments ?}
    sys_msg_parm_int (msg_parm[1], 2);
    sys_msg_parm_int (msg_parm[2], args_n);
    syn_error (term.str_h, 'sst', 'func_intrinsic_args_too_few', msg_parm, 2);
    end;
  if sst_dtype_float_k in args_dt      {any argument floating point ?}
    then begin                         {result will be floating point}
      term.dtype_p := sst_dtype_float_max_p;
      case ifarg_p^.exp_p^.val.dtype of
sst_dtype_int_k: term.val.float_val := ifarg_p^.exp_p^.val.int_val;
sst_dtype_float_k: term.val.float_val := ifarg_p^.exp_p^.val.float_val;
        end;
      end
    else begin                         {result will be integer}
      term.dtype_p := sst_dtype_int_max_p;
      term.val.int_val := ifarg_p^.exp_p^.val.int_val;
      end
    ;
  ifarg_p := ifarg_p^.next_p;          {go to second argument}

  if term.val_fnd then begin           {all the arguments have known constant val ?}
    while ifarg_p <> nil do begin      {once for each argument after first}
      if sst_dtype_float_k in args_dt
        then begin                     {result is floating point}
          case ifarg_p^.exp_p^.val.dtype of
sst_dtype_int_k: term.val.float_val :=
              max(term.val.float_val, ifarg_p^.exp_p^.val.int_val);
sst_dtype_float_k: term.val.float_val :=
              max(term.val.float_val, ifarg_p^.exp_p^.val.float_val);
            end;
          end
        else begin                     {result is integer}
          term.val.int_val :=
            max(term.val.int_val, ifarg_p^.exp_p^.val.int_val);
          end
        ;
      ifarg_p := ifarg_p^.next_p;      {advance to next argument in chain}
      end;                             {back and process this next argument}
    end;                               {done handling args are known constants}
  term.dtype_hard := false;
  end;                                 {end of intrinsic function MAX}
{
********
}
sst_ifunc_min_k: begin
  check_ifunc_args (
    term,
    [sst_rwflag_read_k],               {arguments must have readable values}
    args_n_any,                        {number of arguments required}
    args_n,
    [sst_dtype_int_k, sst_dtype_float_k],
    args_dt);
  if args_n < 2 then begin             {too few arguments ?}
    sys_msg_parm_int (msg_parm[1], 2);
    sys_msg_parm_int (msg_parm[2], args_n);
    syn_error (term.str_h, 'sst', 'func_intrinsic_args_too_few', msg_parm, 2);
    end;
  if sst_dtype_float_k in args_dt      {any argument floating point ?}
    then begin                         {result will be floating point}
      term.dtype_p := sst_dtype_float_max_p;
      case ifarg_p^.exp_p^.val.dtype of
sst_dtype_int_k: term.val.float_val := ifarg_p^.exp_p^.val.int_val;
sst_dtype_float_k: term.val.float_val := ifarg_p^.exp_p^.val.float_val;
        end;
      end
    else begin                         {result will be integer}
      term.dtype_p := sst_dtype_int_max_p;
      term.val.int_val := ifarg_p^.exp_p^.val.int_val;
      end
    ;
  ifarg_p := ifarg_p^.next_p;          {go to second argument}

  if term.val_fnd then begin           {all the arguments have known constant val ?}
    while ifarg_p <> nil do begin      {once for each argument after first}
      if sst_dtype_float_k in args_dt
        then begin                     {result is floating point}
          case ifarg_p^.exp_p^.val.dtype of
sst_dtype_int_k: term.val.float_val :=
              min(term.val.float_val, ifarg_p^.exp_p^.val.int_val);
sst_dtype_float_k: term.val.float_val :=
              min(term.val.float_val, ifarg_p^.exp_p^.val.float_val);
            end;
          end
        else begin                     {result is integer}
          term.val.int_val :=
            min(term.val.int_val, ifarg_p^.exp_p^.val.int_val);
          end
        ;
      ifarg_p := ifarg_p^.next_p;      {advance to next argument in chain}
      end;                             {back and process this next argument}
    end;                               {done handling args are known constants}
  term.dtype_hard := false;
  end;                                 {end of intrinsic function MIN}
{
********
}
sst_ifunc_offset_k: begin
  check_ifunc_args (
    term,
    [],                                {no read/write access needed to arguments}
    1,
    args_n,
    [ sst_dtype_int_k,
      sst_dtype_enum_k,
      sst_dtype_float_k,
      sst_dtype_bool_k,
      sst_dtype_char_k,
      sst_dtype_rec_k,
      sst_dtype_array_k,
      sst_dtype_set_k,
      sst_dtype_range_k,
      sst_dtype_proc_k,
      sst_dtype_pnt_k],
    args_dt);
  term.dtype_p := sst_dtype_int_max_p; {result data type is always integer}
  term.dtype_hard := false;
  term.val_fnd := true;                {this function always has a value}
  with ifarg_p^.exp_p^.term1: t do begin {T is first term in argument expression}
    if                                 {check for definite errors}
        (t.next_p <> nil) or           {compound expression ?}
        (t.op1 <> sst_op1_none_k) or   {unary operator exists ?}
        (t.op2 <> sst_op2_none_k) or   {binary operator exists ?}
        (t.ttype <> sst_term_var_k)    {term not a "variable" descriptor ?}
      then goto arg_bad_offset;
    end;                               {done with T abbreviation}
  mod_p := addr(ifarg_p^.exp_p^.term1.var_var_p^.mod1); {init pnt to curr modifier}
  while mod_p <> nil do begin          {once for each modifier in var descriptor}
    field_last := false;               {init to last mod was not field in record}
    case mod_p^.modtyp of              {what kind of modifier is this ?}

sst_var_modtyp_top_k: begin            {"root" modifier}
        case mod_p^.top_sym_p^.symtype of {what type of symbol is this ?}
sst_symtype_dtype_k: begin             {root modifier is a data type symbol}
            dt_p := mod_p^.top_sym_p^.dtype_dtype_p;
            end;
sst_symtype_var_k: begin               {root modifier is a variable name}
            dt_p := mod_p^.top_sym_p^.var_dtype_p;
            end;
otherwise goto arg_bad_offset;
          end;                         {end of top modifier symbol type cases}
        term.val.int_val := 0;         {init offset from start of this record}
        end;                           {end of modifier is top modifier}

sst_var_modtyp_unpnt_k: begin          {modifier is pointer dereference}
        if dt_p^.dtype <> sst_dtype_pnt_k {data type is not a pointer ?}
          then goto arg_bad_offset;
        dt_p := dt_p^.pnt_dtype_p;     {pnt to dtype descriptor after unpoint}
        term.val.int_val := 0;         {reset accumulated field offsets to zero}
        end;

sst_var_modtyp_subscr_k: begin         {modifier is array subscript}
        if dt_p^.ar_dtype_rem_p <> nil
          then dt_p := dt_p^.ar_dtype_rem_p
          else dt_p := dt_p^.ar_dtype_ele_p;
        term.val.int_val := 0;         {reset accumulated field offsets to zero}
        end;

sst_var_modtyp_field_k: begin          {modifier is field in record}
        if mod_p^.field_sym_p^.field_ofs_bits <> 0 then begin {not address aligned ?}
          syn_error (ifarg_p^.exp_p^.str_h, 'sst', 'arg_ifunc_offset_unaligned', nil, 0);
          end;
        term.val.int_val := term.val.int_val + {add in offset for this field}
          mod_p^.field_sym_p^.field_ofs_adr;
        field_last := true;            {last modifier was field in record}
        end;

otherwise
      sys_msg_parm_int (msg_parm[1], ord(mod_p^.modtyp));
      syn_error (ifarg_p^.exp_p^.str_h, 'sst', 'var_modifier_unknown', msg_parm, 1);
      end;

    while dt_p^.dtype = sst_dtype_copy_k do begin {resolve base data type}
      dt_p := dt_p^.copy_dtype_p;
      end;
    mod_p := mod_p^.next_p;            {advance to next modifier in chain}
    end;                               {back for next modifier in this var desc}

  if not field_last then goto arg_bad_offset; {last modifier wasn't field in rec ?}
  end;
{
********
}
sst_ifunc_ord_val_k: begin
  check_ifunc_args (
    term,
    [sst_rwflag_read_k],               {arguments must have readable values}
    1,
    args_n,
    [ sst_dtype_int_k,                 {all the data types with ordinal values}
      sst_dtype_enum_k,
      sst_dtype_bool_k,
      sst_dtype_char_k],
    args_dt);
  term.dtype_p := sst_dtype_int_max_p; {result data type is always integer}
  term.dtype_hard := false;
  if term.val_fnd then begin           {argument has known constant value ?}
    sst_ordval (ifarg_p^.exp_p^.val, term.val.int_val, stat); {get ordinal value}
    syn_error_abort (stat, ifarg_p^.exp_p^.str_h, '', '', nil, 0);
    end;
  end;
{
********
}
sst_ifunc_shift_lo_k: begin            {logical shift of arg1 by arg2 bits right}
  check_ifunc_args (
    term,
    [sst_rwflag_read_k],               {arguments must have readable values}
    2,                                 {number of arguments required}
    args_n,
    [sst_dtype_int_k],
    args_dt);
  term.dtype_p := sst_dtype_int_max_p;
  term.dtype_hard := false;
  if term.val_fnd then begin           {arguments have known constant values ?}
    i1 := ifarg_p^.exp_p^.val.int_val; {get first argument value}
    ifarg_p := ifarg_p^.next_p;
    i2 := ifarg_p^.exp_p^.val.int_val; {get second argument value}
    if i2 >= 0
      then begin                       {shifting to the right}
        term.val.int_val := rshft(i1, i2);
        end
      else begin                       {shifting to the left}
        term.val.int_val := lshft(i1, -i2);
        end
      ;
    end;
  end;
{
********
}
sst_ifunc_shiftl_lo_k: begin           {logical shift left arg1 by arg2 bits}
  check_ifunc_args (
    term,
    [sst_rwflag_read_k],               {arguments must have readable values}
    2,                                 {number of arguments required}
    args_n,
    [sst_dtype_int_k],
    args_dt);
  term.dtype_p := sst_dtype_int_max_p;
  term.dtype_hard := false;
  if term.val_fnd then begin           {arguments have known constant values ?}
    i1 := ifarg_p^.exp_p^.val.int_val; {get first argument value}
    ifarg_p := ifarg_p^.next_p;
    i2 := ifarg_p^.exp_p^.val.int_val; {get second argument value}
    term.val.int_val := lshft(i1, i2);
    end;
  end;
{
********
}
sst_ifunc_shiftr_ar_k: begin           {arithmetic shift right arg1 by arg2 bits}
  check_ifunc_args (
    term,
    [sst_rwflag_read_k],               {arguments must have readable values}
    2,                                 {number of arguments required}
    args_n,
    [sst_dtype_int_k],
    args_dt);
  term.dtype_p := sst_dtype_int_max_p;
  term.dtype_hard := false;
  if term.val_fnd then begin           {arguments have known constant values ?}
    i1 := ifarg_p^.exp_p^.val.int_val; {get first argument value}
    ifarg_p := ifarg_p^.next_p;
    i2 := ifarg_p^.exp_p^.val.int_val; {get second argument value}
    term.val.int_val := arshft(i1, i2);
    end;
  end;
{
********
}
sst_ifunc_shiftr_lo_k: begin           {logical shift right arg1 by arg2 bits}
  check_ifunc_args (
    term,
    [sst_rwflag_read_k],               {arguments must have readable values}
    2,                                 {number of arguments required}
    args_n,
    [sst_dtype_int_k],
    args_dt);
  term.dtype_p := sst_dtype_int_max_p;
  term.dtype_hard := false;
  if term.val_fnd then begin           {arguments have known constant values ?}
    i1 := ifarg_p^.exp_p^.val.int_val; {get first argument value}
    ifarg_p := ifarg_p^.next_p;
    i2 := ifarg_p^.exp_p^.val.int_val; {get second argument value}
    term.val.int_val := rshft(i1, i2);
    end;
  end;
{
********
}
sst_ifunc_sin_k: begin
  check_ifunc_args (
    term,
    [sst_rwflag_read_k],               {arguments must have readable values}
    1,                                 {number of arguments required}
    args_n,
    [sst_dtype_int_k, sst_dtype_float_k],
    args_dt);
  term.dtype_p := sst_dtype_float_max_p; {result will always be floating point}
  term.dtype_hard := false;
  if term.val_fnd then begin           {argument has known constant value ?}
    case ifarg_p^.exp_p^.val.dtype of
sst_dtype_int_k: term.val.float_val := sin(ifarg_p^.exp_p^.val.int_val);
sst_dtype_float_k: term.val.float_val := sin(ifarg_p^.exp_p^.val.float_val);
      end;
    end;                               {done handling arg has known constant value}
  end;
{
********
}
sst_ifunc_size_align_k: begin          {size padded to its own alignment rule}
  check_ifunc_args (
    term,
    [],                                {argument does not need to have a value}
    1,
    args_n,
    [ sst_dtype_int_k,
      sst_dtype_enum_k,
      sst_dtype_float_k,
      sst_dtype_bool_k,
      sst_dtype_char_k,
      sst_dtype_rec_k,
      sst_dtype_array_k,
      sst_dtype_set_k,
      sst_dtype_proc_k,
      sst_dtype_pnt_k],
    args_dt);
  term.dtype_p := sst_config.int_adr_p;
  term.dtype_hard := false;
  term.val.int_val := ifarg_p^.exp_p^.dtype_p^.size_align;
  term.val_fnd := true;
  end;
{
********
}
sst_ifunc_size_char_k: begin
  check_ifunc_args (
    term,
    [],                                {argument does not need to have a value}
    1,
    args_n,
    [sst_dtype_char_k, sst_dtype_array_k],
    args_dt);
  term.dtype_p := sst_config.int_adr_p;
  term.dtype_hard := false;
  dt_p := ifarg_p^.exp_p^.dtype_p;     {resolve base data type of argument}
  while dt_p^.dtype = sst_dtype_copy_k do dt_p := dt_p^.copy_dtype_p;
  case dt_p^.dtype of                  {what data type ID of argument ?}
sst_dtype_char_k: term.val.int_val := 1;
sst_dtype_array_k: begin
      if not dt_p^.ar_string then begin
        syn_error (ifarg_p^.exp_p^.str_h, 'sst', 'func_intrinsic_arg_dtype_bad', nil, 0);
        end;
      term.val.int_val := dt_p^.ar_ind_n;
      end;
    end;
  term.val_fnd := true;
  end;
{
********
}
sst_ifunc_size_min_k: begin
  check_ifunc_args (
    term,
    [],                                {argument does not need to have a value}
    1,
    args_n,
    [ sst_dtype_int_k,
      sst_dtype_enum_k,
      sst_dtype_float_k,
      sst_dtype_bool_k,
      sst_dtype_char_k,
      sst_dtype_rec_k,
      sst_dtype_array_k,
      sst_dtype_set_k,
      sst_dtype_proc_k,
      sst_dtype_pnt_k],
    args_dt);
  term.dtype_p := sst_config.int_adr_p;
  term.dtype_hard := false;
  term.val.int_val := ifarg_p^.exp_p^.dtype_p^.size_used;
  term.val_fnd := true;
  end;
{
********
}
sst_ifunc_sqr_k: begin                 {value is square of argument}
  check_ifunc_args (                   {get info about and check ifunc arguments}
    term,                              {term descriptor for whole intrinsic function}
    [sst_rwflag_read_k],               {arguments must have readable values}
    1,                                 {number of arguments required}
    args_n,                            {number of arguments found}
    [sst_dtype_int_k, sst_dtype_float_k], {set of all the legal base data type IDs}
    args_dt);                          {set of all base data type IDs found}
  case ifarg_p^.exp_p^.val.dtype of    {what is base data type ID of term}
sst_dtype_int_k: begin                 {argument is integer}
      term.dtype_p := sst_dtype_int_max_p;
      if term.val_fnd then begin       {argument has known value ?}
        term.val.int_val := sqr(ifarg_p^.exp_p^.val.int_val);
        end;
      end;
sst_dtype_float_k: begin               {argument is floating point}
      term.dtype_p := sst_dtype_float_max_p;
      if term.val_fnd then begin       {argument has known value ?}
        term.val.float_val := sqr(ifarg_p^.exp_p^.val.float_val);
        end;
      end;
    end;                               {end of argument data type cases}
  term.dtype_hard := false;
  end;
{
********
}
sst_ifunc_sqrt_k: begin                {value is square root of argument}
  check_ifunc_args (                   {get info about and check ifunc arguments}
    term,                              {term descriptor for whole intrinsic function}
    [sst_rwflag_read_k],               {arguments must have readable values}
    1,                                 {number of arguments required}
    args_n,                            {number of arguments found}
    [sst_dtype_int_k, sst_dtype_float_k], {set of all the legal base data type IDs}
    args_dt);                          {set of all base data type IDs found}
  term.dtype_p := sst_dtype_float_max_p; {result will always be floating point}
  term.dtype_hard := false;
  if term.val_fnd then begin           {argument has known constant value ?}
    case ifarg_p^.exp_p^.val.dtype of
sst_dtype_int_k: term.val.float_val := sqrt(ifarg_p^.exp_p^.val.int_val);
sst_dtype_float_k: term.val.float_val := sqrt(ifarg_p^.exp_p^.val.float_val);
      end;
    end;                               {done handling arg has known constant value}
  end;
{
********
}
sst_ifunc_xor_k: begin                 {exclusive or of all arguments}
  check_ifunc_args (
    term,
    [sst_rwflag_read_k],               {arguments must have readable values}
    args_n_any,                        {number of arguments required}
    args_n,
    [sst_dtype_int_k],
    args_dt);
  if args_n < 2 then begin             {too few arguments ?}
    sys_msg_parm_int (msg_parm[1], 2);
    sys_msg_parm_int (msg_parm[2], args_n);
    syn_error (term.str_h, 'sst', 'func_intrinsic_args_too_few', msg_parm, 2);
    end;
  term.dtype_p := sst_dtype_int_max_p; {result is always integer}
  term.dtype_hard := false;
  if term.val_fnd then begin           {all the arguments have known value ?}
    term.val.int_val := ifarg_p^.exp_p^.val.int_val; {init value to first argument}
    ifarg_p := ifarg_p^.next_p;        {advance to second argument}
    while ifarg_p <> nil do begin      {once for each argument 2 thru N}
      term.val.int_val :=              {accumulate XOR result}
        xor(term.val.int_val, ifarg_p^.exp_p^.val.int_val);
      ifarg_p := ifarg_p^.next_p;      {advance to next argument in chain}
      end;                             {back and process this new argument}
    end;                               {done handling term has know constant value}
  end;
{
********
}
sst_ifunc_setinv_k: begin              {inversion of a set}
  check_ifunc_args (
    term,                              {term descriptor for whole intrinsic function}
    [sst_rwflag_read_k],               {arguments must have readable values}
    1,                                 {number of arguments required}
    args_n,                            {number of arguments found}
    [sst_dtype_set_k],                 {set of all the legal base data type IDs}
    args_dt);                          {set of all base data type IDs found}
  term.dtype_p := ifarg_p^.exp_p^.dtype_p; {same data type as argument}
  term.dtype_hard := ifarg_p^.exp_p^.dtype_hard;
  if not term.dtype_hard then begin    {can't invert set of unsure data type}
    syn_error (term.str_h, 'sst', 'set_invert_dtype_ambiguous', nil, 0);
    end;
  term.val_fnd := false;               {resolving value not implemented yet}
  end;
{
********
}
otherwise
    sys_msg_parm_int (msg_parm[1], ord(term.ifunc_id));
    syn_error (term.str_h, 'sst', 'func_intrinsic_unknown', msg_parm, 1);
    end;                               {end of intrinsic function ID cases}
done_ifunc:                            {jump here if done processing ifunc}
  end;                                 {done with term is an intrinsic function}
{
******************************
*
*   Term is a type-casting function.
}
sst_term_type_k: begin
  term.dtype_p := term.type_dtype_p;
  term.rwflag := term.type_exp_p^.rwflag;
  if term.dtype_p^.size_used <> term.type_exp_p^.dtype_p^.size_used then begin
    syn_error (term.str_h, 'sst', 'type_cast_mismatch_size', nil, 0);
    end;
{
*   The constant value is resolved after the type-casting function for some
*   of the resulting data types when the argument has a known ordinal value.
}
  if term.type_exp_p^.val_fnd then begin {argument has known value ?}
    sst_ordval (                       {try to get argument's ordinal value}
      term.type_exp_p^.val,            {argument's known value descriptor}
      i1,                              {returned ordinal value}
      stat);                           {error if no constant ordinal value exists}
    if sys_error(stat) then goto done_term_type; {can't convert this arg's value}
    sst_dtype_resolve (                {get resulting term's base data types}
      term.dtype_p^, dt_p, term.val.dtype);
    case term.val.dtype of             {what kind of constant data type is here ?}
sst_dtype_int_k: term.val.int_val := i1;
otherwise
      goto done_term_type;             {can't convert into these types}
      end;
    term.val_fnd := true;
    end;
  end;
{
******************************
*
*   Term is a SET.
}
sst_term_set_k: begin
  term.rwflag := [sst_rwflag_read_k];  {set with explicit elements is read-only}
  sst_dtype_new (term.dtype_p);        {create and init data type descriptor}
  term.dtype_p^.dtype := sst_dtype_set_k; {data type is SET}
  term.dtype_p^.set_dtype_final := false; {init to data type not wholly known}
  ele_p := term.set_first_p;           {init current set element to first}
  if ele_p = nil then begin            {NULL set ?}
    term.dtype_p^.set_dtype_p := sst_dtype_bool_p; {pick simple data type}
    term.dtype_p^.set_n_ent := 0;
    goto done_term_type;               {all done processing this term}
    end;
  term.dtype_p^.set_dtype_p := nil;    {indicate element data type not set yet}
  while ele_p <> nil do begin          {once for each element listed in set}
    do_ele_exp (term, ele_p^.first_p^); {process ele value or range start expression}
    if ele_p^.last_p <> nil            {range end expression exists ?}
      then do_ele_exp (term, ele_p^.last_p^); {process range end expression}
    ele_p := ele_p^.next_p;            {advance to next element in set}
    end;                               {back and process this new set element}
{
*   TERM.DTYPE_P^.SET_DTYPE_P is pointing to the base data type descriptor for
*   the set elements.  ORD_MIN and ORD_MAX are the min/max possible ordinal
*   values for any elements as far as we know.  If this range matches the
*   full range of the base data type, then use it directly for the elements
*   base data type.  Otherwise, create an implicit subrange data type for
*   the set elements.
}
  case term.dtype_p^.set_dtype_p^.dtype of
sst_dtype_int_k: begin
      ord_min_dt := ord_min - 1;       {always force subrange of this data type}
      ord_max_dt := ord_max + 1;
      end;
sst_dtype_enum_k: begin
      ord_min_dt := 0;
      ord_max_dt := term.dtype_p^.set_dtype_p^.enum_last_p^.enum_ordval;
      end;
sst_dtype_bool_k: begin
      ord_min_dt := 0;
      ord_max_dt := 1;
      end;
sst_dtype_char_k: begin
      ord_min_dt := 0;
      ord_max_dt := 255;
      end;
    end;                               {end of set element data type cases}
  if
      (ord_min = ord_min_dt) and
      (ord_max = ord_max_dt)
    then begin                         {the subrange spans the whole data type}
      term.dtype_p^.set_dtype_final := true; {we know final data type for this set}
      end
    else begin                         {create subrange of base data type}
      if (ord_max - ord_min) > 1023 then begin {subrange too large ?}
        ord_min := 0;                  {pick arbitrary subrange}
        ord_max := 0;
        end;
      sst_dtype_new (dt_p);            {create and init new data type descriptor}
      dt_p^.dtype := sst_dtype_range_k; {new data type is a subrange}
      dt_p^.bits_min := sst_config.int_machine_p^.bits_min; {init base fields}
      dt_p^.align_nat := sst_config.int_machine_p^.align_nat;
      dt_p^.align := sst_config.int_machine_p^.align;
      dt_p^.size_used := sst_config.int_machine_p^.size_used;
      dt_p^.size_align := sst_config.int_machine_p^.size_align;
      dt_p^.range_dtype_p := term.dtype_p^.set_dtype_p; {point to base data type}
      dt_p^.range_first_p := nil;
      dt_p^.range_last_p := nil;
      dt_p^.range_ord_first := ord_min; {starting ordinal value of range}
      dt_p^.range_n_vals := ord_max - ord_min + 1; {number of values in range}
      term.dtype_p^.set_dtype_p := dt_p; {subrange dtype is dtype of set elements}
      end
    ;
  term.dtype_p^.bits_min := ord_max - ord_min + 1; {one bit for each set element}
  term.dtype_p^.align_nat := sst_config.int_machine_p^.align_nat;
  term.dtype_p^.align := sst_config.int_machine_p^.align;
  term.dtype_p^.size_used :=
    ( (term.dtype_p^.bits_min + sst_config.int_machine_p^.bits_min - 1)
      div sst_config.int_machine_p^.bits_min)
    * sst_config.int_machine_p^.size_align;
  term.dtype_p^.size_align := term.dtype_p^.size_used;
  term.dtype_p^.set_n_ent :=
    (term.dtype_p^.bits_min + sst_set_ele_per_word - 1) div
    sst_set_ele_per_word;
  term.dtype_hard := term.dtype_p^.set_dtype_final;
  end;
{
******************************
*
*   Term is a nested expression.
}
sst_term_exp_k: begin
  sst_exp_eval (term.exp_exp_p^, nval_err); {evaluate nested expression}
  term.dtype_p := term.exp_exp_p^.dtype_p; {copy data type from expression}
  term.rwflag := term.exp_exp_p^.rwflag; {copy read/write flags from expression}
  term.dtype_hard := term.exp_exp_p^.dtype_hard; {copy data type is hard flag}
  term.val_fnd := term.exp_exp_p^.val_fnd; {copy value exists flag from expression}
  if term.val_fnd then begin           {expression has known value ?}
    term.val := term.exp_exp_p^.val;   {copy value from expression}
    end;
  end;
{
******************************
*
*   Unrecognized term type.
}
otherwise
    sys_msg_parm_int (msg_parm[1], ord(term.ttype));
    syn_error (term.str_h, 'sst', 'term_type_unknown', msg_parm, 1);
    end;                               {end of term type cases}
done_term_type:                        {jump here if done with term type case}
  sst_dtype_resolve (                  {set base data types of term}
    term.dtype_p^, dt_p, term.val.dtype);
{
*   Handle preceeding unadic operator, if any.
}
  if term.op1 <> sst_op1_none_k then begin {unadic operator exists ?}
    term.rwflag :=                     {term with unadic operator is not writeable}
      term.rwflag - [sst_rwflag_write_k];
      case term.val.dtype of           {which base data type is term}
{
*   Data type is INTEGER.
}
sst_dtype_int_k: begin                 {value is INTEGER}
        case term.op1 of
sst_op1_plus_k: ;
sst_op1_minus_k:
          if term.val_fnd then term.val.int_val := -term.val.int_val;
sst_op1_1comp_k:
          if term.val_fnd then term.val.int_val := ~term.val.int_val;
otherwise goto bad_op1;
          end;
        end;
{
*   Data type is FLOATING POINT.
}
sst_dtype_float_k: begin               {value is FLOATING POINT}
        case term.op1 of
sst_op1_plus_k: ;
sst_op1_minus_k:
          if term.val_fnd then term.val.float_val := -term.val.float_val;
otherwise goto bad_op1;
          end;
        end;
{
*   Data type is BOOLEAN.
}
sst_dtype_bool_k: begin                {value is BOOLEAN}
        case term.op1 of
sst_op1_not_k:
          if term.val_fnd then term.val.bool_val := not term.val.bool_val;
otherwise goto bad_op1;
          end;
        end;
{
*   The remaining data types are not allowed to have any unadic operator.
}
otherwise
bad_op1:                               {jump here on bad unadic operator}
      syn_error (term.str_h, 'sst', 'operator_mismatch_unadic', nil, 0);
      end;                             {end of data type cases}
    end;                               {done handling unadic operator exists}
{
*   Done handling unadic operator.
}
leave:                                 {common exit point}
  if nval_err and (not term.val_fnd) then begin {need value but not possible ?}
    syn_error (term.str_h, 'sst', 'exp_not_const_val', nil, 0);
    end;
  return;
{
*   Error exits.  These bomb out and don't return.
}
arg_bad_offset:
  syn_error (ifarg_p^.exp_p^.str_h, 'sst', 'arg_ifunc_offset_bad', nil, 0);
  end;
