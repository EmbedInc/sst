{   Subroutine SST_R_PAS_IFUNC (SYM, TERM)
*
*   Process an intrinsic function call.  SYM is the symbol descriptor for the
*   intrinsic function.  TERM is the expression term descriptor to fill in.
*   The OP2, OP1, STR_H, and VAL_EVAL fields have already been set or initialized.
*   This routine needs to fill in the TTYPE field, and the fields specific to
*   the particular chosen term type.  The DTYPE_P, VAL_FND,
*   VAL, and DTYPE_HARD fields will be inferred later from the other information.
*
*   The syntax position is in the ITEM syntax, and the last tag read was for
*   VARIABLE right before any possible function arguments.  The next tag is
*   for the function arguments, if any.  The syntax position should be left
*   at the end of the ITEM syntax.
}
module sst_r_pas_IFUNC;
define sst_r_pas_ifunc;
%include 'sst_r_pas.ins.pas';

procedure sst_r_pas_ifunc (            {process intrinsic function call}
  in      sym: sst_symbol_t;           {symbol descriptor for intrinsic function}
  in out  term: sst_exp_term_t);       {filled in descriptor for term in expression}

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  tag: sys_int_machine_t;              {syntax tag ID}
  str_h: syn_string_t;                 {handle to string associated with TAG}
  n_args: sys_int_machine_t;           {number of function arguments found}
  exp_chain_p: sst_exp_chain_p_t;      {points to one link in func args chain}
  exp_chain_pp: sst_exp_chain_pp_t;    {points to current end of func args chain}
  exp_p: sst_exp_p_t;                  {scratch pointer to expression descriptor}
  term_p: sst_exp_term_p_t;            {scratch pointer to term in an expression}
  ele_p: sst_ele_exp_p_t;              {points to current set element descriptor}
  ele_pp: ^sst_ele_exp_p_t;            {points to set elements expression chain link}
  dt_p: sst_dtype_p_t;                 {scratch pointer to data type}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

label
  loop_arg;
{
*************************************************************
*
*   Local subroutine CHECK_ARGS_N (N)
*
*   Check that the number of function arguments matches N.
}
procedure check_args_n (
  in      n: sys_int_machine_t);       {number of arguments required}

begin
  if n_args = n then return;           {no problems ?}
  sys_msg_parm_int (msg_parm[1], n);
  syn_error (str_h, 'sst', 'func_intrinsic_args_n_bad', msg_parm, 1);
  end;
{
*************************************************************
*
*   Local subroutine INSERT_EXP (EXP, EXP_CHAIN_P)
*
*   Insert the expression EXP into an expressions chain.  EXP_CHAIN_P is the
*   pointer to the expression just before where EXP is to be inserted.
}
procedure insert_exp (
  in      exp: sst_exp_t;              {expression to add to chain}
  in out  exp_chain_p: sst_exp_chain_p_t); {chain pointer to start of chain}

var
  chain_p: sst_exp_chain_p_t;          {points to new chain link record}

begin
  sst_mem_alloc_scope (sizeof(chain_p^), chain_p); {create new chain link descriptor}
  chain_p^.exp_p := addr(exp);         {point chain link to its expression}
  chain_p^.next_p := exp_chain_p;      {point to next link in chain}
  exp_chain_p := chain_p;              {insert new link in chain}
  end;
{
*************************************************************
*
*   Start of main routine.
}
begin
{
*   Init the term as if the Pascal intrinsic function maps directly to a
*   translator intrinsic function (since most do).  Since no information is
*   lost in this process, any functions that don't map directly can be handled
*   later.
}
  term.ttype := sst_term_ifunc_k;      {init to term is an intrinsic function}
  n_args := 0;                         {init number of function arguments found}
  syn_get_tag_msg (                    {get function arguments tag}
    tag, str_h, 'sst_pas_read', 'exp_bad', nil, 0);
  case tag of

1: begin                               {no function arguments exists here}
      term.ifunc_args_p := nil;
      end;

2: begin                               {function arguments exist within ()}
      syn_level_down;                  {down into FUNCTION_ARGUMENTS syntax}
      exp_chain_pp := addr(term.ifunc_args_p); {init pointer to end of args chain}
loop_arg:                              {back here each new function argument}
      syn_get_tag_msg (                {get tag for argument expression}
        tag, str_h, 'sst_pas_read', 'exp_bad', nil, 0);
      case tag of
1:      begin                          {TAG is for new function argument expression}
          sst_mem_alloc_scope (        {get mem for new link in args chain}
            sizeof(exp_chain_p^), exp_chain_p);
          exp_chain_pp^ := exp_chain_p; {add new link to end of chain}
          exp_chain_p^.next_p := nil;  {init new link as end of chain}
          exp_chain_pp := addr(exp_chain_p^.next_p); {update pointer to end of chain}
          if sym.front_p^.ifunc = ifunc_addr_k then begin {ADDR function ?}
            addr_of := true;           {indicate we are processing arg to ADDR}
            end;
          sst_r_pas_exp (str_h, false, exp_chain_p^.exp_p); {get func arg expression}
          if sym.front_p^.ifunc = ifunc_addr_k then begin {ADDR function ?}
            addr_of := false;          {done processing ADDR argument}
            end;
          n_args := n_args + 1;        {count one more function argument}
          goto loop_arg;               {back for next function argument}
          end;
syn_tag_end_k: begin
          syn_level_up;                {back up from FUNCTION_ARGUMENTS syntax}
          end;
otherwise                              {unexpected tag value in FUNCTION_ARGUMENTS}
        syn_error_tag_unexp (tag, str_h);
        end;
      end;                             {end of function args exist case}

otherwise                              {unexpected tag value in ITEM syntax}
    syn_error_tag_unexp (tag, str_h);
    end;
{
*   TERM has been initialized as an intrinsic function with the function arguments
*   chain in place.  The intrinsic function type has not been set.  N_ARGS is
*   the number of function arguments that were found.
}
  str_h := term.str_h;                 {save locally so that nested proc can see it}
  case sym.front_p^.ifunc of           {which intrinsic function is this ?}
{
*******************
}
ifunc_abs_k: begin
  term.ifunc_id := sst_ifunc_abs_k;
  end;
{
*******************
}
ifunc_addr_k: begin
  term.ifunc_id := sst_ifunc_addr_k;
  end;
{
*******************
}
ifunc_arctan_k: begin
  check_args_n (1);
  sst_exp_const_float (1.0, exp_p);    {add 1.0 as second argument}
  insert_exp (exp_p^, term.ifunc_args_p^.next_p);
  term.ifunc_id := sst_ifunc_atan_k;
  end;
{
*******************
}
ifunc_arshft_k: begin
  term.ifunc_id := sst_ifunc_shiftr_ar_k;
  end;
{
*******************
}
ifunc_chr_k: begin
  term.ifunc_id := sst_ifunc_char_k;
  end;
{
*******************
}
ifunc_cos_k: begin
  term.ifunc_id := sst_ifunc_cos_k;
  end;
{
*******************
}
ifunc_exp_k: begin
  term.ifunc_id := sst_ifunc_exp_k;
  end;
{
*******************
}
ifunc_firstof_k: begin
  term.ifunc_id := sst_ifunc_first_k;
  end;
{
*******************
}
ifunc_lastof_k: begin
  term.ifunc_id := sst_ifunc_last_k;
  end;
{
*******************
}
ifunc_ln_k: begin
  term.ifunc_id := sst_ifunc_ln_k;
  end;
{
*******************
}
ifunc_lshft_k: begin
  term.ifunc_id := sst_ifunc_shiftl_lo_k;
  end;
{
*******************
}
ifunc_max_k: begin
  term.ifunc_id := sst_ifunc_max_k;
  end;
{
*******************
}
ifunc_min_k: begin
  term.ifunc_id := sst_ifunc_min_k;
  end;
{
*******************
*
*   Pascal intrinsic function ODD.  This function returns TRUE if the argument
*   is odd.  The argument must be of type integer.
*
*   This function will be emulated with the expression:
*
*     (arg & 1) <> 0
*
*   EXP_P will be used to point to the top expression descriptor.  TERM_P will
*   will point to each term in the expression in turn.
}
ifunc_odd_k: begin
  check_args_n (1);                    {must have exactly one argument}
  sst_mem_alloc_scope (sizeof(exp_p^), exp_p); {allocate top expression descriptor}
  exp_p^.str_h := term.str_h;
  exp_p^.val_eval := false;
  with term.ifunc_args_p^.exp_p^: arg do begin {ARG is exp desc for argument}
    if arg.term1.next_p = nil
      then begin                       {argument expression has only one term}
        exp_p^.term1 := arg.term1;     {argument will be first term in new exp}
        end
      else begin                       {argument expression has more than one term}
        exp_p^.term1.next_p := nil;
        exp_p^.term1.op2 := sst_op2_none_k;
        exp_p^.term1.op1 := sst_op1_none_k;
        exp_p^.term1.ttype := sst_term_exp_k;
        exp_p^.term1.str_h := exp_p^.str_h;
        exp_p^.term1.val_eval := false;
        exp_p^.term1.exp_exp_p := addr(arg);
        sst_term_eval (exp_p^.term1, false);
        end
      ;                                {done setting first term in expression}
    end;                               {done with ARG abbreviation}

  sst_mem_alloc_scope (sizeof(term_p^), term_p); {alloc memory for second term}
  exp_p^.term1.next_p := term_p;
  term_p^.next_p := nil;
  term_p^.op2 := sst_op2_btand_k;
  term_p^.op1 := sst_op1_none_k;
  term_p^.ttype := sst_term_const_k;
  term_p^.str_h := term.ifunc_args_p^.exp_p^.str_h;
  term_p^.dtype_p := sst_dtype_int_max_p;
  term_p^.dtype_hard := false;
  term_p^.val_eval := true;
  term_p^.val_fnd := true;
  term_p^.val.dtype := sst_dtype_int_k;
  term_p^.val.int_val := 1;
  term_p^.rwflag := [sst_rwflag_read_k];

  sst_mem_alloc_scope (                {allocate memory for third term}
    sizeof(term_p^.next_p^), term_p^.next_p);
  term_p := term_p^.next_p;            {make third term the current term}
  term_p^.next_p := nil;
  term_p^.op2 := sst_op2_ne_k;
  term_p^.op1 := sst_op1_none_k;
  term_p^.ttype := sst_term_const_k;
  term_p^.str_h := term.ifunc_args_p^.exp_p^.str_h;
  term_p^.dtype_p := sst_dtype_int_max_p;
  term_p^.dtype_hard := false;
  term_p^.val_eval := true;
  term_p^.val_fnd := true;
  term_p^.val.dtype := sst_dtype_int_k;
  term_p^.val.int_val := 0;
  term_p^.rwflag := [sst_rwflag_read_k];

  sst_exp_eval (exp_p^, false);        {evaluate compound expression, if possible}
  term.ttype := sst_term_exp_k;        {term for ifunc value is an expression}
  term.exp_exp_p := exp_p;             {point term to its expression descriptor}
  end;
{
*******************
}
ifunc_ord_k: begin
  term.ifunc_id := sst_ifunc_ord_val_k;
  end;
{
*******************
}
ifunc_pred_k: begin
  term.ifunc_id := sst_ifunc_dec_k;
  end;
{
*******************
}
ifunc_round_k: begin
  term.ifunc_id := sst_ifunc_int_near_k;
  end;
{
*******************
}
ifunc_rshft_k: begin
  term.ifunc_id := sst_ifunc_shiftr_lo_k;
  end;
{
*******************
}
ifunc_sin_k: begin
  term.ifunc_id := sst_ifunc_sin_k;
  end;
{
*******************
}
ifunc_sizeof_k: begin
  term.ifunc_id := sst_ifunc_size_align_k;
  end;
{
*******************
}
ifunc_sqr_k: begin
  term.ifunc_id := sst_ifunc_sqr_k;
  end;
{
*******************
}
ifunc_sqrt_k: begin
  term.ifunc_id := sst_ifunc_sqrt_k;
  end;
{
*******************
}
ifunc_succ_k: begin
  term.ifunc_id := sst_ifunc_inc_k;
  end;
{
*******************
}
ifunc_trunc_k: begin
  term.ifunc_id := sst_ifunc_int_zero_k;
  end;
{
*******************
}
ifunc_xor_k: begin
  term.ifunc_id := sst_ifunc_xor_k;
  end;
{
*******************
}
ifunc_alignof_k: begin
  term.ifunc_id := sst_ifunc_align_k;
  end;
{
*******************
}
ifunc_arctan2_k: begin
  term.ifunc_id := sst_ifunc_atan_k;
  end;
{
*******************
}
ifunc_offset_k: begin
  term.ifunc_id := sst_ifunc_offset_k;
  end;
{
*******************
}
ifunc_shift_k: begin
  term.ifunc_id := sst_ifunc_shift_lo_k;
  end;
{
*******************
}
ifunc_szchar_k: begin
  term.ifunc_id := sst_ifunc_size_char_k;
  end;
{
*******************
}
ifunc_szmin_k: begin
  term.ifunc_id := sst_ifunc_size_min_k;
  end;
{
*******************
}
ifunc_val_k: begin
  check_args_n (1);                    {must have exatly one argument}
  if not term.ifunc_args_p^.exp_p^.val_fnd then begin {argument not a known const ?}
    syn_error (term.ifunc_args_p^.exp_p^.str_h, 'sst', 'exp_not_const_val', nil, 0);
    end;
  term.val := term.ifunc_args_p^.exp_p^.val;
  term.ttype := sst_term_const_k;
  end;
{
*******************
*
*   COG Intrinsic function SETOF.  The first argument declares the SET data type,
*   and the remaining arguments are set element expressions.
}
ifunc_setof_k: begin
  exp_chain_p := term.ifunc_args_p;    {init pointer to first SETOF argument}
  if exp_chain_p = nil then begin
    syn_error (term.str_h, 'sst_pas_read', 'setof_arg1_missing', nil, 0);
    end;
  term.dtype_p := exp_chain_p^.exp_p^.dtype_p; {init function's data type}
  while term.dtype_p^.dtype = sst_dtype_copy_k {resolve base data type}
    do term.dtype_p := term.dtype_p^.copy_dtype_p;
  if term.dtype_p^.dtype <> sst_dtype_set_k then begin
    syn_error (term.str_h, 'sst_pas_read', 'setof_dtype_bad', nil, 0);
    end;
  if                                   {specified data type is ambiguous ?}
      (not exp_chain_p^.exp_p^.dtype_hard) or
      (not term.dtype_p^.set_dtype_final) then begin
    syn_error (term.str_h, 'sst_pas_read', 'setof_dtype_ambiguous', nil, 0);
    end;
  dt_p := term.dtype_p^.set_dtype_p;   {resolve base data type of set elements}
  while dt_p^.dtype = sst_dtype_copy_k do dt_p := dt_p^.copy_dtype_p;
{
*   Move the remaining arguments onto set elements expressions chain.
}
  term.set_first_p := nil;             {init to no element expressions exist}
  ele_pp := addr(term.set_first_p);    {init pointer to end of elements exp chain}
  exp_chain_p := exp_chain_p^.next_p;  {init pointer to first ifunc arg descriptor}
  while exp_chain_p <> nil do begin    {once for each intrinsic function argument}
    sst_exp_useage_check (             {check that expression is valid ele value}
      exp_chain_p^.exp_p^,             {expression to check}
      [sst_rwflag_read_k],             {expression must be readable}
      dt_p^);                          {must be convertable to base elements dtype}
    sst_mem_alloc_scope (sizeof(ele_p^), ele_p); {alloc mem for set ele chain desc}
    ele_p^.next_p := nil;              {fill in set descriptor for this set element}
    ele_p^.first_p := exp_chain_p^.exp_p;
    ele_p^.last_p := nil;
    ele_pp^ := ele_p;                  {link this element expression to chain}
    ele_pp := addr(ele_p^.next_p);
    exp_chain_p := exp_chain_p^.next_p; {advance to next intrinsic function argument}
    end;                               {back and process this new ifunc arg}

  term.ttype := sst_term_set_k;        {indicate term is a set expression}
  term.dtype_hard := true;
  term.val_eval := true;
  term.val_fnd := false;
  term.val.dtype := sst_dtype_set_k;
  term.rwflag := [sst_rwflag_read_k];
  end;                                 {end of intrinsic function SETOF}
{
*******************
}
otherwise
    sys_msg_parm_int (msg_parm[1], ord(sym.front_p^.ifunc));
    syn_error (term.str_h, 'sst_pas_read', 'ifunc_unexpected', msg_parm, 1);
    end;
  end;
