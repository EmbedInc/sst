{   Module of routines that generate comparison expressions.
}
module sst_r_syo_compare;
define sst_r_syo_comp_var_int;
define sst_r_syo_comp_var_sym;
define sst_r_syo_set_mflag;
%include 'sst_r_syo.ins.pas';
{
*******************************************************
}
procedure sst_r_syo_comp_var_int (     {create exp comparing var to integer constant}
  in      sym1: sst_symbol_t;          {variable for first term in comparison}
  in      val: sys_int_machine_t;      {integer value to compare against}
  in      op: sst_op2_k_t;             {comparison operator}
  out     exp_p: sst_exp_p_t);         {returned pointer to new expression}
  val_param;

var
  term_p: sst_exp_term_p_t;            {pointer to second term in expression}

begin
  exp_p := sst_exp_make_var (sym1);    {init expression with just variable}

  sst_mem_alloc_scope (sizeof(term_p^), term_p); {get mem for second term}
  exp_p^.term1.next_p := term_p;       {link new term as second term in exp}

  term_p^.next_p := nil;               {init term descriptor}
  term_p^.op2 := op;
  term_p^.op1 := sst_op1_none_k;
  term_p^.str_h.first_char.crange_p := nil;
  term_p^.str_h.first_char.ofs := 0;
  term_p^.str_h.last_char := term_p^.str_h.first_char;
  term_p^.rwflag := [sst_rwflag_read_k];
  term_p^.ttype := sst_term_const_k;
  term_p^.dtype_p := sym_int_machine_t_p^.dtype_dtype_p;
  term_p^.dtype_hard := false;
  term_p^.val_eval := true;
  term_p^.val_fnd := true;
  term_p^.val.dtype := sst_dtype_int_k;
  term_p^.val.int_val := val;

  exp_p^.val_eval := false;            {reset expression to not evaluated}
  sst_exp_eval (exp_p^, false);        {re-evaluate compound expression}
  end;
{
*******************************************************
}
procedure sst_r_syo_comp_var_sym (     {create exp comparing var to other symbol}
  in      sym1: sst_symbol_t;          {variable for first term in comparison}
  in      sym2: sst_symbol_t;          {symbol for second term in comparison}
  in      op: sst_op2_k_t;             {comparison operator}
  out     exp_p: sst_exp_p_t);         {returned pointer to new expression}
  val_param;

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  term_p: sst_exp_term_p_t;            {pointer to second term in expression}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

begin
  exp_p := sst_exp_make_var (sym1);    {init expression with just variable}

  sst_mem_alloc_scope (sizeof(term_p^), term_p); {get mem for second term}
  exp_p^.term1.next_p := term_p;       {link new term as second term in exp}

  term_p^.next_p := nil;               {init term descriptor}
  term_p^.op2 := op;
  term_p^.op1 := sst_op1_none_k;
  term_p^.str_h.first_char := sym2.char_h;
  term_p^.str_h.last_char := sym2.char_h;
  term_p^.val_eval := false;
  term_p^.val_fnd := false;
  term_p^.rwflag := [sst_rwflag_read_k];

  case sym2.symtype of

sst_symtype_const_k: begin             {symbol 2 is a mnemonic constant}
      term_p^.ttype := sst_term_const_k;
      term_p^.dtype_p := sym2.const_exp_p^.dtype_p;
      term_p^.dtype_hard := sym2.const_exp_p^.dtype_hard;
      term_p^.val_eval := true;
      term_p^.val_fnd := true;
      term_p^.val := sym2.const_exp_p^.val;
      end;

sst_symtype_enum_k: begin              {symbol 2 is enumerated constant}
      term_p^.ttype := sst_term_const_k;
      term_p^.dtype_p := sym2.enum_dtype_p;
      term_p^.dtype_hard := true;
      term_p^.val_eval := true;
      term_p^.val_fnd := true;
      term_p^.val.dtype := sst_dtype_enum_k;
      term_p^.val.enum_p := addr(sym2);
      end;

otherwise
    sys_msg_parm_int (msg_parm[1], ord(sym2.symtype));
    sys_message_bomb ('sst', 'symbol_type_unknown', msg_parm, 1);
    end;

  exp_p^.val_eval := false;            {reset expression to not evaluated}
  sst_exp_eval (exp_p^, false);        {re-evaluate compound expression}
  end;
{
*******************************************************
*
*   Subroutine SST_R_SYO_SET_MFLAG (SYM1, SYM2, OP, USE_ERR, SYM_MFLAG)
*
*   Create opcodes that set the MFLAG variable to either YES or NO,
*   depending on the result of a comparison.  SYM1 is a variable used for
*   the first term of the comparision, and SYM2 is a symbol used for the
*   second term of the comparision.  OP indicates the comparision operator.
*   If USE_ERR is TRUE, then the whole comparison will always be TRUE when
*   the ERROR flag is set.  The MFLAG variable is set to YES when the
*   comparison is TRUE, NO otherwise.
}
procedure sst_r_syo_set_mflag (        {set MFLAG YES if comparison TRUE, else NO}
  in      sym1: sst_symbol_t;          {variable for first term in comparison}
  in      sym2: sst_symbol_t;          {symbol for second term in comparison}
  in      op: sst_op2_k_t;             {comparison operator}
  in      use_err: boolean;            {TRUE if OR error flag to comparison}
  in      sym_mflag: sst_symbol_t);    {handle to MFLAG symbol}
  val_param;

var
  exp_p: sst_exp_p_t;                  {pointer to simple comparison expression}
  expe_p: sst_exp_p_t;                 {pointer to expression with error flag}
  term_p: sst_exp_term_p_t;            {pointer to error flag expression term}
  var_p: sst_var_p_t;                  {pointer to variable descriptor for ERROR}

begin
  sst_opcode_new;                      {create opcode for IF statement}
  sst_opc_p^.opcode := sst_opc_if_k;
  sst_r_syo_comp_var_sym (             {make raw comparison expression}
    sym1, sym2,                        {the symbols to compare}
    op,                                {the comparison operator}
    exp_p);                            {returned pointer to comparision expression}
  if use_err
    then begin                         {need to OR error flag with comparison}
      sst_mem_alloc_scope (sizeof(expe_p^), expe_p); {alloc mem for new descriptors}
      sst_mem_alloc_scope (sizeof(term_p^), term_p);
      sst_mem_alloc_scope (sizeof(var_p^), var_p);

      expe_p^.str_h := exp_p^.str_h;   {fill in top level expression descriptor}
      expe_p^.dtype_p := nil;
      expe_p^.dtype_hard := true;
      expe_p^.val_eval := false;
      expe_p^.val_fnd := false;
      expe_p^.rwflag := [sst_rwflag_read_k];

      expe_p^.term1.next_p := term_p;  {fill in first term in top expression}
      expe_p^.term1.op2 := sst_op2_none_k;
      expe_p^.term1.op1 := sst_op1_none_k;
      expe_p^.term1.ttype := sst_term_exp_k;
      expe_p^.term1.str_h := exp_p^.str_h;
      expe_p^.term1.dtype_p := nil;
      expe_p^.term1.dtype_hard := true;
      expe_p^.term1.val_eval := false;
      expe_p^.term1.val_fnd := false;
      expe_p^.term1.rwflag := exp_p^.rwflag;
      expe_p^.term1.exp_exp_p := exp_p;

      term_p^.next_p := nil;           {fill in second term in top expression}
      term_p^.op2 := sst_op2_or_k;
      term_p^.op1 := sst_op1_none_k;
      term_p^.ttype := sst_term_var_k;
      term_p^.str_h := exp_p^.str_h;
      term_p^.dtype_p := nil;
      term_p^.dtype_hard := true;
      term_p^.val_eval := false;
      term_p^.val_fnd := false;
      term_p^.rwflag := [sst_rwflag_read_k, sst_rwflag_write_k];
      term_p^.var_var_p := var_p;

      var_p^.dtype_p := sym_error_p^.var_dtype_p; {fill in variable reference desc}
      var_p^.rwflag := [sst_rwflag_read_k, sst_rwflag_write_k];
      var_p^.vtype := sst_vtype_var_k;
      var_p^.mod1.next_p := nil;
      var_p^.mod1.modtyp := sst_var_modtyp_top_k;
      var_p^.mod1.top_str_h := exp_p^.str_h;
      var_p^.mod1.top_sym_p := sym_error_p;

      sst_opc_p^.if_exp_p := expe_p;   {set expression to use for IF decision}
      end
    else begin                         {not figuring error flag in comparison}
      sst_opc_p^.if_exp_p := exp_p;    {use raw comparison expression directly}
      end
    ;
  sst_exp_eval (sst_opc_p^.if_exp_p^, false); {fully evaluate conditional expression}
{
*   Write code for YES case.
}
  sst_opcode_pos_push (sst_opc_p^.if_true_p); {set up for writing TRUE code}
  sst_r_syo_opc_assign (sym_mflag, sym_mflag_yes_p^);
  sst_opcode_pos_pop;                  {pop back to parent opcode chain}
{
*   Write code for NO case.
}
  sst_opcode_pos_push (sst_opc_p^.if_false_p); {set up for writing FALSE code}
  sst_r_syo_opc_assign (sym_mflag, sym_mflag_no_p^);
  sst_opcode_pos_pop;                  {pop back to parent opcode chain}
  end;
