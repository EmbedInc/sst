{   Module of routines that help writing code to deal with jump targets.
}
module sst_r_syo_goto;
define sst_r_syo_goto;
%include 'sst_r_syo.ins.pas';
{
************************************************
*
*   Subroutine SST_R_SYO_GOTO (JTARG, FLAGS, SYM_MFLAG)
*
*   Make sure execution ends up as specified in the jump targets JTARG.
*   At this point, the MFLAG variable indicates the yes/no value.
*   SYM_MFLAG is the symbol descriptor for this MFLAG variable.
*   Execution is already at the right place for the "fall thru" case.
}
procedure sst_r_syo_goto (             {go to jump targets, as required}
  in out  jtarg: jump_targets_t;       {indicates where execution is to end up}
  in      flags: jtarg_t;              {indicates which jump targets to use}
  in      sym_mflag: sst_symbol_t);    {handle to MFLAG symbol}
  val_param;

begin
{
*   Jump to ERR target if not fall thru.
}
  if
      (jtarg_err_k in flags) and       {this jump target enabled ?}
      ( (not (jflag_fall_k in jtarg.err.flags)) or {not fall thru ?}
        (jflag_indir_k in jtarg.err.flags)) {indirect reference ?}
      then begin
    sst_opcode_new;                    {create new opcode for IF}
    sst_opc_p^.opcode := sst_opc_if_k;
    sst_opc_p^.str_h.first_char.crange_p := nil;
    sst_opc_p^.str_h.first_char.ofs := 0;
    sst_opc_p^.str_h.last_char := sst_opc_p^.str_h.first_char;
    sst_opc_p^.if_exp_p := sst_exp_make_var(sym_error_p^);
    sst_opc_p^.if_true_p := nil;
    sst_opc_p^.if_false_p := nil;

    sst_opcode_pos_push (sst_opc_p^.if_true_p); {set up for writing TRUE code}

    sst_opcode_new;                    {create GOTO opcode}
    sst_opc_p^.opcode := sst_opc_goto_k;
    sst_opc_p^.str_h.first_char.crange_p := nil;
    sst_opc_p^.str_h.first_char.ofs := 0;
    sst_opc_p^.str_h.last_char := sst_opc_p^.str_h.first_char;
    sst_r_syo_jtarget_sym (            {get or make jump target symbol}
      jtarg.err,                       {jump target descriptor}
      sst_opc_p^.goto_sym_p);          {returned pointer to label symbol}

    sst_opcode_pos_pop;                {done writing TRUE case opcodes}
    end;
{
*   Jump to YES target if not fall thru.
}
  if
      (jtarg_yes_k in flags) and       {this jump target enabled ?}
      ( (not (jflag_fall_k in jtarg.yes.flags)) or {not fall thru ?}
        (jflag_indir_k in jtarg.yes.flags)) {indirect reference ?}
      then begin
    sst_opcode_new;                    {create new opcode for IF}
    sst_opc_p^.opcode := sst_opc_if_k;
    sst_opc_p^.str_h.first_char.crange_p := nil;
    sst_opc_p^.str_h.first_char.ofs := 0;
    sst_opc_p^.str_h.last_char := sst_opc_p^.str_h.first_char;
    sst_r_syo_comp_var_sym (           {create comparison expression}
      sym_mflag,                       {symbol for term 1}
      sym_mflag_yes_p^,                {enum constant for term 2}
      sst_op2_eq_k,                    {comparison operation}
      sst_opc_p^.if_exp_p);            {returned pointer to new expression}
    sst_opc_p^.if_true_p := nil;
    sst_opc_p^.if_false_p := nil;

    sst_opcode_pos_push (sst_opc_p^.if_true_p); {set up for writing TRUE code}

    sst_opcode_new;                    {create GOTO opcode}
    sst_opc_p^.opcode := sst_opc_goto_k;
    sst_opc_p^.str_h.first_char.crange_p := nil;
    sst_opc_p^.str_h.first_char.ofs := 0;
    sst_opc_p^.str_h.last_char := sst_opc_p^.str_h.first_char;
    sst_r_syo_jtarget_sym (            {get or make jump target symbol}
      jtarg.yes,                       {jump target descriptor}
      sst_opc_p^.goto_sym_p);          {returned pointer to label symbol}

    sst_opcode_pos_pop;                {done writing TRUE case opcodes}
    end;
{
*   Jump to NO target if not fall thru.
}
  if
      (jtarg_no_k in flags) and        {this jump target enabled ?}
      ( (not (jflag_fall_k in jtarg.no.flags)) or {not fall thru ?}
        (jflag_indir_k in jtarg.no.flags)) {indirect reference ?}
      then begin
    sst_opcode_new;                    {create new opcode for IF}
    sst_opc_p^.opcode := sst_opc_if_k;
    sst_opc_p^.str_h.first_char.crange_p := nil;
    sst_opc_p^.str_h.first_char.ofs := 0;
    sst_opc_p^.str_h.last_char := sst_opc_p^.str_h.first_char;
    sst_r_syo_comp_var_sym (           {create comparison expression}
      sym_mflag,                       {symbol for term 1}
      sym_mflag_no_p^,                 {enum constant for term 2}
      sst_op2_eq_k,                    {comparison operation}
      sst_opc_p^.if_exp_p);            {returned pointer to new expression}
    sst_opc_p^.if_true_p := nil;
    sst_opc_p^.if_false_p := nil;

    sst_opcode_pos_push (sst_opc_p^.if_true_p); {set up for writing TRUE code}

    sst_opcode_new;                    {create GOTO opcode}
    sst_opc_p^.opcode := sst_opc_goto_k;
    sst_opc_p^.str_h.first_char.crange_p := nil;
    sst_opc_p^.str_h.first_char.ofs := 0;
    sst_opc_p^.str_h.last_char := sst_opc_p^.str_h.first_char;
    sst_r_syo_jtarget_sym (            {get or make jump target symbol}
      jtarg.no,                        {jump target descriptor}
      sst_opc_p^.goto_sym_p);          {returned pointer to label symbol}

    sst_opcode_pos_pop;                {done writing TRUE case opcodes}
    end;

  end;
