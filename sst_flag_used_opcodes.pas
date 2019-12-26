{   Subroutine SST_FLAG_USED_OPCODES (FIRST_P)
*
*   Set the USED flags for all symbols eventually referenced by a chain of
*   ocpcodes.  FIRST_P is pointing to the first opcode in the chain.  It is
*   permissable for FIRST_P to be NIL to indicate an empty chain.
*
*   It is assumed that the front end has at least flagged the basic compilable
*   blocks (modules, programs, routines) as USED where apporpriate.  Unused
*   blocks will be skipped here.
}
module sst_FLAG_USED_OPCODES;
define sst_flag_used_opcodes;
%include 'sst2.ins.pas';

procedure sst_flag_used_opcodes (      {flag symbols eventually used from opcodes}
  in      first_p: sst_opc_p_t);       {points to first opcode in chain, may be NIL}

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  opc_p: sst_opc_p_t;                  {pointer to current opcode descriptor}
  case_val_p: sst_case_val_p_t;        {points to curr descriptor in chain}
  case_opc_p: sst_case_opc_p_t;        {points to curr descriptor in chain}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

begin
  opc_p := first_p;                    {init pointer to current opcode}
  while opc_p <> nil do begin          {loop thru each opcode in this chain}
    case opc_p^.opcode of              {what kind of opcode is this ?}

sst_opc_module_k: begin                {start of a grouping of routines}
  if sst_symflag_used_k in opc_p^.module_sym_p^.flags then begin {used ?}
    sst_flag_used_symbol (opc_p^.module_sym_p^);
    sst_flag_used_opcodes (opc_p^.module_p);
    end;
  end;

sst_opc_prog_k: begin                  {start of top level program}
  if sst_symflag_used_k in opc_p^.prog_sym_p^.flags then begin {used ?}
    sst_flag_used_symbol (opc_p^.prog_sym_p^);
    sst_flag_used_opcodes (opc_p^.prog_p);
    end;
  end;

sst_opc_rout_k: begin                  {start of a routine}
  if sst_symflag_used_k in opc_p^.rout_sym_p^.flags then begin {used ?}
    sst_flag_used_symbol (opc_p^.rout_sym_p^);
    sst_flag_used_opcodes (opc_p^.rout_p);
    end;
  end;

sst_opc_exec_k: begin                  {points to chain of executable code}
  sst_flag_used_opcodes (opc_p^.exec_p);
  end;

sst_opc_label_k: begin                 {indicate handle for a label}
  sst_flag_used_symbol (opc_p^.label_sym_p^);
  end;

sst_opc_call_k: begin                  {subroutine call}
  sst_flag_used_var (opc_p^.call_var_p^);
  sst_flag_used_rout (opc_p^.call_proc_p^);
  end;

sst_opc_assign_k: begin                {assignment statement}
  sst_flag_used_var (opc_p^.assign_var_p^);
  sst_flag_used_exp (opc_p^.assign_exp_p^);
  end;

sst_opc_goto_k: begin                  {unconditional transfer of control}
  sst_flag_used_symbol (opc_p^.goto_sym_p^);
  end;

sst_opc_case_k: begin                  {execute one of N blocks of code}
  sst_flag_used_exp (opc_p^.case_exp_p^);
  sst_flag_used_opcodes (opc_p^.case_none_p);
  case_val_p := opc_p^.case_val_p;
  while case_val_p <> nil do begin     {loop thru all the CASE_VAL blocks}
    sst_flag_used_exp (case_val_p^.exp_p^);
    case_val_p := case_val_p^.next_val_p;
    end;
  case_opc_p := opc_p^.case_opc_p;
  while case_opc_p <> nil do begin     {loop thru all the CASE_OPC blocks}
    sst_flag_used_opcodes (case_opc_p^.code_p);
    case_opc_p := case_opc_p^.next_p;
    end;
  end;

sst_opc_if_k: begin                    {IF ... THEN ... ELSE ... statement}
  sst_flag_used_exp (opc_p^.if_exp_p^);
  sst_flag_used_opcodes (opc_p^.if_true_p);
  sst_flag_used_opcodes (opc_p^.if_false_p);
  end;

sst_opc_loop_cnt_k: begin              {counted loop (Pascal FOR, Fortran DO, etc)}
  sst_flag_used_var (opc_p^.lpcn_var_p^);
  sst_flag_used_exp (opc_p^.lpcn_exp_start_p^);
  sst_flag_used_exp (opc_p^.lpcn_exp_end_p^);
  sst_flag_used_exp (opc_p^.lpcn_exp_inc_p^);
  sst_flag_used_opcodes (opc_p^.lpcn_code_p);
  end;

sst_opc_loop_ttop_k: begin             {loop with test at start of loop}
  sst_flag_used_exp (opc_p^.lptp_exp_p^);
  sst_flag_used_opcodes (opc_p^.lptp_code_p);
  end;

sst_opc_loop_tbot_k: begin             {loop with test at end of loop}
  sst_flag_used_exp (opc_p^.lpbt_exp_p^);
  sst_flag_used_opcodes (opc_p^.lpbt_code_p);
  end;

sst_opc_loop_next_k: begin             {go to start of next time around loop}
  end;

sst_opc_loop_exit_k: begin             {unconditionally exit loop}
  end;

sst_opc_return_k: begin                {return from subroutine}
  end;

sst_opc_abbrev_k: begin                {abbreviations in effect for block of code}
  sst_flag_used_opcodes (opc_p^.abbrev_code_p);
  end;

sst_opc_discard_k: begin               {call function, but discard its return value}
  sst_flag_used_exp (opc_p^.discard_exp_p^);
  end;

sst_opc_write_k: begin                 {write expression value to standard output}
  sst_flag_used_exp (opc_p^.write_exp_p^);
  if opc_p^.write_width_exp_p <> nil then begin
    sst_flag_used_exp (opc_p^.write_width_exp_p^);
    end;
  if opc_p^.write_width2_exp_p <> nil then begin
    sst_flag_used_exp (opc_p^.write_width2_exp_p^);
    end;
  end;

sst_opc_write_eol_k: begin             {write end of line to standard output}
  end;


otherwise
      sys_msg_parm_int (msg_parm[1], ord(opc_p^.opcode));
      syn_error (opc_p^.str_h, 'sst', 'opcode_unexpected', msg_parm, 1);
      end;                             {end of opcode type cases}
    opc_p := opc_p^.next_p;            {advance to next opcode in this chain}
    end;                               {back and process this new opcode}
  end;
