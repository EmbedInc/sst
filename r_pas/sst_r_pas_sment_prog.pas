{   Subroutine SST_R_PAS_SMENT_PROG (STR_PROG_H)
*
*   Process PROGRAM_STATEMENT syntax.  We assume we stay in this program until
*   a new PROGRAM statement.  STR_PROG_H is the string handle to the
*   PROGRAM_STATEMENT syntax.
}
module sst_r_pas_SMENT_PROG;
define sst_r_pas_sment_prog;
%include 'sst_r_pas.ins.pas';

procedure sst_r_pas_sment_prog (       {process PROGRAM_STATEMENT syntax}
  in      str_prog_h: syo_string_t);   {string handle to PROGRAM_STATEMENT syntax}

var
  tag: sys_int_machine_t;              {syntax tag from .syn file}
  str_h: syo_string_t;                 {handle to string for a tag}
  sym_p: sst_symbol_p_t;               {pointer to program name symbol descriptor}
  stat: sys_err_t;

begin
  case nest_level of                   {how deep are we nested in blocks}
0: ;                                   {not in any block, no problem}
1: begin                               {already in top MODULE or PROGRAM block}
      sst_opcode_pos_pop;              {this finishes block we are currently in}
      sst_scope_old;                   {pop back to previous scope}
      nest_level := nest_level - 1;    {should now be above any top block}
      end;
otherwise
    syo_error (str_prog_h, 'sst_pas_read', 'prog_not_allowed_here', nil, 0);
    end;
  nest_level := nest_level + 1;        {down into this PROGRAM block}
  top_block := top_block_prog_k;       {indicate top block is a PROGRAM}

  syo_level_down;                      {down into PROGRAM_STATEMENT syntax level}
  syo_get_tag_msg (                    {get tag for program name}
    tag, str_h, 'sst_pas_read', 'statement_prog_bad', nil, 0);
  if tag <> 1 then begin               {unexpected TAG value}
    syo_error_tag_unexp (tag, str_h);
    end;

  sst_symbol_new (                     {add program name to symbol table}
    str_h, syo_charcase_down_k, sym_p, stat);
  sst_scope_new;                       {make new scope level for this program}
  sym_p^.prog_scope_p := sst_scope_p;  {save pointer to program's scope}
  syo_error_abort (stat, str_h, '', '', nil, 0);
  sym_p^.symtype := sst_symtype_prog_k; {this symbol is a program name}
  sym_p^.flags := sym_p^.flags +
    [sst_symflag_def_k, sst_symflag_used_k];
  sst_scope_p^.symbol_p := sym_p;      {point scope to its defining symbol}

  sst_opcode_new;                      {make opcode for this program}
  sst_opc_p^.opcode := sst_opc_prog_k; {this opcode is a program}
  sst_opc_p^.str_h := str_h;           {save source file string handle}
  sst_opc_p^.prog_sym_p := sym_p;      {point opcode to program symbol}
  sst_opcode_pos_push (sst_opc_p^.prog_p); {new opcodes are for this program}


  syo_get_tag_msg (                    {get next tag in PROGRAM_STATEMENT}
    tag, str_h, 'sst_pas_read', 'statement_prog_bad', nil, 0);
  if tag <> syo_tag_end_k then begin   {there should be no tag here}
    syo_error_tag_unexp (tag, str_h);
    end;
  syo_level_up;                        {back up from PROGRAM_STATEMENT syntax}
  end;
