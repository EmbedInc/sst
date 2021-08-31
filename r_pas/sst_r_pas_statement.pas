{   Subroutine SST_R_PAS_STATEMENT (STAT)
*
*   Process STATEMENT construction.  STAT is returned with EOD status when end of
*   data is encountered.
}
module sst_r_pas_STATEMENT;
define sst_r_pas_statement;
%include 'sst_r_pas.ins.pas';

procedure sst_r_pas_statement (        {process STATEMENT construction}
  out     stat: sys_err_t);

var
  tag: sys_int_machine_t;              {tag number from .syn file}
  str_h: syo_string_t;                 {handle to string for current tag}

begin
  sys_error_none (stat);               {init STAT to indicate no error}
  syo_level_down;                      {down into STATEMENT construction}
  syo_get_tag_msg (tag, str_h, 'sst_pas_read', 'statement_bad', nil, 0);
  case tag of
{
*   CONST
}
1: begin
  sst_r_pas_sment_const;               {process CONST_STATEMENT}
  end;
{
*   TYPE
}
2: begin
  sst_r_pas_sment_type;                {process TYPE_STATEMENT}
  end;
{
*   VAR
}
3: begin
  sst_r_pas_sment_var;                 {process VAR_STATEMENT}
  end;
{
*   LABEL
}
4: begin
  sst_r_pas_sment_label;               {process LABEL_STATEMENT syntax}
  end;
{
*   DEFINE
}
5: begin
  sst_r_pas_sment_define;              {process DEFINE_STATEMENT syntax}
  end;
{
*   Routine heading
}
6: begin
  sst_r_pas_sment_rout (str_h);        {process ROUTINE_HEADING syntax}
  end;
{
*   Executable block
}
7: begin
  sst_opcode_new;                      {make opcode descriptor for executable block}
  sst_opc_p^.opcode := sst_opc_exec_k; {opcode is chain of executable statements}
  sst_opc_p^.str_h := str_h;           {save source file char range handle}

  sst_opcode_pos_push (sst_opc_p^.exec_p); {executable opcodes get chained on here}
  syo_level_down;                      {down into EXECUTABLE_BLOCK syntax}
  syo_get_tag_msg (tag, str_h, 'sst_pas_read', 'statement_bad', nil, 0);
  sst_r_pas_statements;                {build opcodes from STATEMENTS syntax}
  syo_level_up;                        {back up from EXECUTABLE_BLOCK syntax}
  sst_opcode_pos_pop;                  {end of block of executable code}

  sst_opcode_pos_pop;                  {end of program/subroutine}
  sst_scope_old;                       {pop back to parent scope from prog/subr}
  nest_level := nest_level - 1;        {one less layer deep in nested blocks}
  end;
{
*   PROGRAM
}
8: begin
  sst_r_pas_sment_prog (str_h);        {process PROGRAM_STATEMENT syntax}
  end;
{
*   MODULE
}
9: begin
  sst_r_pas_sment_module (str_h);      {process MODULE_STATEMENT syntax}
  end;
{
*   End of data.
}
10: begin
  sys_stat_set (sst_subsys_k, sst_stat_eod_k, stat); {indicate end of data condition}
  end;

otherwise
    syo_error_tag_unexp (tag, str_h);
    end;                               {done with all the statement type cases}
  end;
