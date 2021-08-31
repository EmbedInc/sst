{   Subroutine SST_R_PAS_STATEMENTS
*
*   Build a chain of opcodes from the STATEMENTS syntax.  All the opcodes on the
*   chain will be of "executable" type.
}
module sst_r_pas_STATEMENTS;
define sst_r_pas_statements;
%include 'sst_r_pas.ins.pas';

procedure sst_r_pas_statements;        {build opcodes from STATEMENTS syntax}

var
  tag: sys_int_machine_t;              {syntax tag ID}
  str_h: syo_string_t;                 {handle to string associated with TAG}

label
  tag_loop;

begin
  sst_scope_p^.flag_ref_used := true;  {referenced symbols will be flagged as used}
  syo_level_down;                      {down into STATEMENTS syntax level}

tag_loop:                              {back here each new RAW_STATEMENT syntax}
  syo_get_tag_msg (tag, str_h, 'sst_pas_read', 'statement_exec_bad', nil, 0);
  case tag of
{
*   Hit end of STATMENTS syntax.
}
syo_tag_end_k: begin
  syo_level_up;                        {up from STATEMENTS syntax}
  return;
  end;
{
*   Tag is a new RAW_STATEMENT syntax.
}
1: begin
  sst_r_pas_raw_sment;                 {add RAW_STATEMENT onto opcodes chain}
  end;
{
*   Unexpected tag value.
}
otherwise
    syo_error_tag_unexp (tag, str_h);
    end;                               {end of tag value cases}
  goto tag_loop;                       {back for next syntax tag in STATEMENTS}
  end;
