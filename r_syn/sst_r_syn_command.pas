{   Subroutine SST_R_SYN_COMMAND (STAT)
*
*   Process COMMAND syntax.  This is the top level SYN file syntax.
}
module sst_r_syn_command;
define sst_r_syn_command;
%include 'sst_r_syn.ins.pas';

procedure sst_r_syn_command (          {process COMMAND syntax}
  out     stat: sys_err_t);            {completion status code}

label
  trerr;

begin
  sys_error_none (stat);               {init to no errors}

  if not syn_trav_down (syn_p^)        {down into COMMAND syntax level}
    then goto trerr;

  case syn_trav_next_tag(syn_p^) of    {which tag ?}
{
*   End of input data.
}
1:  begin
      sys_stat_set (sst_subsys_k, sst_stat_eod_k, stat); {indicate EOD}
      end;
{
*   Syntax definition.
}
2:  begin
      sst_r_syn_define;                {process DEFINE syntax}
      end;
{
*   Symbol declaration.
}
3:  begin
      sst_r_syn_declare;               {process DECLARE syntax}
      end;

otherwise
    syn_msg_tag_bomb (syn_p^, 'sst_syn_read', 'syerr_command', nil, 0);
    end;                               {end of top level tag cases}

  if not syn_trav_up (syn_p^)          {back up from COMMAND syntax level}
    then goto trerr;
  return;
{
*   The syntax tree is not as expected.  We assume this is due to a syntax
*   error.
}
trerr:
  sys_message ('sst_syn_read', 'syerr_command');
  syn_parse_err_show (syn_p^);
  sys_bomb;
  end;
