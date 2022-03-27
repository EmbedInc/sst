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
  err;

begin
  sys_error_none (stat);               {init to no errors}

  if not syn_trav_down (syn_p^)        {down into COMMAND syntax level}
    then goto err;

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
    syn_trav_tag_err (syn_p^, 'sst_syn_read', 'syerr_command', nil, 0);
    sys_bomb;
    end;                               {end of top level tag cases}

  if not syn_trav_up (syn_p^)          {back up from COMMAND syntax level}
    then goto err;
  return;

err:
  sys_message_bomb ('sst_syn_read', 'syerr_command', nil, 0);
  end;
