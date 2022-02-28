{   Subroutine SST_R_SYO_COMMAND (STAT)
*
*   Process COMMAND syntax.  This is the top level SYN file syntax.
}
module sst_r_syo_command;
define sst_r_syo_command;
%include 'sst_r_syo.ins.pas';

procedure sst_r_syo_command (          {process COMMAND syntax}
  out     stat: sys_err_t);            {completion status code}

var
  tag: sys_int_machine_t;              {tag from syntax tree}
  str_h: syo_string_t;                 {handle to string from input file}

begin
  sys_error_none (stat);               {init to no errors}

  syo_level_down;                      {down into COMMAND syntax level}
  syo_get_tag_msg (tag, str_h, 'sst_syo_read', 'syerr_command', nil, 0);
  case tag of
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
      sst_r_syo_define;                {process DEFINE syntax}
      end;
{
*   Symbol declaration.
}
3:  begin
      sst_r_syo_declare;               {process DECLARE syntax}
      end;

otherwise
    syo_error_tag_unexp (tag, str_h);
    end;                               {end of top level tag cases}

  syo_level_up;                        {back up from COMMAND syntax level}
  end;
