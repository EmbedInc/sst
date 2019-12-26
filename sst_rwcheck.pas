{   Subroutine SST_RWCHECK (RW_USED,RW_ALLOWED,STAT)
*
*   Check for proper read/write access.  RW_USED indicates the type of
*   access attempted, and RW_ALLOWED indicates the legal access types.
*   STAT is returned with no error if the access is legal.
}
module sst_RWCHECK;
define sst_rwcheck;
%include 'sst2.ins.pas';

procedure sst_rwcheck (                {check for proper read/write access used}
  in      rw_used: sst_rwflag_t;       {read/write access actually used}
  in      rw_allowed: sst_rwflag_t;    {read/write access allowed}
  out     stat: sys_err_t);            {no error if legal access}

begin
  sys_error_none (stat);               {init to access is legal}
  if rw_used <= rw_allowed then return; {access is completely legal ?}

  if
      (sst_rwflag_write_k in rw_used) and
      (not (sst_rwflag_write_k in rw_allowed))
      then begin
    sys_stat_set (sst_subsys_k, sst_stat_write_bad_k, stat);
    return;
    end;

  if
      (sst_rwflag_read_k in rw_used) and
      (not (sst_rwflag_read_k in rw_allowed))
      then begin
    sys_stat_set (sst_subsys_k, sst_stat_read_bad_k, stat);
    return;
    end;

  writeln ('Internal error in subroutine SST_RWCHECK.');
  sys_bomb;
  end;
