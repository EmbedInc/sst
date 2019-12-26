{   Subroutine SST_ROUTINES_MATCH (PROC1, PROC2, STAT)
*
*   Check whether two routine definitions are compatible.  PROC1 must be the
*   descriptor for a routine template.  If PROC2 also describes a routine, then
*   both must match exactly (argument names, data types, and any flags).
*
*   If PROC2 describes a call to a procedure, then it must conform to the
*   template PROC1.
*
*   STAT is returned with no error if the routines matched.
}
module sst_ROUTINES_MATCH;
define sst_routines_match;
%include 'sst2.ins.pas';

procedure sst_routines_match (         {check that two routine descriptions match}
  in      proc1: sst_proc_t;           {descriptor for first routine}
  in      proc2: sst_proc_t;           {descriptor for second routine}
  out     stat: sys_err_t);            {no error if routines match}

var
  arg1_p: sst_proc_arg_p_t;            {points to argument of routine 1}
  arg2_p: sst_proc_arg_p_t;            {points to argument of routine 2}
  argn: sys_int_machine_t;             {current argument number}
  dt1_p, dt2_p: sst_dtype_p_t;         {resolved data type descriptors}

label
  arg_dtype_ok, arg_err;

begin
  if proc2.sym_p <> nil then begin     {routine 2 also a template ?}
    if
        (sst_symflag_global_k in proc2.sym_p^.flags) <>
        (sst_symflag_global_k in proc1.sym_p^.flags)
        then begin
      sys_stat_set (sst_subsys_k, sst_stat_rout_glbl_nmatch_k, stat);
      return;
      end;
    if
        (sst_symflag_extern_k in proc2.sym_p^.flags) <>
        (sst_symflag_extern_k in proc1.sym_p^.flags)
        then begin
      sys_stat_set (sst_subsys_k, sst_stat_rout_ext_nmatch_k, stat);
      return;
      end;
    if
        (sst_procflag_noreturn_k in proc2.flags) <>
        (sst_procflag_noreturn_k in proc1.flags)
        then begin
      sys_stat_set (sst_subsys_k, sst_stat_rout_nret_nmatch_k, stat);
      return;
      end;
    end;                               {done with routine 2 is also a template}

  if
      (proc2.dtype_func_p = nil) or
      (proc1.dtype_func_p = nil)
    then begin                         {at least one is a non-function routine}
      if proc2.dtype_func_p <> proc1.dtype_func_p then begin {one is a function ?}
        sys_stat_set (sst_subsys_k, sst_stat_rout_dtypef_nmatch_k, stat);
        return;
        end;
      end
    else begin                         {both routines are functions}
      dt1_p := proc1.dtype_func_p;     {resolve base data type for routine 1}
      while dt1_p^.dtype = sst_dtype_copy_k do begin
        dt1_p := dt1_p^.copy_dtype_p;
        end;
      dt2_p := proc2.dtype_func_p;     {resolve base data type for routine 2}
      while dt2_p^.dtype = sst_dtype_copy_k do begin
        dt2_p := dt2_p^.copy_dtype_p;
        end;
      if dt1_p <> dt2_p then begin     {function data types mismatched ?}
        sys_stat_set (sst_subsys_k, sst_stat_rout_dtypef_nmatch_k, stat);
        return;
        end;
      end
    ;

  if proc2.n_args <> proc1.n_args then begin
    sys_stat_set (sst_subsys_k, sst_stat_rout_nargs_nmatch_k, stat);
    return;
    end;

  arg1_p := proc1.first_arg_p;         {init curr arg to first in list}
  arg2_p := proc2.first_arg_p;
  argn := 1;
  while arg1_p <> nil do begin         {loop thru each call argument template}
    dt1_p := arg1_p^.dtype_p;          {resolve base data type for arg template}
    while dt1_p^.dtype = sst_dtype_copy_k
      do dt1_p := dt1_p^.copy_dtype_p;
    dt2_p := arg2_p^.dtype_p;          {resolve base data type for arg}
    while dt2_p^.dtype = sst_dtype_copy_k
      do dt2_p := dt2_p^.copy_dtype_p;
    if arg2_p^.name_p = nil
      then begin                       {routine 2 descriptor is for actual call}
        if arg1_p^.univ                {any argument allowed to match template ?}
          then goto arg_dtype_ok;
        if sst_rwflag_write_k in arg1_p^.rwflag_ext
          then begin                   {value is being passed back to argument}
            if dt1_p = dt2_p then goto arg_dtype_ok;
            if                         {both data types are pointers ?}
                (dt1_p^.dtype = sst_dtype_pnt_k) and {template dtype is a pointer ?}
                (dt2_p^.dtype = sst_dtype_pnt_k) {argument dtype is any pointer ?}
                then begin
              if                       {either one is a UNIV pointer ?}
                  (dt1_p^.pnt_dtype_p = nil) or
                  (dt2_p^.pnt_dtype_p = nil)
                then goto arg_dtype_ok;
              end;                     {done with both dtypes were pointers}
            end
          else begin                   {argument is only passed in to routine}
            if sst_dtype_convertable(dt2_p^, dt1_p^) then goto arg_dtype_ok;
            end
          ;
        sys_stat_set (sst_subsys_k, sst_stat_rout_dtypea_nmatch_k, stat);
        goto arg_err;
arg_dtype_ok:                          {argument data type has been typed checked}

        if
            (not (arg1_p^.rwflag_ext <= arg2_p^.rwflag_ext))
            then begin
          sys_stat_set (sst_subsys_k, sst_stat_rout_dir_nmatch_k, stat);
          goto arg_err;
          end;
        end
      else begin                       {routine 2 descriptor is a routine template}
        if dt1_p <> dt2_p then begin
          sys_stat_set (sst_subsys_k, sst_stat_rout_dtypea_nmatch_k, stat);
          goto arg_err;
          end;
        if arg2_p^.rwflag_ext <> arg1_p^.rwflag_ext then begin
          sys_stat_set (sst_subsys_k, sst_stat_rout_dir_nmatch_k, stat);
          goto arg_err;
          end;
        end
      ;
    if arg2_p^.pass <> arg1_p^.pass then begin
      sys_stat_set (sst_subsys_k, sst_stat_rout_pass_nmatch_k, stat);
      goto arg_err;
      end;
    arg1_p := arg1_p^.next_p;          {advance to next call argument descriptors}
    arg2_p := arg2_p^.next_p;
    argn := argn + 1;
    end;                               {back and check next call argument}

  sys_error_none (stat);               {indicate no mismatches}
  return;                              {return and indicate routines matched}

arg_err:                               {jump here if mismatch error on an argument}
  sys_stat_parm_int (argn, stat);      {indicate argument number that had error}
  end;
