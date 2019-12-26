{   Subroutine SST_R_PAS_VPARAM (PROC)
*
*   Apply the VAL_PARAM routine option to the arguments of a routine.  PROC
*   is the descriptor for the routine.
}
module sst_r_pas_vparam;
define sst_r_pas_vparam;
%include 'sst_r_pas.ins.pas';

procedure sst_r_pas_vparam (           {apply VAL_PARAM to routine template}
  in out  proc: sst_proc_t);           {routine descriptor that will have args fixed}

var
  arg_p: sst_proc_arg_p_t;             {points to current argument being processed}
  dt_p: sst_dtype_p_t;                 {scratch pointer to data type descriptor}

label
  next_arg;

begin
  arg_p := proc.first_arg_p;           {init first argument as current}
  while arg_p <> nil do begin          {once for each call argument}
    if arg_p^.univ                     {no effect if UNIV specified}
      then goto next_arg;
    if sst_rwflag_write_k in arg_p^.rwflag_ext {value being passed back ?}
      then goto next_arg;
    if arg_p^.pass = sst_pass_val_k    {already passing by value ?}
      then goto next_arg;
    if                                 {too large to pass by value automatically ?}
        arg_p^.dtype_p^.size_used > sst_config.pass_val_size_max
      then goto next_arg;
    dt_p := arg_p^.dtype_p;            {get pointer to argument data type descriptor}
    while                              {resolve argument's base data type}
      dt_p^.dtype = sst_dtype_copy_k do dt_p := dt_p^.copy_dtype_p;
    if dt_p^.dtype = sst_dtype_array_k {argument is an array ?}
      then goto next_arg;

    case sst_config.os of              {special cases for different target OSs}
sys_os_aix_k,                          {IBM AIX}
sys_os_domain_k: begin                 {Apollo Domain/OS}
        if dt_p^.dtype = sst_dtype_rec_k then begin {passing a record ?}
          goto next_arg;               {VAL_PARAM doesn't apply to records}
          end;
        end;
      end;                             {end of particular OS special handling cases}

    arg_p^.pass := sst_pass_val_k;     {argument will be passed by value}
    arg_p^.rwflag_int :=               {all access allowed to internal copy}
      [sst_rwflag_read_k, sst_rwflag_write_k];
    arg_p^.rwflag_ext :=               {caller's argument will not be altered}
      [sst_rwflag_read_k];
next_arg:                              {jump here to advance to next call argument}
    arg_p := arg_p^.next_p;            {advance to next call argument}
    end;                               {back and process this new call argument}
  end;
