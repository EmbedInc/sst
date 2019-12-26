{   Subroutine SST_FLAG_USED_ROUT (PROC)
*
*   Flag all the symbols as used that are eventually referenced by the routine
*   descriptor PROC.
}
module sst_FLAG_USED_ROUT;
define sst_flag_used_rout;
%include 'sst2.ins.pas';

procedure sst_flag_used_rout (         {flag symbols eventually used from rout call}
  in      proc: sst_proc_t);           {routine descriptor}

var
  arg_p: sst_proc_arg_p_t;             {points to current routine argument descriptor}

begin
  if proc.sym_p <> nil then begin      {do routine name symbol, if any}
    sst_flag_used_symbol (proc.sym_p^);
    end;

  if proc.dtype_func_p <> nil then begin {do function return data type, if any}
    sst_flag_used_dtype (proc.dtype_func_p^);
    end;

  arg_p := proc.first_arg_p;           {init current call argument to first}
  while arg_p <> nil do begin          {once for each call argument}
    if arg_p^.sym_p <> nil then begin  {do dummy arg name, if any}
      sst_flag_used_symbol (arg_p^.sym_p^);
      end;
    if arg_p^.exp_p <> nil then begin  {do expression for passed arg value, if any}
      sst_flag_used_exp (arg_p^.exp_p^);
      end;
    sst_flag_used_dtype (arg_p^.dtype_p^); {do data type of this argument}
    arg_p := arg_p^.next_p;            {advance to next argument of this routine}
    end;                               {back and process next call argument}
  end;
