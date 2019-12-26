{   Subroutine SST_W_C_DECL_SYM_ROUT (PROC)
*
*   Declare any symbols eventually referenced by a routine call or declaration.
*   PROC is the routine descriptor.
}
module sst_w_c_DECL_SYM_ROUT;
define sst_w_c_decl_sym_rout;
%include 'sst_w_c.ins.pas';

procedure sst_w_c_decl_sym_rout (      {declare symbols referenced by routine call}
  in      proc: sst_proc_t);           {routine descriptor}

var
  arg_p: sst_proc_arg_p_t;             {points to current routine argument descriptor}

begin
  if proc.sym_p <> nil then begin      {do routine name symbol, if any}
    sst_w_c_symbol (proc.sym_p^);
    end;

  if proc.dtype_func_p <> nil then begin {do function return data type, if any}
    sst_w_c_decl_sym_dtype (proc.dtype_func_p^);
    end;

  arg_p := proc.first_arg_p;           {init current call argument to first}
  while arg_p <> nil do begin          {once for each call argument}
    if arg_p^.sym_p <> nil then begin  {do dummy arg name, if any}
      sst_w_c_symbol (arg_p^.sym_p^);
      end;
    if arg_p^.exp_p <> nil then begin  {do expression for passed arg value, if any}
      sst_w_c_decl_sym_exp (arg_p^.exp_p^);
      end;
    sst_w_c_decl_sym_dtype (arg_p^.dtype_p^); {do data type of this argument}
    arg_p := arg_p^.next_p;            {advance to next argument of this routine}
    end;                               {back and process next call argument}
  end;
