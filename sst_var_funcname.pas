{   Subroutine SST_VAR_FUNCNAME (V)
*
*   Modify the variable descriptor V to represent a function's main symbol
*   instead of its return value.  Nothing is done if the variable descriptor
*   does not already represent a function's return value "variable".
}
module sst_VAR_FUNCNAME;
define sst_var_funcname;
%include 'sst2.ins.pas';

procedure sst_var_funcname (           {change var from func return val to func name}
  in out  v: sst_var_t);               {variable descriptor to convert}

begin
  if v.vtype <> sst_vtype_var_k then return; {not referencing a variable ?}
  if v.mod1.next_p <> nil then return; {compound name reference ?}
  if                                   {symbol not a variable ?}
    v.mod1.top_sym_p^.symtype <> sst_symtype_var_k then return;
  if                                   {symbol not a function return value ?}
      (v.mod1.top_sym_p^.var_proc_p = nil) or
      (v.mod1.top_sym_p^.var_arg_p <> nil)
    then return;
{
*   Symbol does represent the return value "variable" of a function.
}
  v.mod1.top_sym_p :=                  {point to function definition symbol}
    v.mod1.top_sym_p^.var_proc_p^.sym_p;
  v.dtype_p :=                         {var data type is data type returned by func}
    v.mod1.top_sym_p^.proc.dtype_func_p;
  v.rwflag := [sst_rwflag_read_k];     {function value is read-only}
  v.vtype := sst_vtype_rout_k;         {var descriptor now represents a routine}
  v.rout_proc_p :=                     {point to routine template descriptor}
    addr(v.mod1.top_sym_p^.proc);
  end;
