{   Subroutine SST_FLAG_USED_VAR (V)
*
*   Flag all symbols as USED that are eventually referenced by the "variable"
*   desecriptor V.
}
module sst_FLAG_USED_VAR;
define sst_flag_used_var;
%include 'sst2.ins.pas';

procedure sst_flag_used_var (          {flag symbols eventually used from var desc}
  in      v: sst_var_t);               {var descriptor that may reference symbols}

begin
  sst_flag_used_symbol (v.mod1.top_sym_p^);
  if v.dtype_p <> nil then begin
    sst_flag_used_dtype (v.dtype_p^);
    end;
  end;
