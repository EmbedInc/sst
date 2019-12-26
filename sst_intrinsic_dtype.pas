{   Subroutine SST_INTRINSIC_DTYPE (NAME,DTYPE,SIZE,DT_P)
*
*   Create a new intrinsic data type in the current scope.  NAME is the symbol
*   name for the new data type.  DTYPE is the SST library data type ID.
*   SIZE if the size of the data type in machine addresses.  DT_P will be returned
*   pointing to the new data type descriptor block.  The data type block will
*   be initialized, but may need to have additional fields set or fixed by the
*   calling routine.
*
*   The SIZE parameter will be used to initialize the BITS_MIN, ALIGN_NAT,
*   SIZE_USED, and SIZE_ALIGN fields.  It will be assumed that the natural alignment
*   size is the same as SIZE.  Natural alignment will be assumed regardless of
*   the current setting.
}
module sst_INTRINSIC_DTYPE;
define sst_intrinsic_dtype;
%include 'sst2.ins.pas';

procedure sst_intrinsic_dtype (        {create intrinsic data type in curr scope}
  in      name: string;                {data type name}
  in      dtype: sst_dtype_k_t;        {which base data type is this}
  in      size: sys_int_adr_t;         {size in machine addresses}
  out     dt_p: sst_dtype_p_t);        {pointer to created and initialized data type}

var
  vname: string_var80_t;               {var string symbol name}
  stat: sys_err_t;

begin
  vname.max := sizeof(vname.str);      {init local var string}

  sst_dtype_new (dt_p);                {create new data type}
  dt_p^.dtype := dtype;                {set the data type}
  dt_p^.bits_min := size * sst_config.bits_adr;
  dt_p^.align_nat := size;
  dt_p^.align := size;
  dt_p^.size_used := size;
  dt_p^.size_align := size;

  string_vstring (vname, name, sizeof(name));
  sst_symbol_new_name (vname, dt_p^.symbol_p, stat); {create symbol for new data type}
  sys_error_abort (stat, '', '', nil, 0);
  dt_p^.symbol_p^.symtype := sst_symtype_dtype_k; {symbol is a data type}
  dt_p^.symbol_p^.flags := dt_p^.symbol_p^.flags + {symbol is intrinsic and defined}
    [sst_symflag_def_k, sst_symflag_intrinsic_in_k];
  dt_p^.symbol_p^.dtype_dtype_p := dt_p; {point symbol to the data type block}
  end;
