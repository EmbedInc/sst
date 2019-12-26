{   Subroutine SST_DTYPE_NEW_STRING (STR_LEN,DTYPE_P)
*
*   Create a new data type descriptor for a string.  Strings are really
*   arrays of characters.
}
module sst_DTYPE_NEW_STRING;
define sst_dtype_new_string;
%include 'sst2.ins.pas';

procedure sst_dtype_new_string (       {create new dtype descriptor for a string}
  in      str_len: string_index_t;     {length of string}
  out     dtype_p: sst_dtype_p_t);     {pointer to new data type descriptor}

begin
  sst_mem_alloc_scope (sizeof(dtype_p^), dtype_p); {alloc new data type descriptor}
{
*   Fill in fixed fields.
}
  dtype_p^.symbol_p := nil;
  dtype_p^.dtype := sst_dtype_array_k;
  dtype_p^.bits_min := sst_config.bits_char * str_len;
  dtype_p^.align_nat := sst_dtype_char_p^.align_nat;
  dtype_p^.align := sst_dtype_char_p^.align;
  sst_dtype_size (dtype_p^);           {set SIZE_USED and SIZE_ALIGN fields}
{
*   Fill in fields specific to this data type.
}
  dtype_p^.ar_dtype_ele_p := sst_dtype_char_p;
  dtype_p^.ar_dtype_rem_p := nil;
  sst_exp_const_int (1, dtype_p^.ar_ind_first_p);
  sst_exp_const_int (str_len, dtype_p^.ar_ind_last_p);
  dtype_p^.ar_ind_n := str_len;
  dtype_p^.ar_n_subscr := 1;
  dtype_p^.ar_string := true;
  end;
