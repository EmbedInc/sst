{   Subroutine SST_DTYPE_SIZE (DTYPE)
*
*   Set the SIZE_USED and SIZE_ALIGN fields in the data type descriptor DTYPE.
*   The BITS_MIN and ALIGN fields must be previously set.
}
module sst_DTYPE_SIZE;
define sst_dtype_size;
%include 'sst2.ins.pas';

procedure sst_dtype_size (             {set all the size fields given basic info}
  in out  dtype: sst_dtype_t);         {data type to set sizes for}

begin
  dtype.size_used :=                   {minimum machine addresses needed}
    (dtype.bits_min + sst_config.bits_adr - 1) div sst_config.bits_adr;
  dtype.size_align :=                  {size when padded to own alignment rule}
    (dtype.size_used + dtype.align - 1) div dtype.align;
  end;
