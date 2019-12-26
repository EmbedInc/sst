{   Subroutine SST_DTYPE_RESOLVE (DTYPE, DTYPE_BASE_P, DTYPE_BASE)
*
*   Resolve the base data type implied by the data type descriptor DTYPE.
*   DTYPE_BASE_P will be pointing to the first data type descriptor that
*   is not a copy, which could be DTYPE directly.  DTYPE_BASE further
*   indicates the raw data type ID of the root data type.  DTYPE_BASE
*   differs from DTYPE_BASE_P^.DT if the base data type is a subrange.
*   In that case DTYPE_BASE is the data type ID of the root data type
*   the subrange is formed from.  Therefore DTYPE_BASE will never indicate
*   COPY or SUBRANGE, while DTYPE_BASE_P may point to a subrange data type
*   descriptor.
}
module sst_DTYPE_RESOLVE;
define sst_dtype_resolve;
%include 'sst2.ins.pas';

procedure sst_dtype_resolve (          {resolve arbitrary data type to base dtype}
  in      dtype: sst_dtype_t;          {descriptor for data type to resolve}
  out     dtype_base_p: sst_dtype_p_t; {points to base data type descriptor}
  out     dtype_base: sst_dtype_k_t);  {resolved base data type}

var
  dt_p: sst_dtype_p_t;                 {scratch data type descriptor pointer}

begin
  dtype_base_p := addr(dtype);         {init root dtype pointer to starting dtype}
  while dtype_base_p^.dtype = sst_dtype_copy_k do begin {resolve nested "copy" types}
    dtype_base_p := dtype_base_p^.copy_dtype_p; {resolve one layer of "copy"}
    end;                               {back and check new data type}
  dt_p := dtype_base_p;                {init dtype descriptor originating DTYPE_BASE}

  dtype_base := dt_p^.dtype;           {init root data type ID}
  while dtype_base = sst_dtype_range_k do begin {root data type is SUBRANGE ?}
    dt_p := dt_p^.range_dtype_p;       {point to base dtype descriptor of subrange}
    while dt_p^.dtype = sst_dtype_copy_k do begin {resolve nested "copy" types}
      dt_p := dt_p^.copy_dtype_p;
      end;                             {back and resolve next copy}
    dtype_base := dt_p^.dtype;         {get new root data type ID}
    end;                               {back to retry with new root data type desc}
  end;
