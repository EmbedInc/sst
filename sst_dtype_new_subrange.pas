{   Subroutine SST_DTYPE_NEW_SUBRANGE (DTYPE_BASE,ORD_MIN,ORD_MAX,DTYPE_P)
*
*   Create a new subrange data type, if necessary.  DTYPE_BASE is the data type
*   descriptor for the subrange.  ORD_MIN and ORD_MAX are the min/max ordinal
*   values the the new data type is to have within the base data type.
*   DTYPE_P will be returned pointing to the new subrange data type.  It will
*   point to the base data type if the ordinal value range spans the whole
*   base data type.
}
module sst_DTYPE_NEW_SUBRANGE;
define sst_dtype_new_subrange;
%include 'sst2.ins.pas';

procedure sst_dtype_new_subrange (     {create new subrange data type, if necessary}
  in      dtype_base: sst_dtype_t;     {base data type to create subrange of}
  in      ord_min: sys_int_max_t;      {minimum ordinal value of subrange}
  in      ord_max: sys_int_max_t;      {maximum ordinal value of subrange}
  out     dtype_p: sst_dtype_p_t);     {subrange dtype, or base dtype if whole range}

var
  odt_min, odt_max: sys_int_max_t;     {ordinal range of base data type}
  dt_p: sst_dtype_p_t;                 {points to resolved base data type}
  range: sys_int_max_t;                {number of ordinal values in data type}
  bits: sys_int_machine_t;             {number of bits needed for ordinal range}
  mask: sys_int_max_t;                 {for testing whether BITS is high enough}
  align: sys_int_machine_t;            {alignment for this data type}

label
  resolve_dtype;

begin
  dt_p := addr(dtype_base);            {init pointer to resolved base data type}
resolve_dtype:                         {back here to try again with next layer dtype}
  case dt_p^.dtype of                  {what kind of data type is this ?}
sst_dtype_int_k: begin
      odt_max := rshft(~0, 1);
      odt_min := ~odt_max;
      end;
sst_dtype_enum_k: begin
      odt_min := 0;
      odt_max := dt_p^.enum_last_p^.enum_ordval;
      end;
sst_dtype_bool_k: begin
      odt_min := 0;
      odt_max := 1;
      end;
sst_dtype_char_k: begin
      odt_min := 0;
      odt_max := 255;
      end;
sst_dtype_range_k: begin
      odt_min := dt_p^.range_ord_first;
      odt_max := odt_min + dt_p^.range_n_vals - 1;
      if (ord_min = odt_min) and (ord_max = odt_max) then begin {exact fit ?}
        dtype_p := dt_p;
        return;
        end;
      dt_p := dt_p^.range_dtype_p;
      goto resolve_dtype;
      end;
sst_dtype_copy_k: begin
      dt_p := dt_p^.copy_dtype_p;
      goto resolve_dtype;
      end;
otherwise
    sys_message_bomb ('sst', 'dtype_not_ordinal', nil, 0);
    end;
{
*   DT_P is pointing to the base data type, and ODT_MIN and ODT_MAX are the
*   min/max possible ordinal values of that data type.
}
  if (ord_min = odt_min) and (ord_max = odt_max) then begin {no subrange needed ?}
    dtype_p := dt_p;
    return;
    end;
{
*   The requested min/max ordinal value range does not exactly match any of the
*   data types we know about.  Create a new data type.
}
  range := ord_max - ord_min;          {unsigned number of values - 1}
  bits := 0;                           {init number of bits needed}
  mask := ~0;                          {init mask for testing if BITS big enough}
  while (range & mask) <> 0 do begin   {BITS still not big enough ?}
    bits := bits + 1;                  {one more bit}
    mask := lshft(mask, 1);
    end;                               {try again with one more bit}

  align := 1;                          {init to minimum alignment}
  mask := sst_config.bits_adr;         {init max bits for this alignment}
  while mask < bits do begin           {alignment not enough for number of bits ?}
    align := align * 2;                {try next natural alignment}
    mask := mask * 2;                  {update max bits for this alignment}
    end;                               {back and check for this alignment enough}

  sst_dtype_new (dtype_p);             {create and init new data type}
  dtype_p^.dtype := sst_dtype_range_k;
  dtype_p^.bits_min := bits;
  dtype_p^.align_nat := align;
  dtype_p^.align := align;
  dtype_p^.size_used := (bits + sst_config.bits_adr - 1) div sst_config.bits_adr;
  dtype_p^.size_align := align;
  dtype_p^.range_dtype_p := dt_p;
  dtype_p^.range_first_p := nil;
  dtype_p^.range_last_p := nil;
  dtype_p^.range_ord_first := ord_min;
  dtype_p^.range_n_vals := range + 1;
  end;
