{   Subroutine SST_DTYPE_CONVERTABLE (DTYPE_IN,DTYPE_RES)
*
*   Check whether the data type DTYPE_IN can be unambigously converted to
*   the data type DTYPE_RES.  It is illegal to build opcodes that imply a
*   conversion that can not be done unambigously.  It is up to each back end
*   how to handle any such conversions.
*
*   Unambigous conversion here means that the VALUE is preserved, not the
*   bit pattern.
}
module sst_DTYPE_CONVERTABLE;
define sst_dtype_convertable;
%include 'sst2.ins.pas';

function sst_dtype_convertable (       {check if data type convertable to another}
  in      dtype_in: sst_dtype_t;       {data type to be converted}
  in      dtype_res: sst_dtype_t)      {target data type to convert to}
  :boolean;                            {TRUE unambigous conversion possible}

var
  dt_in_p: sst_dtype_p_t;              {pointer to base IN dtype descriptor}
  dt_res_p: sst_dtype_p_t;             {pointer to base RES dtype descriptor}
  stat: sys_err_t;

label
  retry, no_match;
{
********************************************************************************
*
*   Local subroutine DTYPE_RESOLVE (DIN,DOUT_P)
*
*   Resolve the base data type descriptor.  All COPY and SUBRANGES will be
*   resolved.
}
procedure dtype_resolve (
  in      din: sst_dtype_t;            {data type to resolve base of}
  out     dout_p: sst_dtype_p_t);      {will point to base data type descriptor}

label
  loop;

begin
  dout_p := addr(din);                 {init resolved data type to input data type}
loop:                                  {back here to resolve next data type layer}
  case dout_p^.dtype of
sst_dtype_range_k: begin               {SUBRANGE}
      dout_p := dout_p^.range_dtype_p;
      goto loop;
      end;
sst_dtype_copy_k: begin                {COPY}
      dout_p := dout_p^.copy_dtype_p;
      goto loop;
      end;
    end;
  end;
{
********************************************************************************
*
*   Start execution
}
begin
  sst_dtype_convertable := true;       {init to conversion IS possible}
  if addr(dtype_in) = addr(dtype_res) then return; {do trivial accept test}

  dtype_resolve (dtype_in, dt_in_p);   {resolve base IN data types}
  dtype_resolve (dtype_res, dt_res_p); {resolve base RES data types}

retry:                                 {back here to try again with new data types}
  if dt_in_p = dt_res_p then return;   {identical base data type descriptors ?}
{
*   The base data type descriptors are not the same.  This means the data types
*   are different.  However, some data types can be converted to others, and
*   some require special handling to determine if they are the same.  Only
*   these data types will be checked below.  Anything that is not explicitly
*   decided is convertable will be assumed unconvertable.
}
  case dt_res_p^.dtype of              {what type is base RES descriptor ?}
{
********************************
*
*   Base result data type is INTEGER or BOOLEAN.
}
sst_dtype_int_k,
sst_dtype_bool_k: begin
  if dt_in_p^.dtype = dt_res_p^.dtype then return;
  end;
{
********************************
*
*   Base result data type is FLOATING POINT
}
sst_dtype_float_k: begin               {FLOATING POINT}
  case dt_in_p^.dtype of               {what is base IN data type ?}
sst_dtype_int_k,
sst_dtype_float_k: return;
    end;
  end;
{
********************************
*
*   Base result data type is CHARACTER
}
sst_dtype_char_k: begin                {CHARACTER}
  case dt_in_p^.dtype of
sst_dtype_char_k: return;
sst_dtype_array_k: begin
      if not dt_in_p^.ar_string then goto no_match; {array is not a string}
      if dt_in_p^.ar_ind_n = 1 then return; {string is exactly one character long ?}
      end;
    end;
  end;
{
********************************
*
*   Base result data type is ARRAY.
}
sst_dtype_array_k: begin               {ARRAY}
  if not dt_res_p^.ar_string then goto no_match; {not a STRING ?}
  case dt_in_p^.dtype of
sst_dtype_char_k: return;
sst_dtype_array_k: begin
      if dt_in_p^.ar_string then return;
      end;
    end;
  end;
{
********************************
*
*   Base result data type is SET
}
sst_dtype_set_k: begin                 {SET}
  if dt_in_p^.dtype <> sst_dtype_set_k {IN data type not also a SET ?}
    then goto no_match;
  if dt_in_p^.set_n_ent = 0            {input set is NULL set ?}
    then return;
  if dt_in_p^.set_dtype_p = dt_res_p^.set_dtype_p {sets of exactly the same dtype ?}
    then return;
  if dt_in_p^.set_dtype_final and dt_res_p^.set_dtype_final {both ele dtypes firm ?}
    then goto no_match;
  dtype_resolve (dt_in_p^.set_dtype_p^, dt_in_p); {resolve base element data types}
  dtype_resolve (dt_res_p^.set_dtype_p^, dt_res_p);
  if dt_in_p = dt_res_p                {elements have identical data types ?}
    then return;
  if dt_in_p^.dtype <> dt_res_p^.dtype {element data type IDs don't match ?}
    then goto no_match;
  if dt_in_p^.dtype = sst_dtype_enum_k {both ele ENUMERATED, but different types ?}
    then goto no_match;
  return;                              {element data types DO match}
  end;
{
********************************
*
*   Base result data type is PROCEDURE
}
sst_dtype_proc_k: begin                {PROCEDURE}
  if dt_in_p^.dtype <> sst_dtype_proc_k {IN data type not also a procedure ?}
    then goto no_match;
  sst_routines_match (                 {compare the two routine templates}
    dt_in_p^.proc_p^, dt_res_p^.proc_p^, stat);
  if not sys_error(stat) then return;
  end;
{
********************************
*
*   Base result data type is POINTER
}
sst_dtype_pnt_k: begin                 {POINTER}
  case dt_in_p^.dtype of
sst_dtype_pnt_k: begin                 {IN data type is also pointer}
      if dt_res_p^.pnt_dtype_p = nil then return; {RES is universal pointer ?}
      if dt_in_p^.pnt_dtype_p = nil then return; {IN is universal pointer ?}
      dtype_resolve (dt_in_p^.pnt_dtype_p^, dt_in_p);
      dtype_resolve (dt_res_p^.pnt_dtype_p^, dt_res_p);
      goto retry;                      {back to try again with new data types}
      end;
    end;
  end;

    end;                               {end of result data type cases}
no_match:                              {jump here if can't convert data types}
  sst_dtype_convertable := false;      {conversion is not possible}
  end;
