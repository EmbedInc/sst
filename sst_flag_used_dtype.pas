{   Subroutine SST_FLAG_USED_DTYPE (DTYPE)
*
*   Follow all symbols eventually referenced by the data type DTYPE, and flag
*   them as used.
}
module sst_FLAG_USED_DTYPE;
define sst_flag_used_dtype;
%include 'sst2.ins.pas';

procedure sst_flag_used_dtype (        {flag symbols eventually used from a dtype}
  in      dtype: sst_dtype_t);         {dtype descriptor that may reference symbols}

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  sym_p: sst_symbol_p_t;               {scratch symbol pointer}
  msg_parm:                            {references to paramters for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

begin
  if dtype.symbol_p <> nil then begin  {symbol exists for this data type ?}
    if sst_symflag_followed_k in dtype.symbol_p^.flags {already done this symbol ?}
      then return;
    if sst_symflag_following_dt_k in dtype.symbol_p^.flags {already doing dtype ?}
      then return;
    if                                 {not already working on this symbol ?}
        not (sst_symflag_following_k in dtype.symbol_p^.flags)
        then begin
      sst_flag_used_symbol (dtype.symbol_p^); {flag data type's parent symbol}
      return;
      end;
    dtype.symbol_p^.flags :=           {indicate we are now doing this data type}
      dtype.symbol_p^.flags + [sst_symflag_following_dt_k];
    end;

  case dtype.dtype of                  {what kind of data type is this ?}
{
*   Data type is enumerated type.
}
sst_dtype_enum_k: begin
  sym_p := dtype.enum_first_p;         {init current enumerated value to first}
  while sym_p <> nil do begin          {once for each enumerated value}
    sst_flag_used_symbol (sym_p^);     {flag each enumerated name as used}
    sym_p := sym_p^.enum_next_p;       {advance to next enumerated name}
    end;
  end;
{
*   Data type is a record.
}
sst_dtype_rec_k: begin
  sym_p := dtype.rec_first_p;          {init curr symbol to first field name}
  while sym_p <> nil do begin          {once for each field in record}
    sst_flag_used_dtype (sym_p^.field_dtype_p^); {process field's data type}
    sym_p := sym_p^.field_next_p;      {advance to next field in record}
    end;
  end;
{
*   Data type is an array.
}
sst_dtype_array_k: begin
  sst_flag_used_exp (dtype.ar_ind_first_p^); {process exp for subscript range start}
  if dtype.ar_ind_last_p <> nil then begin {fixed subscript ending exp exists ?}
    sst_flag_used_exp (dtype.ar_ind_last_p^); {process exp for subscript range end}
    end;
  if dtype.ar_dtype_rem_p = nil
    then begin                         {this is a one-dimensional array}
      sst_flag_used_dtype (dtype.ar_dtype_ele_p^); {process elements data type}
      end
    else begin                         {there is more than one subscript}
      sst_flag_used_dtype (dtype.ar_dtype_rem_p^); {process "rest" of array}
      end
    ;
  end;
{
*   Data type is a set.
}
sst_dtype_set_k: begin
  sst_flag_used_dtype (dtype.set_dtype_p^); {declare base data type of set}
  end;
{
*   Data type is a subrange of another data type.
}
sst_dtype_range_k: begin
  sst_flag_used_dtype (dtype.range_dtype_p^); {flag base data type as used}
  if dtype.range_first_p <> nil then begin
    sst_flag_used_exp (dtype.range_first_p^); {process expression for first value}
    end;
  if dtype.range_last_p <> nil then begin
    sst_flag_used_exp (dtype.range_last_p^); {process expression for last value}
    end;
  end;
{
*   Data type is a procedure.
}
sst_dtype_proc_k: begin
  sst_flag_used_rout (dtype.proc_p^);  {flag symbols used by this routine}
  end;
{
*   Data type is a pointer.
}
sst_dtype_pnt_k: begin
  if dtype.pnt_dtype_p <> nil then begin {not a NIL pointer ?}
    sst_flag_used_dtype (dtype.pnt_dtype_p^); {flag pointed-to data type as used}
    end;
  end;
{
*   Data type is a copy of another data type.
}
sst_dtype_copy_k: begin
  if dtype.copy_symbol_p <> nil then begin
    sst_flag_used_symbol (dtype.copy_symbol_p^); {flag copied symbol name}
    end;
  sst_flag_used_dtype (dtype.copy_dtype_p^); {flag copied data type}
  end;
{
*   Undefined data type.  This could happen under legal circumstances if
*   a symbol was defined that pointed to a data type that was never defined,
*   but also never used directly.  In this case, we want to make sure the
*   undefined data type is not flagged as used.  This would cause problems
*   for the back end by asking it to write an undefined symbol.
}
sst_dtype_undef_k: begin
  if dtype.symbol_p <> nil then begin  {this data type has a symbol ?}
    dtype.symbol_p^.flags := dtype.symbol_p^.flags - {flag symbol as unused}
      [sst_symflag_used_k];
    end;
  end;
{
*   All the data types that require no special processing.
}
sst_dtype_int_k: ;
sst_dtype_float_k: ;
sst_dtype_bool_k: ;
sst_dtype_char_k: ;
otherwise
    sys_msg_parm_int (msg_parm[1], ord(dtype.dtype));
    sys_message_bomb ('sst', 'dtype_unexpected_exp', msg_parm, 1);
    end;                               {end of data type cases}
  end;
