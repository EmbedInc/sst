{   Subroutine SST_W_C_DECL_SYM_DTYPE (DTYPE)
*
*   Make sure that any symbols referenced by the data type descriptor DTYPE
*   are declared.
}
module sst_w_c_DECL_SYM_DTYPE;
define sst_w_c_decl_sym_dtype;
%include 'sst_w_c.ins.pas';

procedure sst_w_c_decl_sym_dtype (     {declare symbols reference by a data type}
  in      dtype: sst_dtype_t);         {data type descriptor that may reference syms}

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  sym_p: sst_symbol_p_t;               {scratch symbol pointer}
  dt_p, dt2_p: sst_dtype_p_t;          {scratch pointers to data type descriptor}
  msg_parm:                            {references to paramters for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

label
  loop_pnt;

begin
  if dtype.symbol_p <> nil then begin  {symbol exists for this data type ?}
    if                                 {already completely declared ?}
      sst_symflag_written_k in dtype.symbol_p^.flags then return;
    if                                 {already working on this data type ?}
      sst_symflag_writing_dt_k in dtype.symbol_p^.flags then return;
    if                                 {not already working on this symbol ?}
        not (sst_symflag_writing_k in dtype.symbol_p^.flags)
        then begin
      sst_w_c_symbol (dtype.symbol_p^); {declare data type's parent symbol}
      return;
      end;
    dtype.symbol_p^.flags :=           {flag that we are now working on this dtype}
      dtype.symbol_p^.flags + [sst_symflag_writing_dt_k];
    end;
  case dtype.dtype of                  {what kind of data type is this ?}
{
*   Data type is an enumerated type.
}
sst_dtype_enum_k: begin
  sym_p := dtype.enum_first_p;         {init current enumerated name to first name}
  while sym_p <> nil do begin          {once for each enumerated name}
    sst_w_c_symbol (sym_p^);           {set up enumerated name symbol for output}
    sym_p := sym_p^.enum_next_p;       {advance to next enumerated name in dtype}
    end;                               {back and process this new enumerated name}
  end;
{
*   Data type is a record.
}
sst_dtype_rec_k: begin
  sym_p := dtype.rec_first_p;          {init curr symbol to first field name}
  while sym_p <> nil do begin          {once for each field in record}
    sst_w_c_symbol (sym_p^);           {process field name symbol}
    sym_p := sym_p^.field_next_p;      {advance to next field in record}
    end;
  end;
{
*   Data type is an array.
}
sst_dtype_array_k: begin
  sst_w_c_decl_sym_exp (dtype.ar_ind_first_p^); {process exp for subscript range start}
  if dtype.ar_ind_last_p <> nil then begin {fixed subscript ending exp exists ?}
    sst_w_c_decl_sym_exp (dtype.ar_ind_last_p^); {process exp for subscript range end}
    end;
  if dtype.ar_dtype_rem_p = nil
    then begin                         {this is a one-dimensional array}
      sst_w_c_decl_sym_dtype (dtype.ar_dtype_ele_p^); {process elements data type}
      end
    else begin                         {there is more than one subscript}
      sst_w_c_decl_sym_dtype (dtype.ar_dtype_rem_p^); {process "rest" of array}
      end
    ;
  end;
{
*   Data type is a set.
}
sst_dtype_set_k: begin
  sst_w_c_decl_sym_dtype (dtype.set_dtype_p^); {declare base data type of set}
  end;
{
*   Data type is a subrange of another data type.
}
sst_dtype_range_k: begin
  sst_w_c_decl_sym_dtype (dtype.range_dtype_p^); {declare base data type of range}
  sst_w_c_decl_sym_exp (dtype.range_first_p^); {declare symbols for min expression}
  sst_w_c_decl_sym_exp (dtype.range_last_p^); {declare symbols for max expression}
  end;
{
*   Data type is a procedure.
}
sst_dtype_proc_k: begin
  sst_w_c_decl_sym_rout (dtype.proc_p^); {declare symbols of this procedure}
  end;
{
*   Data type is a pointer.
}
sst_dtype_pnt_k: begin
  dt_p := dtype.pnt_dtype_p;           {init pointer to raw target data type}
loop_pnt:
  if dt_p = nil then return;           {NIL pointer ?}
  if dt_p^.symbol_p <> nil then begin  {this copy level has a symbol ?}
    dt2_p := dt_p;                     {resolve base pointed-to data type}
    while dt2_p^.dtype = sst_dtype_copy_k do dt2_p := dt2_p^.copy_dtype_p;
    if dt2_p^.dtype = sst_dtype_undef_k {DTYPE is pointer to undefined data type ?}
      then return;
    if dt2_p^.dtype = sst_dtype_rec_k
      then begin                       {pointer ultimately points to a record}
        sst_w.name_sym^ (dt_p^.symbol_p^); {make sure symbol has an output name}
        end
      else begin                       {this is not a pointer to a record}
        sst_w_c_symbol (dt_p^.symbol_p^); {write full nested symbol declaration}
        end
      ;
    return;
    end;
  if dt_p^.dtype = sst_dtype_copy_k then begin {curr dtype is copy of another ?}
    dt_p := dt_p^.copy_dtype_p;        {resolve one level of copy}
    goto loop_pnt;                     {back to try at this new copy level}
    end;
  sst_w_c_decl_sym_dtype (dt_p^);      {declare nested symbols in procedure template}
  end;
{
*   Data type is a copy of another data type.
}
sst_dtype_copy_k: begin
  if dtype.copy_symbol_p <> nil then begin {there is a copied symbol ?}
    sst_w_c_symbol (dtype.copy_symbol_p^); {declare copied data type symbol}
    end;
  sst_w_c_decl_sym_dtype (dtype.copy_dtype_p^); {declare copied data type}
  end;
{
*   All the data types that require no special processing.
}
sst_dtype_int_k: ;
sst_dtype_float_k: ;
sst_dtype_bool_k: ;
sst_dtype_char_k: ;
sst_dtype_undef_k: ;
otherwise
    sys_msg_parm_int (msg_parm[1], ord(dtype.dtype));
    sys_message_bomb ('sst', 'dtype_unexpected_exp', msg_parm, 1);
    end;                               {end of data type cases}
  end;
