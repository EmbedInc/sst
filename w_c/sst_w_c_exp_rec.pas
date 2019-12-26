{   Subroutine SST_W_C_EXP_REC (EXP)
*
*   Write the value of the expression EXP.  EXP is a constant record expression.
*   This is only allowed in the initialization of a record variable.
}
module sst_w_c_EXP_REC;
define sst_w_c_exp_rec;
%include 'sst_w_c.ins.pas';

procedure sst_w_c_exp_rec (            {write value of record constant expression}
  in      exp: sst_exp_t);

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  term_p: sst_exp_term_p_t;            {points to current term in expression}
  dt_exp_p: sst_dtype_p_t;             {points to base data type of record}
  field_p: sst_symbol_p_t;             {points to symbol for current field in rec}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

label
  next_field;

begin
  dt_exp_p := exp.dtype_p;             {resolve expression's base data type}
  while dt_exp_p^.dtype = sst_dtype_copy_k
    do dt_exp_p := dt_exp_p^.copy_dtype_p;
  if dt_exp_p^.dtype <> sst_dtype_rec_k then begin {expression dtype not RECORD ?}
    sys_msg_parm_int (msg_parm[1], ord(dt_exp_p^.dtype));
    sys_message_bomb ('sst', 'dtype_unexpected', msg_parm, 1);
    end;

  sst_w.appendn^ ('{', 1);             {start list of values for each field}
  field_p := dt_exp_p^.rec_first_p;    {init current field to first in record}
  while field_p <> nil do begin        {once for each field in record}
    term_p := addr(exp.term1);         {init current term to first in expression}
    repeat                             {scan thru terms looking for this field}
      if term_p^.field_sym_p = field_p then begin {found term for this field ?}
        sst_w_c_exp                    {write value for this field}
          (term_p^.field_exp_p^, 0, nil, enclose_no_k);
        goto next_field;               {done writing value for this field}
        end;
      term_p := term_p^.next_p;        {advance to next term in expression}
      until term_p = nil;              {back and process next term in expression}
    sst_w_c_ival_unspec (field_p^.field_dtype_p^); {use "unspecified" value}
{
*   Done writing value for this field, advance to next field.
}
next_field:
    field_p := field_p^.field_next_p;  {advance to next field in record}
    if field_p <> nil then begin       {last value was not last in record ?}
      sst_w.appendn^ (',', 1);
      sst_w.delimit^;
      end;
    end;                               {back and process this new field}
{
*   Done writing the value for all the fields in this record.
}
  sst_w.appendn^ ('}', 1);
  end;
