{   Subroutine SST_REC_VARIANT (DTYPE)
*
*   This function returns TRUE if the record data type in DTYPE contains any
*   variants (overlays).  It is an error if DTYPE is not a record data type.
}
module sst_REC_VARIANT;
define sst_rec_variant;
%include 'sst2.ins.pas';

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

function sst_rec_variant (             {determine if record has any overlays}
  in      dtype: sst_dtype_t)          {descriptor for record's data type}
  :boolean;                            {TRUE if record has variants}
  val_param;

var
  sym_p: sst_symbol_p_t;               {pointer to current field symbol}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

begin
  if dtype.dtype <> sst_dtype_rec_k then begin {data type is not a record ?}
    sys_msg_parm_int (msg_parm[1], ord(dtype.dtype));
    sys_message_bomb ('sst', 'dtype_not_record', msg_parm, 1);
    end;

  sst_rec_variant := false;            {init to this record has no overlays}
  sym_p := dtype.rec_first_p;          {init current field symbol to first field}
  while sym_p <> nil do begin          {loop thru all the fields in this record}
    if sym_p^.field_variant <> 0 then begin {this field is in an overlay ?}
      sst_rec_variant := true;         {this record definately has overlays}
      return;
      end;
    sym_p := sym_p^.field_next_p;      {advance to next field in this record}
    end;                               {back and process this new field}
  end;                                 {return indicating record has no overlays}
