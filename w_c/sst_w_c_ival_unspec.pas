{   Subroutine SST_W_C_IVAL_UNSPEC (DTYPE)
*
*   Write the default "unspecified" initial value for a variable of type DTYPE.
}
module sst_w_c_IVAL_UNSPEC;
define sst_w_c_ival_unspec;
%include 'sst_w_c.ins.pas';

procedure sst_w_c_ival_unspec (        {write "unspecified" initial variable value}
  in      dtype: sst_dtype_t);         {data type descriptor for variable}

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  dt_p: sst_dtype_p_t;                 {pointer to base data type descriptor}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

label
  dtype_new;

begin
  dt_p := addr(dtype);                 {init pointer to base data type descriptor}

dtype_new:                             {back here after create new data type}
  case dt_p^.dtype of                  {what is data type ID of this descriptor}
sst_dtype_int_k: begin
      sst_w_c_intrinsic (intr_unspec_int_k);
      end;
sst_dtype_enum_k: begin
      sst_w_c_intrinsic (intr_unspec_enum_k);
      end;
sst_dtype_float_k: begin
      sst_w_c_intrinsic (intr_unspec_float_k);
      end;
sst_dtype_bool_k: begin
      sst_w_c_intrinsic (intr_unspec_bool_k);
      end;
sst_dtype_char_k: begin
      sst_w_c_intrinsic (intr_unspec_char_k);
      end;
sst_dtype_rec_k: begin
      if dt_p^.rec_first_p = nil
        then begin                     {this record has no fields}
          sst_w.appendn^ ('0', 1);
          end
        else begin
          sst_w.appendn^ ('{', 1);
          sst_w_c_ival_unspec (dt_p^.rec_first_p^.field_dtype_p^);
          sst_w.appendn^ ('}', 1);
          end
        ;
      end;
sst_dtype_array_k: begin
      sst_w.appendn^ ('{', 1);
      if dt_p^.ar_dtype_rem_p = nil
        then sst_w_c_ival_unspec (dt_p^.ar_dtype_ele_p^)
        else sst_w_c_ival_unspec (dt_p^.ar_dtype_rem_p^);
      sst_w.appendn^ ('}', 1);
      end;
sst_dtype_set_k: begin
      sst_w_c_intrinsic (intr_unspec_set_k);
      end;
sst_dtype_range_k: begin
      sst_w_c_ival_unspec (dt_p^.range_dtype_p^);
      end;
sst_dtype_pnt_k: begin
      sst_w_c_intrinsic (intr_unspec_pnt_k);
      end;
sst_dtype_copy_k: begin
      dt_p := dt_p^.copy_dtype_p;
      goto dtype_new;
      end;
otherwise
    sys_msg_parm_int (msg_parm[1], ord(dt_p^.dtype));
    sys_message_bomb ('sst', 'dtype_unexpected', msg_parm, 1);
    end;
  end;
