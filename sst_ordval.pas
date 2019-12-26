{   Subroutine SST_ORDVAL (VAL,ORDVAL,STAT)
*
*   Find the ordinal integer value from the constant value descriptor VAL.
*   The ordinal value is returned in ORDVAL.  STAT is the returned completion
*   status code.  It is set to an error if the value in VAL does not have an
*   ordinal value.
}
module sst_ORDVAL;
define sst_ordval;
%include 'sst2.ins.pas';

procedure sst_ordval (                 {find ordinal value from value descriptor}
  in      val: sst_var_value_t;        {input constant value descriptor}
  out     ordval: sys_int_max_t;       {returned ordinal value}
  out     stat: sys_err_t);            {set to error if ordinal value not exist}

begin
  sys_error_none (stat);               {init to no error finding ordinal value}

  case val.dtype of                    {what base data type is this constant ?}
sst_dtype_int_k: begin                 {integer}
      ordval := val.int_val;
      end;
sst_dtype_enum_k: begin                {enumerated type}
      ordval := val.enum_p^.enum_ordval;
      end;
sst_dtype_bool_k: begin                {boolean}
      if val.bool_val
        then ordval := 1
        else ordval := 0;
      end;
sst_dtype_char_k: begin                {character}
      ordval := ord(val.char_val);
      end;
otherwise
    sys_stat_set (sst_subsys_k, sst_stat_no_ordval_k, stat);
    end;
  end;
