{   Subroutine SST_W_C_EXP_CONST (EXP,ADDR_CNT,DT_OUT_P,ENCLOSE)
*
*   Write the value of the expression EXP.  An error will result if the
*   expression does not have a constant value known at compile time.
*   ADDR_CNT is the number of times to take the address of the expression.
*   It may be negative to indicate the number of times the expression
*   should be assumed to be a pointer and dereferenced.
*
*   DT_OUT_P is the desired data type of the final output expression.
*   Currently this is only implemented for strings.  Strings are always written
*   to the full length of the desired output data type since C appends a
*   NULL character at the end if there is room.
*
*   ENCLOSE indicates whether the final expression should be enclosed in
*   parentheses.  Values of ENCLOSE can be:
*
*     ENCLOSE_YES_K  -  Enclose in parentheses, if neccessary, to make the
*       entire expression be one term.
*
*     ENCLOSE_NO_K  -  Don't enclose expression in parentheses, even if it is
*       written as more than one term with operators in between.
}
module sst_w_c_EXP_CONST;
define sst_w_c_exp_const;
%include 'sst_w_c.ins.pas';

procedure sst_w_c_exp_const (          {write exp value, const value must be known}
  in      exp: sst_exp_t;              {expression descriptor}
  in      addr_cnt: sys_int_machine_t; {number of times to take address of}
  in      dt_out_p: sst_dtype_p_t;     {desired output data type, NIL = as is}
  in      enclose: enclose_k_t);       {enclose in () yes/no}

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  i: sys_int_machine_t;                {scratch integer an loop counter}
  dt_p: sst_dtype_p_t;                 {points to base desired expression data type}
  enc: enclose_k_t;                    {internal enclose flag}
  c: char;                             {either address-of or dereference operator}
  str: string_var256_t;                {string value with desired output length}
  val: sst_var_value_t;                {value descriptor used when out dtype forced}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

label
  raw_value, exp_value;

begin
  str.max := sizeof(str.str);          {init local var string}
  enc := enclose;                      {init our enclose flag to caller's}

  if not exp.val_fnd then begin        {expression has no constant value ?}
    syo_error (exp.str_h, 'sst', 'exp_not_const_val', nil, 0);
    end;

  if exp.term1.ttype = sst_term_field_k then begin {exp is record constant ?}
    sst_w_c_exp_rec (exp);             {handle in separate routine}
    return;
    end;
  if exp.term1.ttype = sst_term_arele_k then begin {exp is array constant ?}
    sst_w_c_exp_array (exp);           {handle in separate routine}
    return;
    end;

  if exp.term1.next_p = nil then begin {expression has only one term ?}
    case exp.term1.ttype of
sst_term_var_k: begin                  {term is a "variable", could be named const}
        case exp.term1.var_var_p^.vtype of {what kind of "variable" is this ?}
sst_vtype_const_k: goto exp_value;     {term is a named constant}
otherwise
          goto raw_value;
          end;
        end;
      end;                             {end of special handling term type cases}
    end;                               {done handling non-compound expression}

raw_value:                             {jump here to write raw numeric value}
  if addr_cnt <> 0 then begin          {need to write starting "*" or "&" ?}
    enc := enclose_yes_k;              {must now be enclosed in ()}
    if addr_cnt > 0                    {select which operator to write}
      then c := '&'
      else c := '*';
    for i := 1 to abs(addr_cnt) do begin {once for each time to write operator}
      sst_w.appendn^ (c, 1);
      end;
    end;                               {done handling ADDR_CNT not zero}

  if dt_out_p <> nil then begin        {an output data type was specified ?}
    dt_p := dt_out_p;
    while dt_p^.dtype = sst_dtype_copy_k {resolve base requested output dtype}
      do dt_p := dt_p^.copy_dtype_p;
    if                                 {requested output dtype is a string ?}
        (dt_p^.dtype = sst_dtype_array_k) and
        dt_p^.ar_string
        then begin                     {output data type is a string}
      str.max := min(sizeof(str.str), dt_p^.ar_ind_n); {length of desired string}
      case exp.val.dtype of            {what is data type of constant descriptor ?}
sst_dtype_array_k: begin               {constant is a string}
          string_copy (exp.val.ar_str_p^, str); {make local copy of string}
          end;
sst_dtype_char_k: begin                {constant is a character}
          str.str[1] := exp.val.char_val; {set first character of string}
          str.len := 1;
          end;
otherwise                              {incompatible data type}
        sys_msg_parm_int (msg_parm[1], ord(exp.val.dtype));
        syo_error (exp.str_h, 'sst_c_write', 'dtype_exp_unexpected', msg_parm, 1);
        end;
      string_pad (str);                {pad to max length by adding blanks}
      val.dtype := sst_dtype_array_k;  {create const descriptor for final string}
      val.ar_str_p := univ_ptr(addr(str));
      sst_w_c_value (val, enc);        {write value of temp constant descriptor}
      return;
      end;                             {done handling output data type is a string}
    end;                               {done handling output data type specified}

  sst_w_c_value (exp.val, enc);        {write constant value}
  return;

exp_value:                             {jump here to write full expression}
  sst_w_c_exp (exp, addr_cnt, nil, enclose);
  end;
