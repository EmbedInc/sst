{   Subroutine SST_W_C_VALUE (VAL,ENCLOSE)
*
*   Write a constant value.  VAL is the descriptor for the value.
*   ENCLOSE indicates whether the final expression should be enclosed in
*   parentheses.  Values of ENCLOSE can be:
*
*     ENCLOSE_YES_K  -  Enclose in parentheses, if neccessary, to make the
*       entire expression be one term.  This is done when the constant is
*       written with a unary operator (such as minus sign).
*
*     ENCLOSE_NO_K  -  Don't enclose expression in parentheses, even if
*       a unary operator is used.
}
module sst_w_c_VALUE;
define sst_w_c_value;
%include 'sst_w_c.ins.pas';

procedure sst_w_c_value (              {write the value of a constant}
  in      val: sst_var_value_t;        {value descriptor}
  in      enclose: enclose_k_t);       {enclose in () yes/no}

const
  quote_char_c = '''';                 {character that starts/stops quoted char}
  quote_char_s = '"';                  {character that starts/stops quoted string}
  esc_char = '\';                      {escape char for special characters}
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  token: string_var8192_t;             {sratch string for number conversion, etc}
  i: sys_int_machine_t;                {loop counter}
  paren: boolean;                      {TRUE if wrote open parenthesis}
  stat: sys_err_t;
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

label
  done_fp, done_dtype;

begin
  token.max := sizeof(token.str);      {init local var string}
  paren := false;                      {init to no open parenthesis written}
  case val.dtype of
{
*   Data type is integer.
}
sst_dtype_int_k: begin
  if                                   {need parentheses around value ?}
      (enclose = enclose_yes_k) and
      (val.int_val < 0)
      then begin
    sst_w.appendn^ ('(', 1);
    paren := true;
    end;
  string_f_int_max (token, val.int_val);
  sst_w.append^ (token);
  end;
{
*   Data type is value of an enumerated type.
}
sst_dtype_enum_k: begin
  sst_w.append_sym_name^ (val.enum_p^);
  end;
{
*   Data type is floating point.
}
sst_dtype_float_k: begin
  if                                   {need parentheses around value ?}
      (enclose = enclose_yes_k) and
      (val.float_val < 0.0)
      then begin
    sst_w.appendn^ ('(', 1);
    paren := true;
    end;
  string_f_fp (                        {try to convert without using exp notation}
    token,                             {output string}
    val.float_val,                     {input floating point number}
    0,                                 {free format}
    0,                                 {free format for exponent (unused)}
    7,                                 {min required significant digits}
    6,                                 {max digits allowed left of point}
    1,                                 {min digits right of point}
    8,                                 {max digits right of point}
    [string_ffp_exp_no_k],             {don't allow exponential notation}
    stat);                             {completion status code}
  if not sys_error(stat) then begin    {conversion to string was successfull ?}
    while                              {truncate trailing zeros}
        (token.str[token.len] = '0') and
        (token.str[token.len-1] >= '0') and
        (token.str[token.len-1] <= '9')
        do begin
      token.len := token.len - 1;
      end;
    goto done_fp;                      {TOKEN all set}
    end;
  string_f_fp (                        {resort to exponential notation}
    token,                             {output string}
    val.float_val,                     {input floating point number}
    0,                                 {free format}
    0,                                 {free format for exponent}
    7,                                 {min required significant digits}
    0,                                 {max digits allowed left of point (unused)}
    0,                                 {min digits right of point}
    0,                                 {max digits right of point (unused)}
    [ string_ffp_exp_k,                {use exponential notation}
      string_ffp_exp_eng_k,            {use engineering notation}
      string_ffp_z_aft_k],             {always write at least one digit right of pnt}
    stat);                             {completion status code}
done_fp:                               {final floating point number is in TOKEN}
  sst_w.append^ (token);
  end;
{
*   Data type is boolean.
}
sst_dtype_bool_k: begin
  if val.bool_val
    then sst_w_c_intrinsic (intr_true_k)
    else sst_w_c_intrinsic (intr_false_k);
  end;
{
*   Data type is character.
}
sst_dtype_char_k: begin
  sst_w.appendn^ (quote_char_c, 1);    {put leading quote}
  if
      (val.char_val >= ' ') and
      (val.char_val <= '~')
    then begin                         {this is a printable character}
      if                               {this char needs to be "escaped" ?}
          (val.char_val = quote_char_c) or
          (val.char_val = esc_char)
          then begin
        sst_w.appendn^ (esc_char, 1);  {write escape character}
        end;
      sst_w.appendn^ (val.char_val, 1); {write the character itself}
      end
    else begin                         {this is not a printable character}
      sst_w.appendn^ (esc_char, 1);    {write escape character}
      string_f_int_max_base (          {convert character value to octal integer}
        token,                         {output string}
        ord(val.char_val),             {character value}
        8,                             {number base of output string}
        0,                             {indicate free-form formatting}
        [string_fi_unsig_k],           {input number if unsigned}
        stat);                         {error status code}
      sys_error_abort (stat, '', '', nil, 0);
      sst_w.append^ (token);           {write octal character value}
      end
    ;
  sst_w.appendn^ (quote_char_c, 1);    {put trailing quote}
  end;
{
*   Data type is an array.  We only know how to write string-type arrays.
}
sst_dtype_array_k: begin
  if                                   {target system needs special handling ?}
      (frame_scope_p^.sment_type <> sment_type_exec_k) and {not in executable code ?}
      (val.ar_str_p^.len > 0) and      {string has at least one character ?}
      (sst_config.os = sys_os_aix_k) and {IBM XLC C compiler ?}
      (not no_ibm_str_kluge)           {this special case not explicitly inhibited ?}
      then begin                       {write string as separate characters}
    sst_w.appendn^ ('{', 1);
    for i := 1 to val.ar_str_p^.len do begin {once for each character in string}
      sst_w.appendn^ ('''', 1);        {write leading quote for this character}
      if
          (val.ar_str_p^.str[i] >= ' ') and
          (val.ar_str_p^.str[i] <= '~')
        then begin                     {this is a printable character}
          if                           {this char needs to be "escaped" ?}
              (val.ar_str_p^.str[i] = quote_char_c) or
              (val.ar_str_p^.str[i] = esc_char)
              then begin
            sst_w.appendn^ (esc_char, 1); {write escape character}
            end;
          sst_w.appendn^ (val.ar_str_p^.str[i], 1); {write the character itself}
          end
        else begin                     {this is not a printable character}
          sst_w.appendn^ (esc_char, 1); {write escape character}
          string_f_int_max_base (      {convert character value to octal integer}
            token,                     {output string}
            ord(val.ar_str_p^.str[i]), {character value}
            8,                         {number base of output string}
            0,                         {indicate free-form formatting}
            [string_fi_unsig_k],       {input number if unsigned}
            stat);                     {error status code}
          sys_error_abort (stat, '', '', nil, 0);
          sst_w.append^ (token);       {write octal character value}
          end
        ;
      sst_w.appendn^ ('''', 1);        {write trailing quote for this character}
      if i <> val.ar_str_p^.len then begin {another character will follow ?}
        sst_w.appendn^ (',', 1);
        sst_w.allow_break^;
        end;
      end;                             {back for next source character}
    sst_w.appendn^ ('}', 1);
    goto done_dtype;                   {all done writing value}
    end;                               {done with IBM XLC C special case}

  token.len := 0;
  sst_w.appendn^ ('"', 1);             {write leading quote}
  for i := 1 to val.ar_str_p^.len do begin {once for each character in string}
    if
        (val.ar_str_p^.str[i] >= ' ') and
        (val.ar_str_p^.str[i] <= '~')
      then begin                       {this is a printable character}
        if                             {this char needs to be "escaped" ?}
            (val.ar_str_p^.str[i] = quote_char_s) or
            (val.ar_str_p^.str[i] = esc_char)
            then begin
          sst_w.appendn^ (esc_char, 1); {write escape character}
          end;
        sst_w.appendn^ (val.ar_str_p^.str[i], 1); {write the character itself}
        end
      else begin                       {this is not a printable character}
        sst_w.appendn^ (esc_char, 1);  {write escape character}
        string_f_int_max_base (        {convert character value to octal integer}
          token,                       {output string}
          ord(val.ar_str_p^.str[i]),   {character value}
          8,                           {number base of output string}
          3,                           {we always need 3 digits}
          [ string_fi_unsig_k,         {input number if unsigned}
            string_fi_leadz_k],        {always write leading zeros}
          stat);                       {error status code}
        sys_error_abort (stat, '', '', nil, 0);
        sst_w.append^ (token);         {write octal character value}
        end
      ;
    end;                               {back for next source character}
  sst_w.appendn^ ('"', 1);             {write trailing quote}
  end;
{
*   Data type is pointer.
}
sst_dtype_pnt_k: begin
  if val.pnt_exp_p = nil
    then begin                         {NIL pointer}
      sst_w_c_intrinsic (intr_nil_k);
      end
    else begin                         {pointing to a real symbol}
      sst_w_c_exp (val.pnt_exp_p^, 1, nil, enclose);
      end
    ;
  end;
{
*   Unexpected data type.
}
otherwise
    sys_msg_parm_int (msg_parm[1], ord(val.dtype));
    sys_message_bomb ('sst', 'dtype_unexpected', msg_parm, 1);
    end;                               {end of data type cases}
done_dtype:                            {jump here if done with data type case}

  if paren then begin                  {need to write close parenthesis ?}
    sst_w.appendn^ (')', 1);
    end;
  end;
