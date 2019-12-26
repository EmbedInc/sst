{   Subroutine SST_R_PAS_INTEGER
}
module sst_r_pas_INTEGER;
define sst_r_pas_integer;
%include 'sst_r_pas.ins.pas';

procedure sst_r_pas_integer (          {read UNSIGNED_LIT_INTEGER and return value}
  out     ival: sys_int_max_t);        {returned integer value}

var
  tag: sys_int_machine_t;              {syntax tag ID}
  str_h: syn_string_t;                 {handle to string associated with TAG}
  token: string_var80_t;               {scratch string for number conversion}
  base: sys_int_machine_t;             {number base of integer string}
  stat: sys_err_t;

begin
  token.max := sizeof(token.str);      {init var string}

  syn_level_down;                      {down into UNSIGNED_LIT_INTEGER syntax level}
  syn_get_tag_msg (tag, str_h, 'sst_pas_read', 'constant_bad', nil, 0);
  if tag <> 1 then begin
    syn_error_tag_unexp (tag, str_h);
    end;
  syn_get_tag_string (str_h, token);   {get decimal integer or optional base string}
  string_t_int_max (token, ival, stat); {convert string to integer value}
  syn_error_abort (stat, str_h, 'sst_pas_read', 'constant_bad', nil, 0);
  syn_get_tag_msg (tag, str_h, 'sst_pas_read', 'constant_bad', nil, 0);
  case tag of

syn_tag_end_k: begin                   {token was decimal integer value}
  end;

1: begin                               {token was base for next token}
  base := ival;                        {save number base to use}
  syn_get_tag_string (str_h, token);   {get integer value string}
  string_t_int_max_base (              {convert string to integer value}
    token,                             {input string}
    base,                              {number base of input string}
    [string_ti_unsig_k],               {string is unsigned, null string is error}
    ival,                              {output value}
    stat);
  syn_error_abort (stat, str_h, 'sst_pas_read', 'constant_bad', nil, 0);
  end;

otherwise
    syn_error_tag_unexp (tag, str_h);
    end;
  syn_level_up;                        {up from UNSIGNED_LIT_INTEGER syntax level}
  end;
