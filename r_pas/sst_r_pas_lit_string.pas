{   Subroutine SST_R_PAS_LIT_STRING (STR)
*
*   Process the LIT_STRING syntax.  The string represented by this syntax
*   is returned in STR.
}
module sst_r_pas_LIT_STRING;
define sst_r_pas_lit_string;
%include 'sst_r_pas.ins.pas';

procedure sst_r_pas_lit_string (       {read LIT_STRING syntax and return string}
  in out  str: univ string_var_arg_t); {returned string}

var
  tag: sys_int_machine_t;              {syntax tag ID}
  str_h: syo_string_t;                 {handle to string associated with TAG}
  val: sst_var_value_t;                {value of ASCII character number}

label
  str_loop;
{
************************************************
*
*   Local subroutine QUOTED_STRING (S)
*
*   Append the string characters from the syntax QUOTED_STRING_CHARS to the
*   string S.
}
procedure quoted_string (
  in out  s: univ string_var_arg_t);   {string to append new characters to}

var
  tag: sys_int_machine_t;              {syntax tag ID}
  str_h: syo_string_t;                 {handle to string associated with TAG}
  token: string_var8192_t;             {scratch string for extracting characters}

label
  quoted_loop;

begin
  token.max := sizeof(token.str);      {init local var string}
  syo_level_down;                      {down into QUOTED_STRING_CHARS syntax}

quoted_loop:
  syo_get_tag_msg (tag, str_h, 'sst_pas_read', 'constant_bad', nil, 0);
  case tag of

1: begin                               {chunk of raw characters}
      syo_get_tag_string (str_h, token); {get raw string characters}
      string_append (s, token);        {add characters to output string}
      end;

2: begin                               {double quote, interpret as literal quote}
      string_append1 (s, '''');
      end;

syo_tag_end_k: begin                   {done reading quoted string}
      syo_level_up;                    {back up from QUOTED_STRING_CHARS syntax}
      return;
      end;

otherwise
    syo_error_tag_unexp (tag, str_h);
    end;                               {done with TAG cases}
  goto quoted_loop;                    {back and read next chunk of string}
  end;
{
************************************************
*
*   Start of main routine.
}
begin
  syo_level_down;                      {down into LIT_STRING syntax}
  str.len := 0;                        {init accumulated string to empty}

str_loop:                              {back here each new part of quoted string}
  syo_get_tag_msg (tag, str_h, 'sst_pas_read', 'constant_bad', nil, 0);
  case tag of

1: begin                               {quoted string characters}
  quoted_string (str);                 {append characters from this quoted part}
  goto str_loop;                       {back for next part of this string}
  end;

2: begin                               {ASCII character value in ()}
  sst_r_pas_exp_eval (val);            {get value of ASCII character}
  if val.dtype <> sst_dtype_int_k then begin {expression was not an integer ?}
    syo_error (str_h, 'sst_pas_read', 'exp_not_int', nil, 0);
    end;
  string_append1 (str, chr(val.int_val)); {append this character to output string}
  goto str_loop;                       {back for next part of this string}
  end;

syo_tag_end_k: begin                   {hit end of string, STR all set}
  end;

otherwise
    syo_error_tag_unexp (tag, str_h);
    end;

  syo_level_up;                        {up from LIT_STRING syntax}
  end;
