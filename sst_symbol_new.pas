{   Subroutine SST_SYMBOL_NEW (NAME_H, CHARCASE, SYMBOL_P, STAT)
*
*   Add a new symbol to the current scope.  It is an error if the symbol
*   already exists in that name space, although it may exists in more global
*   name spaces.  NAME_H is the string handle for the symbol name returned
*   from one of the SYN_GET_TAG routines.  SYMBOL_P is returned pointing to
*   the newly created data block for the symbol.  STAT is returned with an
*   error if the symbol already exists in the current scope.
*
*   The value of CHARCASE effects what kind of case conversions are applied to
*   the symbol name before storing it in the symbol table.  Values can be:
*
*     SYN_CHARCASE_DOWN_K - Force symbol name to all lower case.
*     SYN_CHARCASE_UP_K   - Force symbol name to all upper case.
*     SYN_CHARCASE_ASIS_K - Do not alter name.
}
module sst_SYMBOL_NEW;
define sst_symbol_new;
%include 'sst2.ins.pas';

procedure sst_symbol_new (             {add symbol to current scope given syn handle}
  in      name_h: syn_string_t;        {handle to name string from tag}
  in      charcase: syn_charcase_k_t;  {SYN_CHARCASE_xxx_K with DOWN, UP or ASIS}
  out     symbol_p: sst_symbol_p_t;    {points to new symbol descriptor}
  out     stat: sys_err_t);            {completion status code}
  val_param;

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  name: string_var132_t;               {symbol name string}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

begin
  name.max := sizeof(name.str);        {init local var string}

  syn_get_tag_string (name_h, name);   {get actual symbol name}
  case charcase of
syn_charcase_down_k: string_downcase(name);
syn_charcase_up_k: string_upcase(name);
syn_charcase_asis_k: ;
otherwise
    sys_msg_parm_int (msg_parm[1], ord(charcase));
    sys_message_bomb ('syn', 'charcase_bad', msg_parm, 1);
    end;

  sst_symbol_new_name (name, symbol_p, stat); {add new symbol to symbol table}
  if not sys_error(stat) then begin    {really did create new symbol ?}
    symbol_p^.char_h := name_h.first_char; {save handle to first symbol source char}
    end;
  end;
