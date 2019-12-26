{   Subroutine SST_W_C_NAME_COM_VAR (SYM)
*
*   Set the output name of a variable in a common block.  It is an error if
*   the output name is already set.
*
*   In the C language, common blocks are emulated with variables that are
*   STRUCTs, and declared globally known.  Each variable in the common block
*   becomes a field in the struct.
*
*   The syntax for referring to one of these variables is
*   "<common block name>.<var name>".  Since the common block variable names
*   are now in their own namespace, one further refinement is added.
*   If the variable name starts with the name of the common block followed
*   by an underscore, then that portion of the variable name is stripped off.
*   For the purposes of the stripping algorithm, only letters in the common
*   block name are taken into account, and any non-letters around or in the
*   common block name at the start of the variable name are also stripped off.
*   For example:
*
*   COMMON BLOCK NAME   ORIGINAL VAR NAME   RESULTING VAR NAME
*                xxxx               stuff                stuff
*                rend          rend_stuff                stuff
*               rend2          rend_stuff                stuff
*                 xxx            xxxx_zzz             xxxx_zzz
*                 xxx            xxx2_zzz                  zzz
}
module sst_w_c_NAME_COM_VAR;
define sst_w_c_name_com_var;
%include 'sst_w_c.ins.pas';

procedure sst_w_c_name_com_var (       {set output name for var in common block}
  in out  sym: sst_symbol_t);          {symbol to set output name for}

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  ind_c: string_index_t;               {index into common block name string}
  ind_v: string_index_t;               {index into variable name string}
  char_c, char_v: char;                {characters extracted from com and var names}
  name_p_old: string_var_p_t;          {saved pointer to original var input name}
  name: string_var132_t;               {for making stripped variable name}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

label
  char_v_next, skip_char_v, vname_start_set, no_strip;

begin
  name.max := size_char(name.str);     {init local var string}

  if sym.name_out_p <> nil then begin  {symbol already has output name ?}
    sys_msg_parm_vstr (msg_parm[1], sym.name_in_p^);
    sys_message_bomb ('sst_c_write', 'name_out_already_exists', msg_parm, 1);
    end;

  with
      sym.name_in_p^: name_v,          {NAME_V is input variable name}
      sym.var_com_p^.name_in_p^: name_c {NAME_C is common block input name}
      do begin
    ind_v := 0;                        {init index into variable name}
    for ind_c := 1 to name_c.len do begin {once for each character in com name}
      char_c := name_c.str[ind_c];     {get this common block name char}
      if (char_c >= 'a') and (char_c <= 'z') then begin {lower case letter ?}
        char_c := chr(ord(char_c) - ord('a') + ord('A')); {convert to upper case}
        end;
      if (char_c < 'A') or (char_c > 'Z') {common block name char not a letter ?}
        then next;                     {skip this char, go on to next}
char_v_next:                           {back here to skip to next var name char}
      ind_v := ind_v + 1;              {make index of new var name char}
      if ind_v > name_v.len then goto no_strip; {exhausted variable name string ?}
      char_v := name_v.str[ind_v];     {get this variable name char}
      if (char_v >= 'a') and (char_v <= 'z') then begin {lower case letter ?}
        char_v := chr(ord(char_v) - ord('a') + ord('A')); {convert to upper case}
        end;
      if (char_v < 'A') or (char_v > 'Z') {variable name char not a letter ?}
        then goto char_v_next;         {skip this char, go on to next}
      if char_v = char_c then next;    {these characters match, go to next}
      if                               {check for matched up to "_"}
          (ind_c > 1) and then         {this is not first common block char ?}
          (name_c.str[ind_c - 1] = '_') {previous com block name char was "_" ?}
          then begin
        ind_v := ind_v - 2;            {make index of last var char that matched}
        if ind_v < 0 then goto no_strip;
        goto skip_char_v;              {done macthing var and com symbol names}
        end;
      goto no_strip;                   {definately no match}
      end;                             {back and try next common block name char}
{
*   The start of the variable name does match the common block name by the
*   rules described above.  Now make sure that the next meaningful variable
*   name character is an underscore, and set IND_V to the first character in
*   the variable name after the common block name is stripped off.
}
skip_char_v:                           {back here to skip the current var name char}
    ind_v := ind_v + 1;                {make index of next var name char}
    if ind_v > name_v.len then goto no_strip; {exhausted variable name string ?}
    char_v := name_v.str[ind_v];       {get this variable name char}
    if char_v = '_' then begin         {this is the character we were looking for}
      ind_v := ind_v + 1;              {make index of first non-stripped char}
      goto vname_start_set;            {IND_V set to first non-stripped char}
      end;
    if                                 {this character is not a letter ?}
        ((char_v < 'a') or (char_v > 'z')) and
        ((char_v < 'A') or (char_v > 'Z'))
      then goto skip_char_v;           {skip over non-letter characters until "_"}
{
*   The start of the variable name did not match the common block name by
*   the rules described in the routine header comments.  The input variable
*   name will be translated to the output variable name in the regular way.
}
no_strip:
    sst_w.name_sym^ (sym);             {create output name in normal way}
    return;
{
*   The start of the variable name did match the common block name by the
*   rules described in the routine header comments.  IND_V is the index of
*   the first variable name character after the common block name is stripped
*   from the beginning.  The stripped name will be temporarily installed
*   as the symbols input name.  This will be used to derive the output name,
*   then the symbols input name pointer will be restored.
}
vname_start_set:
    if ind_v > name_v.len              {nothing left after com name stripped off ?}
      then goto no_strip;
    string_substr (                    {extract stripped name from var input name}
      name_v,                          {string to extract substring from}
      ind_v,                           {start index of substring}
      name_v.len,                      {ending index of substring}
      name);                           {output substring}
    end;                               {done with NAME_V and NAME_C abbreviations}
  name_p_old := sym.name_in_p;         {save pointer to variables input name}
  sym.name_in_p := univ_ptr(addr(name)); {temp set stripped name as input name}
  sst_w.name_sym^ (sym);               {set output name using stripped name as input}
  sym.name_in_p := name_p_old;         {restore pointer to symbol's input name}
  end;
