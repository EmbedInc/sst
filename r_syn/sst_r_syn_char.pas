{   Handle the SYN CHAR syntax.
}
module sst_r_syn_char;
define sst_r_syn_char_get;
%include 'sst_r_syn.ins.pas';
{
********************************************************************************
*
*   Function SST_R_SYN_CHAR_GET (CCODE)
*
*   Processes the CHAR syntax and returns the resulting character code.  The
*   current parsing position must be immediately before the link to the
*   subordinate CHAR syntax.
}
function sst_r_syn_char_get (          {get the result of the SYN CHAR syntax}
  out     ccode: sys_int_machine_t)    {0-N character code}
  :boolean;                            {success, no syntax error encountered}
  val_param;

var
  tk: string_var4_t;                   {single character being defined}

begin
  tk.max := size_char(tk.str);         {init local var string}

  sst_r_syn_char_get := false;         {init to not completed without error}

  if not syn_trav_next_down (syn_p^)   {down into CHAR syntax}
    then return;

  if syn_trav_next_tag(syn_p^) <> 1 then begin {get tag, should be 1}
    syn_msg_tag_bomb (syn_p^, '', '', nil, 0); {abort on unexpected tag}
    end;

  syn_trav_tag_string (syn_p^, tk);    {get the single-character string}
  if tk.len <> 1 then return;          {not a single character as expected ?}
  ccode := ord(tk.str[1]);             {return the character code}

  if not syn_trav_up (syn_p^)          {pop back up from CHAR syntax}
    then return;

  sst_r_syn_char_get := true;          {indicate success, no errors}
  end;
