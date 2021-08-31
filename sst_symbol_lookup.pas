{   Subroutine SST_SYMBOL_LOOKUP (NAME_H, SYM_P, STAT)
*
*   Look up an existing symbol in the currently active name spaces.
*   NAME_H is the string handle returned by one of the SYO_GET_TAG routines.
*   SYM_P is returned pointing to the symbol data block.
*   STAT is returned with an error if the symbol was not found.
}
module sst_symbol_lookup;
define sst_symbol_lookup;
%include 'sst2.ins.pas';

procedure sst_symbol_lookup (          {get data about an existing symbol}
  in      name_h: syo_string_t;        {handle to name string from tag}
  out     sym_p: sst_symbol_p_t;       {returned pointer to symbol descriptor}
  out     stat: sys_err_t);            {completion status code}

var
  name: string_var132_t;               {symbol name string}

begin
  name.max := sizeof(name.str);        {init local var string}

  syo_get_tag_string (name_h, name);   {get raw symbol name}
  sst_symbol_lookup_name (name, sym_p, stat); {lookup explicit name}
  end;
