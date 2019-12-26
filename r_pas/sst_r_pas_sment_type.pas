{   Subroutine SST_R_PAS_SMENT_TYPE
*
*   Process TYPE_STATEMENT syntax.  This will also involve processing any
*   number of TYPE_SUBSTATEMENT syntaxes.  These will be invoked explicitly
*   with the syntax subroutine SST_R_PAS_SYN_TYPE.
}
module sst_r_pas_SMENT_TYPE;
define sst_r_pas_sment_type;
%include 'sst_r_pas.ins.pas';

procedure sst_r_pas_sment_type;        {process TYPE_STATMENT construction}

const
  max_msg_parms = 3;                   {max parameters we can pass to a message}

var
  tag: sys_int_machine_t;              {syntax tag from .syn file}
  str_h: syn_string_t;                 {handle to string for a tag}
  chain_p: sst_symbol_p_t;             {points to symbols declared here}
  symbol_p: sst_symbol_p_t;            {points to current symbol definition}
  dtype_p: sst_dtype_p_t;              {pointer to new data type definition}
  dt_p: sst_dtype_p_t;                 {scratch data type block pointer}
  fnam: string_treename_t;             {file name passed to a message}
  lnum: sys_int_machine_t;             {line number passed to a message}
  mflag: syn_mflag_k_t;                {syntaxed matched yes/no flag}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;
  stat: sys_err_t;                     {completion status code}

label
  loop_statement, loop_tag;

begin
  fnam.max := sizeof(fnam.str);

  chain_p := nil;                      {init chain of new symbols to empty}

loop_statement:                        {back here each new TYPE_SUBSTATEMENT syntax}
  syn_tree_clear;                      {set up for parsing}
  sst_r_pas_syn_type (mflag);          {try to parse next TYPE_SUBSTATEMENT syntax}
  if mflag = syn_mflag_yes_k
    then begin                         {syntax matched}
      error_syn_found := false;        {indicate no syntax error}
      end
    else begin                         {syntax did not match}
      return;                          {no more TYPE_SUBSTATMENTS here}
      end
    ;
  syn_tree_setup;                      {set up syntax tree for getting tags}
  syn_level_down;                      {down into TYPE_SUBSTATEMENT syntax level}

loop_tag:                              {back here for each new TYPE_SUBSTATEMENT tag}
  syn_get_tag_msg (tag, str_h, 'sst_pas_read', 'type_sment_bad', nil, 0); {get next tag}
  case tag of                          {what kind of tag is it ?}
{
*   End of this TYPE statement.
}
syn_tag_end_k: begin
  goto loop_statement;                 {back for next TYPE_SUBSTATEMENT syntax}
  end;
{
*   Tag is name of new symbol being declared as a data type.
}
1: begin
  sst_symbol_new (                     {add new symbol to current scope}
    str_h, syn_charcase_down_k, symbol_p, stat);
  if sys_stat_match (sst_subsys_k, sst_stat_sym_prev_def_k, stat)
    then begin                         {symbol already exists}
      if  (symbol_p^.symtype <> sst_symtype_dtype_k) or else
          (symbol_p^.dtype_dtype_p^.dtype <> sst_dtype_undef_k)
          then begin                   {existing symbol is not undefined data type ?}
        sst_charh_info (symbol_p^.char_h, fnam, lnum);
        sys_msg_parm_vstr (msg_parm[1], symbol_p^.name_in_p^);
        sys_msg_parm_int (msg_parm[2], lnum);
        sys_msg_parm_vstr (msg_parm[3], fnam);
        syn_error (str_h, 'sst_pas_read', 'symbol_already_def', msg_parm, 3);
        end;
      end
    else begin                         {symbol did not already exist}
      syn_error_abort (stat, str_h, '', '', nil, 0); {check for error}
      symbol_p^.symtype := sst_symtype_dtype_k; {new symbol is a data type}
      sst_dtype_new (symbol_p^.dtype_dtype_p); {create dtype block for this symbol}
      symbol_p^.dtype_dtype_p^.symbol_p := symbol_p; {point dtype block to symbol}
      end
    ;
  symbol_p^.flags := symbol_p^.flags + {this symbol will be defined here}
    [sst_symflag_def_k];
  symbol_p^.next_p := chain_p;         {link with other symbols defined together}
  chain_p := symbol_p;                 {update pointer to start of chain}
  end;
{
*   Tag is the data type definition for the previous symbol names.
*   All the symbols have empty data type blocks linked to them.  Set the first
*   one from the data type definition and then copy the information to the
*   others.
}
2: begin
  dtype_p := chain_p^.dtype_dtype_p;   {get pointer to dtype block of first symbol}
  sst_r_pas_data_type (dtype_p);       {fill in dtype block of first symbol}

  chain_p := chain_p^.next_p;          {start looping at second symbol in chain}
  while chain_p <> nil do begin        {once for each symbol of this data type}
    dt_p := chain_p^.dtype_dtype_p;    {get pointer to symbols data type block}
    dt_p^ := dtype_p^;                 {init our dtype to exact copy of base}
    if sst_symflag_intrinsic_out_k in dtype_p^.symbol_p^.flags
        then begin                     {base data type is intrinsic to output}
      dt_p^.dtype := sst_dtype_copy_k; {make new data type soft copy of old}
      dt_p^.copy_symbol_p := dtype_p^.symbol_p;
      dt_p^.copy_dtype_p := dtype_p;
      end;
    dt_p^.symbol_p := chain_p;         {point data type block to this symbol}
    chain_p := chain_p^.next_p;        {point to next symbol in chain}
    end;                               {back and process next symbol in chain}
  end;

otherwise
    syn_error_tag_unexp (tag, str_h);
    end;                               {end of TAG cases}

  goto loop_tag;                       {back and process next tag}
  end;
