{   Subroutine SST_R_PAS_SMENT_VAR
*
*   Process the Pascal VAR_STATEMENT syntax.  This will also involve processing
*   any number of VAR_SUBSTATEMENT syntaxes.  These will be invoked explicitly
*   with the syntax subroutine SSR_R_PAS_SYN_VAR.
}
module sst_r_pas_SMENT_VAR;
define sst_r_pas_sment_var;
%include 'sst_r_pas.ins.pas';

procedure sst_r_pas_sment_var;         {process VAR_STATEMENT syntax}

var
  tag: sys_int_machine_t;              {syntax tag from .syn file}
  str_h: syo_string_t;                 {handle to string for a tag}
  sym_p: sst_symbol_p_t;               {scratch pointer to symbol table entry}
  chain_p: sst_symbol_p_t;             {points to symbols declared here}
  dtype_p: sst_dtype_p_t;              {points to data type for curr set of vars}
  com_p: sst_symbol_p_t;               {points to common block symbol, if any}
  com_chain_pp: sst_symbol_pp_t;       {points to end of common block names chain}
  exp_p: sst_exp_p_t;                  {points to initial value for variables}
  new_chain: boolean;                  {TRUE if next name start new chain}
  charcase: syo_charcase_k_t;          {name character upper/lower case flag}
  mflag: syo_mflag_k_t;                {syntaxed matched yes/no flag}
  stat: sys_err_t;

label
  loop_statement, loop_tag;

begin
  syo_level_down;                      {down into VAR_STATEMENT syntax}
{
*   Process optional common block name for all the variables of this VAR statement.
}
  syo_get_tag_msg                      {get common block name tag}
    (tag, str_h, 'sst_pas_read', 'var_sment_bad', nil, 0);
  case tag of
1:  begin                              {no common block name}
      com_p := nil;
      com_chain_pp := nil;
      end;
2:  begin                              {common block name present}
      sst_symbol_new (                 {create and init common block name symbol}
        str_h, syo_charcase_asis_k, com_p, stat);
      syo_error_abort (stat, str_h, '', '', nil, 0);
      com_p^.symtype := sst_symtype_com_k; {set up common block symbol descriptor}
      if not (sst_symflag_global_k in com_p^.flags) then begin {not in DEFINE ?}
        com_p^.flags := com_p^.flags + {init to common block defined elsewhere}
          [sst_symflag_global_k, sst_symflag_extern_k];
        end;
      if sst_config.os = sys_os_domain_k then begin {target machine is Apollo ?}
        com_p^.flags := com_p^.flags + {common blocks are always "defined"}
          [sst_symflag_global_k];
        com_p^.flags := com_p^.flags -
          [sst_symflag_extern_k];
        end;
      com_p^.com_first_p := nil;
      com_p^.com_size := 0;
      com_chain_pp := addr(com_p^.com_first_p); {init pointer to com names chain end}
      end;
otherwise
    syo_error_tag_unexp (tag, str_h);
    end;

  new_chain := true;                   {next symbol name starts new chain of vars}
{
*   Back here each new VAR_SUBSTATEMENT syntaxes.
}
loop_statement:
  syo_tree_clear;                      {set up for parsing}
  sst_r_pas_syn_var (mflag);           {try to parse next VAR_SUBSTATEMENT syntax}
  if mflag = syo_mflag_yes_k
    then begin                         {syntax matched}
      error_syo_found := false;        {indicate no syntax error}
      end
    else begin                         {syntax did not match}
      return;                          {no more VAR_SUBSTATMENTS here}
      end
    ;
  syo_tree_setup;                      {set up syntax tree for getting tags}
  syo_level_down;                      {down into VAR_SUBSTATEMENT syntax level}
{
*   Determine whether the variable name cases should be preserved or not.
*   The variable names are always converted to lower case unless they are
*   globally known symbols.  This is only true if the EXTERN keyword is present.
}
  charcase := syo_charcase_down_k;     {init to assume conversion to lower case}

  syo_push_pos;                        {save state of syntax tree traversal}
  repeat                               {loop over all the variable name tags}
    syo_get_tag_msg (tag, str_h, 'sst_pas_read', 'var_sment_bad', nil, 0);
    until tag <> 1;
  if tag = 2 then begin                {found EXTERN keyword ?}
    charcase := syo_charcase_asis_k;   {preserve character upper/lower case}
    end;
  syo_pop_pos;                         {restore syntax tree traversal state}
{
*   Back here each new tag.
}
loop_tag:                              {back here for next syntax tag}
  syo_get_tag_msg (tag, str_h, 'sst_pas_read', 'var_sment_bad', nil, 0);
  case tag of
{
*   Tag is name of new symbol being declared as a variable.
}
1: begin
  if new_chain
    then begin                         {this symbol starts a new chain of var names}
      sst_symbol_new (                 {add new symbol to current scope}
        str_h, charcase, sym_p, stat);
      chain_p := sym_p;                {init start of chain}
      new_chain := false;              {next symbol adds to this chain}
      end
    else begin                         {this symbol will be added to existing chain}
      sst_symbol_new (                 {add new symbol to current scope}
        str_h, charcase, sym_p^.next_p, stat);
      sym_p := sym_p^.next_p;          {update current symbol to new symbol}
      end
    ;
  syo_error_abort (stat, str_h, '', '', nil, 0);
  sym_p^.symtype := sst_symtype_var_k; {new symbol is a variable}
  sym_p^.flags := [                    {init flags for this symbol}
    sst_symflag_def_k];                {symbol will be defined}
  if                                   {this variable is in static storage ?}
      (com_p <> nil) or                {variable is in a common block ?}
      (nest_level <= 1)                {variable is at PROGRAM or MODULE level ?}
      then begin
    sym_p^.flags := sym_p^.flags + [sst_symflag_static_k];
    end;
  end;
{
*   All the variable names of the current chain were flagged with the EXTERN
*   keyword.
}
2: begin
  sym_p := chain_p;                    {init current symbol to first symbol in chain}
  while sym_p <> nil do begin          {once for each symbol in chain}
    if com_p <> nil then begin         {symbols are in a common block ?}
      syo_error (str_h, 'sst_pas_read', 'common_and_extern', nil, 0);
      end;
    sym_p^.flags := sym_p^.flags + [sst_symflag_global_k, sst_symflag_extern_k];
    sym_p := sym_p^.next_p;            {advance to next symbol in chain}
    end;                               {back and process this new symbol}
  end;
{
*   Tag is data type definition for the variables currently on the chain.
}
3: begin
  new_chain := true;                   {next variable name starts a new chain}
  dtype_p := nil;                      {indicate to create new data type descriptor}
  sst_r_pas_data_type (dtype_p);       {get pointer to data type for vars on chain}
  sym_p := chain_p;                    {init to first symbol in chain}
  while sym_p <> nil do begin          {once for each var name in chain}
    sym_p^.var_dtype_p := dtype_p;     {set data type for this var name}
    sym_p^.var_val_p := nil;           {init to no initializer value here}
    sym_p^.var_arg_p := nil;           {this variable is not a dummy argument}
    sym_p^.var_proc_p := nil;
    sym_p^.var_com_p := com_p;         {point to common block symbol, if any}
    sym_p^.var_next_p := nil;          {init to this is last symbol in common block}
    if com_p <> nil then begin         {this variable is in a common block ?}
      com_chain_pp^ := sym_p;          {add variable to common block chain}
      com_chain_pp := addr(sym_p^.var_next_p); {update pointer to end of chain}
      if dtype_p^.align > 0 then begin
        com_p^.com_size :=             {pad size for this var's alignment}
          ((com_p^.com_size + dtype_p^.align - 1) div dtype_p^.align) *
          dtype_p^.align;
        end;
      com_p^.com_size := com_p^.com_size + {add size of this variable}
        dtype_p^.size_used;
      end;                             {done handling var is in common block}
    sym_p := sym_p^.next_p;            {advance to next symbol in chain}
    end;                               {back to process new symbol in chain}
  end;
{
*   No tag was found.  This is the normal way to indicate end of VAR_SUBSTATEMENT.
}
syo_tag_end_k: begin
  goto loop_statement;                 {back for next VAR_SUBSTATEMENT syntax}
  end;
{
*   Tag is initial value for all the variables in the chain.
}
4: begin
  sst_r_pas_var_init (dtype_p^, exp_p); {get expression for initial var value}
  sym_p := chain_p;                    {init current symbol to first in chain}
  while sym_p <> nil do begin          {once for each symbol in chain}
    if not (sst_symflag_static_k in sym_p^.flags) then begin {variable not static ?}
      syo_error (str_h, 'sst', 'var_init_not_static', nil, 0);
      end;
    sym_p^.var_val_p := exp_p;         {indicate this var has initial value}
    sym_p := sym_p^.next_p;            {to next var symbol with same initial value}
    end;                               {back and process this new var}
  end;
{
*   Tag is STATIC keyword.  This explicitly makes all the variables on the
*   chain allocated in static storage (not on the routine stack frame).
}
5: begin
  sym_p := chain_p;                    {init current symbol to first in chain}
  while sym_p <> nil do begin          {once for each symbol in chain}
    sym_p^.flags := sym_p^.flags + [sst_symflag_static_k]; {flag this var as static}
    sym_p := sym_p^.next_p;            {advance to next var in chain}
    end;                               {back and process this new var}
  end;
{
*   Unexpected tag value
}
otherwise
    syo_error_tag_unexp (tag, str_h);
    end;                               {end of tag cases}
  goto loop_tag;                       {back for next tag in VAR_SUBSTATEMENT}
  end;
