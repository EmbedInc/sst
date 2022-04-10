{   Subroutine SST_R_SYN_DOIT (FNAM, GNAM, STAT)
*
*   Process one top level SYN language input file.  FNAM is the input file name.
*   GNAM is returned as the generic name of FNAM.  Each call to this routine
*   adds to the single module that was created when this SST front end was
*   initialized (SST_R_SYN_INIT called).
}
module sst_r_syn_doit;
define sst_r_syn_doit;
%include 'sst_r_syn.ins.pas';

procedure sst_r_syn_doit (             {do SYN language front end phase}
  in      fnam: univ string_var_arg_t; {raw input file name}
  in out  gnam: univ string_var_arg_t; {returned as generic name of input file}
  out     stat: sys_err_t);            {completion status code}

var
  fline_p: fline_p_t;                  {to FLINE library use state}
  coll_p: fline_coll_p_t;              {the input file lines in FLINE collection}
  match: boolean;                      {syntax matched}
  hpos: string_hash_pos_t;             {position handle into our hash table}
  found: boolean;                      {TRUE when hash table entry found}
  name_p: string_var_p_t;              {pointer to name in hash table entry}
  data_p: symbol_data_p_t;             {pointer to our data in hash table entry}
  undefined_syms: boolean;             {TRUE if undefined symbols found}
  unused_syms: boolean;                {TRUE after first unused symbol encountered}

label
  loop_cmd, eod;
{
********************************************************************************
*
*   Local subroutine FOLLOW_SYM (DATA)
*
*   Propagate USED flag to all symbols ultimately referenced by the syntax
*   construction with the symbol table data DATA.
}
procedure follow_sym (
  in      data: symbol_data_t);        {data about symbol to follow}
  val_param;

var
  call_p: call_p_t;                    {points to data about called symbol}

begin
  data.sym_p^.flags :=                 {set FOLLOWED flag to prevent recursive calls}
    data.sym_p^.flags + [sst_symflag_followed_k];

  call_p := data.call_p;               {init to first entry in called chain}
  while call_p <> nil do begin         {once for each entry in called chain}
    call_p^.data_p^.sym_p^.flags :=    {set USED flag}
      call_p^.data_p^.sym_p^.flags + [sst_symflag_used_k];
    if not (sst_symflag_followed_k in call_p^.data_p^.sym_p^.flags) then begin
      follow_sym (call_p^.data_p^);    {follow this nested symbol}
      end;
    call_p := call_p^.next_p;          {advance to next called symbol in list}
    end;
  end;
{
********************************************************************************
*
*   Start of main routine.
}
begin
  string_generic_fnam (fnam, '.syn', gnam); {make generic input file name}
  string_copy (gnam, prefix);          {use generic name as subroutine prefix}
{
*   Read the input file into a collection of text lines managed by the FLINE
*   library.  COLL_P will be set pointing to the collection.
}
  fline_lib_new (                      {open the FLINE library}
    util_top_mem_context,              {parent memory context}
    fline_p,                           {returned pointer to new library use state}
    stat);
  if sys_error(stat) then return;

  fline_file_get_suff (                {read the input file into a collection}
    fline_p^,                          {FLINE library use state}
    fnam, '.syn',                      {file name and mandatory suffix}
    coll_p,                            {returned pointer to the collection}
    stat);
  if sys_error(stat) then return;

  syn_parse_pos_coll (syn_p^, coll_p^); {init parse position to start of collection}
{
*   Main loop.  Come back here each new command in the SYN file.  This loop
*   terminates either on an error or end of data.
}
loop_cmd:
  match := syn_parse_next (            {parse next command from input file}
    syn_p^,                            {our SYN library use state}
    addr(syn_chsyn_command));          {top level parsing routine to call}
  if not match then begin              {syntax error encountered ?}
    syn_parse_err_reparse (syn_p^);    {do error re-parse}
    end;

  syn_trav_init (syn_p^);              {init for traversing syntax tree}
  sst_r_syn_command (stat);            {process this SYN file command}
  if sys_stat_match (sst_subsys_k, sst_stat_eod_k, stat) {hit end of input data ?}
    then goto eod;
  if sys_error(stat) then return;      {encountered hard error ?}

  if not match then begin              {syntax error not caught as content error ?}
    syn_parse_err_show (syn_p^);       {show the syntax error}
    sys_stat_set (                     {indicate error, already handled}
      sst_subsys_k, sst_stat_err_handled_k, stat);
    return;                            {return with error condition}
    end;
  goto loop_cmd;                       {back for next command from SYN file}

eod:                                   {reached end of all input data}
{
*   End of input data encountered.
*
*   Propagate used flags to all appropriate SYN file symbols.
}
  string_hash_pos_first (table_sym, hpos, found); {go to first entry in hash table}

  while found do begin                 {once for each entry in hash table}
    string_hash_ent_atpos (            {get info about this hash table entry}
      hpos,                            {handle to hash table position}
      name_p,                          {returned pointing to name in table entry}
      data_p);                         {returned pointing to our data area in entry}
    if                                 {this symbol used but not followed yet ?}
        (sst_symflag_used_k in data_p^.sym_p^.flags) and
        (not (sst_symflag_followed_k in data_p^.sym_p^.flags))
        then begin
      follow_sym (data_p^);            {propagate USED flag for this symbol}
      end;
    string_hash_pos_next (hpos, found); {advance to next hash table entry}
    end;                               {back and process this new hash table entry}
{
*   List any unused SYN symbols and clear FOLLOWED flags left from previous
*   pass over all the symbols.
}
  unused_syms := false;                {init to no unused symbols found}
  string_hash_pos_first (table_sym, hpos, found); {go to first entry in hash table}

  while found do begin                 {once for each entry in hash table}
    string_hash_ent_atpos (            {get info about this hash table entry}
      hpos,                            {handle to hash table position}
      name_p,                          {returned pointing to name in table entry}
      data_p);                         {returned pointing to our data area in entry}
    data_p^.sym_p^.flags :=            {clear FOLLOWED flag}
      data_p^.sym_p^.flags - [sst_symflag_followed_k];
    if                                 {this symbol was never used ?}
        not (sst_symflag_used_k in data_p^.sym_p^.flags)
        then begin
      if not unused_syms then begin    {this is first unused symbol ?}
        sys_message ('sst_syn_read', 'symbols_unused_show');
        unused_syms := true;           {flag that unused symbols were encountered}
        end;
      writeln ('  ', name_p^.str:name_p^.len); {write name of unused symbol}
      end;
    string_hash_pos_next (hpos, found); {advance to next hash table entry}
    end;                               {back and process this new hash table entry}
{
*   Check for undefined SYN symbols.
}
  undefined_syms := false;             {init to no undefined symbols found}
  string_hash_pos_first (table_sym, hpos, found); {go to first entry in hash table}

  while found do begin                 {once for each entry in hash table}
    string_hash_ent_atpos (            {get info about this hash table entry}
      hpos,                            {handle to hash table position}
      name_p,                          {returned pointing to name in table entry}
      data_p);                         {returned pointing to our data area in entry}
    if                                 {used but undefined symbol ?}
        (sst_symflag_used_k in data_p^.sym_p^.flags) and {symbol used ?}
        (not (sst_symflag_def_k in data_p^.sym_p^.flags)) and {not defined here?}
        (not (sst_symflag_extern_k in data_p^.sym_p^.flags)) {not external ?}
        then begin
      if not undefined_syms then begin {this is first undefined symbol ?}
        sys_message ('sst_syn_read', 'symbols_undefined_show');
        undefined_syms := true;        {flag that undefined symbols were encountered}
        end;
      writeln ('  ', name_p^.str:name_p^.len); {write name of undefined symbol}
      end;
    string_hash_pos_next (hpos, found); {advance to next hash table entry}
    end;                               {back and process this new hash table entry}

  string_hash_delete (table_sym);      {delete table for this SYN file symbols}

  if undefined_syms then begin         {some undefined symbols were found ?}
    sys_message_bomb ('sst_syn_read', 'symbols_undefined_abort', nil, 0);
    end;
  end;
