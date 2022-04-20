{   Interface to the SYN symbol table.  This table holds the symbols that are
*   defined in the syntax definition file being read.
}
module sst_r_syn_sym;
define sst_r_syn_sym_init;
define sst_r_syn_sym_delete;
define sst_r_syn_sym_add;
define sst_r_syn_sym_lookup;
define sst_r_syn_sym_called;
define sst_r_syn_sym_loop_init;
define sst_r_syn_sym_loop_next;
%include 'sst_r_syn.ins.pas';

var
  pos: string_hash_pos_t;              {table position when looping over entries}
  notend: boolean;                     {not yet hit end of table when looping}
{
********************************************************************************
*
*   Subroutine SST_R_SYN_SYM_INIT
*
*   Initialize the symbol table.  The memory for the symbol table will be
*   subordinate to the memory for the root SST scope.
}
procedure sst_r_syn_sym_init;          {create and initialize the SYN symbol table}
  val_param;

begin
  string_hash_create (                 {create hash table for SYN file symbols}
    table_sym,                         {hash table to initialize}
    sst_r_syn_nbuck_k,                 {number of buckets in hash table}
    syn_name_maxlen_k,                 {max length of any entry name}
    sizeof(symbol_data_t),             {amount of user data per entry}
    [string_hashcre_nodel_k],          {won't need to deallocate individual entries}
    sst_scope_root_p^.mem_p^);         {parent memory context for hash table}
  end;
{
********************************************************************************
*
*   Subroutine SST_R_SYN_SYM_DELETE
*
*   Delete the symbol table and release its resources.
}
procedure sst_r_syn_sym_delete;        {delete the symbol table, dealloc resources}
  val_param;

begin
  string_hash_delete (table_sym);
  end;
{
********************************************************************************
*
*   Subroutine SST_R_SYN_SYM_ADD (NAME, DATA_P)
*
*   Add the symbol of name NAME to the symbol table.  DATA_P is returned
*   pointing to the data in the symbol table for the new symbol.
*
*   The program is bombed with an appropriate error message if NAME is already
*   in the symbol table.
}
procedure sst_r_syn_sym_add (          {add new symbol to table, bomb if already exist}
  in      name: univ string_var_arg_t; {name of symbol to add, case-insensitive}
  out     data_p: symbol_data_p_t);    {pointer to data for new symbol in the table}
  val_param;

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  uname: string_var80_t;               {upper case symbol name}
  pos: string_hash_pos_t;              {position into symbol table}
  found: boolean;                      {name found in symbol table}
  name_p: string_var_p_t;              {pointer to symbol name in table}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

begin
  uname.max := size_char(uname.str);   {init local var string}

  string_copy (name, uname);           {make upper case symbol name in UNAME}
  string_upcase (uname);

  string_hash_pos_lookup (             {get position handle for this name}
    table_sym,                         {the table to find position in}
    uname,                             {name to get position for}
    pos,                               {returned position for this name}
    found);                            {TRUE iff name already exists}
  if found then begin                  {trying to create duplicate symbol ?}
    sys_msg_parm_vstr (msg_parm[1], uname);
    syn_msg_pos_bomb (syn_p^, 'sst_syn_read', 'symbol_already_used', msg_parm, 1);
    end;

  string_hash_ent_add (pos, name_p, data_p); {create new entry, get pointer to data}

  data_p^.name_p := name_p;            {point to table entry name string}
  data_p^.sym_p := nil;                {no SST symbol created for this name yet}
  data_p^.call_p := nil;               {init to this symbol doesn't call any others}
  end;
{
********************************************************************************
*
*   Subroutine SST_R_SYN_SYM_LOOKUP (NAME, DATA_P)
*
*   Look up the name NAME in the SYN symbol table.  DATA_P is returned pointing
*   to the data for the symbol in the symbol table.
*
*   The program is bombed with an appropriate error message if NAME is not in
*   the symbol table.
}
procedure sst_r_syn_sym_lookup (       {look up name in table, bomb if not found}
  in      name: univ string_var_arg_t; {name of symbol to look up, case-insensitive}
  out     data_p: symbol_data_p_t);    {pointer to symbol data}
  val_param;

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  uname: string_var80_t;               {upper case symbol name}
  name_p: string_var_p_t;              {pointer to name string in symbol table}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

begin
  uname.max := size_char(uname.str);   {init local var string}

  string_copy (name, uname);           {make upper case symbol name in UNAME}
  string_upcase (uname);

  string_hash_ent_lookup (             {look up name in the symbol table}
    table_sym,                         {the symbol table to look in}
    uname,                             {name to look up}
    name_p,                            {returned pointer to name string in table}
    data_p);                           {returned pointer to data for this entry}

  if data_p = nil then begin           {no such entry in the symbol table ?}
    sys_msg_parm_vstr (msg_parm[1], uname);
    syn_msg_pos_bomb (syn_p^, 'sst_syn_read', 'symbol_not_declared', msg_parm, 1);
    end;
  end;
{
********************************************************************************
*
*   Subroutine SST_R_SYN_SYM_CALLED (NAME)
*
*   Indicate that the syntax construction NAME is called from the current syntax
*   parsing function being built.  The named symbol is added to the list of
*   syntaxes called by the current syntax, if not already in the list.
}
procedure sst_r_syn_sym_called (       {symbol is called from curr syn parsing function}
  in      name: univ string_var_arg_t); {name of called symbol, case-insensitive}
  val_param;

var
  data_p: symbol_data_p_t;             {pointer to data for the called symbol}
  call_p: call_p_t;                    {points to current called chain entry}

begin
  sst_r_syn_sym_lookup (name, data_p); {get pointer to data for the called symbol}

  call_p := def_syn_p^.call_p;         {init to first entry in caller's list}
  while call_p <> nil do begin         {scan the list of called syntaxes}
    if string_equal (                  {called routine is already in the list ?}
        call_p^.data_p^.name_p^, data_p^.name_p^)
      then return;
    call_p := call_p^.next_p;          {advance to next called syntax list entry}
    end;                               {back to check this new entry}
{
*   The syntax NAME is not already in the list of syntaxes called by the current
*   syntax.
}
  string_hash_mem_alloc_ndel (         {allocate memory for new called list entry}
    table_sym, sizeof(call_p^), call_p);
  call_p^.data_p := data_p;            {point to data of called syntax}

  call_p^.next_p := def_syn_p^.call_p; {link new entry to start of list}
  def_syn_p^.call_p := call_p;
  end;
{
********************************************************************************
*
*   Subroutine SST_R_SYN_SYM_LOOP_INIT
*
*   Initialize for looping over all symbol table entries.  Only one thread can
*   use this looping feature at a time.
}
procedure sst_r_syn_sym_loop_init;     {init loop over sym table entries, one thread only}
  val_param;

begin
  string_hash_pos_first (table_sym, pos, notend);
  end;
{
********************************************************************************
*
*   Function SST_R_SYN_SYM_LOOP_NEXT (DATA_P)
*
*   Get the next table entry in the current loop.  Looping must have been
*   previously initialized with SST_R_SYN_SYM_LOOP_INIT.
*
*   When there is a next entry, DATA_P is returned pointing to the data for that
*   entry, and the function returns TRUE.  When the end of table is reached,
*   DATA_P is returned NIL and the function returns FALSE.
*
*   Only one thread can use this looping mechanism at a time.
}
function sst_r_syn_sym_loop_next (     {get next symbol table entry, one thread only}
  out     data_p: symbol_data_p_t)     {pointer to next entry, NIL at end}
  :boolean;                            {returning with entry, not hit end of table}
  val_param;

var
  name_p: string_var_p_t;              {pointer to entry name string in the table}

begin
  if not notend then begin             {at end of table ?}
    data_p := nil;
    sst_r_syn_sym_loop_next := false;
    return;
    end;

  string_hash_ent_atpos (pos, name_p, data_p); {get info about this entry}
  sst_r_syn_sym_loop_next := true;     {indicate returning with an entry}

  string_hash_pos_next (pos, notend);  {advance saved state to next entry}
  end;
