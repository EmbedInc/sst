{   Subroutine SST_W_C_DOIT (GNAM, STAT)
*
*   Do the backend phase.  This routine is specific to the C backend.
*   GNAM is the full treename of the output file.  All the file suffix
*   stuff has already been handled.
}
module sst_w_c_doit;
define sst_w_c_doit;
%include 'sst_w_c.ins.pas';

procedure sst_w_c_doit (               {do the whole back end phase}
  in      gnam: univ string_var_arg_t; {raw output file name}
  out     stat: sys_err_t);            {completion status code}

var
  conn: file_conn_t;                   {connection handle to output file}
  stack_loc: util_stack_loc_handle_t;  {stack location handle}
  univ_p: univ_ptr;                    {used for resetting stack to empty}
  include: boolean;                    {TRUE if writing include file}
{
********************************************************************
*
*   Local subroutine WRITE_STRING (S, STAT)
*
*   Write the Pascal string S to the output file.  S may be NULL terminated.
}
procedure write_string (               {write Pascal/C string to output file}
  in      s: string;                   {string to write, may be NULL terminated}
  out     stat: sys_err_t);            {completion status code}
  val_param;

var
  vstr: string_var80_t;                {var string version of S}

begin
  vstr.max := sizeof(vstr.str);        {init local var string}

  string_vstring (vstr, s, sizeof(s)); {make var string version of input string}
  file_write_text (vstr, conn, stat);  {write string to file}
  end;
{
********************************************************************
*
*   Start of main routine.
}
begin
  include := sst_ins or sst_writeall;  {TRUE if writing an include file}
{
*   Set up output file.
}
  file_open_write_text (               {open the output file for write}
    gnam,                              {output file tree name}
    '',                                {mandatory file name suffix}
    conn,                              {returned connection handle}
    stat);
  if sys_error(stat) then return;
{
*   Make a pass over all the data, and rearrange it if neccessary to conform
*   to C language requirements.
}
  sst_w_c_rearrange;                   {rearrange internal data structures for C}
{
*   Reset the state after the rearrange phase.  Most of this should not be
*   neccessary, but is done here to be defensive.
}
  sst_scope_p := sst_scope_root_p;     {reset the current scope to the root scope}
  sst_names_p := sst_scope_p;
  sst_opc_p := sst_opc_first_p;        {reset the current opcode to first in list}
  util_stack_loc_start (sst_stack, stack_loc, univ_p); {reset the stack to empty}
  util_stack_popto (sst_stack, univ_p);
{
*   Init the state for writing the C source code into memory.
}
  util_stack_push (                    {create stack frame for starting scope}
    sst_stack, sizeof(frame_scope_p^), frame_scope_p);

  frame_scope_p^.prev_p := nil;        {init stack frame for base scope}
  frame_scope_p^.pos_decll := sst_out.dyn_p^;
  frame_scope_p^.pos_exec := sst_out.dyn_p^;
  frame_scope_p^.scope_p := sst_scope_p;
  frame_scope_p^.funcval_sym_p := nil;
  frame_scope_p^.const_p := nil;
  frame_scope_p^.scope_type := scope_type_global_k;
  frame_scope_p^.sment_type := sment_type_declg_k;

  frame_sment_p := nil;                {not currently in an executable statement}

  pos_declg := sst_out.dyn_p^;         {init position for global declarations}
  sst_out.dyn_p := addr(pos_declg);    {init to use position for global declarations}
{
*   Write the output source code into the in-memory list of output lines.
}
  sst_w_c_symbols (false);             {declare any symbols used from root scope}
  sst_w_c_opcodes (sst_opc_first_p);   {process opcodes and write output lines}

  if include then begin                {writing include file ?}
    write_string ('#ifdef __cplusplus', stat); if sys_error(stat) then return;
    write_string ('extern "C" {', stat); if sys_error(stat) then return;
    write_string ('#endif', stat); if sys_error(stat) then return;
    write_string ('', stat); if sys_error(stat) then return;
    end;
{
*   Write the in-memory list of source lines to the output file.
}
  sst_w.write^ (conn, stat);           {write in-memory output lines to output file}

  if include then begin                {writing include file ?}
    write_string ('#ifdef __cplusplus', stat); if sys_error(stat) then return;
    write_string ('}', stat); if sys_error(stat) then return;
    write_string ('#endif', stat); if sys_error(stat) then return;
    end;

  file_close (conn);                   {truncate and close output file}
  end;
