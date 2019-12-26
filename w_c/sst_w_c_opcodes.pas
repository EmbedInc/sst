{   Subroutine SST_W_C_OPCODES (FIRST_P)
*
*   Read a chain of opcodes and produce the output source code from them.
*   FIRST_P points to the first opcode in the chain.  FIRST_P may be NIL,
*   which indicates an empty chain.
}
module sst_w_c_OPCODES;
define sst_w_c_opcodes;
%include 'sst_w_c.ins.pas';

var
  arg1_p, arg2_p, arg3_p, arg4_p:      {pointers to args of main program routine}
    sst_symbol_p_t;

procedure sst_w_c_opcodes (            {read opcode chain and write output lines}
  in      first_p: sst_opc_p_t);       {points to first opcode in chain, may be NIL}

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

type
  apptype_k_t = (                      {application type}
    apptype_cmline_k,                  {command line application}
    apptype_wingui_k);                 {Microsoft Windows GUI application}

var
  opc_p: sst_opc_p_t;                  {pointer to current opcode descriptor}
  opc2: sst_opc_t;                     {scratch local opcode descriptor}
  dyn_old_p: sst_out_dyn_p_t;          {saved copy of SST_OUT.DYN_P}
  sym_p: sst_symbol_p_t;               {scratch pointer to symbol descriptor}
  sment_type_old: sment_type_k_t;      {saved copy of current statement type}
  name: string_var80_t;                {for printing symbol names}
  apptype: apptype_k_t;                {application type ID}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

label
  done_opcode;
{
*************************************************************************
*
*   Local subroutine CONST_UNDEF
*
*   Write #UNDEF compiler directives for all the constants declared at the
*   beginning with #DEFINE directives.
}
procedure const_undef;

var
  pos: string_hash_pos_t;              {position handle into hash table}
  name_p: univ_ptr;                    {unused subroutine argument}
  sym_p: sst_symbol_p_t;               {pointer to to current symbol to write out}
  sym_pp: sst_symbol_pp_t;             {pointer to hash table user data area}
  found: boolean;                      {TRUE if hash table entry was found}
  first: boolean;                      {TRUE for first UNDEF directive written}

label
  next_sym;

begin
  string_hash_pos_first (sst_scope_p^.hash_h, pos, found); {get pos for first sym}
  first := true;                       {next UNDEF directive will be the first}
  while found do begin                 {once for each hash table entry}
    string_hash_ent_atpos (pos, name_p, sym_pp); {get pointer to user data area}
    sym_p := sym_pp^;                  {get pointer to symbol descriptor}
    if
        (sst_symflag_used_k in sym_p^.flags) and {this symbol was used ?}
        (sym_p^.symtype = sst_symtype_const_k) {this symbol is a constant ?}
        then begin
      if                               {string constant in IBM AIX OS ?}
          (sst_config.os = sys_os_aix_k) and {IBM AIX operating system ?}
          (sym_p^.const_exp_p^.dtype_p^.dtype = sst_dtype_array_k) and {ARRAY ?}
          sym_p^.const_exp_p^.dtype_p^.ar_string {array is a string of characters ?}
        then goto next_sym;            {these weren't written as string consts}
      if first then begin              {is this the first UNDEF here ?}
        sst_w.blank_line^;             {leave blank line before first UNDEF}
        first := false;                {next UNDEF won't be the first anymore}
        end;
      sst_w.tab_indent^;
      sst_w.appendn^ ('#undef ', 7);
      sst_w.append^ (sym_p^.name_out_p^);
      sst_w.line_close^;
      end;

next_sym:                              {jump here to advance to next symbol in list}
    string_hash_pos_next (pos, found); {advance to next hash table entry}
    end;                               {back and do this new hash table entry}
  end;
{
*************************************************************************
*
*   Local subroutine MAKE_ARG_SYM (NAME, DTYPE, SYM_P)
*
*   Create a new call argument symbol in the current scope.  NAME will be
*   the symbol's input name, and DTYPE is the descriptor for its data type.
*   SYM_P will be returned pointing to the new symbol descriptor.
}
procedure make_arg_sym (               {make new call argument symbol}
  in      name: string;                {symbol's input name}
  in      dtype: sst_dtype_t;          {descriptor for new symbol's data type}
  out     sym_p: sst_symbol_p_t);      {returned pointer to the new symbol}

var
  vname: string_var80_t;               {var string symbol name}

begin
  vname.max := sizeof(vname.str);      {init local var string}

  string_vstring (vname, name, sizeof(name)); {make var string symbol input name}
  sst_mem_alloc_scope (sizeof(sym_p^), sym_p); {allocate new symbol descriptor}

  with sym_p^: sym do begin            {SYM is abbrev for newly allocated sym desc}
    sst_mem_alloc_scope (              {allocate space for input name}
      string_size(vname.len), sym.name_in_p);
    sym.name_in_p^.max := vname.len;   {init input name string for this symbol}
    string_copy (vname, sym.name_in_p^); {set symbol's input name}
    sym.name_out_p := nil;             {fill in rest of symbol descriptor}
    sym.next_p := nil;
    sym.char_h.crange_p := nil;
    sym.char_h.ofs := 0;
    sym.scope_p := sst_scope_p;
    sym.symtype := sst_symtype_var_k;
    sym.flags := [
      sst_symflag_def_k,
      sst_symflag_used_k,
      sst_symflag_written_k,
      sst_symflag_created_k];
    sym.var_dtype_p := addr(dtype);
    sym.var_val_p := nil;
    sym.var_arg_p := nil;
    sym.var_proc_p := nil;
    sym.var_com_p := nil;
    sym.var_next_p := nil;
    sst_w.name_sym^ (sym);             {set output name for this symbol}
    end;                               {done with SYM abbreviation}
  end;
{
*************************************************************************
*
*   Start of main routine.
}
begin
  name.max := sizeof(name.str);        {init local var string}

  apptype := apptype_cmline_k;         {init to this is normal command line app}
  if                                   {check for Win32 GUI application}
      (sst_config.os = sys_os_win32_k) and {target OS is Win32 ?}
      sst_gui                          {GUI, not command line application ?}
    then apptype := apptype_wingui_k;

  opc_p := first_p;                    {init current opcode to first opcode in chain}
  while opc_p <> nil do begin          {keep looping until end of opcode chain}
    with opc_p^: opc do begin          {OPC is current opcode descriptor}
      case opc.opcode of               {what kind of opcode descriptor is this ?}
{
*   Start of a module containing executable units.
}
sst_opc_module_k: begin
  sst_w.undent_all^;
  sst_w_c_scope_push (                 {set module's scope as current}
    opc.module_sym_p^.module_scope_p^, scope_type_module_k);
  sst_w_c_opcodes (opc.module_p);      {process opcodes for this module}
  sst_w_c_scope_pop;                   {restore previous scope}
  end;
{
*   Start of a top level program.
}
sst_opc_prog_k: begin
  sst_w_c_scope_push (                 {set program's scope as current}
    opc.prog_sym_p^.prog_scope_p^, scope_type_prog_k);
{
*   Top level programs are really subroutines that are passed special standard
*   arguments dictated by the system.  The variable APPTYPE uniquely indentifies
*   all the different combinations of system interface and implicit
*   initialization we must perform.
}
  case apptype of                      {which interface/init situation is this ?}
{
****************
*
*   Application type is a Unix-style command line interface program.
*
*   System interface:
*
*     int main (
*       int argc,                      (number of command line arguments)
*       char * argv)                   (list of pointers to command line args)
*
*   Implicit initialization:
*
*     string_cmline_set (argc, argv, <program name>)
*
*   First declare the STRING_CMLINE_SET external subroutine.
}
apptype_cmline_k: begin                {app is Unix command line program}
  sst_w.blank_line^;
  sst_w.indent^;
  sst_w.appends^ ('extern void string_cmline_set (');
  sst_w.line_new^;
  sst_w.tab_indent^;
  sst_w.appendn^ ('int,', 4);
  sst_w.line_new^;
  sst_w.tab_indent^;
  sst_w.appendn^ ('char *,', 7);
  sst_w.line_new^;
  sst_w.tab_indent^;
  sst_w.appendn^ ('char *);', 8);
  sst_w.line_close^;
  sst_w.undent^;
{
*   Create the symbol descriptors for the main routine's arguments.
}
  make_arg_sym (
    'argc',                            {argument name}
    sst_config.int_machine_p^,         {data type}
    arg1_p);                           {returned pointer to ARGC}

  make_arg_sym (
    'argv',                            {argument name}
    sst_dtype_uptr_p^,                 {data type}
    arg2_p);                           {returned pointer to ARGV}
{
*   Write header for program and declare routine MAIN.
}
  sym_p := opc.prog_sym_p;             {get pointer to program name symbol}
  sst_w.blank_line^;
  sst_w.appends^ ('/*****************************');
  sst_w.line_close^;
  sst_w.appendn^ ('**', 2);
  sst_w.line_close^;
  sst_w.appendn^ ('**   Start of program ', 22);
  sst_w.name_sym^ (sym_p^);            {make sure routine has a name}
  string_copy (sym_p^.name_out_p^, name); {make local copy of name}
  string_upcase (name);
  sst_w.append^ (name);
  sst_w.appendn^ ('.', 1);
  sst_w.line_close^;
  sst_w.appendn^ ('*/', 2);
  sst_w.line_close^;

  frame_scope_p^.sment_type := sment_type_decll_k; {declarations are now local}
  frame_scope_p^.pos_decll := sst_out.dyn_p^; {start local declarations here}
  sst_out.dyn_p := addr(frame_scope_p^.pos_decll); {use local declarations position}

  sst_w.line_new^;                     {leave blank line before declaration}
  sst_w.indent^;
  sst_w.indent^;
  if sst_config.os = sys_os_win32_k then begin {writing for Win32 API ?}
    sst_w.appends^ ('__declspec(dllexport)'(0)); {export all global routines}
    sst_w.delimit^;
    end;
  sst_w.appendn^ ('int main (', 10);
  sst_w.line_new^;
  sst_w.tab_indent^;
  sst_w.appendn^ ('int', 3);
  sst_w.delimit^;
  sst_w.append_sym_name^ (arg1_p^);
  sst_w.appendn^ (',', 1);
  sst_w.line_new^;
  sst_w.tab_indent^;
  sst_w.appendn^ ('char *', 6);
  sst_w.delimit^;
  sst_w.append_sym_name^ (arg2_p^);
  sst_w.appendn^ (') {', 3);
  sst_w.undent^;
  sst_w.line_close^;

  sst_w_c_symbols (false);             {declare local symbols for the program}
  sst_w_c_opcodes (opc.prog_p);        {process opcodes for this program}
  const_undef;                         {UNDEF the constants defined earlier}
  sst_w_c_scope_pop;                   {restore previous scope}
  sst_w.tab_indent^;
  sst_w.appendn^ ('}', 1);
  sst_w.line_close^;
  sst_w.undent^;
  end;                                 {end of Unix command line app case}
{
****************
*
*   Application type is Microsoft Windows GUI.
*
*   System interface:
*
*     int WinMain (
*       int arg_instance_h,            (handle to this instance of this app)
*       int arg_previous_h,            (handle to previous instance of this app)
*       char * arg_cmdline,            (NULL-term raw command line string)
*       int arg_show_state)            (requested initial window show state)
*
*     Note that ARG_INSTANCE_H and ARG_PREVIOUS_H are really supposed to be
*     unsigned integers.  We use signed integers here because unsigned
*     integers aren't a basic data type.
*
*   Implicit initialization:
*
*     sys_sys_init_wingui (arg_instance_h, arg_previous_h, arg_show_state,
*       <program name>)
*
*   Declare the SYS_SYS_INIT_WINGUI external subroutine.
}
apptype_wingui_k: begin
  sst_w.blank_line^;
  sst_w.indent^;
  sst_w.appends^ ('extern void sys_sys_init_wingui (');
  sst_w.line_new^;
  sst_w.tab_indent^;
  sst_w.appends^ ('int,'(0));
  sst_w.line_new^;
  sst_w.tab_indent^;
  sst_w.appends^ ('int,'(0));
  sst_w.line_new^;
  sst_w.tab_indent^;
  sst_w.appends^ ('int,'(0));
  sst_w.line_new^;
  sst_w.tab_indent^;
  sst_w.appends^ ('char *);'(0));
  sst_w.line_close^;
  sst_w.undent^;
{
*   Create the symbol descriptors for the main routine's arguments.
}
  make_arg_sym (
    'arg_instance_h',                  {argument name}
    sst_config.int_machine_p^,         {data type}
    arg1_p);                           {returned pointer to argument symbol}

  make_arg_sym (
    'arg_previous_h',                  {argument name}
    sst_config.int_machine_p^,         {data type}
    arg2_p);                           {returned pointer to argument symbol}

  make_arg_sym (
    'arg_cmdline',                     {argument name}
    sst_dtype_uptr_p^,                 {data type}
    arg3_p);                           {returned pointer to argument symbol}

  make_arg_sym (
    'arg_show_state',                  {argument name}
    sst_config.int_machine_p^,         {data type}
    arg4_p);                           {returned pointer to argument symbol}
{
*   Write header for program and declare routine WinMain.
}
  sym_p := opc.prog_sym_p;             {get pointer to program name symbol}
  sst_w.blank_line^;
  sst_w.appends^ ('/*****************************');
  sst_w.line_close^;
  sst_w.appendn^ ('**', 2);
  sst_w.line_close^;
  sst_w.appendn^ ('**   Start of program ', 22);
  sst_w.name_sym^ (sym_p^);            {make sure program has an output name}
  string_copy (sym_p^.name_out_p^, name); {make local copy of program name}
  string_upcase (name);
  sst_w.append^ (name);
  sst_w.appendn^ ('.', 1);
  sst_w.line_close^;
  sst_w.appendn^ ('*/', 2);
  sst_w.line_close^;

  frame_scope_p^.sment_type := sment_type_decll_k; {declarations are now local}
  frame_scope_p^.pos_decll := sst_out.dyn_p^; {start local declarations here}
  sst_out.dyn_p := addr(frame_scope_p^.pos_decll); {use local declarations position}

  sst_w.line_new^;                     {leave blank line before declaration}
  sst_w.indent^;
  sst_w.indent^;
  sst_w.appends^ ('__declspec(dllexport)'(0)); {export all global routines}
  sst_w.delimit^;
  sst_w.appends^ ('int WinMain ('(0));
  sst_w.line_new^;

  sst_w.tab_indent^;
  sst_w.appendn^ ('int', 3);
  sst_w.delimit^;
  sst_w.append_sym_name^ (arg1_p^);
  sst_w.appendn^ (',', 1);
  sst_w.line_new^;

  sst_w.tab_indent^;
  sst_w.appendn^ ('int', 3);
  sst_w.delimit^;
  sst_w.append_sym_name^ (arg2_p^);
  sst_w.appendn^ (',', 1);
  sst_w.line_new^;

  sst_w.tab_indent^;
  sst_w.appendn^ ('char *', 6);
  sst_w.delimit^;
  sst_w.append_sym_name^ (arg3_p^);
  sst_w.appendn^ (',', 1);
  sst_w.line_new^;

  sst_w.tab_indent^;
  sst_w.appendn^ ('int', 3);
  sst_w.delimit^;
  sst_w.append_sym_name^ (arg4_p^);
  sst_w.appendn^ (') {', 3);
  sst_w.undent^;
  sst_w.line_close^;

  sst_w_c_symbols (false);             {declare local symbols for the program}
  sst_w_c_opcodes (opc.prog_p);        {process opcodes for this program}
  const_undef;                         {UNDEF the constants defined earlier}
  sst_w_c_scope_pop;                   {restore previous scope}
  sst_w.tab_indent^;
  sst_w.appendn^ ('}', 1);
  sst_w.line_close^;
  sst_w.undent^;
  end;                                 {end of Windows GUI application type}
{
****************
}
    end;                               {end of application type cases}
  end;                                 {end of opcode is for top level program}
{
*   Opcode is a routine.
}
sst_opc_rout_k: begin
  if not (sst_symflag_used_k in opc.rout_sym_p^.flags) {this routine never used ?}
    then goto done_opcode;             {don't write an unused routine}
  opc.rout_sym_p^.flags := opc.rout_sym_p^.flags + {will define routine here now}
    [sst_symflag_defnow_k];

  sym_p := opc.rout_sym_p;             {get pointer to routine symbol descriptor}
  sst_w.blank_line^;
  sst_w.appends^ ('/*****************************');
  sst_w.line_close^;
  sst_w.appendn^ ('**', 2);
  sst_w.line_close^;
  sst_w.appendn^ ('**   Start of ', 14);
  if sst_symflag_global_k in sym_p^.flags
    then sst_w.appendn^ ('global', 6)
    else sst_w.appendn^ ('local', 5);
  sst_w.appendn^ (' routine ', 9);
  sst_w.name_sym^ (sym_p^);            {make sure routine has a name}
  string_copy (sym_p^.name_out_p^, name); {make local copy of name}
  string_upcase (name);
  sst_w.append^ (name);
  sst_w.appendn^ ('.', 1);
  sst_w.line_close^;
  sst_w.appendn^ ('*/', 2);
  sst_w.line_close^;

  dyn_old_p := sst_out.dyn_p;          {save pointer to output state block}
  sment_type_old := frame_scope_p^.sment_type; {save statement type before routine}
  frame_scope_p^.sment_type := sment_type_decll_k; {declarations are now local}
  frame_scope_p^.pos_decll := sst_out.dyn_p^; {start local declarations here}
  sst_out.dyn_p := addr(frame_scope_p^.pos_decll); {use local declarations position}

  sst_w_c_symbol (opc.rout_sym_p^);    {write routine header}
  sst_w_c_scope_push (                 {set routine's scope as current}
    opc.rout_sym_p^.proc_scope_p^, scope_type_rout_k);
  sst_w_c_opcodes (opc.rout_p);        {process opcodes for this routine}
  const_undef;                         {UNDEF the constants defined earlier}
  sst_w_c_scope_pop;                   {restore previous scope}

  dyn_old_p^ := sst_out.dyn_p^;        {update old state block with current state}
  sst_out.dyn_p := dyn_old_p;          {restore old state block as current}
  frame_scope_p^.sment_type := sment_type_old; {restore old statement type}

  sst_w.tab_indent^;
  sst_w.appendn^ ('}', 1);
  sst_w.line_close^;
  sst_w.undent^;
  end;
{
*   Chain of opcodes representing executable code.
}
sst_opc_exec_k: begin
  path_to_here := true;                {init to path exists to this opcode}
  frame_scope_p^.pos_exec := sst_out.dyn_p^; {init exec out state to curr out state}
  frame_scope_p^.sment_type := sment_type_exec_k; {now writing executable code}
  sst_out.dyn_p := addr(frame_scope_p^.pos_exec); {use exec out state}
  sst_w.tab_indent^;
  sst_w.appendn^ ('/*', 2);
  sst_w.line_close^;
  sst_w.tab_indent^;
  sst_w.appendn^ ('**   Executable code for ', 25);
  case frame_scope_p^.scope_type of    {what kind of structure owns this scope ?}
scope_type_prog_k: sst_w.appendn^ ('program', 7);
scope_type_rout_k: sst_w.appendn^ ('routine', 7);
otherwise
    sys_msg_parm_int (msg_parm[1], ord(frame_scope_p^.scope_type));
    sys_message_bomb ('sst_c_write', 'owner_exec_bad', msg_parm, 1);
    end;
  sst_w.appendn^ (' ', 1);
  string_copy (sst_scope_p^.symbol_p^.name_out_p^, name);
  string_upcase (name);
  sst_w.append^ (name);
  sst_w.appendn^ ('.', 1);
  sst_w.line_close^;
  sst_w.tab_indent^;
  sst_w.appendn^ ('*/', 2);
  sst_w.line_close^;
{
*   Write implicit initialization at the beginning of the start of the
*   program.  The program routine's call argument symbol pointers are
*   ARG1_P to ARG4_P.  These may not all be used, depending on the system
*   interface to the top level program routine.  APPTYPE id the unique
*   ID for this combination of system interface and implicit intialization.
}
  if frame_scope_p^.scope_type = scope_type_prog_k then begin {top level program ?}
    case apptype of                    {what kind of application type is this}
{
*   Main routine is MAIN, with implicit initialization call to STRING_CMLINE_SET.
}
apptype_cmline_k: begin
        sst_w.tab_indent^;
        sst_w.indent^;
        sst_w.appendn^ ('string_cmline_set (', 19);
        sst_w.allow_break^;
        sst_w.append_sym_name^ (arg1_p^);
        sst_w.appendn^ (',', 1);
        sst_w.delimit^;
        sst_w.append_sym_name^ (arg2_p^);
        sst_w.appendn^ (',', 1);
        sst_w.delimit^;
        sst_w.appendn^ ('"', 1);
        sst_w.append^ (sst_scope_p^.symbol_p^.name_out_p^);
        sst_w.appendn^ ('");', 3);
        sst_w.line_close^;
        sst_w.undent^;
        end;
{
*   Main routine is WinMain, with implicit intialization call to
*   SYS_SYS_INIT_WINGUI.
}
apptype_wingui_k: begin
        sst_w.tab_indent^;
        sst_w.indent^;
        sst_w.appends^ ('sys_sys_init_wingui ('(0));
        sst_w.allow_break^;
        sst_w.append_sym_name^ (arg1_p^);
        sst_w.appendn^ (',', 1);
        sst_w.delimit^;
        sst_w.append_sym_name^ (arg2_p^);
        sst_w.appendn^ (',', 1);
        sst_w.delimit^;
        sst_w.append_sym_name^ (arg4_p^);
        sst_w.appendn^ (',', 1);
        sst_w.delimit^;
        sst_w.appendn^ ('"', 1);
        sst_w.append^ (sst_scope_p^.symbol_p^.name_out_p^);
        sst_w.appendn^ ('");', 3);
        sst_w.line_close^;
        sst_w.undent^;
        end;
      end;                             {end of app type cases}
    end;                               {end of top level program start case}

  sst_w_c_exec (opc.exec_p);           {process list of executable opcodes}
{
*   We may need an explicit RETURN statement at the end of executable code.
*   SST defines routines to implicitly return if the end of executable code
*   is reached.  In C, however, functions and top level programs need to return
*   a value, which is done with a RETURN statement.  Therefore, if this is
*   a function or top level program, write out the code as if a RETURN
*   opcode were at the end of the chain.  This is only done if there is an
*   executable path to the end of the executable opcodes.
}
  if                                   {need explicit RETURN ?}
      path_to_here and                 {end of routine is implicit RETURN ?}
      ( (frame_scope_p^.scope_type = scope_type_prog_k) or {routine is top program ?}
        (frame_scope_p^.funcval_sym_p <> nil) {function return value exists ?}
        )
      then begin
    opc2.next_p := nil;                {fill in dummy RETURN opcode}
    opc2.opcode := sst_opc_return_k;
    opc2.str_h := opc.str_h;
    sst_w_c_exec (addr(opc2));         {write explicit return from function}
    end;

  frame_scope_p^.sment_type := sment_type_decll_k; {no longer in executable code}
  end;
{
*   Unrecognized or unexpected opcode.
}
otherwise
        sys_msg_parm_int (msg_parm[1], ord(opc.opcode));
        sys_message_bomb ('sst', 'opcode_unexpected', msg_parm, 1);
        end;                           {end of opcode type cases}
      end;                             {done with OPC abbreviation}

done_opcode:                           {jump here if definately done curr opcode}
    opc_p := opc_p^.next_p;            {advance to next opcode descriptor in chain}
    end;                               {back and process new opcode}
  end;
