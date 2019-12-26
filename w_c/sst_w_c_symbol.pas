{   Subroutine SST_W_C_SYMBOL (SYM)
*
*   Write the declaration for the symbol SYM, if appropriate.  If the
*   declaration of this symbol depends on other symbols, then these symbols will
*   be declared first, if not already done so.
*
*   Nothing will be done if the symbol declaration was previously written.
}
module sst_w_c_symbol;
define sst_w_c_symbol;
%include 'sst_w_c.ins.pas';

define sst_w_c;

var
  nest_level: sys_int_machine_t := 0;  {recursive nesting level}

procedure sst_w_c_symbol (             {declare symbol and others depended on}
  in out  sym: sst_symbol_t);          {symbol to declare}

const
  max_msg_parms = 2;                   {max parameters we can pass to a message}

var
  i: sys_int_machine_t;                {scratch integer}
  arg_p: sst_proc_arg_p_t;             {pointer to procedure argument descriptor}
  dt_p, dt2_p: sst_dtype_p_t;          {scratch data type descriptor pointers}
  dt: sst_dtype_t;                     {scratch data type descriptor}
  name_ele: string_var132_t;           {for making declared element name}
  scope_old_p: sst_scope_p_t;          {saved current scope pointer}
  names_old_p: sst_scope_p_t;          {saved current namespace pointer}
  sym_p: sst_symbol_p_t;               {scratch pointer to another symbol}
  sym_pp: sst_symbol_pp_t;             {pointer to hash entry user data area}
  pos: string_hash_pos_t;              {handle to output name symbol table position}
  last_init_p: sst_symbol_p_t;         {points to last comblock var with init value}
  val_p: sst_var_value_p_t;            {points to constant value descriptor}
  val: sst_var_value_t;                {scratch constant value descriptor}
  sment_decl: sment_type_k_t;          {selects global/local declaration statement}
  pop: boolean;                        {TRUE if need to pop to old state at end}
  kluge_old: boolean;                  {saved copy of NO_IBM_STR_KLUGE flag}
  ins_ignore: boolean;                 {doing INS translate, but symbol not from ins}
  msg_parm:                            {references to paramters for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

label
  var_done_init, comblock, routine_done_args, loop_com, com_done_init, leave;

begin
  if                                   {already did or working on this symbol ?}
      ( (sst_symflag_written_k in sym.flags) or
        (sst_symflag_writing_k in sym.flags) or
        (sst_symflag_intrinsic_out_k in sym.flags))
      and not (sst_symflag_defnow_k in sym.flags)
    then return;

  if sst_ins
    then begin                         {this is an include file translation}
      ins_ignore :=                    {true if not supposed to write this symbol}
        (not sst_char_from_ins(sym.char_h)) and
        (not (sst_symflag_created_k in sym.flags));
      end
    else begin                         {this is a regular non-include file translate}
      ins_ignore := false;
      end
    ;

  if                                   {var in common block not being declared now ?}
      (sym.symtype = sst_symtype_var_k) and {symbol is a variable ?}
      (sym.var_com_p <> nil) and then  {variable is in a common block ?}
      (not (sst_symflag_writing_k in sym.var_com_p^.flags)) {not doing block now ?}
      then begin
    sst_w_c_symbol (sym.var_com_p^);   {declare the whole common block}
    return;
    end;

  name_ele.max := size_char(name_ele.str); {init local var string}

  if sym.name_out_p = nil then begin   {no output name currently exists ?}
    case sym.symtype of                {check for special handling symbol types}
sst_symtype_abbrev_k: begin            {symbol is abbreviation for variable ref}
        scope_old_p := sst_scope_p;    {save old scope/namespace context}
        names_old_p := sst_names_p;
        sst_scope_p := sym.scope_p;    {temp swap to symbol's scope}
        sst_names_p := sst_scope_p;
        sst_w.name^ (                  {make output name for abbrev symbol}
          sym.name_in_p^.str, sym.name_in_p^.len, {input name and length}
          '_p', 2,                     {suffix name and length}
          sst_rename_all_k,            {make unique name in all visible scopes}
          name_ele,                    {returned output symbol name}
          pos);                        {pos handle where name goes in symbol table}
        string_hash_ent_add (          {add name to output symbol table}
          pos,                         {hash table handle where to add entry}
          sym.name_out_p,              {returned pnt to name string in hash table}
          sym_pp);                     {returned pnt to hash table data area}
        sym_pp^ := addr(sym);          {point hash table entry to symbol descriptor}
        sst_scope_p := scope_old_p;    {restore old scope}
        sst_names_p := names_old_p;
        end;                           {done handling var reference abbrev symbol}
otherwise                              {normal symbol output naming case}
      sst_w.name_sym^ (sym);           {make output name for this symbol}
      end;
    end;
  if                                   {global symbol being defined here ?}
      (sst_symflag_global_k in sym.flags) and
      (not (sst_symflag_extern_k in sym.flags))
    then sment_decl := sment_type_declg_k
    else sment_decl := sment_type_decll_k;

%debug 2; writeln ('':nest_level*2, sym.name_out_p^.str:sym.name_out_p^.len);
  nest_level := nest_level + 1;

  sym.flags := sym.flags + [sst_symflag_writing_k]; {symbol write is in progress}
  pop := false;                        {init to not pop writing state before exit}
  case sym.symtype of                  {what kind of symbol is this ?}
{
*************************************
*
*   Symbol is a constant.
}
sst_symtype_const_k: begin
  sst_w_c_decl_sym_exp (sym.const_exp_p^); {declare nested symbols in expression}
  if ins_ignore then goto leave;
  if
      (frame_scope_p^.scope_type = scope_type_prog_k) or
      (frame_scope_p^.scope_type = scope_type_module_k)
      then begin                       {at top level PROGRAM or MODULE scope ?}
    sment_decl := sment_type_declg_k;  {write declaration at global level}
    end;
  sst_w_c_header_decl (sment_decl, pop); {prepare for declaration statement}
  sst_w_c_sment_start;

  if                                   {trying to declare string const on IBM ?}
      (sst_config.os = sys_os_aix_k) and {IBM AIX operating system ?}
      (sym.const_exp_p^.dtype_p^.dtype = sst_dtype_array_k) and {dtype is ARRAY ?}
      sym.const_exp_p^.dtype_p^.ar_string {array is a string of characters ?}
      then begin
    sst_w.appendn^ ('const char', 10);
    sst_w.delimit^;
    sst_w.append_sym_name^ (sym);      {write constant's name}
    sst_w.appendn^ ('[', 1);
    string_f_int (name_ele, sym.const_exp_p^.dtype_p^.ar_ind_n);
    sst_w.append^ (name_ele);          {write size of string in characters}
    sst_w.appendn^ (']', 1);
    sst_w.delimit^;
    sst_w.appendn^ ('=', 1);
    sst_w.delimit^;
    sst_w_c_exp_const (sym.const_exp_p^, 0, nil, enclose_yes_k); {write string}
    sst_w_c_sment_end;
    goto leave;
    end;                               {done special case AIX string constants}

  sst_w.appendn^ ('#define', 7);
  sst_w.delimit^;
  sst_w.append_sym_name^ (sym);        {write constant's name}
  sst_w.delimit^;
  sst_w_c_exp_const (sym.const_exp_p^, 0, nil, enclose_yes_k); {write constant's value}
  sst_w_c_sment_end_nclose;
  sst_w.line_close^;
  end;
{
*************************************
*
*   Symbol is a data type.
}
sst_symtype_dtype_k: begin
  sst_w_c_decl_sym_dtype (sym.dtype_dtype_p^); {declare nested symbols in dtype}
  if ins_ignore then goto leave;       {don't really want to write this symbol ?}
  if
      (frame_scope_p^.scope_type = scope_type_prog_k) or
      (frame_scope_p^.scope_type = scope_type_module_k)
      then begin                       {at top level PROGRAM or MODULE scope ?}
    sment_decl := sment_type_declg_k;  {write declaration at global level}
    end;
  sst_w_c_header_decl (sment_decl, pop); {prepare for declaration statement}
  sst_w_c_sment_start;                 {start a new statement}
  sst_w.appendn^ ('typedef', 7);
  sst_w.delimit^;
  sst_w_c_dtype (sym.dtype_dtype_p^, sym.name_out_p^, false); {write dtype definition}
  sst_w_c_sment_end;                   {end this statement}
  sst_w.blank_line^;                   {skip one line after each dtype definition}
  end;                                 {end of symbol is data type case}
{
*************************************
*
*   Symbol is a field name of a record.
}
sst_symtype_field_k: begin
  sst_w_c_decl_sym_dtype (sym.field_dtype_p^); {declare data type of this field}
  end;
{
*************************************
*
*   Symbol is a variable.
}
sst_symtype_var_k: begin
  sst_w_c_decl_sym_dtype (sym.var_dtype_p^); {declare nested symbols in dtype}
  if sym.var_val_p <> nil then begin   {variable has initial value ?}
    sst_w_c_decl_sym_exp (sym.var_val_p^); {declare nested symbols in initial val}
    end;
  if ins_ignore then goto leave;

  if sym.var_com_p <> nil              {variable is in a common block}
    then goto comblock;                {common block vars are handled separately}
{
************
*
*   This variable is not in a common block.
}
  sst_w_c_header_decl (sment_decl, pop); {prepare for declaration statement}
  sst_w_c_sment_start;                 {start a new statement}
{
*   Write the storage class.  Variables declared at the module level default
*   to being globally known.  The storage class STATIC makes them local.
*   The default for variables in routines is AUTO, however we must make sure
*   that any variable in a routine that is initialized is explicitly declared
*   as STATIC.
}
  i := 0;                              {make GLOBAL/EXTERNAL flags value}
  if sst_symflag_extern_k in sym.flags
    then i := i + 1;
  if sst_symflag_global_k in sym.flags
    then i := i + 2;
  case i of                            {where is variable supposed to live ?}
0:    begin                            {variable is local only}
      if                               {need to explicitly declare as STATIC ?}
          (frame_scope_p^.sment_type = sment_type_declg_k) or {outside routine ?}
          (sst_symflag_static_k in sym.flags) {variable is in static storage ?}
          then begin
        sst_w.appendn^ ('static', 6);
        sst_w.delimit^;
        end;
      end;
1:    begin                            {externally defined, not globally known}
      sys_msg_parm_vstr (msg_parm[1], sym.name_in_p^);
      sys_message_bomb ('sst_c_write', 'extern_not_global', msg_parm, 1);
      end;
2: ;                                   {globally known and defined here}
3:    begin                            {globally known, defined elsewhere}
      sst_w.appendn^ ('extern', 6);
      sst_w.delimit^;
      end;
    end;                               {end of GLOBAL/EXTERNAL cases}
{
*   Special handling for Franklin C51 compiler for Intel 8051 and its
*   derivatives.  Static variables with intial values are really constants,
*   and must therefore be forced into CODE address space, since that's
*   the only place where any constants can come from.
}
  if
      (sst_config.os = sys_os_solaris_k) and {Franklin C51 compiler ?}
      (sst_config.int_machine_p^.size_used = 1) and
      (sym.var_val_p <> nil)           {variable has an initial value ?}
      then begin
    sst_w.delimit^;                    {make sure "variable" lives in ROM}
    sst_w.appendn^ ('code', 4);
    sst_w.delimit^;
    end;                               {end of Franklin C51 special case}

  sst_w_c_dtype_simple (               {declare the variable}
    sym.var_dtype_p^, sym.name_out_p^, false);
{
*   Handle initial value, if any.
}
  if sym.var_val_p <> nil then begin   {this variable has an initial value ?}
    sst_w.delimit^;
    sst_w.appendn^ ('=', 1);
    sst_w.delimit^;
    dt_p := sym.var_dtype_p;           {resolve symbol's base data type}
    while dt_p^.dtype = sst_dtype_copy_k do dt_p := dt_p^.copy_dtype_p;
    dt2_p := sym.var_val_p^.dtype_p;   {resolve expressions base data type}
    while dt2_p^.dtype = sst_dtype_copy_k do dt2_p := dt2_p^.copy_dtype_p;
    if                                 {variable is a string ?}
        (dt_p^.dtype = sst_dtype_array_k) and
        dt_p^.ar_string
        then begin
      val_p := addr(sym.var_val_p^.val); {make pointer to constant value descriptor}
      if val_p^.dtype = sst_dtype_char_k then begin {value is CHAR, not STRING ?}
        val.dtype := sst_dtype_array_k; {fill in local val descriptor as a string}
        val.ar_str_p := univ_ptr(addr(name_ele));
        name_ele.len := 1;
        name_ele.str[1] := val_p^.char_val;
        val_p := addr(val);            {point to converted STRING value descriptor}
        end;
      kluge_old := no_ibm_str_kluge;   {save value of kluge inhibit flag}
      no_ibm_str_kluge :=              {OK to write value as normal quoted string ?}
        val_p^.ar_str_p^.len < dt_p^.ar_ind_n;
      sst_w_c_value (val_p^, enclose_no_k); {write string initial value constant}
      no_ibm_str_kluge := kluge_old;   {restore kluge inhibit flag}
      goto var_done_init;              {done writing initial value for this variable}
      end;                             {end of variable is string special case}
    sst_w_c_exp_const (sym.var_val_p^, 0, nil, enclose_yes_k); {write init value exp}
    end;
var_done_init:
{
*  Done handling initial value, if any.
}
  sst_w_c_sment_end;                   {end this statement}
  goto leave;
{
************
*
*   This variable is a member of a common block.
}
comblock:
  sst_w.tab_indent^;
  sst_w.indent^;
  sst_w_c_dtype_simple (               {declare the variable}
    sym.var_dtype_p^, sym.name_out_p^, false);
  sst_w.appendn^ (';', 1);
  sst_w.undent^;
  sst_w.line_close^;
  end;
{
*************************************
*
*   Symbol is an abbreviation for a variable reference.
*   These symbols are declared as local variables with a data type
*   of pointer to the abbreviation expansion.
}
sst_symtype_abbrev_k: begin
  sst_w_c_symbol (                     {declare nested symbols in abbrev expansion}
    sym.abbrev_var_p^.mod1.top_sym_p^);
  if ins_ignore then goto leave;

  sst_w_c_header_decl (sment_decl, pop); {prepare for declaration statement}
  sst_w_c_sment_start;                 {start a new statement}

  dt.symbol_p := nil;                  {make dtype of pointer to abbrev expansion}
  dt.dtype := sst_dtype_pnt_k;
  dt.bits_min := sst_dtype_uptr_p^.bits_min;
  dt.align_nat := sst_dtype_uptr_p^.align_nat;
  dt.align := sst_dtype_uptr_p^.align;
  dt.size_used := sst_dtype_uptr_p^.size_used;
  dt.size_align := sst_dtype_uptr_p^.size_align;
  dt.pnt_dtype_p := sym.abbrev_var_p^.dtype_p;

  sst_w_c_dtype_simple (               {declare the abbrev pointer variable}
    dt,                                {data type descriptor}
    sym.name_out_p^,                   {name to declare with data type}
    false);                            {not part of packed record}
  sst_w_c_sment_end;                   {end variable declaration statement}
  end;
{
*************************************
*
*   Symbol is a routine name.
}
sst_symtype_proc_k: begin
{
*   Make sure all other symbols referenced by this declaration are declared first.
}
  if sym.proc.dtype_func_p <> nil then begin {routine is a function ?}
    sst_w_c_decl_sym_dtype (sym.proc.dtype_func_p^); {insure func dtype declared}
    end;
  arg_p := sym.proc.first_arg_p;       {init current argument to first arg}
  while arg_p <> nil do begin          {loop thru all the routine arguments}
    sst_w_c_decl_sym_dtype (arg_p^.dtype_p^); {insure arg data type is declared}
    arg_p := arg_p^.next_p;
    end;                               {back and do next call argument}
  if ins_ignore then goto leave;
{
*   Set up the output text formatting environment.
}
  sst_w.blank_line^;
  sst_w_c_sment_start;                 {function declaration will be a "statement"}
  if sst_symflag_defnow_k in sym.flags then begin {body will be written here ?}
    sst_w.indent^;                     {indent one more time for function body}
    end;
{
*   Write the EXTERN or STATIC keywords, when appropriate.
}
  if sst_symflag_global_k in sym.flags
    then begin                         {function is globally known}
      if sst_symflag_extern_k in sym.flags then begin {function lives externally ?}
        sst_w.appendn^ ('extern', 6);
        sst_w.delimit^;
        end;
      if sst_config.os = sys_os_win32_k then begin {writing for Win32 API ?}
        sst_w.appends^ ('__declspec(dllexport)'(0)); {export all global routines}
        sst_w.delimit^;
        end;
      end
    else begin                         {function is local to this file}
      sst_w.appendn^ ('static', 6);
      sst_w.delimit^;
      end
    ;
{
*   If the function is only being declared (not defined) here, then call
*   SST_W_C_DTYPE to do all the work.
}
  if not (sst_symflag_defnow_k in sym.flags) then begin {just declared here ?}
    sst_w_c_dtype (sym.proc_dtype_p^, sym.name_out_p^, false); {declare routine}
    sst_w_c_sment_end;
    goto leave;                        {all done declaring routine}
    end;
{
*   The routine is not just being declared here, but also being defined here.
*   that means the arguments template includes the dummy argument names.
*
*   Write the function's return value data type.  The C language only has
*   functions, so subroutines are declared with the VOID data type.
}
  scope_old_p := sst_scope_p;          {save old scope/namespace context}
  names_old_p := sst_names_p;
  sst_scope_p := sym.proc_scope_p;     {temp swap in scope of this routine}
  sst_names_p := sst_scope_p;

  if sym.proc.dtype_func_p <> nil
    then begin                         {routine is a true function (returns a value)}
      name_ele.len := 0;               {null to avoid writing symbol name}
      sst_w_c_dtype_simple (sym.proc.dtype_func_p^, name_ele, false); {declare func value}
      end
    else begin                         {routine does not return a value}
      sst_w.appendn^ ('void', 4);
      end
    ;
  sst_w.delimit^;
{
*   Write function's name and start the parameter list.
}
  if
      (sst_symflag_global_k in sym.flags) and {this is a global function ?}
      (sst_config.os = sys_os_win32_k) {writing for Win32 API ?}
      then begin
    sst_w.appends^ ('__stdcall'(0));   {specify linkage conventions}
    sst_w.delimit^;
    end;

  sst_w.append_sym_name^ (sym);
  sst_w.delimit^;
  sst_w.appendn^ ('(', 1);
{
*   Handle special case where function takes no passed parameters.
}
  if sym.proc.n_args <= 0 then begin   {function takes no arguments ?}
    sst_w.appendn^ ('void', 4);
    goto routine_done_args;
    end;
{
*   Declare the call parameters.
}
  arg_p := sym.proc.first_arg_p;       {init current argument to first argument}
  while arg_p <> nil do begin          {loop thru each call argument}
    sst_w.line_close^;                 {put each argument on its own line}
    sst_w.tab_indent^;
    sst_w.name_sym^ (arg_p^.sym_p^);   {make output name for this dummy argument}
    arg_p^.sym_p^.flags :=             {prevent dummy arg being declared later}
      arg_p^.sym_p^.flags + [sst_symflag_written_k];
    dt_p := arg_p^.dtype_p;            {get argument's base data type}
    while dt_p^.dtype = sst_dtype_copy_k
      do dt_p := dt_p^.copy_dtype_p;
    case arg_p^.pass of                {how is this argument passed ?}
sst_pass_ref_k: begin                  {argument is passed by reference}
        if dt_p^.dtype = sst_dtype_array_k
          then begin                   {argument is implicitly passed as pointer}
            name_ele.len := 0;
            end
          else begin                   {explicitly indicate arg passed as pointer}
            name_ele.str[1] := '*';    {indicate argument is a pointer}
            name_ele.len := 1;
            end
          ;
        string_append (name_ele, arg_p^.sym_p^.name_out_p^); {add on argument name}
        sst_w_c_dtype_simple (         {declare data type of this argument}
          arg_p^.dtype_p^,             {data type descriptor}
          name_ele,                    {name of symbol to declare}
          false);                      {no, this is not a field in a packed record}
        end;
sst_pass_val_k: begin                  {argument is passed by value}
        sst_w_c_dtype_simple (         {declare data type of this argument}
          arg_p^.dtype_p^,             {data type descriptor}
          arg_p^.sym_p^.name_out_p^,   {name of symbol to declare}
          false);                      {no, this is not a field in a packed record}
        end;
otherwise
      sys_msg_parm_int (msg_parm[1], ord(arg_p^.pass));
      sys_message_bomb ('sst_c_write', 'arg_pass_method_bad', msg_parm, 1);
      end;
    arg_p := arg_p^.next_p;            {advance to next argument descriptor in list}
    if arg_p <> nil then begin         {more arguments follow ?}
      sst_w.appendn^ (',', 1);
      sst_w.delimit^;
      end;
    end;                               {back and process next call argument}

routine_done_args:                     {done writing last call argument}
  sst_w.appendn^ (')', 1);             {close argument list}
{
*   Just finished writing the end of the argument list.
}
  sst_w.delimit^;
  sst_w.appendn^ ('{', 1);
  sst_w_c_sment_end_nclose;
  sst_w.line_close^;
  sst_scope_p := scope_old_p;          {restore old scope/namespace}
  sst_names_p := names_old_p;
  end;
{
*************************************
*
*   Symbol is a common block name.
*
*   Common blocks are emulated in C with globally known variables that are
*   STRUCTs, where each field in the struct is a variable in the common block.
}
sst_symtype_com_k: begin
  if sym.com_first_p = nil then goto leave; {don't write empty common blocks}

  last_init_p := nil;                  {init to no variable has an initial value}
  sst_scope_new;                       {create new scope for var names in com block}
  sym_p := sym.com_first_p;            {init current common var to first in list}
  while sym_p <> nil do begin          {once for each variable in common block}
    sym_p^.flags :=                    {prevent recursive follow of this variable}
      sym_p^.flags + [sst_symflag_writing_k];
    scope_old_p := sym_p^.scope_p;     {save pointer to var's scope}
    sym_p^.scope_p := sst_scope_p;     {temp put variable into scope for com block}
    sst_w_c_name_com_var (sym_p^);     {set output name for common block variable}
    sym_p^.scope_p := scope_old_p;     {restore variable's original scope}
    if sym_p^.var_val_p <> nil then begin {this variable has an initial value ?}
      sst_w_c_decl_sym_exp (sym_p^.var_val_p^); {declare nested symbols in init val}
      last_init_p := sym_p;            {update last variable with initial value}
      end;
    sym_p := sym_p^.var_next_p;        {point to next variable in common block}
    end;                               {back and process this new variable name}
  sst_scope_old;                       {restore current scope}
  if ins_ignore then goto leave;

  sst_w.blank_line^;
  sst_w_c_header_decl (sment_decl, pop); {prepare for declaration statement}
  sst_w_c_sment_start;                 {avoid interrupting common block declaration}
  if sst_symflag_extern_k in sym.flags then begin {common block not def here ?}
    sst_w.appendn^ ('extern', 6);
    sst_w.delimit^;
    end;
  sst_w.appendn^ ('struct', 6);
  sst_w.delimit^;
  sst_w.appendn^ ('{', 1);
  sst_w.line_close^;

  sym_p := sym.com_first_p;            {init current common var to first in list}
  while sym_p <> nil do begin          {once for each variable in common block}
    sym_p^.flags :=                    {turn off temporary recursive prevent lock}
      sym_p^.flags - [sst_symflag_writing_k];
    sst_w_c_symbol (sym_p^);           {declare this variable}
    sym_p := sym_p^.var_next_p;        {point to next variable in common block}
    end;                               {back and process this new variable name}

  sst_w.tab_indent^;
  sst_w.appendn^ ('}', 1);             {close the STRUCT definition}
  sst_w.delimit^;
  sst_w.append_sym_name^ (sym);        {name of var being declared with the STRUCT}
{
*   If any variable in the common block has an initial value, then every variable
*   in the common block, up to the last one with an initial value, must be
*   initialized.  This is because the common block is really a structure, and
*   that is how structures are initialized in C.
*
*   LAST_INIT_P is pointing to the symbol descriptor for the last variable in the
*   common block that has an initial value.  It is NIL if none of the variables
*   had initial values.
}
  if
      (last_init_p <> nil) and         {at least one variable had initial value ?}
      (not (sst_symflag_extern_k in sym.flags)) {common block being defined here ?}
      then begin
    sst_w.appendn^ (' = {', 4);
    sst_w.indent^;                     {indent extra level for initial values}
    sst_w.tab_indent^;
    sym_p := sym.com_first_p;          {init current com block var to first in list}

loop_com:                              {back here each new variable in common block}
    sst_w.line_close^;
    sst_w.tab_indent^;
    sst_w.indent^;
    if sym_p^.var_val_p = nil
      then begin                       {this variable has no initial value}
        sst_w_c_ival_unspec (          {write the "unspecified" value for this dtype}
          sym_p^.var_dtype_p^);
        end
      else begin                       {this variable has an explicit initial value}
        dt_p := sym_p^.var_dtype_p;    {resolve symbol's base data type}
        while dt_p^.dtype = sst_dtype_copy_k do dt_p := dt_p^.copy_dtype_p;
        dt2_p := sym_p^.var_val_p^.dtype_p; {resolve expressions base data type}
        while dt2_p^.dtype = sst_dtype_copy_k do dt2_p := dt2_p^.copy_dtype_p;
        if                             {variable is a string ?}
            (dt_p^.dtype = sst_dtype_array_k) and
            dt_p^.ar_string
            then begin
          val_p := addr(sym.var_val_p^.val); {make pointer to constant value descriptor}
          if val_p^.dtype = sst_dtype_char_k then begin {value is CHAR, not STRING ?}
            val.dtype := sst_dtype_array_k; {fill in local val descriptor as a string}
            val.ar_str_p := univ_ptr(addr(name_ele));
            name_ele.len := 1;
            name_ele.str[1] := val_p^.char_val;
            val_p := addr(val);        {point to converted STRING value descriptor}
            end;
          kluge_old := no_ibm_str_kluge; {save value of kluge inhibit flag}
          no_ibm_str_kluge :=          {OK to write value as normal quoted string ?}
            val_p^.ar_str_p^.len < dt_p^.ar_ind_n;
          sst_w_c_value (val_p^, enclose_no_k); {write string initial value constant}
          no_ibm_str_kluge := kluge_old; {restore kluge inhibit flag}
          goto com_done_init;          {done writing initial value for this variable}
          end;                         {end of variable is string special case}
        sst_w_c_exp (                  {write explicit initial value for this var}
          sym_p^.var_val_p^, 0, nil, enclose_no_k);
com_done_init:                         {done writing initial value for this var}
        end
      ;
    sst_w.undent^;
    if sym_p <> last_init_p then begin {this was not last variable to initialize ?}
      sst_w.appendn^ (',', 1);
      sst_w.delimit^;
      sym_p := sym_p^.var_next_p;      {advance to next common block variable}
      goto loop_com;                   {back and process this new variable}
      end;

    sst_w.line_close^;
    sst_w.tab_indent^;
    sst_w.appendn^ ('}', 1);           {close initial values list}
    sst_w.undent^;                     {undo extra indent for initial values}
    sst_w.line_close^;
    sst_w.tab_indent^;
    end;                               {all done initializing variables}
{
*   Done handling initial values, if any.
}
  sst_w_c_sment_end;                   {end the common block definition}
  sst_w.blank_line^;
  end;
{
*************************************
*
*   Symbol types that require no special processing.
}
sst_symtype_enum_k: ;
sst_symtype_label_k: ;
sst_symtype_prog_k: ;
sst_symtype_module_k: ;
otherwise
    sys_msg_parm_int (msg_parm[1], ord(sym.symtype));
    sys_message_bomb ('sst', 'symbol_type_unknown', msg_parm, 1);
    end;                               {end of symbol type cases}
{
*************************************
*
*   Done with each of the code sections that handle the symbol uniquely depending
*   on what type of symbol it is.  Now do common cleanup, and then leave.
}
leave:                                 {jump here if done declaring symbol}
  sym.flags := sym.flags +             {flag symbol as completely written}
    [sst_symflag_written_k];
  sym.flags := sym.flags -             {symbol write no longer in process}
    [sst_symflag_writing_k, sst_symflag_defnow_k, sst_symflag_writing_dt_k];

  if pop then begin                    {need to pop to previous writing state ?}
    sst_w_c_pos_pop;
    end;
  nest_level := nest_level - 1;
  end;
