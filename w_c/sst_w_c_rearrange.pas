{   Subroutine SST_W_C_REARRANGE
*
*   This subroutine makes a complete pass over all the opcodes and symbols and
*   rearranges any internal data structures as needed to conform to C language
*   restrictions or conventions.
*
*   The C language does not support nested routines.  Therefore, any variables
*   in any parent routine that are referenced by a nested routine will be passed
*   as explicit call arguments.  All other symbols from a parent routine referenced
*   by a nested routine will be moved to the top level.
}
module sst_w_c_REARRANGE;
define sst_w_c_rearrange;
%include 'sst_w_c.ins.pas';

procedure sst_w_c_rearrange;

type
  frame_p_t = ^frame_t;                {pointer to stack frame for nested proc}

  frame_t = record                     {stack frame for each nested procedure}
    prev_p: frame_p_t;                 {pointer to stack frame for parent procedure}
    scope_p: sst_scope_p_t;            {points to scope for this procedure}
    proc_p: sst_proc_p_t;              {points to procedure template}
    arg_pp: sst_proc_arg_pp_t;         {points to end of regular args chain pointer}
    end;

var
  scope_top_p: sst_scope_p_t;          {symbols considered top level when here}
  rename_num: sys_int_machine_t;       {number for making unique symbol name}
  level_nest: sys_int_machine_t;       {routine nesting level}
  frame_p: frame_p_t;                  {points to data about current nested routine}
  changes_made: boolean;               {TRUE if anything changed this pass}
  args_added: boolean;                 {TRUE if any call arguments got added anywhere}
  move_routines: boolean;              {actually move routines in this pass}

procedure check_opcodes (              {check a tree of opcodes}
  in out  opc_start_p: sst_opc_p_t);   {pointer to root opcode of tree}
  forward;

procedure check_var (                  {check a VAR descriptor for symbols to move}
  in out  v: sst_var_t);               {variable reference descriptor}
  forward;

procedure check_proc (                 {check routine descriptor for syms to move}
  in      proc: sst_proc_t);           {routine call or template descriptor}
  forward;

procedure check_call (                 {check routine call, possibly fix up}
  in out  call: sst_proc_t;            {call descriptor}
  in      temp: sst_proc_t);           {template for routine being called}
  forward;

procedure check_exp (                  {check expression for symbols to move}
  in out  exp: sst_exp_t);             {expression descriptor}
  forward;

procedure check_dtype (                {check data type for symbols to move}
  in out  dtype: sst_dtype_t);         {data type descriptor}
  forward;

procedure symbol_move (                {move symbol to a different scope}
  in out  sym: sst_symbol_t;           {descriptor of symbol to move}
  in      scope: sst_scope_t);         {descriptor of scope to move symbol to}
  forward;

procedure scope_reset (                {reset all symbol FOLLOW flags in scope}
  in      scope: sst_scope_t);         {scope to reset all symbols in}
  forward;

procedure check_sym (                  {move symbol to global scope, if necessary}
  in out  sym_p: sst_symbol_p_t);      {pointer to symbol, altered if new sym made}
  forward;

procedure add_arg (                    {add arg to routine, if necessary}
  in out  proc: sst_proc_t;            {descriptor for routine to add arg to}
  in      scope: sst_scope_t;          {scope of routine to add argument to}
  in out  sym_p: sst_symbol_p_t);      {pnts to remote var, returned dummy arg pnt}
  forward;
{
****************************
*
*   Local subroutine CHECK_OPCODES (OPC_START_P)
*
*   Check the tree of opcodes starting at FIRST_OPC_P for symbols referenced by
*   nested routines that are in a more global scope than the nested routine.
}
procedure check_opcodes (              {check a tree of opcodes}
  in out  opc_start_p: sst_opc_p_t);   {pointer to root opcode of tree}

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  opc_p: sst_opc_p_t;                  {points to current opcode}
  prev_pp: sst_opc_pp_t;               {points to chain pointer to current opcode}
  scope_old_p: sst_scope_p_t;          {saved copy of scope pointer}
  case_val_p: sst_case_val_p_t;        {points to curr descriptor in chain}
  case_opc_p: sst_case_opc_p_t;        {points to curr descriptor in chain}
  frame_new_p: frame_p_t;              {points to newly created stack frame}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

label
  next_opc, got_opc;

begin
  opc_p := opc_start_p;                {init pointer to first opcode in list}
  prev_pp := addr(opc_start_p);        {save pointer to previous chain pointer}
  while opc_p <> nil do begin          {once for each opcode in list}
    case opc_p^.opcode of              {what kind of opcode is this ?}

sst_opc_module_k: begin                {start of a grouping of routines}
  if not (sst_symflag_used_k in opc_p^.module_sym_p^.flags)
    then goto next_opc;                {this module is unused ?}
  scope_old_p := sst_scope_p;          {save current scope before module}
  sst_scope_p := opc_p^.module_sym_p^.module_scope_p; {make module scope current}
  sst_names_p := sst_scope_p;
  scope_reset (sst_scope_p^);          {reset symbols in this new scope}
  scope_top_p := sst_scope_p;          {this scope is "global enough"}
  check_opcodes (opc_p^.module_p);     {process opcodes in module}
  sst_scope_p := scope_old_p;          {restore current scope}
  sst_names_p := sst_scope_p;
  end;

sst_opc_prog_k: begin                  {start of top level program}
  if not (sst_symflag_used_k in opc_p^.prog_sym_p^.flags)
    then goto next_opc;                {this prog is unused ?}
  scope_old_p := sst_scope_p;          {save current scope before prog}
  sst_scope_p := opc_p^.prog_sym_p^.prog_scope_p; {make prog scope current}
  sst_names_p := sst_scope_p;
  scope_reset (sst_scope_p^);          {reset symbols in this new scope}
  level_nest := level_nest + 1;        {one more level within nested routine}
  check_opcodes (opc_p^.prog_p);       {process opcodes in prog}
  level_nest := level_nest - 1;        {one less level within nested routine}
  sst_scope_p := scope_old_p;          {restore current scope}
  sst_names_p := sst_scope_p;
  end;

sst_opc_rout_k: begin                  {start of a routine}
  if not (sst_symflag_used_k in opc_p^.rout_sym_p^.flags)
    then goto next_opc;                {this rout is unused ?}
  level_nest := level_nest + 1;        {one more level within nested routine}
  scope_old_p := sst_scope_p;          {save current scope before rout}
  sst_scope_p := opc_p^.rout_sym_p^.proc_scope_p; {make routine scope current}
  sst_names_p := sst_scope_p;
  scope_reset (sst_scope_p^);          {reset symbols in this new scope}
  if level_nest >= 2 then begin        {this is an internal nested routine ?}
    util_stack_push (                  {create stack frame for this routine}
      sst_stack, sizeof(frame_new_p^), frame_new_p);
    frame_new_p^.prev_p := frame_p;    {link to stack frame for parent routine}
    frame_new_p^.scope_p := sst_scope_p; {save pointer to scope for this routine}
    frame_new_p^.proc_p := addr(opc_p^.rout_sym_p^.proc); {point to routine template}
    frame_p := frame_new_p;            {make new stack frame the current frame}
    frame_p^.arg_pp := addr(frame_p^.proc_p^.first_arg_p); {init end of chain ptr}
    while                              {skip over all the "regular" call args}
        (frame_p^.arg_pp^ <> nil) and then
        (frame_p^.arg_pp^^.exp_p = nil)
        do begin
      frame_p^.arg_pp := addr(frame_p^.arg_pp^^.next_p); {skip over this call arg}
      end;
    end;
  check_opcodes (opc_p^.rout_p);       {process opcodes in rout}
  sst_scope_p := scope_old_p;          {restore current scope}
  sst_names_p := sst_scope_p;
  level_nest := level_nest - 1;        {one less level within nested routine}
  if level_nest >= 1 then begin        {this was an internal nested routine ?}
    scope_old_p := frame_p^.scope_p;   {save pointer to routine's scope}
    frame_p := frame_p^.prev_p;        {pop back to stack frame for parent routine}
    util_stack_pop (sst_stack, sizeof(frame_p^)); {remove old frame from stack}
    if move_routines then begin        {OK to move routines around on this pass ?}
      scope_old_p^.parent_p :=         {routine's scope is not child of global scope}
        scope_top_p;
      symbol_move (                    {move routine symbol to global scope}
        opc_p^.rout_sym_p^, scope_top_p^);
      prev_pp^ := opc_p^.next_p;       {unlink this opcode from chain}
      opc_p^.next_p := sst_opc_p;      {link to start of relocated opcodes chain}
      sst_opc_p := opc_p;
      opc_p := prev_pp^;               {advance current opcode to next after routine}
      goto got_opc;                    {already advanced to next opcode}
      end;
    end;
  end;

sst_opc_exec_k: begin                  {points to chain of executable code}
  if
      move_routines or                 {only looking for routines ?}
      ((not args_added) and (level_nest <= 1)) {no changes made that effect us ?}
    then goto next_opc;
  check_opcodes (opc_p^.exec_p);       {check this chain of opcodes}
  end;

sst_opc_label_k: begin                 {indicate handle for a label}
  end;

sst_opc_call_k: begin                  {subroutine call}
  if
      move_routines or                 {only looking for routines ?}
      ((not args_added) and (level_nest <= 1)) {no changes made that effect us ?}
    then goto next_opc;
  check_var (opc_p^.call_var_p^);
  check_call (opc_p^.call_proc_p^, opc_p^.call_proct_p^);
  end;

sst_opc_assign_k: begin                {assignment statement}
  if
      move_routines or                 {only looking for routines ?}
      ((not args_added) and (level_nest <= 1)) {no changes made that effect us ?}
    then goto next_opc;
  check_var (opc_p^.assign_var_p^);
  check_exp (opc_p^.assign_exp_p^);
  end;

sst_opc_goto_k: begin                  {unconditional transfer of control}
  if
      move_routines or                 {only looking for routines ?}
      ((not args_added) and (level_nest <= 1)) {no changes made that effect us ?}
    then goto next_opc;
  check_sym (opc_p^.goto_sym_p);
  end;

sst_opc_case_k: begin                  {execute one of N blocks of code}
  if
      move_routines or                 {only looking for routines ?}
      ((not args_added) and (level_nest <= 1)) {no changes made that effect us ?}
    then goto next_opc;
  check_exp (opc_p^.case_exp_p^);
  case_val_p := opc_p^.case_val_p;
  while case_val_p <> nil do begin     {loop thru all the CASE_VAL blocks}
    check_exp (case_val_p^.exp_p^);
    case_val_p := case_val_p^.next_val_p;
    end;
  case_opc_p := opc_p^.case_opc_p;
  while case_opc_p <> nil do begin     {loop thru all the CASE_OPC blocks}
    check_opcodes (case_opc_p^.code_p);
    case_opc_p := case_opc_p^.next_p;
    end;
  check_opcodes (opc_p^.case_none_p);
  end;

sst_opc_if_k: begin                    {IF ... THEN ... ELSE ... statement}
  if
      move_routines or                 {only looking for routines ?}
      ((not args_added) and (level_nest <= 1)) {no changes made that effect us ?}
    then goto next_opc;
  check_exp (opc_p^.if_exp_p^);
  check_opcodes (opc_p^.if_true_p);
  check_opcodes (opc_p^.if_false_p);
  end;

sst_opc_loop_cnt_k: begin              {counted loop (Pascal FOR, Fortran DO, etc)}
  if
      move_routines or                 {only looking for routines ?}
      ((not args_added) and (level_nest <= 1)) {no changes made that effect us ?}
    then goto next_opc;
  check_var (opc_p^.lpcn_var_p^);
  check_exp (opc_p^.lpcn_exp_start_p^);
  check_exp (opc_p^.lpcn_exp_end_p^);
  check_exp (opc_p^.lpcn_exp_inc_p^);
  check_opcodes (opc_p^.lpcn_code_p);
  end;

sst_opc_loop_ttop_k: begin             {loop with test at start of loop}
  if
      move_routines or                 {only looking for routines ?}
      ((not args_added) and (level_nest <= 1)) {no changes made that effect us ?}
    then goto next_opc;
  check_exp (opc_p^.lptp_exp_p^);
  check_opcodes (opc_p^.lptp_code_p);
  end;

sst_opc_loop_tbot_k: begin             {loop with test at end of loop}
  if
      move_routines or                 {only looking for routines ?}
      ((not args_added) and (level_nest <= 1)) {no changes made that effect us ?}
    then goto next_opc;
  check_exp (opc_p^.lpbt_exp_p^);
  check_opcodes (opc_p^.lpbt_code_p);
  end;

sst_opc_loop_next_k: begin             {go to start of next time around loop}
  end;

sst_opc_loop_exit_k: begin             {unconditionally exit loop}
  end;

sst_opc_return_k: begin                {return from subroutine}
  end;

sst_opc_abbrev_k: begin                {abbreviations in effect for block of code}
  if
      move_routines or                 {only looking for routines ?}
      ((not args_added) and (level_nest <= 1)) {no changes made that effect us ?}
    then goto next_opc;
  scope_reset (opc_p^.abbrev_scope_p^); {reset symbol flags in this scope}
  check_opcodes (opc_p^.abbrev_code_p);
  end;

sst_opc_discard_k: begin               {call function, but discard its return value}
  if
      move_routines or                 {only looking for routines ?}
      ((not args_added) and (level_nest <= 1)) {no changes made that effect us ?}
    then goto next_opc;
  check_exp (opc_p^.discard_exp_p^);
  end;

sst_opc_write_k: begin                 {write expression value to standard output}
  if
      move_routines or                 {only looking for routines ?}
      ((not args_added) and (level_nest <= 1)) {no changes made that effect us ?}
    then goto next_opc;
  check_exp (opc_p^.write_exp_p^);
  if opc_p^.write_width_exp_p <> nil then begin
    check_exp (opc_p^.write_width_exp_p^);
    end;
  if opc_p^.write_width2_exp_p <> nil then begin
    check_exp (opc_p^.write_width2_exp_p^);
    end;
  end;

sst_opc_write_eol_k: begin             {write end of line to standard output}
  end;

otherwise
      sys_msg_parm_int (msg_parm[1], ord(opc_p^.opcode));
      sys_message_bomb ('sst', 'opcode_unexpected', msg_parm, 1);
      end;
next_opc:                              {jump here to advance to next opcode}
    prev_pp := addr(opc_p^.next_p);    {update chain pointer to new opcode}
    opc_p := opc_p^.next_p;            {advance to next opcode in chain}
got_opc:                               {jump here if OPC_P and PREV_PP all set}
    end;                               {back and process this new opcode}
  end;
{
****************************
*
*   Local subroutine CHECK_VAR (V)
*
*   Check the variable descriptor V for symbols that may have to be moved to
*   a global scope.
}
procedure check_var (                  {check a VAR descriptor for symbols to move}
  in out     v: sst_var_t);            {variable reference descriptor}

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  mod_p: sst_var_mod_p_t;              {point to current modifier in var descriptor}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

begin
  if v.dtype_p <> nil then begin       {check data type, if there is one}
    check_dtype (v.dtype_p^);
    end;

  mod_p := addr(v.mod1);               {init current modifier to top modifier}
  while mod_p <> nil do begin          {once for each modifier in chain}
    case mod_p^.modtyp of              {what kind of modifier is this ?}
sst_var_modtyp_top_k: begin            {modifier is top symbol of var reference}
        check_sym (mod_p^.top_sym_p);
        end;
sst_var_modtyp_subscr_k: begin         {modifier is subsrcipt expression of array}
        check_exp (mod_p^.subscr_exp_p^);
        end;
sst_var_modtyp_unpnt_k,
sst_var_modtyp_field_k: ;
otherwise
      sys_msg_parm_int (msg_parm[1], ord(mod_p^.modtyp));
      sys_message_bomb ('sst', 'var_modifier_unknown', msg_parm, 1);
      end;
    mod_p := mod_p^.next_p;            {advance to next modifier in chain}
    end;                               {back and process this new modifier}
  end;
{
****************************
*
*   Local subroutine CHECK_PROC (PROC)
*
*   Check the routine template descriptor PROC for non-global nested symbol
*   references.
}
procedure check_proc (                 {check routine descriptor for syms to move}
  in      proc: sst_proc_t);           {routine call or template descriptor}

var
  arg_p: sst_proc_arg_p_t;             {points to current routine argument descriptor}

begin
  if proc.dtype_func_p <> nil then begin {do function return data type, if any}
    check_dtype (proc.dtype_func_p^);
    end;

  arg_p := proc.first_arg_p;           {init current call argument to first}
  while arg_p <> nil do begin          {once for each call argument}
    check_dtype (arg_p^.dtype_p^);     {do data type of this argument}
    arg_p := arg_p^.next_p;            {advance to next argument of this routine}
    end;                               {back and process next call argument}
  end;
{
****************************
*
*   Local subroutine CHECK_CALL (CALL,TEMP)
*
*   Check the routine call descriptor CALL for non-global nested symbol references
*   and make sure it is updated to include any newly added call arguments.
*   TEMP is the template for the called routine.
}
procedure check_call (                 {check routine call, possibly fix up}
  in out  call: sst_proc_t;            {call descriptor}
  in      temp: sst_proc_t);           {template for routine being called}

var
  argt_p: sst_proc_arg_p_t;            {pointer to current argument template}
  argc_p: sst_proc_arg_p_t;            {pointer to current call argument}
  argc_pp: sst_proc_arg_pp_t;          {points to end of passed arguments chain}
  exp_p: sst_exp_p_t;                  {pointer to expression for new argument}
  v_p: sst_var_p_t;                    {pointer to var descriptor for new arg}

begin
  if temp.dtype_func_p <> nil then begin {do function return data type, if any}
    check_dtype (temp.dtype_func_p^);
    end;

  argt_p := temp.first_arg_p;          {init pointers to first argument}
  argc_p := call.first_arg_p;
  argc_pp := addr(call.first_arg_p);   {init pointer to end of passed args chain}
  while argt_p <> nil do begin         {once for each argument listed in template}
    check_dtype (argt_p^.dtype_p^);    {check data type of argument template}
    if argc_p = nil then begin         {nothing is being passed for this argument ?}
      if argt_p^.exp_p = nil then begin
        sys_message_bomb ('sst', 'argt_exp_missing', nil, 0);
        end;
      sst_mem_alloc_scope (sizeof(argc_p^), argc_p); {create new argument descriptor}
      argc_pp^ := argc_p;              {link arg to end of chain}
      argc_p^.next_p := nil;           {init new argument descriptor}
      argc_p^.sym_p := nil;
      argc_p^.name_p := nil;
      argc_p^.dtype_p := argt_p^.exp_p^.dtype_p;
      argc_p^.pass := argt_p^.pass;
      argc_p^.rwflag_ext := argt_p^.rwflag_ext;
      argc_p^.rwflag_int := argt_p^.rwflag_int;
      argc_p^.univ := argt_p^.univ;
      sst_mem_alloc_scope (sizeof(exp_p^), exp_p); {create new expression descriptor}
      exp_p^ := argt_p^.exp_p^;        {make copy of template's expression}
      if
          (exp_p^.term1.next_p <> nil) or
          (exp_p^.term1.ttype <> sst_term_var_k)
          then begin
        sys_message_bomb ('sst', 'argt_exp_not_var', nil, 0);
        end;
      sst_mem_alloc_scope (sizeof(v_p^), v_p); {create new var descriptor}
      v_p^ := exp_p^.term1.var_var_p^; {make copy of template's var descriptor}
      argc_p^.exp_p := exp_p;          {link new call arg to its expression}
      exp_p^.term1.var_var_p := v_p;   {link expression to its new var descriptor}
      call.n_args := call.n_args + 1;  {log one more argument being passed here}
      end;                             {passed argument now definately exists}
    check_exp (argc_p^.exp_p^);        {check passed value expression for this arg}
    argt_p := argt_p^.next_p;          {advance to next call argument template}
    argc_pp := addr(argc_p^.next_p);   {update end of passed arguments chain}
    argc_p := argc_p^.next_p;          {advance to next passed argument}
    end;                               {back and process this new call argument}
  end;
{
****************************
*
*   Local subroutine CHECK_EXP (EXP)
*
*   Check the expression descriptor, EXP, for symbols that may need to be
*   moved to a global scope.
}
procedure check_exp (                  {check expression for symbols to move}
  in out  exp: sst_exp_t);             {expression descriptor}

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  term_p: sst_exp_term_p_t;            {points to current term in expression}
  ele_p: sst_ele_exp_p_t;              {points to current set elements descriptor}
  ifarg_p: sst_exp_chain_p_t;          {points to current itrinsic function argument}
  msg_parm:                            {references to paramters for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

begin
  if exp.dtype_p <> nil then begin     {this expression has a data type ?}
    check_dtype (exp.dtype_p^);        {flag symbols used by data type}
    end;

  term_p := addr(exp.term1);           {init current term to first in expression}

  while term_p <> nil do begin         {once for each term in expression}
    if exp.term1.next_p <> nil then begin {expression has more than one term ?}
      check_dtype (term_p^.dtype_p^);  {declare dtype syms for this term}
      end;
    case term_p^.ttype of              {what kind of term is this ?}
{
*   Term is a constant.
}
sst_term_const_k: ;
{
*   Term is a variable reference.
}
sst_term_var_k: begin
  check_var (term_p^.var_var_p^);
  end;
{
*   Term is a function reference.
}
sst_term_func_k: begin
  check_var (term_p^.func_var_p^);
  check_call (term_p^.func_proc_p^, term_p^.func_proct_p^);
  end;
{
*   Term is an intrinsic function reference.
}
sst_term_ifunc_k: begin
  ifarg_p := term_p^.ifunc_args_p;     {init current ifunc argument to first arg}
  while ifarg_p <> nil do begin        {once for each argument}
    check_exp (ifarg_p^.exp_p^);
    ifarg_p := ifarg_p^.next_p;        {advance to next argument}
    end;                               {back and process this new argument}
  end;
{
*   Term is explicit type-casting function.
}
sst_term_type_k: begin
  check_dtype (term_p^.type_dtype_p^);
  check_exp (term_p^.type_exp_p^);
  end;
{
*   Term is a SET value.
}
sst_term_set_k: begin
  ele_p := term_p^.set_first_p;        {init first elements descriptor to current}
  while ele_p <> nil do begin          {once for each elements descriptor in set}
    check_exp (ele_p^.first_p^);       {flag ele range start value exp as used}
    if ele_p^.last_p <> nil then begin {ele end range expression exists ?}
      check_exp (ele_p^.last_p^);
      end;
    ele_p := ele_p^.next_p;            {advance to next elements descriptor}
    end;                               {back and process this new elements descriptor}
  end;
{
*   Term is nested expression.
}
sst_term_exp_k: begin
  check_exp (term_p^.exp_exp_p^);      {process nested expression}
  end;
{
*   Term is the value of a field in a record.
}
sst_term_field_k: begin
  check_sym (term_p^.field_sym_p);
  check_exp (term_p^.field_exp_p^);
  end;
{
*   Term is the value of a range of array subscripts.
}
sst_term_arele_k: begin
  check_exp (term_p^.arele_exp_p^);
  end;
{
*   Unrecognized or illegal term type.
}
otherwise
      sys_msg_parm_int (msg_parm[1], ord(term_p^.ttype));
      syo_error (term_p^.str_h, 'sst', 'term_type_unknown', msg_parm, 1);
      end;                             {end of term type cases we needed to handle}
    term_p := term_p^.next_p;          {advance to next term in expression}
    end;                               {back and process this new term in expression}
  end;
{
****************************
*
*   Local subroutine CHECK_DTYPE (DTYPE)
*
*   Check the data type descriptor, DTYPE, for symbols that may need to be
*   moved to a global scope.
}
procedure check_dtype (                {check data type for symbols to move}
  in out  dtype: sst_dtype_t);         {data type descriptor}

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  sym_p: sst_symbol_p_t;               {scratch symbol pointer}
  msg_parm:                            {references to paramters for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

begin
  if dtype.symbol_p <> nil then begin  {symbol exists for this data type ?}
    if sst_symflag_followed_k in dtype.symbol_p^.flags {already done this symbol ?}
      then return;
    if sst_symflag_following_dt_k in dtype.symbol_p^.flags {already doing dtype ?}
      then return;
    if                                 {not already working on this symbol ?}
        not (sst_symflag_following_k in dtype.symbol_p^.flags)
        then begin
      check_sym (dtype.symbol_p);      {flag data type's parent symbol}
      return;
      end;
    dtype.symbol_p^.flags :=           {indicate we are now doing this data type}
      dtype.symbol_p^.flags + [sst_symflag_following_dt_k];
    end;

  case dtype.dtype of                  {what kind of data type is this ?}
{
*   Data type is enumerated type.
}
sst_dtype_enum_k: begin
  check_sym (dtype.enum_first_p);      {all enumerated names handled similarly}
  end;
{
*   Data type is a record.
}
sst_dtype_rec_k: begin
  sym_p := dtype.rec_first_p;          {init curr symbol to first field name}
  while sym_p <> nil do begin          {once for each field in record}
    check_dtype (sym_p^.field_dtype_p^); {process field's data type}
    sym_p := sym_p^.field_next_p;      {advance to next field in record}
    end;
  end;
{
*   Data type is an array.
}
sst_dtype_array_k: begin
  check_exp (dtype.ar_ind_first_p^);   {process exp for subscript range start}
  if dtype.ar_ind_last_p <> nil then begin {fixed subscript ending exp exists ?}
    check_exp (dtype.ar_ind_last_p^);  {process exp for subscript range end}
    end;
  if dtype.ar_dtype_rem_p = nil
    then begin                         {this is a one-dimensional array}
      check_dtype (dtype.ar_dtype_ele_p^); {process elements data type}
      end
    else begin                         {there is more than one subscript}
      check_dtype (dtype.ar_dtype_rem_p^); {process "rest" of array}
      end
    ;
  end;
{
*   Data type is a set.
}
sst_dtype_set_k: begin
  check_dtype (dtype.set_dtype_p^);    {check set's base data type}
  end;
{
*   Data type is a subrange of another data type.
}
sst_dtype_range_k: begin
  check_dtype (dtype.range_dtype_p^);  {check base data type}
  if dtype.range_first_p <> nil then begin
    check_exp (dtype.range_first_p^);  {process expression for first value}
    end;
  if dtype.range_last_p <> nil then begin
    check_exp (dtype.range_last_p^);   {process expression for last value}
    end;
  end;
{
*   Data type is a procedure.
}
sst_dtype_proc_k: begin
  check_proc (dtype.proc_p^);          {check procedure descriptor}
  end;
{
*   Data type is a pointer.
}
sst_dtype_pnt_k: begin
  if dtype.pnt_dtype_p <> nil then begin {not a NIL pointer ?}
    check_dtype (dtype.pnt_dtype_p^);  {check pointed-to data type}
    end;
  end;
{
*   Data type is a copy of another data type.
}
sst_dtype_copy_k: begin
  if dtype.copy_symbol_p <> nil then begin
    check_sym (dtype.copy_symbol_p);   {check copied symbol name}
    end;
  check_dtype (dtype.copy_dtype_p^);   {check copied data type}
  end;
{
*   Undefined data type.  This could happen under legal circumstances if
*   a symbol was defined that pointed to a data type that was never defined,
*   but also never used directly.
}
sst_dtype_undef_k: ;
{
*   All the data types that require no special processing.
}
sst_dtype_int_k: ;
sst_dtype_float_k: ;
sst_dtype_bool_k: ;
sst_dtype_char_k: ;
otherwise
    sys_msg_parm_int (msg_parm[1], ord(dtype.dtype));
    sys_message_bomb ('sst', 'dtype_unexpected_exp', msg_parm, 1);
    end;                               {end of data type cases}
  end;
{
****************************
*
*   Local subroutine SYMBOL_MOVE (SYM,SCOPE)
*
*   Move the symbol SYM to the scope SCOPE.  Nothing is done if the symbol is
*   already in that scope.
}
procedure symbol_move (                {move symbol to a different scope}
  in out  sym: sst_symbol_t;           {descriptor of symbol to move}
  in      scope: sst_scope_t);         {descriptor of scope to move symbol to}

var
  sym_pp: ^sst_symbol_p_t;             {points to hash table user data area}
  name_p: string_var_p_t;              {points to new copy of symbol name}
  dt_p: sst_dtype_p_t;                 {pointer to base data type}
  sz: sys_int_adr_t;                   {size of symbol name var string}
  pos: string_hash_pos_t;              {handle to position in a hash table}
  rename_name: string_var32_t;         {used for making name string from RENAME_NUM}
  found: boolean;                      {TRUE if found name in hash table}

label
  not_rec;

begin
  rename_name.max := sizeof(rename_name.str); {init local var string}

  if sym.scope_p = addr(scope) then return; {symbol already at the target scope ?}

  changes_made := true;                {definately changes made this pass}
  sz := sizeof(name_p^) -              {find size of symbol name's var string}
    sizeof(name_p^.str) +
    (sym.name_in_p^.len * sizeof(char));
  util_mem_grab (sz, scope.mem_p^, false, name_p); {allocate mem for symbol name copy}
  name_p^.max := sym.name_in_p^.len;   {set max length of symbol name copy}
  string_copy (sym.name_in_p^, name_p^); {copy symbol name to mem in new scope}
  sym.name_in_p := name_p;             {point symbol to copied name in new scope}

  string_hash_pos_lookup (             {look up symbol name in its curr hash table}
    sym.scope_p^.hash_h,               {handle to hash table of source scope}
    sym.name_in_p^,                    {symbol's name}
    pos,                               {returned position handle}
    found);                            {TRUE if entry of this name found}
  if found then begin                  {name found, this should always happen}
    string_hash_ent_del (pos);         {delete symbol from source hash table}
    end;

  repeat                               {loop until found unused name}
    string_f_int (rename_name, rename_num); {make dummy name string from seq number}
    rename_num := rename_num + 1;      {make starting sequence number for next time}
    string_hash_pos_lookup (           {look up dummy name in target hash table}
      scope.hash_h,                    {handle to hash table of destination scope}
      rename_name,                     {dummy name to use in destination scope}
      pos,                             {returned handle to position in new table}
      found);                          {true if this name already in dest scope}
    until not found;                   {this name used, try again with new name ?}
  string_hash_ent_add (                {add symbol to hash table using dummy name}
    pos,                               {handle to position where name will go}
    name_p,                            {returned pointer to where dummy name stored}
    sym_pp);                           {returned pointer to hash entry data area}
  sym_pp^ := addr(sym);                {point new hash entry to the symbol}
  sym.scope_p := addr(scope);          {point symbol to its new scope}
{
*   If this symbol was a record data type, then the scope for the fields
*   must be re-linked to be a direct child of the symbol's new scope.
}
  if sym.symtype <> sst_symtype_dtype_k {symbol is not a data type ?}
    then goto not_rec;
  dt_p := sym.dtype_dtype_p;
  while dt_p^.dtype = sst_dtype_copy_k do dt_p := dt_p^.copy_dtype_p;
  if dt_p^.dtype <> sst_dtype_rec_k    {data type is not a record ?}
    then goto not_rec;
  dt_p^.rec_scope_p^.parent_p :=       {symbol's new scope is fields' parent scope}
    addr(scope);
not_rec:                               {jump here if symbol not record data type}
  end;
{
****************************
*
*   Local subroutine SCOPE_RESET (SCOPE)
*
*   Resets all the FOLLOWING, FOLLOWING_DT, and FOLLOWED flags for every symbol
*   in the scope.
}
procedure scope_reset (                {reset all symbol FOLLOW flags in scope}
  in      scope: sst_scope_t);         {scope to reset all symbols in}

var
  pos: string_hash_pos_t;              {handle of position in hash table}
  name_p: univ_ptr;                    {unused argument returned from subroutine}
  sym_pp: ^sst_symbol_p_t;             {pointer to hash entry user data area}
  sym_p: sst_symbol_p_t;               {pointer to current symbol}
  found: boolean;

begin
  if move_routines then return;        {don't need to re-check symbols this pass}
  string_hash_pos_first (              {get position handle to first hash entry}
    scope.hash_h,                      {handle to hash table for this scope}
    pos,                               {returned position to first hash table entry}
    found);                            {returned TRUE if entry exists}

  while found do begin                 {once for each symbol in hash table}
    string_hash_ent_atpos (            {get hash entry data from position handle}
      pos, name_p, sym_pp);
    sym_p := sym_pp^;                  {point to symbol descriptor for this entry}
    sym_p^.flags := sym_p^.flags -     {remove the FOLLOW flags}
      [sst_symflag_following_k, sst_symflag_following_dt_k, sst_symflag_followed_k];
    string_hash_pos_next (             {advance to next entry in hash table}
      pos, found);
    end;                               {back and process this new hash table entry}
  end;
{
****************************
*
*   Local subroutine CHECK_SYM (SYM_P)
*
*   Check the symbol SYM_P^ to see whether it is a reference from a nested internal
*   routine to a parent routine.  If so, the symbol will be moved to a global scope
*   unless it is a variable local to the parent routine.  In that case, the
*   variable will be added as a call argument to the nested routine, and SYM_P
*   will be returned pointing to the new symbol descriptor for the dummy argument.
}
procedure check_sym (                  {move symbol to global scope, if necessary}
  in out  sym_p: sst_symbol_p_t);      {pointer to symbol, altered if new sym made}

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  scope_p: sst_scope_p_t;              {scratch pointer to scope descriptor}
  sym2_p: sst_symbol_p_t;              {scratch pointer to symbol descriptor}
  str_h: syo_string_t;                 {handle to source streams chars range}
  msg_parm:                            {references to paramters for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

label
  nested_ref, done_nested, done_sym;

begin
  with sym_p^: sym do begin            {SYM is abbreviation for symbol descriptor}
{
*   Some symbols live in private scopes, and are subordinate to other symbols.
*   These are handled by checking the parent symbol.
}
  case sym.symtype of                  {what kind of symbol is this ?}
sst_symtype_enum_k: begin              {symbol is enumerated name}
      check_dtype (sym.enum_dtype_p^);
      goto done_sym;
      end;
sst_symtype_field_k: begin             {symbol is field in a record}
      check_dtype (sym.field_parent_p^);
      goto done_sym;
      end;
sst_symtype_var_k: begin               {symbol is a variable}
      if sym.var_com_p <> nil then begin {variable is in a common block ?}
        check_sym (sym.var_com_p);     {process whole common block}
        goto done_sym;
        end
      end;
sst_symtype_proc_k: begin              {symbol is routine name}
      goto done_nested;                {routine references handled in different way}
      end;
    end;
{
*   Done dealing with special symbols that aren't handled directly.
*
*   Determine whether this symbol is referenced from a nested procedure, and
*   lives local to any of its parent procedures.
}
  if level_nest <= 1                   {not currently in nested procedure ?}
    then goto done_nested;
  if sym.scope_p = sst_scope_p         {symbol is local to current procedure ?}
    then goto done_nested;
  if sym.scope_p = scope_top_p         {symbol already in global enough scope ?}
    then goto done_nested;
  scope_p := sym.scope_p^.parent_p;    {init to next most global scope from symbol's}
  while scope_p <> nil do begin        {loop thru all parent scopes from here}
    if scope_p = sst_scope_p           {symbol is within current procedure ?}
      then goto done_nested;
    if scope_p = scope_top_p           {the global scope is a parent scope ?}
      then goto nested_ref;            {this symbol is a nested reference}
    scope_p := scope_p^.parent_p;      {advance to one level more global scope}
    end;
  goto done_nested;                    {symbol was already past min global space}
{
*   The symbol lives in a scope more global than the current one, but less
*   local than the minimum acceptable "global" scope.
}
nested_ref:
  scope_p := sym.scope_p;              {first scope to reset flags in}
  while scope_p <> nil do begin        {loop thru each scope to the most global}
    scope_reset (scope_p^);            {reset symbol flags in this scope}
    scope_p := scope_p^.parent_p;      {advance to next most global scope}
    end;                               {back and process this new scope}
  case sym.symtype of                  {what kind of symbol is this ?}
{
**************
*
*   These symbols can be handled by simply moving them to the global space.
}
sst_symtype_const_k,                   {symbol is a constant}
sst_symtype_dtype_k: begin             {symbol is a data type}
  symbol_move (sym, scope_top_p^);
  end;
{
**************
*
*   The nested referenced symbol is a variable.  If the variable is static,
*   then just move it to the global scope.  Otherwise, it must be explicitly
*   added to the call arguments of this routine.  It will be propagated to
*   any applicable parent routines by multiple passes as a higher level later.
*
*   The variable is not a member of a common block, since this was
*   handled separately.
}
sst_symtype_var_k: begin
  if sst_symflag_static_k in sym.flags then begin {variable is in static storage ?}
    symbol_move (sym, scope_top_p^);   {move to global scope}
    goto done_nested;
    end;

  add_arg (                            {add variable to call argument list}
    frame_p^.proc_p^,                  {arguments template to add variable to}
    frame_p^.scope_p^,                 {routine's scope}
    sym_p);                            {points to old symbol, will point to new arg}
  end;
{
**************
*
*   The nested referenced symbol is a procedure.  This will be handled by
*   the OPCODES_CHECK routine at the end of the procedure.
}
sst_symtype_proc_k: ;
{
**************
*
*   The nested referenced symbol is a common block.  Move the whole common
*   block to the global scope.
}
sst_symtype_com_k: begin
  symbol_move (sym, scope_top_p^);     {move common block symbol to global scope}
  sym2_p := sym.com_first_p;
  while sym2_p <> nil do begin         {once for each variable on common block}
    symbol_move (sym2_p^, scope_top_p^); {move this common block variable to global}
    sym2_p := sym2_p^.var_next_p;      {advance to next symbol in common block}
    end;                               {back and process this new com block variable}
  end;
{
**************
*
*   The nested referenced symbol is a label.
}
sst_symtype_label_k: begin
  str_h.first_char := sym.char_h;
  str_h.last_char := sym.char_h;
  syo_error (str_h, 'sst_c_write', 'goto_nonlocal_unimp', nil, 0);
  end;
{
**************
*
*   Unexpected symbol type.
}
otherwise
    sys_msg_parm_int (msg_parm[1], ord(sym.symtype));
    sys_message_bomb ('sst', 'symbol_type_unknown', msg_parm, 1);
    end;                               {end of symbol type cases}
done_nested:
{
****************************************
*
*   All done making sure the symbol is in the right scope.
*   Now make sure all other implicitly referenced symbols are also checked.
}
  if                                   {don't process this symbol any further ?}
      (sst_symflag_followed_k in sym.flags) or {already did this symbol ?}
      (sst_symflag_following_k in sym.flags) {already working on this symbol ?}
    then return;
  sym.flags := sym.flags + [sst_symflag_following_k]; {now working on this symbol}
  case sym.symtype of                  {what kind of symbol is this ?}
{
**************
*
*   Symbol is a constant.
}
sst_symtype_const_k: begin
  check_exp (sym.const_exp_p^);        {declare nested symbols in expression}
  end;
{
**************
*
*   Symbol is a data type.
}
sst_symtype_dtype_k: begin
  check_dtype (sym.dtype_dtype_p^);    {declare nested symbols in dtype}
  end;                                 {end of symbol is data type case}
{
**************
*
*   Symbol is a variable.
}
sst_symtype_var_k: begin
  check_dtype (sym.var_dtype_p^);      {declare nested symbols in data type}
  if sym.var_val_p <> nil then begin   {initial value expression exists ?}
    check_exp (sym.var_val_p^);        {flag symbols in initial value exp if used}
    end;
  end;
{
**************
*
*   Symbol is an abbreviation.
}
sst_symtype_abbrev_k: begin
  check_var (sym.abbrev_var_p^);
  end;
{
**************
*
*   Symbol is a routine name.
}
sst_symtype_proc_k: begin
  check_proc (sym.proc);               {declare nested symbols in routine}
  end;
{
**************
*
*   Symbol is a common block name.
}
sst_symtype_com_k: begin
  sym2_p := sym.com_first_p;           {init current symbol to first in com block}
  while sym2_p <> nil do begin         {once for each symbol in common block}
    check_dtype (sym2_p^.var_dtype_p^); {check variable's data type}
    if sym2_p^.var_val_p <> nil then begin {check initial value, if it exists}
      check_exp (sym2_p^.var_val_p^);
      end;
    sym2_p := sym2_p^.var_next_p;      {advance to next variable in common block}
    end;                               {back and process this new variable}
  end;
{
**************
*
*   Symbol types that require no special processing.
}
sst_symtype_prog_k: ;
sst_symtype_module_k: ;
sst_symtype_label_k: ;
otherwise
    sys_msg_parm_int (msg_parm[1], ord(sym.symtype));
    sys_message_bomb ('sst', 'symbol_type_unknown', msg_parm, 1);
    end;                               {end of symbol type cases}
  end;                                 {done with SYM abbreviation}

done_sym:                              {common exit point}
  sym_p^.flags := sym_p^.flags +       {all done following this symbol}
    [sst_symflag_followed_k];
  sym_p^.flags := sym_p^.flags -       {this symbol no longer in process}
    [sst_symflag_following_k, sst_symflag_following_dt_k];
  end;
{
****************************
*
*   Local subroutine ADD_ARG (PROC,SYM_P)
*
*   Add a call argument to the routine described by PROC, if necessary.
*   On entry, SYM_P is pointing to the remote symbol that needs to be
*   passed as a call argument.  SYM_P will be returned pointing to the
*   dummy argument descriptor.  A new call argument will only be created
*   if this symbol is not already passed.
}
procedure add_arg (                    {add arg to routine, if necessary}
  in out  proc: sst_proc_t;            {descriptor for routine to add arg to}
  in      scope: sst_scope_t;          {scope of routine to add argument to}
  in out  sym_p: sst_symbol_p_t);      {pnts to remote var, returned dummy arg pnt}

var
  arg_p: sst_proc_arg_p_t;             {points to call argument for passing symbol}
  arg_pp: sst_proc_arg_pp_t;           {points to end of arguments chain}
  exp_p: sst_exp_p_t;                  {points to exp for remote variable reference}
  var_p: sst_var_p_t;                  {points to var descriptor for remote var ref}
  synew_p: sst_symbol_p_t;             {points to symbol descriptor for dummy arg}

label
  got_arg;

begin
  arg_pp := frame_p^.arg_pp;           {init pointer to end of args chain}
  arg_p := arg_pp^;                    {init pointer to first added call arg}
  while arg_p <> nil do begin          {scan all the added call arguments}
    if                                 {this arg already passes the symbol ?}
        (arg_p^.exp_p <> nil) and then
        (arg_p^.exp_p^.term1.ttype = sst_term_var_k) and
        (arg_p^.exp_p^.term1.var_var_p <> nil) and then
        (arg_p^.exp_p^.term1.var_var_p^.mod1.next_p = nil) and
        (arg_p^.exp_p^.term1.var_var_p^.mod1.top_sym_p = sym_p)
        then begin
      goto got_arg;
      end;
    arg_pp := addr(arg_p^.next_p);     {update end of arguments chain}
    arg_p := arg_p^.next_p;            {advance to next argument in chain}
    end;                               {back and check this next argument}
{
*   None of the added call arguments pass this symbol.  Add this symbol as
*   call argument to start of added call arguments chain.  The argument
*   template will be flagged with an expression that is the variable reference
*   to the remote variable.
}
  changes_made := true;                {a change will definately be made this pass}
  args_added := true;                  {an argument will definately be added}
  proc.n_args := proc.n_args + 1;      {this routine takes one more argument}

  util_mem_grab (                      {allocate memory for argument descriptor}
    sizeof(arg_p^), scope.mem_p^, false, arg_p);
  util_mem_grab (                      {allocate memory for expression descriptor}
    sizeof(exp_p^), scope.mem_p^, false, exp_p);
  util_mem_grab (                      {allocate memory for var reference descriptor}
    sizeof(var_p^), scope.mem_p^, false, var_p);
{
*   Fill in the variable reference descriptor.
}
  var_p^.mod1.next_p := nil;
  var_p^.mod1.modtyp := sst_var_modtyp_top_k;
  var_p^.mod1.top_str_h.first_char.crange_p := nil;
  var_p^.mod1.top_str_h.first_char.ofs := 0;
  var_p^.mod1.top_str_h.last_char.crange_p := nil;
  var_p^.mod1.top_str_h.last_char.ofs := 0;
  var_p^.mod1.top_sym_p := sym_p;
  var_p^.dtype_p := sym_p^.var_dtype_p;
  var_p^.rwflag := [sst_rwflag_read_k, sst_rwflag_write_k];
  var_p^.vtype := sst_vtype_var_k;
{
*   Fill in the expression descriptor that references the variable descriptor.
}
  exp_p^.str_h := var_p^.mod1.top_str_h;
  exp_p^.dtype_p := var_p^.dtype_p;
  exp_p^.dtype_hard := true;
  exp_p^.val_eval := true;
  exp_p^.val_fnd := false;
  exp_p^.rwflag := var_p^.rwflag;
  exp_p^.term1.next_p := nil;
  exp_p^.term1.op2 := sst_op2_none_k;
  exp_p^.term1.op1 := sst_op1_none_k;
  exp_p^.term1.ttype := sst_term_var_k;
  exp_p^.term1.str_h := exp_p^.str_h;
  exp_p^.term1.dtype_p := exp_p^.dtype_p;
  exp_p^.term1.dtype_hard := true;
  exp_p^.term1.val_eval := true;
  exp_p^.term1.val_fnd := false;
  exp_p^.term1.rwflag := exp_p^.rwflag;
  exp_p^.term1.var_var_p := var_p;
{
*   Fill in the symbol descriptor for the dummy argument.
}
  util_mem_grab (                      {allocate memory for dummy arg symbol desc}
    sizeof(synew_p^), sst_scope_p^.mem_p^, false, synew_p);
  synew_p^.name_in_p := sym_p^.name_in_p; {use referenced symbol's input name}
  synew_p^.name_out_p := nil;
  synew_p^.next_p := nil;
  synew_p^.char_h := exp_p^.str_h.first_char;
  synew_p^.scope_p := addr(scope);
  synew_p^.symtype := sst_symtype_var_k;
  synew_p^.flags := [sst_symflag_def_k, sst_symflag_used_k, sst_symflag_created_k];
  synew_p^.var_dtype_p := exp_p^.dtype_p;
  synew_p^.var_val_p := nil;
  synew_p^.var_arg_p := arg_p;
  synew_p^.var_proc_p := addr(proc);
  synew_p^.var_com_p := nil;
  synew_p^.var_next_p := nil;
{
*   Fill in the argument template descriptor that references the expression
*   descriptor and the dummy argument symbol.
}
  arg_p^.next_p := nil;
  arg_p^.sym_p := synew_p;
  arg_p^.name_p := synew_p^.name_in_p;
  arg_p^.exp_p := exp_p;
  arg_p^.dtype_p := exp_p^.dtype_p;
  arg_p^.pass := sst_pass_ref_k;
  arg_p^.rwflag_ext := exp_p^.rwflag;
  arg_p^.univ := false;

  arg_pp^ := arg_p;                    {add new argument to end of arguments chain}
{
*   One way or another, ARG_P is pointing to the routine argument that the
*   variable reference should now be using.
}
got_arg:
  sym_p := arg_p^.sym_p;               {switch symbol to be the dummy argument}
  end;
{
****************************
*
*   Start of main routine.
}
begin
  rename_num := 1;                     {init seq number for making arbitrary name}
  move_routines := false;              {init to don't move routines around}
  args_added := false;                 {init to no arguments added anywhere}

  repeat                               {loop until no call arguments added}
    scope_top_p := sst_scope_root_p;   {init pointer to "global enough" scope}
    sst_scope_p := sst_scope_root_p;   {init current scope to root scope}
    scope_reset (scope_top_p^);        {reset symbol flags in top scope}
    sst_names_p := sst_scope_p;
    level_nest := 0;                   {depth in nested routines}
    frame_p := nil;                    {init to no current stack frame exists}
    changes_made := false;             {init to nothing got changed this pass}
    check_opcodes (sst_opc_first_p);   {check the entire list of opcodes}
    until not changes_made;            {try again if any call arguments were created}
{
*   No more call arguments were added to any routine on the last pass.
*   Now move the internal nested routines to the top level on this pass.
}
  move_routines := true;
  sst_opc_p := nil;                    {init moved opcodes chain to empty}
  scope_top_p := sst_scope_root_p;     {init pointer to "global enough" scope}
  sst_scope_p := sst_scope_root_p;     {init current scope to root scope}
  scope_reset (scope_top_p^);          {reset symbol flags in top scope}
  level_nest := 0;                     {depth in nested routines}
  frame_p := nil;                      {init to no current stack frame exists}
  check_opcodes (sst_opc_first_p);     {check the entire list of opcodes}

  if sst_opc_p <> nil then begin       {moved opcodes chain is not empty ?}
    sst_opc_next_pp := addr(sst_opc_first_p); {init pointer to next opcode pointer}
    while sst_opc_next_pp^ <> nil do begin {loop to find end of opcode chain}
      sst_opc_next_pp := addr(sst_opc_next_pp^^.next_p); {advance one link in chain}
      end;
    sst_opc_next_pp^ := sst_opc_p;     {link moved opcodes to end of chain}
    end;
  end;
