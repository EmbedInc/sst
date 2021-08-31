{   Subroutine SST_R_PAS_SMENT_ROUT (STR_ALL_H)
*
*   Process the ROUTINE_HEADING syntax and put the appropriate information into
*   the symbol table.  STR_ALL_H is the string handle to the whole
*   ROUTINE_HEADING syntax.
}
module sst_r_pas_SMENT_ROUT;
define sst_r_pas_sment_rout;
%include 'sst_r_pas.ins.pas';

procedure sst_r_pas_sment_rout (       {process ROUTINE_HEADING syntax}
  in      str_all_h: syo_string_t);    {string handle for whole statement}

const
  max_msg_parms = 3;                   {max parameters we can pass to a message}

var
  tag: sys_int_machine_t;              {syntax tag ID}
  str_h: syo_string_t;                 {handle to string associated with TAG}
  func: boolean;                       {TRUE if subroutine is a function}
  here: boolean;                       {routine actually defined right here}
  existed: boolean;                    {TRUE if routine previously declared}
  declared: boolean;                   {TRUE if symbol already declared as routine}
  scope_internal: boolean;             {TRUE if routine explicitly INTERNAL}
  sym_p: sst_symbol_p_t;               {points to routine name symbol descriptor}
  sym_old_p: sst_symbol_p_t;           {points to old symbol if already declared}
  sym_arg_p: sst_symbol_p_t;           {points to dummy argument symbol descriptor}
  mem_p: util_mem_context_p_t;         {points to mem context for temp scope}
  arg_p: sst_proc_arg_p_t;             {points to current routine argument desc}
  fnam: string_treename_t;             {file name passed to a message}
  lnum: sys_int_machine_t;             {line number passed to a message}

  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;
  stat: sys_err_t;                     {completion status code}

label
  next_opt, vparam, done_opt, proc_mismatch;

begin
  fnam.max := sizeof(fnam.str);

  syo_level_down;                      {down into ROUTINE_HEADING syntax}

  syo_get_tag_msg                      {get PROCEDURE/FUNCTION tag}
    (tag, str_h, 'sst_pas_read', 'statement_proc_bad', nil, 0);
  case tag of                          {what type of routine is this}
1:  begin                              {routine is a procedure}
      func := false;
      end;
2:  begin                              {routine is a function}
      func := true;
      end;
otherwise
    syo_error_tag_unexp (tag, str_h);
    end;                               {done with routine type keyword cases}

  syo_get_tag_msg                      {get routine name tag}
    (tag, str_h, 'sst_pas_read', 'statement_proc_bad', nil, 0);
  sst_symbol_new (                     {try to create new symbol for routine name}
    str_h, syo_charcase_asis_k, sym_p, stat);
  existed :=                           {true if symbol previously declared}
    sys_stat_match(sst_subsys_k, sst_stat_sym_prev_def_k, stat);
  declared := false;                   {init to symbol new declared as routine}
  syo_error_abort (stat, str_h, 'sst_pas_read', 'statement_proc_bad', nil, 0);
{
*   Make sure we are in a nested scope for creating the routine descriptor.
*   We will always create a nested scope here, whether the routine was previously
*   declared or not.  This allows us to always build a new routine descriptor
*   from the incoming data.  If the routine was already declared, then later
*   the new descriptor is compared to the old, and the whole nested scope deleted.
}
  sst_scope_new;                       {create nested scope for the routine}
  sym_old_p := nil;                    {init to no previous declaration existed}
  if existed then begin                {symbol did exist previously ?}
    case sym_p^.symtype of             {what kind of symbol is it so far ?}
sst_symtype_proc_k: begin              {symbol was already declared as a routine}
        declared := true;              {remember that routine already declared}
        sym_old_p := sym_p;            {save pointer to old symbol}
        sst_symbol_new (               {create temporary symbol in new scope}
          str_h, syo_charcase_down_k, sym_p, stat);
        sym_p^.flags := sym_old_p^.flags; {init with existing symbol flags}
        end;
sst_symtype_illegal_k: ;               {symbol defined, but not declared as anything}
otherwise
      sst_charh_info (sym_p^.char_h, fnam, lnum);
      sys_msg_parm_vstr (msg_parm[1], sym_p^.name_in_p^);
      sys_msg_parm_int (msg_parm[2], lnum);
      sys_msg_parm_vstr (msg_parm[3], fnam);
      syo_error (str_h, 'sst_pas_read', 'symbol_already_def', msg_parm, 3);
      end;
    end;                               {done handling symbol already existed case}
  sst_scope_p^.symbol_p := sym_p;      {point scope to its defining symbol}
{
*   The symbol descriptor SYM_P^ is new and will be filled in with this
*   routine declaration.  If the routine was declared before, then SYM_OLD_P
*   is pointing to the old symbol descriptor.  In that case, the resulting data
*   structure will be compared with the old one later.
}
  sym_p^.proc_scope_p := sst_scope_p;  {save pointer to routine's scope}
  sym_p^.symtype := sst_symtype_proc_k; {symbol is a routine}
  sym_p^.proc.sym_p := sym_p;          {point routine descriptor to name symbol}
  sym_p^.proc.dtype_func_p := nil;     {no data type for function value exists yet}
  sym_p^.proc.flags := [];             {init separate flags}
  sst_dtype_new (sym_p^.proc_dtype_p); {create new data type for this routine}
  sym_p^.proc_dtype_p^.symbol_p := sym_p; {fill in dtype descriptor for this routine}
  sym_p^.proc_dtype_p^.dtype := sst_dtype_proc_k;
  sym_p^.proc_dtype_p^.bits_min := 0;
  sym_p^.proc_dtype_p^.align_nat := 1;
  sym_p^.proc_dtype_p^.align := 1;
  sym_p^.proc_dtype_p^.size_used := 0;
  sym_p^.proc_dtype_p^.size_align := 0;
  sym_p^.proc_dtype_p^.proc_p := addr(sym_p^.proc);

  syo_get_tag_msg                      {get tag for routine arguments}
    (tag, str_h, 'sst_pas_read', 'statement_proc_bad', nil, 0);
  if tag <> 1 then syo_error_tag_unexp (tag, str_h);
  sst_r_pas_proc_args (sym_p^.proc);   {process call arguments, if any}

  syo_get_tag_msg                      {get tag for function return data type}
    (tag, str_h, 'sst_pas_read', 'statement_proc_bad', nil, 0);
  case tag of
1:  begin                              {no function data type is declared}
      if func then begin
        syo_error (str_h, 'sst_pas_read', 'func_no_data_type', nil, 0);
        end
      end;
2:  begin                              {function data type IS declared}
      if func
        then begin                     {routine is a function}
          sst_r_pas_data_type (sym_p^.proc.dtype_func_p); {read in data type}
          end
        else begin                     {routine is a procedure}
          syo_error (str_h, 'sst_pas_read', 'proc_data_type', nil, 0);
          end
        ;
      end;
otherwise
    syo_error_tag_unexp (tag, str_h);
    end;                               {end of function dtype tag cases}
{
*   Done processing call arguments and function return data type, if any.
*   Now handle routine options.
}
  here := true;                        {init to routine is defined right here}
  scope_internal := false;             {init to not explicitly internal routine}
next_opt:                              {back here each new routine option}
  syo_get_tag_msg                      {get tag for next routine option}
    (tag, str_h, 'sst_pas_read', 'statement_proc_bad', nil, 0);
  if tag = syo_tag_end_k then goto done_opt; {finished all routine options ?}
  if tag <> 1 then syo_error_tag_unexp (tag, str_h); {unexpected TAG value ?}
  syo_level_down;                      {down into ROUTINE_OPTION syntax}
  syo_get_tag_msg                      {get tag to identify routine option}
    (tag, str_h, 'sst_pas_read', 'statement_proc_bad', nil, 0);
  case tag of
1: begin                               {routine option FORWARD}
      here := false;                   {routine defined later}
      end;
2: begin                               {routine option EXTERN}
      if sym_old_p <> nil then begin   {routine previously declared ?}
        sst_charh_info (sym_old_p^.char_h, fnam, lnum);
        sys_msg_parm_vstr (msg_parm[1], sym_old_p^.name_in_p^);
        sys_msg_parm_int (msg_parm[2], lnum);
        sys_msg_parm_vstr (msg_parm[3], fnam);
        syo_error (str_h, 'sst_pas_read', 'proc_redeclare', msg_parm, 3);
        end;
      if not (sst_symflag_global_k in sym_p^.flags) then begin {not already global ?}
        sym_p^.flags := sym_p^.flags + [
          sst_symflag_def_k,           {symbol is properly defined}
          sst_symflag_global_k,        {symbol will be globally known}
          sst_symflag_extern_k];       {symbol lives externally to this module}
        end;
      here := false;                   {routine defined later, if at all}
      end;
3: begin                               {routine option INTERNAL}
      if sst_symflag_global_k in sym_p^.flags then begin {already global symbol ?}
        sys_message_parms ('sst_pas_read', 'proc_global', nil, 0);
        goto proc_mismatch;
        end;
      scope_internal := true;          {routine is explicitly not globally known}
      goto vparam;                     {go to common code with VAL_PARAM option}
      end;
4: begin                               {routine option VAL_PARAM}
vparam:                                {jump here from INTERNAL option}
      sst_r_pas_vparam (sym_p^.proc);  {adjust call args to VAL_PARAM option}
      end;
5: begin                               {routine option NO_RETURN}
      sym_p^.proc.flags := sym_p^.proc.flags + [
        sst_procflag_noreturn_k];
      end;
otherwise
    syo_error_tag_unexp (tag, str_h);
    end;                               {end of routine option type cases}
  syo_level_up;                        {back up from ROUTINE_OPTION syntax}
  goto next_opt;                       {back to handle next routine option}
done_opt:                              {all done with routine options}
  if                                   {this is a top subroutine in MODULE block ?}
      (not scope_internal) and         {not explicitly flagged as non-global ?}
      (nest_level = 1) and             {just inside top block ?}
      (top_block = top_block_module_k) and {top block is a MODULE ?}
      (not (sst_symflag_extern_k in sym_p^.flags)) {not already declared external ?}
      then begin
    sym_p^.flags := sym_p^.flags +     {routine will be globally known}
      [sst_symflag_global_k];
    end;
{
*   The symbol descriptor SYM_P has been all filled in.
}
  if declared
{
*   The routine was declared before.  Now compare the new declaration to the
*   old, and then completely delete the new symbol with its whole scope.
}
    then begin                         {routine was declared before}
      sst_routines_match (             {compare old and new routine descriptors}
        sym_old_p^.proc,               {original procedure descriptor}
        sym_p^.proc,                   {new procedure descriptor}
        stat);                         {returned status}
      if sys_error(stat) then begin    {routine declarations mismatched ?}
        sys_error_print (stat, '', '', nil, 0);
        goto proc_mismatch;
        end;
      sym_old_p^.flags := sym_p^.flags; {update accumulated symbol flags}
      mem_p := sst_scope_p^.mem_p;     {get pointer to mem context for temp scope}
      sst_scope_old;                   {back to original scope}
      sst_scope_p := sym_old_p^.proc_scope_p; {swap to routine's scope}
      sst_names_p := sst_scope_p;
      util_mem_context_del (mem_p);    {deallocate all traces of temporary scope}
      sym_p := sym_old_p;              {point back to old symbol declared before}
      end
{
*   This was the first declaration of this routine.  Put the call arguments into
*   the symbol table for the routine's scope.  If the routine is a function,
*   this includes the function name.
}
    else begin                         {routine was not declared before}
      arg_p := sym_p^.proc.first_arg_p; {init first argument as current}
      while arg_p <> nil do begin      {loop thru each routine argument descriptor}
        sst_symbol_new_name (          {install call arg in symbol table}
          arg_p^.name_p^,              {dummy argument name}
          sym_arg_p,                   {returned pointer to symbol descriptor}
          stat);
        sys_error_abort (stat, '', '', nil, 0);
        sym_arg_p^.symtype := sst_symtype_var_k; {set up symbol as dummy arg}
        sym_arg_p^.var_dtype_p := arg_p^.dtype_p;
        sym_arg_p^.var_val_p := nil;
        sym_arg_p^.var_arg_p := arg_p;
        sym_arg_p^.var_proc_p := addr(sym_p^.proc);
        sym_arg_p^.var_com_p := nil;
        sym_arg_p^.var_next_p := nil;
        arg_p^.sym_p := sym_arg_p;     {point arg descriptor to its symbol}
        arg_p := arg_p^.next_p;        {advance to next call argument in routine}
        end;                           {back to process this new call argument}
      sym_p^.proc_funcvar_p := nil;    {init to no function value variable exists}
      if sym_p^.proc.dtype_func_p <> nil then begin {routine is a function ?}
        sst_symbol_new_name (          {install function name as variable name}
          sym_p^.name_in_p^,           {function name}
          sym_arg_p,                   {returned pointer to symbol descriptor}
          stat);
        sys_error_abort (stat, '', '', nil, 0);
        sym_arg_p^.symtype := sst_symtype_var_k; {set up symbol as dummy arg}
        sym_arg_p^.var_dtype_p := sym_p^.proc.dtype_func_p;
        sym_arg_p^.var_val_p := nil;
        sym_arg_p^.var_arg_p := nil;
        sym_arg_p^.var_proc_p := addr(sym_p^.proc);
        sym_arg_p^.var_com_p := nil;
        sym_arg_p^.var_next_p := nil;
        sym_p^.proc_funcvar_p := sym_arg_p; {save pointer to function value variable}
        end;                           {done installing function name as local var}
      end
    ;

  if here
    then begin                         {routine is defined right here}
      sym_p^.flags := sym_p^.flags + [sst_symflag_def_k]; {routine is defined}
      sst_opcode_new;                  {create opcode for this routine}
      sst_opc_p^.opcode := sst_opc_rout_k; {opcode is a routine}
      sst_opc_p^.str_h := str_all_h;   {save string handle to routine declaration}
      sst_opc_p^.rout_sym_p := sym_p;  {point opcode to routine name symbol}
      sst_opcode_pos_push (sst_opc_p^.rout_p); {new opcodes are for this routine}
      nest_level := nest_level + 1;    {one more layer deep in nested blocks}
      end
    else begin                         {routine will be defined elswhere, if at all}
      sst_scope_old;                   {pop back to original scope}
      end
    ;
  syo_level_up;                        {back up from ROUTINE_HEADING syntax}
  return;

proc_mismatch:                         {print routines mismatch error and abort}
  if sym_old_p = nil
    then begin                         {there was no previous declaration}
      syo_error (str_h, '', '', nil, 0);
      end
    else begin                         {previous declaration existed}
      sst_charh_info (sym_old_p^.char_h, fnam, lnum);
      sys_msg_parm_vstr (msg_parm[1], sym_old_p^.name_in_p^);
      sys_msg_parm_int (msg_parm[2], lnum);
      sys_msg_parm_vstr (msg_parm[3], fnam);
      syo_error (str_all_h, 'sst_pas_read', 'proc_mismatch', msg_parm, 3);
      end
    ;
  end;
