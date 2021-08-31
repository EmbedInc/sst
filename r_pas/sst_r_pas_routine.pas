{   Subroutine SST_R_PAS_ROUTINE (STR_ROUT_H,V,ARGS_HERE,PROC_P)
*
*   Create a complete routine descriptor.  STR_ROUT_H is the string handle to the
*   whole call.  V is the "variable" descriptor
*   for the routine name.  This is either directly a routine name or a variable
*   with the routine data type.  If TRUE, ARGS_HERE indicates that the next
*   tag read is for the FUNCTION_ARGUMENTS syntax.  When FALSE, it indicates
*   that the routine has no arguments.  PROC_P is returned pointing to the
*   newly created procedure descriptor.  It will be completely filled in.
}
module sst_r_pas_ROUTINE;
define sst_r_pas_routine;
%include 'sst_r_pas.ins.pas';

procedure sst_r_pas_routine (          {create routine descriptor}
  in      str_rout_h: syo_string_t;    {string handle to whole call}
  in      v: sst_var_t;                {"variable" descriptor for routine name}
  in      args_here: boolean;          {TRUE if FUNCTION_ARGUMENTS syntax exists}
  out     proc_p: sst_proc_p_t);       {will point to new routine descriptor}

var
  template_p: sst_proc_p_t;            {points to template for this routine}
  arg_pp: sst_proc_arg_pp_t;           {where to put pointer to next arg descriptor}
  arg_p: sst_proc_arg_p_t;             {points to current call argument}
  argt_p: sst_proc_arg_p_t;            {pointer to argument descriptor from template}
  tag: sys_int_machine_t;              {syntax tag ID}
  str_h: syo_string_t;                 {handle to string associated with TAG}
  stat: sys_err_t;                     {completion status code}

label
  done_args, next_arg, var_not_rout;

begin
  if v.vtype <> sst_vtype_rout_k       {variable does not refer to a routine ?}
    then goto var_not_rout;
  template_p := v.rout_proc_p;         {get pointer to routine template}
{
*   The variable descriptor references a legal routine, and TEMPLATE_P points
*   to the routine template descriptor.
}
  sst_mem_alloc_scope (sizeof(proc_p^), proc_p); {alloc mem for routine descriptor}
  proc_p^ := template_p^;              {init routine descriptor from template}
  proc_p^.n_args := 0;                 {init to no call arguments so far}
  proc_p^.first_arg_p := nil;
  if not args_here then goto done_args; {no call arguments to process ?}
{
*   The FUNCTION_ARGUMENTS syntax exists.  Now process the call arguments.
}
  syo_level_down;                      {down into FUNCTION_ARGUMENTS syntax}
  arg_pp := addr(proc_p^.first_arg_p); {adr of where to put pointer to next arg}
  argt_p := template_p^.first_arg_p;   {init pointer to template for argument}

next_arg:                              {back here each new call argument}
  syo_get_tag_msg (                    {get tag to this call argument}
    tag, str_h, 'sst_pas_read', 'args_syntax_err', nil, 0);
  if tag = syo_tag_end_k then begin    {done with all the call arguments ?}
    syo_level_up;                      {back up from FUNCTION_ARGUMENTS syntax}
    goto done_args;
    end;
  if tag <> 1                          {illegal TAG value ?}
    then syo_error_tag_unexp (tag, str_h);
  proc_p^.n_args := proc_p^.n_args + 1; {one more call argument}
  sst_mem_alloc_scope (sizeof(arg_p^), arg_p); {allocate mem for argument descriptor}
  arg_pp^ := arg_p;                    {link new arg descriptor to end of chain}
  arg_pp := addr(arg_p^.next_p);       {save adr of where next arg pointer goes}
  arg_p^.next_p := nil;                {init argument descriptor}
  arg_p^.sym_p := nil;
  arg_p^.name_p := nil;
  if argt_p <> nil
    then arg_p^.pass := argt_p^.pass
    else arg_p^.pass := sst_pass_none_k;
  arg_p^.univ := false;
  sst_r_pas_exp (str_h, false, arg_p^.exp_p); {create expression descriptor for arg}
  arg_p^.dtype_p := arg_p^.exp_p^.dtype_p; {argument data type comes from expression}
  arg_p^.rwflag_ext := arg_p^.exp_p^.rwflag; {get access permission for this argument}
  if argt_p <> nil                     {advance to next argument template}
    then argt_p := argt_p^.next_p;
  goto next_arg;                       {back to process next call argument}
{
*   All done handling call arguments.  These are all filled in if they exist
*   at all.
*
*   Now verify that the call matches the procedure template.
}
done_args:                             {definately done processing call arguments}
  sst_routines_match (template_p^, proc_p^, stat); {check call against template}
  syo_error_abort (stat, str_rout_h, '', '', nil, 0);
  return;

var_not_rout:                          {"variable" descriptor not indicate a routine}
  syo_error (str_rout_h, 'sst_pas_read', 'not_a_procedure', nil, 0);
  end;
