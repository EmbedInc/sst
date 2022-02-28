{   Subroutine SST_R_SYO_JTARGETS_MAKE (TARG_IN, TARG_OUT, MOD_YES, MOD_NO, MOD_ERR)
*
*   Create subordinate jump targets in TARG_OUT, using the existing jump targets
*   TARG_IN as a template.  The MOD_xxx arguments specify modifications from
*   the template for the yes, no, and error cases.  The modifier arguments
*   are SST symbol pointers.  These are either actual pointers to label
*   symbols, or one of the special "symbol" pointers LAB_xxx_K in the
*   SST_R_SYO common block.  A pointer to a real label symbol indicates that
*   label is the jump target.  The special pointer values have the following
*   meanings:
*
*     LAB_FALL_K  -  The jump target is to fall thru.  No jump is required.
*
*     LAB_SAME_K  -  The jump target will be an indirect reference to the
*       corresponding one in TARG_IN.
}
module sst_r_syo_jtargets_make;
define sst_r_syo_jtargets_make;
%include 'sst_r_syo.ins.pas';

procedure sst_r_syo_jtargets_make (    {make new jump targets from old and modifiers}
  in      targ_in: jump_targets_t;     {old jump targets}
  out     targ_out: jump_targets_t;    {resulting new jump targets}
  in      mod_yes: sst_symbol_p_t;     {modifier for YES branch}
  in      mod_no: sst_symbol_p_t;      {modifier for NO branch}
  in      mod_err: sst_symbol_p_t);    {modifier for ERR branch}
  val_param;
{
**********************************
*
*   Local subroutine DO_TARGET (JTI, JTO, JMOD)
*
*   Create individual jump target JTO from template JTI with modifier JMOD.
}
procedure do_target (
  in      jti_p: jump_target_p_t;      {pointer to template jump target}
  out     jto: jump_target_t;          {output jump_target}
  in      jmod: sst_symbol_p_t);       {modifier to apply from template to output}
  val_param;

begin
{
*   Handle "same" modifier case.
}
  if jmod = lab_same_k then begin      {modifier is "same" ?}
    jto.flags := jti_p^.flags + [jflag_indir_k];
    jto.indir_p := jti_p;
    return;
    end;
{
*   Handle "fall thru" modifier case.
}
  if jmod = lab_fall_k then begin      {modifier is "fall thru" ?}
    jto.flags := [jflag_fall_k, jflag_mfset_k];
    jto.lab_p := nil;
    return;
    end;
{
*   Modifier is explicit label.
}
  jto.flags := [jflag_mfset_k];
  jto.lab_p := jmod;
  end;
{
**********************************
*
*   Start of main routine.
}
begin
  do_target (addr(targ_in.yes), targ_out.yes, mod_yes); {do YES case}
  do_target (addr(targ_in.no), targ_out.no, mod_no); {do NO case}
  do_target (addr(targ_in.err), targ_out.err, mod_err); {do ERR case}
  end;
