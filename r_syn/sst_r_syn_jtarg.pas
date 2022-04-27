{   Routines that handle jump targets.
*
*   Syntax processing routines need to primarily branch on three different
*   criteria:
*
*     1  -  Furthest input stream point reached on error re-parse.
*
*     2  -  Syntax matched template.
*
*     3  -  Syntax did not match template.
*
*   A parsing routine must immediately exit with FALSE and all SYN library state
*   left as-is on case 1.  Whenever the input stream position could be advanced,
*   the end of re-parse must be checked.  If true, execution must jump to the
*   label indicated by LABEL_ERR_P.  This pointer is initialized to NIL when
*   the parsing routine is first created.  It must be filled in on the first
*   attempt to use it.  The parsing routine cleanup code in SST_R_SYN_DEFINE
*   will write the label and the error handling code follwing it when the
*   label exists.
*
*   Code that writes a syntax parsing function should call SST_R_SYN_ERR_CHECK
*   immediately after any action that could advance the input stream parsing
*   position.  SST_R_SYN_ERR_CHECK creates the error abort label if necessary,
*   and writes the conditional jump to that label on end of error re-parse
*   encountered.
*
*   The jump targets mechanism implemented in this module is to handle jumping
*   depending on whether the input stream matched the expected syntax.  This
*   always assumes case 1 has already been handled.  Put another way, hitting
*   the error re-parse end is handled as an exception.  Syntax matched yes/no
*   are normal run time cases, and is what the facilities in this module
*   support.
*
*   At the level of a whole syntax parsing routine, the routine returns TRUE
*   when the input stream matched the syntax and FALSE if not.  In the FALSE
*   case, the input stream position and syntax tree being built are restored to
*   their state on entry to the routine.
*
*   Internally in a syntax parsing routine, there may be subordinate sections
*   that handle syntax matched yes/no in private ways to ultimately derive the
*   yes/no state for the whole section.  Such sections may be nested.
*
*   The jump targets mechanism is a way to keep track of what actions should be
*   taken for each yes/no case in each nested section.  A jump targets data
*   structure, JUMP_TARGETS_T, keeps track of where to go for each syntax
*   matched yes/no case.  The options for each case are fall thru, jump to
*   a specific label, or do whatever it says in another jump target for that
*   case.
*
*   Code that writes syntax parsing functions would use the routines here in
*   the following ways:
*
*     SST_R_SYN_JTARG_INIT (JTARG)
*
*       Initializes the jump targets JTARG to fall thru for all cases.  This
*       routine should only be called to initialize the top level jump targets.
*       This is therfore called once in SST_R_SYN_DEFINE when setting up for
*       writing a syntax parsing function.  Subsequent code that writes the
*       syntax parsing function would generally not call this routine.
*
*     SST_R_SYN_JTARG_SUB (JTARG, SUBTARG, MOD_YES, MOD_NO)
*
*       Used to create jump targets for a subordinate section of code.
*
*     SST_R_SYN_JTARG_GOTO (JTARG, FLAGS)
*
*       Write the conditional GOTOs for the syntax matched yes/no cases listed
*       in FLAGS.
*
*     SST_R_SYN_JTARG_HERE (JTARG)
*
*       Writes labels, as needed, at the current position for cases indicated to
*       fall thru.  This is how labels for jump locations are written into the
*       syntax parsing function.
}
module sst_r_syn_jtarg;
define sst_r_syn_jtarg_init;
define sst_r_syn_jtarg_sub;
define sst_r_syn_jtarg_label;
define sst_r_syn_jtarg_label_define;
define sst_r_syn_jtarg_label_here;
define sst_r_syn_jtarg_label_goto;
define sst_r_syn_jtarg_sym;
define sst_r_syn_jtarg_here;
define sst_r_syn_jtarg_goto;
%include 'sst_r_syn.ins.pas';
{
********************************************************************************
*
*   Subroutine SST_R_SYN_JTARG_INIT (JTARG)
*
*   Initialize the jump targets JTARG.  The target for each case is set to fall
*   thru.
}
procedure sst_r_syn_jtarg_init (       {initialize jump targets}
  out     jtarg: jump_targets_t);      {the set of jump targets to initialize}
  val_param;

begin
  jtarg.yes.flags := [jflag_fall_k];
  jtarg.yes.lab_p := nil;
  jtarg.no.flags := [jflag_fall_k];
  jtarg.no.lab_p := nil;
  end;
{
********************************************************************************
*
*   Local subroutine TARG_SUB (JTI, JTO, JSYM_P)
*
*   Create individual jump target JTO from template JTI with modifier JSYM_P.
}
procedure targ_sub (                   {create subordinate target for one case}
  in var  jti: jump_target_t;          {template target}
  out     jto: jump_target_t;          {output target}
  in      jsym_p: sst_symbol_p_t);     {label to jump to, or LAB_xxx_K special values}
  val_param;

begin
{
*   Handle "same" modifier case.
}
  if jsym_p = lab_same_k then begin    {modifier is "same" ?}
    jto.flags := jti.flags + [jflag_indir_k]; {make indirect to input target}
    jto.indir_p := addr(jti);
    return;
    end;
{
*   Handle "fall thru" modifier case.
}
  if jsym_p = lab_fall_k then begin    {modifier is "fall thru" ?}
    jto.flags := [jflag_fall_k];
    jto.lab_p := nil;
    return;
    end;
{
*   Modifier is explicit label.
}
  jto.flags := [];
  jto.lab_p := jsym_p;
  end;
{
********************************************************************************
*
*   Subroutine SST_R_SYN_JTARG_SUB (JTARG, SUBTARG, LAB_YES_P, LAB_NO_P)
*
*   Create subordinate jump targets in SUBTARG, using the existing jump targets
*   JTARG as a template.
*
*   LAB_YES_P and LAB_NO_P point to labels to jump to for syntax matched yes and
*   no cases.  Or, these can have the following special values:
*
*     LAB_FALL_K  -  The jump target is to fall thru.  No jump is required.
*
*     LAB_SAME_K  -  The jump location in SUBTARG will be the same as in JTARG.
*       This creates an indirect reference in SUBTARG to JTARG.
}
procedure sst_r_syn_jtarg_sub (        {make new jump targets from old and modifiers}
  in var  jtarg: jump_targets_t;       {old jump targets}
  out     subtarg: jump_targets_t;     {resulting new jump targets}
  in      lab_yes_p: sst_symbol_p_t;   {label to jump to for YES case, or LAB_xxx_K}
  in      lab_no_p: sst_symbol_p_t);   {label to jump to for NO case, or LAB_xxx_K}
  val_param;

begin
  targ_sub (jtarg.yes, subtarg.yes, lab_yes_p);
  targ_sub (jtarg.no, subtarg.no, lab_no_p);
  end;
{
********************************************************************************
*
*   Subroutine SST_R_SYN_JTARG_LABEL (LAB_P)
*
*   Create a new label and return LAB_P pointing to the symbol for that label.
}
procedure sst_r_syn_jtarg_label (      {create a label symbol}
  out     lab_p: sst_symbol_p_t);      {returned pointer to the label symbol}
  val_param;

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  name: string_var32_t;                {label name}
  token: string_var32_t;               {scratch token for number conversion}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;
  stat: sys_err_t;

begin
  name.max := sizeof(name.str);        {init local var strings}
  token.max := sizeof(token.str);

  string_vstring (name, 'lab', 3);     {set static part of label name}
  string_f_int (token, seq_label);     {make sequence number string}
  seq_label := seq_label + 1;          {update sequence number for next time}
  string_append (name, token);         {make full label name}
  sst_symbol_new_name (name, lab_p, stat); {create the new symbol}
  sys_msg_parm_vstr (msg_parm[1], name);
  sys_error_abort (stat, 'sst_syn_read', 'symbol_label_create', msg_parm, 1);

  lab_p^.symtype := sst_symtype_label_k; {fill in new label symbol descriptor}
  lab_p^.label_opc_p := nil;           {opcode defining label not created yet}
  end;
{
********************************************************************************
*
*   Subroutine SST_R_SYN_JTARG_LABEL_DEFINE (LAB)
*
*   Define the existing label LAB at the current position in the code being
*   written.
}
procedure sst_r_syn_jtarg_label_define ( {define existing label at current position}
  in var  lab: sst_symbol_t);          {label symbol}
  val_param;

begin
  sst_opcode_new;                      {create label target opcode}
  sst_opc_p^.opcode := sst_opc_label_k; {opcode is a label}
  sst_opc_p^.label_sym_p := addr(lab); {point to the label symbol}
  lab.label_opc_p := sst_opc_p;        {link label symbol to defining opcode}
  end;
{
********************************************************************************
*
*   Subroutine SST_R_SYN_JTARG_LABEL_HERE (LAB_P)
*
*   Create a new label and define it at the current position in the code being
*   written.  LAB_P is returned pointing to the symbol for the new label.
}
procedure sst_r_syn_jtarg_label_here ( {create a label symbol at the current location}
  out     lab_p: sst_symbol_p_t);      {returned pointer to the label symbol}
  val_param;

begin
  sst_r_syn_jtarg_label (lab_p);       {create the new label}
  sst_r_syn_jtarg_label_define (lab_p^); {define it at the current position}
  end;
{
********************************************************************************
*
*   Subroutine SST_R_SYN_JTARG_LABEL_GOTO (LAB)
*
*   Jump to the label LAB.
}
procedure sst_r_syn_jtarg_label_goto ( {unconditionally jump to a label}
  in var  lab: sst_symbol_t);          {label to jump to}
  val_param;

begin
  sst_opcode_new;                      {create new opcode}
  sst_opc_p^.opcode := sst_opc_goto_k; {this opcode is a GOTO}
  sst_opc_p^.goto_sym_p := addr(lab);  {set pointer to label to go to}
  end;
{
********************************************************************************
*
*   Subroutine SST_R_SYN_JTARG_SYM (JT, SYM_P)
*
*   Return pointer to the label corresponding to jump target JT.  A label is
*   implicitly created, if one doesn't already exist.
}
procedure sst_r_syn_jtarg_sym (        {get or make symbol for jump target label}
  in out  jt: jump_target_t;           {descriptor for this jump target}
  out     sym_p: sst_symbol_p_t);      {returned pointing to jump label symbol}
  val_param;

var
  jt_p: jump_target_p_t;               {pointer to base jump target descriptor}

begin
  jt_p := addr(jt);                    {init base descriptor to first descriptor}
  while jflag_indir_k in jt_p^.flags do begin {this is an indirect descriptor ?}
    jt_p := jt_p^.indir_p;             {resolve one level of indirection}
    end;                               {back to resolve next level of indirection}
{
*   JT_P is pointing to the base jump target descriptor.
}
  if jt_p^.lab_p <> nil then begin     {a label symbol already exists here ?}
    sym_p := jt_p^.lab_p;              {fetch label symbol pointer from jump desc}
    return;
    end;
{
*   No label symbol exists for this jump target.  Create one and pass back SYM_P
*   pointing to it.
}
  sst_r_syn_jtarg_label (sym_p);       {create the new label}
  jt_p^.lab_p := sym_p;                {save symbol pointer in jump descriptor}
  end;
{
********************************************************************************
*
*   Local subroutine TARG_HERE (JT)
*
*   Write the label for the jump target JT in the current location, if the jump
*   target is fall thru, has a label, and is the base jump target.
}
procedure targ_here (
  in      jt: jump_target_t);          {descriptor for jump target to process}
  val_param;

begin
  if jflag_indir_k in jt.flags then return; {this is not a base jump target ?}
  if not (jflag_fall_k in jt.flags) then return; {not fall thru case ?}
  if jt.lab_p = nil then return;       {no label used for this jump target ?}

  sst_r_syn_jtarg_label_define (jt.lab_p^); {define label at this location}
  end;
{
********************************************************************************
*
*   Subroutine SST_R_SYN_JTARG_HERE (JTARG)
*
*   Write labels here for jump targets in JTARG as appropriate.
}
procedure sst_r_syn_jtarg_here (       {write implicit labels created by jump targs}
  in      jtarg: jump_targets_t);      {jump targets descriptor now done with}
  val_param;

begin
  targ_here (jtarg.yes);               {write labels as needed for each case}
  targ_here (jtarg.no);
  end;
{
********************************************************************************
*
*   Subroutine SST_R_SYN_JTARG_GOTO (JTARG, FLAGS)
*
*   Jump as specified by the jump targets JTARG.  FLAGS indicates which cases to
*   write conditional code to go to.  Cases not listed in FLAGS are ignored.
*   The local MATCH variable indicates the syntax matched yes/no condition.
}
procedure sst_r_syn_jtarg_goto (       {go to jump targets, as required}
  in out  jtarg: jump_targets_t;       {where to go for each case}
  in      flags: jtarg_t);             {which cases to write code for}
  val_param;

var
  jump_yes, jump_no: boolean;          {need to write conditional jumps for yes/no}

begin
  jump_yes :=                          {need to write cond jump for YES case ?}
    (jtarg_yes_k in flags) and         {this jump target enabled ?}
    ( (not (jflag_fall_k in jtarg.yes.flags)) or {not fall thru ?}
      (jflag_indir_k in jtarg.yes.flags)); {indirect reference ?}

  jump_no :=                           {need to write cond jump for NO case ?}
    (jtarg_no_k in flags) and          {this jump target enabled ?}
    ( (not (jflag_fall_k in jtarg.no.flags)) or {not fall thru ?}
      (jflag_indir_k in jtarg.no.flags)); {indirect reference ?}

  if not (jump_yes or jump_no) then return; {nothing to do ?}
{
*   Handle the special case of only jumping on the NO case.  The conditional
*   expression will the NOT MATCH, with the jump performed on TRUE.
}
  if jump_no and (not jump_yes) then begin {only jumping on NO case ?}
    sst_opcode_new;                    {create new opcode for IF}
    sst_opc_p^.opcode := sst_opc_if_k;
    sst_opc_p^.if_exp_p := match_not_exp_p; {NOT MATCH variable value}
    sst_opc_p^.if_false_p := nil;      {there is no FALSE case code}

    sst_opcode_pos_push (sst_opc_p^.if_true_p); {set up for writing TRUE code}
    sst_opcode_new;                    {create GOTO opcode}
    sst_opc_p^.opcode := sst_opc_goto_k;
    sst_r_syn_jtarg_sym (              {get or make jump target symbol}
      jtarg.no,                        {jump target descriptor}
      sst_opc_p^.goto_sym_p);          {returned pointer to label symbol}
    sst_opcode_pos_pop;                {done writing TRUE case opcodes}

    return;
    end;
{
*   Jumping on the YES case, and maybe also on the NO case.  The conditional
*   expression will be MATCH.
}
  sst_opcode_new;                      {create new opcode for IF}
  sst_opc_p^.opcode := sst_opc_if_k;
  sst_opc_p^.if_exp_p := match_exp_p;  {MATCH variable value}
  sst_opc_p^.if_false_p := nil;        {init to no FALSE case code}

  sst_opcode_pos_push (sst_opc_p^.if_true_p); {set up for writing TRUE code}
  sst_opcode_new;                      {create GOTO opcode}
  sst_opc_p^.opcode := sst_opc_goto_k;
  sst_r_syn_jtarg_sym (                {get or make jump target symbol}
    jtarg.yes,                         {jump target descriptor}
    sst_opc_p^.goto_sym_p);            {returned pointer to label symbol}
  sst_opcode_pos_pop;                  {done writing TRUE case opcodes}

  if jump_no then begin                {write jump for NO case ?}
    sst_opcode_pos_push (sst_opc_p^.if_false_p); {set up for writing FALSE code}
    sst_opcode_new;                    {create GOTO opcode}
    sst_opc_p^.opcode := sst_opc_goto_k;
    sst_r_syn_jtarg_sym (              {get or make jump target symbol}
      jtarg.no,                        {jump target descriptor}
      sst_opc_p^.goto_sym_p);          {returned pointer to label symbol}
    sst_opcode_pos_pop;                {done writing TRUE case opcodes}
    end;
  end;
