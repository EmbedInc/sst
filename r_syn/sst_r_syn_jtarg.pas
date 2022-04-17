{   Routines that handle jump targets.
}
module sst_r_syn_jtarg;
define sst_r_syn_jtarg_init;
define sst_r_syn_jtarg_make;
define sst_r_syn_jtarg_sym;
define sst_r_syn_jtarg_done;
define sst_r_syn_jtarg_goto;
%include 'sst_r_syn.ins.pas';
{
********************************************************************************
*
*   Subroutine SST_R_SYN_JTARG_INIT (JTARG)
*
*   Initialize the jump targets JTARG.  The target for each case set to fall
*   thru with MATCH expected to be set.
}
procedure sst_r_syn_jtarg_init (       {initialize jump targets}
  out     jtarg: jump_targets_t);      {the set of jump targets to initialize}
  val_param;

var
  j: jtarg_k_t;                        {ID for the current jump target}

begin
  for j := firstof(j) to lastof(j) do begin
    jtarg.ar[j].flags := [jflag_fall_k, jflag_mset_k];
    jtarg.ar[j].lab_p := nil;
    end;
  end;
{
********************************************************************************
*
*   Local subroutine TARG_MAKE (JTI, JTO, JMOD)
*
*   Create individual jump target JTO from template JTI with modifier JMOD.
}
procedure targ_make (
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
    jto.flags := [jflag_fall_k, jflag_mset_k];
    jto.lab_p := nil;
    return;
    end;
{
*   Modifier is explicit label.
}
  jto.flags := [jflag_mset_k];
  jto.lab_p := jmod;
  end;
{
********************************************************************************
*
*   Subroutine SST_R_SYN_JTARG_MAKE (TARG_IN, TARG_OUT, MOD_YES, MOD_NO, MOD_ERR)
*
*   Create subordinate jump targets in TARG_OUT, using the existing jump targets
*   TARG_IN as a template.  The MOD_xxx arguments specify modifications from the
*   template for the yes, no, and error cases.  The modifier arguments are SST
*   symbol pointers.  These are either actual pointers to label symbols, or one
*   of the special "symbol" pointers LAB_xxx_K in the SST_R_SYN common block.  A
*   pointer to a real label symbol indicates that label is the jump target.  The
*   special pointer values have the following meanings:
*
*     LAB_FALL_K  -  The jump target is to fall thru.  No jump is required.
*
*     LAB_SAME_K  -  The jump target will be an indirect reference to the
*       corresponding one in TARG_IN.
}
procedure sst_r_syn_jtarg_make (       {make new jump targets from old and modifiers}
  in      targ_in: jump_targets_t;     {old jump targets}
  out     targ_out: jump_targets_t;    {resulting new jump targets}
  in      mod_yes: sst_symbol_p_t;     {modifier for YES branch}
  in      mod_no: sst_symbol_p_t;      {modifier for NO branch}
  in      mod_err: sst_symbol_p_t);    {modifier for ERR branch}
  val_param;

begin
  targ_make (addr(targ_in.yes), targ_out.yes, mod_yes); {do YES case}
  targ_make (addr(targ_in.no), targ_out.no, mod_no); {do NO case}
  targ_make (addr(targ_in.err), targ_out.err, mod_err); {do ERR case}
  end;
{
********************************************************************************
*
*   Subroutine SST_R_SYN_JTARG_SYM (JT, SYM_P)
*
*   Return pointer to descriptor of label corresponding to jump target JT.  A
*   label is implicitly created, if one didn't already exist.
}
procedure sst_r_syn_jtarg_sym (        {get or make symbol for jump target label}
  in out  jt: jump_target_t;           {descriptor for this jump target}
  out     sym_p: sst_symbol_p_t);      {will point to label symbol descriptor}
  val_param;

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  jt_p: jump_target_p_t;               {pointer to base jump target descriptor}
  name: string_var32_t;                {label name}
  token: string_var32_t;               {scratch token for number conversion}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;
  stat: sys_err_t;

begin
  name.max := sizeof(name.str);        {init local var strings}
  token.max := sizeof(token.str);

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
*   No label symbol exists for this jump target.  Create one and pass back
*   the pointer to it.
}
  string_vstring (name, 'lab', 3);     {set static part of label name}
  string_f_int (token, seq_label);     {make sequence number string}
  seq_label := seq_label + 1;          {update sequence number for next time}
  string_append (name, token);         {make full label name}
  sst_symbol_new_name (name, sym_p, stat); {add symbol to symbol table}
  sys_msg_parm_vstr (msg_parm[1], name);
  sys_error_abort (stat, 'sst_syn_read', 'symbol_label_create', msg_parm, 1);

  sym_p^.symtype := sst_symtype_label_k; {fill in new label symbol descriptor}
  sym_p^.label_opc_p := nil;
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

  sst_opcode_new;                      {create label target opcode}
  sst_opc_p^.opcode := sst_opc_label_k;
  sst_opc_p^.label_sym_p := jt.lab_p;
  jt.lab_p^.label_opc_p := sst_opc_p;  {link label symbol to target opcode}
  end;
{
********************************************************************************
*
*   Subroutine SST_R_SYN_JTARG_DONE (TARG)
*
*   Close use of the jump targets TARG.  This will cause any implicitly created
*   symbols for the "fall thru" case to be tagged with the current position.
}
procedure sst_r_syn_jtarg_done (       {write implicit labels created by jump targs}
  in      targ: jump_targets_t);       {jump targets descriptor now done with}
  val_param;

var
  t: jtarg_k_t;                        {ID for current jump target}

begin
  for t := firstof(t) to lastof(t) do begin {once for each individual jump target}
    targ_here (targ.ar[t]);            {process this individual jump target}
    end;                               {back to do next jump target}
  end;
{
********************************************************************************
*
*   Subroutine SST_R_SYN_JTARG_GOTO (JTARG, FLAGS)
*
*   Make sure execution ends up as specified in the jump targets JTARG.  The
*   local MATCH variable indicates the YES versus NO choice.  The SYM library
*   ERR_END flag indicates the ERR case.  Execution is already at the right
*   place for the "fall thru" case.
}
procedure sst_r_syn_jtarg_goto (       {go to jump targets, as required}
  in out  jtarg: jump_targets_t;       {where to go for the ERR, YES, and NO cases}
  in      flags: jtarg_t);             {indicates which jump targets to use}
  val_param;

var
  jump_yes, jump_no: boolean;          {need to write conditional jumps for yes/no}

begin
{
*   Possibly write conditional jump to ERR target.
}
  if
      (jtarg_err_k in flags) and       {this jump target enabled ?}
      ( (not (jflag_fall_k in jtarg.err.flags)) or {not fall thru ?}
        (jflag_indir_k in jtarg.err.flags)) {indirect reference ?}
      then begin
    sst_opcode_new;                    {create new opcode for IF}
    sst_opc_p^.opcode := sst_opc_if_k;
    sst_opc_p^.str_h.first_char.crange_p := nil;
    sst_opc_p^.str_h.first_char.ofs := 0;
    sst_opc_p^.str_h.last_char := sst_opc_p^.str_h.first_char;
    sst_opc_p^.if_exp_p := sym_error_p;
    sst_opc_p^.if_false_p := nil;      {no FALSE case code}

    sst_opcode_pos_push (sst_opc_p^.if_true_p); {set up for writing TRUE code}

    sst_opcode_new;                    {create GOTO opcode}
    sst_opc_p^.opcode := sst_opc_goto_k;
    sst_opc_p^.str_h.first_char.crange_p := nil;
    sst_opc_p^.str_h.first_char.ofs := 0;
    sst_opc_p^.str_h.last_char := sst_opc_p^.str_h.first_char;
    sst_r_syn_jtarg_sym (              {get or make jump target symbol}
      jtarg.err,                       {jump target descriptor}
      sst_opc_p^.goto_sym_p);          {returned pointer to label symbol}

    sst_opcode_pos_pop;                {done writing TRUE case opcodes}
    end;
{
*   Handle match YES and NO cases together.  These will be written as one IF
*   statement, with cases for YES and NO.
}
  jump_yes :=                          {need to write cond jump for YES case ?}
    (jtarg_yes_k in flags) and         {this jump target enabled ?}
    ( (not (jflag_fall_k in jtarg.yes.flags)) or {not fall thru ?}
      (jflag_indir_k in jtarg.yes.flags)); {indirect reference ?}
  jump_no :=                           {need to write condi jump for NO case ?}
    (jtarg_no_k in flags) and          {this jump target enabled ?}
    ( (not (jflag_fall_k in jtarg.no.flags)) or {not fall thru ?}
      (jflag_indir_k in jtarg.no.flags)); {indirect reference ?}

  if jump_yes or jump_no then begin    {need to write IF statement ?}
    sst_opcode_new;                    {create new opcode for IF}
    sst_opc_p^.opcode := sst_opc_if_k;
    sst_opc_p^.str_h.first_char.crange_p := nil;
    sst_opc_p^.str_h.first_char.ofs := 0;
    sst_opc_p^.str_h.last_char := sst_opc_p^.str_h.first_char;
    sst_opc_p^.if_exp_p := match_exp_p; {MATCH variable value}
    sst_opc_p^.if_true_p := nil;       {init to no TRUE case code}
    sst_opc_p^.if_false_p := nil;      {init to no FALSE case code}

    if jump_yes then begin             {write jump for YES case ?}
      sst_opcode_pos_push (sst_opc_p^.if_true_p); {set up for writing TRUE code}
      sst_opcode_new;                  {create GOTO opcode}
      sst_opc_p^.opcode := sst_opc_goto_k;
      sst_opc_p^.str_h.first_char.crange_p := nil;
      sst_opc_p^.str_h.first_char.ofs := 0;
      sst_opc_p^.str_h.last_char := sst_opc_p^.str_h.first_char;
      sst_r_syn_jtarg_sym (            {get or make jump target symbol}
        jtarg.yes,                     {jump target descriptor}
        sst_opc_p^.goto_sym_p);        {returned pointer to label symbol}
      sst_opcode_pos_pop;              {done writing TRUE case opcodes}
      end;

    if jump_no then begin              {write jump for NO case ?}
      sst_opcode_pos_push (sst_opc_p^.if_false_p); {set up for writing TRUE code}
      sst_opcode_new;                  {create GOTO opcode}
      sst_opc_p^.opcode := sst_opc_goto_k;
      sst_opc_p^.str_h.first_char.crange_p := nil;
      sst_opc_p^.str_h.first_char.ofs := 0;
      sst_opc_p^.str_h.last_char := sst_opc_p^.str_h.first_char;
      sst_r_syn_jtarg_sym (            {get or make jump target symbol}
        jtarg.no,                      {jump target descriptor}
        sst_opc_p^.goto_sym_p);        {returned pointer to label symbol}
      sst_opcode_pos_pop;              {done writing TRUE case opcodes}
      end;
    end;
  end;
