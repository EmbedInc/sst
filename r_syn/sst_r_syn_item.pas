{   Subroutine SST_R_SYN_ITEM (JTARG)
*
*   Process ITEM syntax.
}
module sst_r_syn_item;
define sst_r_syn_item;
%include 'sst_r_syn.ins.pas';

procedure sst_r_syn_item (             {process ITEM syntax}
  in out  jtarg: jump_targets_t);      {execution block jump targets info}
  val_param;

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  tag: sys_int_machine_t;              {tag from syntax tree}
  jt: jump_targets_t;                  {jump targets for nested routines}
  itag: sys_int_machine_t;             {tag value if item is tagged}
  token: string_var32_t;               {scratch token for number conversion}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;
  stat: sys_err_t;

label
  trerr;

begin
  token.max := sizeof(token.str);      {init local var string}

  if not syn_trav_next_down (syn_p^)   {down into ITEM syntax}
    then goto trerr;
{
*   Temporarily skip over UNTAGGED_ITEM that always starts the item, and get the
*   next tag.  That determines the format of the overall item, and thereby
*   whether to create a tag or not.
}
  syn_trav_push (syn_p^);              {save current syntax tree position}
  if syn_trav_next(syn_p^) <> syn_tent_sub_k {go to UNTAGGED_ITEM tree entry}
    then goto trerr;
  tag := syn_trav_next_tag (syn_p^);   {get tag after UNTAGGED_ITEM}
{
*   TAG is the next tag after UNTAGGED_ITEM.  The syntax tree position is at the
*   tag, but the position before UNTAGGED_ITEM is on the stack.
}
  case tag of                          {is the item tagged or not ?}
{
**************************************
*
*   Item is tagged.
}
1: begin
  syn_trav_tag_string (syn_p^, token); {get the tagged string}
  string_t_int (token, itag, stat);    {make tag value in ITAG}
  if sys_error(stat) then begin
    sys_msg_parm_vstr (msg_parm[1], token);
    syn_error_bomb (syn_p^, stat, 'sst_syn_read', 'tag_string_bad', msg_parm, 1);
    end;
  if itag < 1 then begin               {invalid tag value ?}
    sys_msg_parm_int (msg_parm[1], itag);
    syn_msg_pos_bomb (syn_p^, 'sst_syn_read', 'tag_val_bad', msg_parm, 1);
    end;
  syn_trav_pop (syn_p^);               {restore position to UNTAGGED_ITEM}

  sst_call (sym_tag_start_p^);         {write call to start tag}
  sst_r_syn_arg_syn;                   {add SYN argument}
  sst_call_arg_int (sst_opc_p^, itag); {pass the tag value}

  sst_r_syn_jtarg_sub (                {make subordinate jump targets for UNTAGGED_ITEM}
    jtarg,                             {parent jump targets}
    jt,                                {new subordinate targets}
    lab_fall_k,                        {fall thru on YES}
    lab_fall_k);                       {fall thru on NO}
  sst_r_syn_utitem (jt);               {process UNTAGGED_ITEM syntax}
  sst_r_syn_jtarg_here (jt);           {define jump target labels here}

  sst_call (sym_tag_end_p^);           {write call to end tag}
  sst_r_syn_arg_syn;                   {add SYN argument}
  sst_r_syn_arg_match;                 {pass MATCH}

  sst_r_syn_jtarg_goto (jtarg, [jtarg_yes_k, jtarg_no_k]);
  end;
{
**************************************
*
*   Item is untagged.
*
*   All other unexpected tag values are also processed here.  Unexpected tags
*   are most likely due to a syntax error that caused the ITEM type tag not to
*   be created.  Continuing with the UNTAGGED_ITEM will allow processing up to
*   the error end of the syntax tree, which results in the best possible error
*   message.
}
otherwise
    syn_trav_pop (syn_p^);             {restore position to UNTAGGED_ITEM}
    sst_r_syn_utitem (jtarg);          {ITEM resolves to just this UNTAGGED_ITEM}
    end;
{
**************************************
}
  if not syn_trav_up(syn_p^)           {back up from UNTAGGED_ITEM syntax}
    then goto trerr;
  return;
{
*   The syntax tree is not as expected.  We assume this is due to a syntax
*   error.
}
trerr:
  sys_message ('sst_syn_read', 'syerr_item');
  syn_parse_err_show (syn_p^);
  sys_bomb;
  end;
