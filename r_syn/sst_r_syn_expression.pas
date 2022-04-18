{   Subroutine SST_R_SYN_EXPRESSION (JTARG)
*
*   Process EXPRESSION syntax.
}
module sst_r_syn_expression;
define sst_r_syn_expression;
%include 'sst_r_syn.ins.pas';

procedure sst_r_syn_expression (       {process EXPRESSION syntax}
  in out  jtarg: jump_targets_t);      {execution block jump targets info}
  val_param;

var
  tag: sys_int_machine_t;              {tag from syntax tree}
  jt: jump_targets_t;                  {jump targets for nested routines}

label
  trerr;

begin
  if not syn_trav_next_down (syn_p^)   {down into EXPRESSION syntax}
    then goto trerr;
{
*   Temporarily skip over ITEM that always starts the expression, and get the
*   next tag.  That determines the format of the overall expression, and thereby
*   how the yes/no answer from ITEM is handled.
}
  syn_trav_push (syn_p^);              {save current syntax tree position}

  if syn_trav_next(syn_p^) <> syn_tent_sub_k {go to ITEM tree entry}
    then goto trerr;
  tag := syn_trav_next_tag (syn_p^);   {get tag after ITEM}

  syn_trav_pop (syn_p^);               {restore position to before ITEM}
{
*   TAG is the next tag after ITEM.  The syntax tree position is before ITEM.
}
  case tag of                          {what is the format of this expression ?}
{
****************************************
*
*   Expression form is:
*   ITEM EXPRESSION
}
1: begin
  sst_r_syn_jtarg_sub (                {make subordinate jump targets for ITEM}
    jtarg,                             {parent jump targets}
    jt,                                {new subordinate targets}
    lab_fall_k,                        {fall thru on YES}
    lab_same_k);                       {same as parent on NO}
  sst_r_syn_item (jt);                 {process subordinate ITEM syntax}
  sst_r_syn_jtarg_here (jt);           {define jump target labels here}

  if syn_trav_next(syn_p^) <> syn_tent_tag_k {skip over the tag}
    then goto trerr;

  sst_r_syn_expression (jtarg);        {process subordinate EXPRESSION syntax}
  sst_r_syn_jtarg_goto (jtarg, [jtarg_yes_k, jtarg_no_k]);
  end;
{
****************************************
*
*   Expression form is:
*   ITEM .or EXPRESSION
}
2: begin
  sst_r_syn_jtarg_sub (                {make subordinate jump targets for ITEM}
    jtarg,                             {parent jump targets}
    jt,                                {new subordinate targets}
    lab_same_k,                        {to parent on YES}
    lab_fall_k);                       {continue here on NO}
  sst_r_syn_item (jt);                 {process subordinate ITEM syntax}
  sst_r_syn_jtarg_here (jt);           {define jump target labels here}

  if syn_trav_next(syn_p^) <> syn_tent_tag_k {skip over the tag}
    then goto trerr;

  sst_r_syn_expression (jtarg);        {process subordinate EXPRESSION syntax}
  sst_r_syn_jtarg_goto (jtarg, [jtarg_yes_k, jtarg_no_k]);
  end;
{
****************************************
*
*   Expression form is:
*   ITEM
}
3: begin
  sst_r_syn_item (jtarg);              {process the item as the whole expression}
  end;
{
**************************************
*
*   Unexpected expression format tag value.
}
otherwise
    discard( syn_trav_next(syn_p^) );  {go to tree entry tag came from}
    syn_msg_tag_bomb (syn_p^, 'sst_syn_read', 'syerr_expression', nil, 0);
    end;                               {end of expression format cases}

  if not syn_trav_up(syn_p^)           {back up from EXPRESSION syntax}
    then goto trerr;
  return;
{
*   The syntax tree is not as expected.  We assume this is due to a syntax
*   error.
}
trerr:
  sys_message ('sst_syn_read', 'syerr_expression');
  syn_parse_err_show (syn_p^);
  sys_bomb;
  end;
