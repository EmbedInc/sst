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
*   Process the item that always starts the expression.
}
  sst_r_syn_jtarg_sub (                {make subordinate jump targets for ITEM}
    jtarg,                             {parent jump targets}
    jt,                                {new subordinate targets}
    lab_fall_k,                        {fall thru on YES}
    lab_fall_k);                       {fall thru on NO}
  sst_r_syn_item (jt);                 {process ITEM, set MATCH accordingly}
  sst_r_syn_jtarg_here (jt);           {define jump target labels here}
{
*   Get the next tag, and handle the remainder of this syntax according to which
*   form of expression it is.
}
  tag := syn_trav_next_tag(syn_p^);    {get tag identifying overall expression form}
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
  sst_r_syn_jtarg_goto (jt, [jtarg_no_k]); {abort on ITEM failed}
  sst_r_syn_jtarg_here (jt);           {define local jump target labels here}

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
  sst_r_syn_jtarg_goto (jt, [jtarg_no_k]); {all done if ITEM matched}
  sst_r_syn_jtarg_here (jt);           {define jump target labels here}

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
  sst_r_syn_jtarg_goto (               {ITEM was whole expression}
    jtarg, [jtarg_yes_k, jtarg_no_k]);
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
