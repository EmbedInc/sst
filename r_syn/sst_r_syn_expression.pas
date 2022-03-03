{   Subroutine SST_R_SYN_EXPRESSION (JTARG, SYM_MFLAG)
*
*   Process EXPRESSION syntax.
}
module sst_r_syn_expression;
define sst_r_syn_expression;
%include 'sst_r_syn.ins.pas';

procedure sst_r_syn_expression (       {process EXPRESSION syntax}
  in out  jtarg: jump_targets_t;       {execution block jump targets info}
  in      sym_mflag: sst_symbol_t);    {desc of parent MFLAG variable symbol}
  val_param;

var
  tag: sys_int_machine_t;              {tag from syntax tree}
  str_h: syo_string_t;                 {handle to string from input file}
  jt: jump_targets_t;                  {jump targets for nested routines}

begin
  syo_level_down;                      {down into EXPRESSION syntax}
  syo_push_pos;                        {save current syntax position}

  syo_get_tag_msg (                    {get expression format tag}
    tag, str_h, 'sst_syo_read', 'syerr_define', nil, 0);
  syo_pop_pos;                         {restore position to start of EXPRESSION}
  case tag of
{
**************************************
*
*   Expression form is:
*   ITEM EXPRESSION
}
1: begin
  sst_r_syn_jtargets_make (            {make jump targets for nested routine}
    jtarg,                             {template jump targets}
    jt,                                {output jump targets}
    lab_fall_k,                        {YES action}
    lab_same_k,                        {NO action}
    lab_same_k);                       {ERR action}
  sst_r_syn_item (jt, sym_mflag);      {process ITEM syntax}
  sst_r_syn_jtargets_done (jt);        {define any implicit labels}

  syo_get_tag_msg (                    {get tag for nested expression}
    tag, str_h, 'sst_syo_read', 'syerr_define', nil, 0);
  sst_r_syn_expression (jtarg, sym_mflag); {process EXPRESSION after ITEM}
  end;
{
**************************************
*
*   Expression form is:
*   ITEM .or EXPRESSION
}
2: begin
  sst_r_syn_jtargets_make (            {make jump targets for nested routine}
    jtarg,                             {template jump targets}
    jt,                                {output jump targets}
    lab_same_k,                        {YES action}
    lab_fall_k,                        {NO action}
    lab_same_k);                       {ERR action}
  sst_r_syn_item (jt, sym_mflag);      {process ITEM syntax}
  sst_r_syn_jtargets_done (jt);        {define any implicit labels}

  syo_get_tag_msg (                    {get tag for nested expression}
    tag, str_h, 'sst_syo_read', 'syerr_define', nil, 0);
  sst_r_syn_expression (jtarg, sym_mflag); {process EXPRESSION after ITEM}
  end;
{
**************************************
*
*   Expression form is:
*   ITEM
}
3: begin
  sst_r_syn_item (jtarg, sym_mflag);
  end;
{
**************************************
*
*   Unexpected expression format tag value.
}
otherwise
    syo_error_tag_unexp (tag, str_h);
    end;                               {end of expression format cases}

  syo_level_up;                        {back up from EXPRESSION syntax}
  end;
