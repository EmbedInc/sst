{   Subroutine SST_R_SYN_ITEM (JTARG, SYM_MFLAG)
*
*   Process ITEM syntax.
}
module sst_r_syn_item;
define sst_r_syn_item;
%include 'sst_r_syn.ins.pas';

procedure sst_r_syn_item (             {process ITEM syntax}
  in out  jtarg: jump_targets_t;       {execution block jump targets info}
  in      sym_mflag: sst_symbol_t);    {desc of parent MFLAG variable symbol}
  val_param;

var
  tag: sys_int_machine_t;              {tag from syntax tree}
  str_h: syn_string_t;                 {handle to string from input file}
  jt: jump_targets_t;                  {jump targets for nested routines}
  itag: sys_int_machine_t;             {tag value if item is tagged}
  token: string_var32_t;               {scratch token for number conversion}
  stat: sys_err_t;

begin
  token.max := sizeof(token.str);      {init local var string}
  syn_level_down;                      {down into ITEM syntax}

  syn_push_pos;                        {save position at start of ITEM}
  syn_get_tag_msg (                    {tag for whether item is tagged or not}
    tag, str_h, 'sst_syn_read', 'syerr_define', nil, 0);
  syn_pop_pos;                         {restore position to start of ITEM}
  case tag of
{
**************************************
*
*   Item is tagged.
}
1: begin
  syn_get_tag_string (str_h, token);   {get tag value string}
  string_t_int (token, itag, stat);    {make tag value in ITAG}
  syn_error_abort (stat, str_h, '', '', nil, 0);

  sst_call (sym_tag_start_p^);         {create call to SYN_P_TAG_START}

  sst_r_syn_jtargets_make (            {make jump targets for nested routine}
    jtarg,                             {template jump targets}
    jt,                                {output jump targets}
    lab_fall_k,                        {YES action}
    lab_fall_k,                        {NO action}
    lab_fall_k);                       {ERR action}
  sst_r_syn_utitem (jt, sym_mflag);    {process UNTAGGED_ITEM syntax}
  sst_r_syn_jtargets_done (jt);        {define any implicit labels}

  sst_call (sym_tag_end_p^);           {create call to SYN_P_TAG_END}
  sst_call_arg_var (sst_opc_p^, sym_mflag); {add MFLAG call argument}
  sst_call_arg_int (sst_opc_p^, itag); {add tag value argument}

  sst_r_syn_goto (                     {go to jump targets, as required}
    jtarg,                             {jump targets data}
    [jtarg_yes_k, jtarg_no_k, jtarg_err_k], {which targets to process}
    sym_mflag);                        {handle to MFLAG variable}
  end;
{
**************************************
*
*   Item is untagged.
}
2: begin
  sst_r_syn_utitem (jtarg, sym_mflag); {ITEM resolves to just this UNTAGGED_ITEM}
  end;
{
**************************************
*
*   Unexpected expression format tag value.
}
otherwise
    syn_error_tag_unexp (tag, str_h);
    end;                               {end of item format cases}

  syn_level_up;                        {back up from ITEM syntax}
  end;
