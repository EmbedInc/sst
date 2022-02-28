{   Private include file for SYN front end to SST.
}
%natural_alignment;
%include 'sys.ins.pas';
%include 'util.ins.pas';
%include 'string.ins.pas';
%include 'file.ins.pas';
%include 'syo.ins.pas';
%include 'sst.ins.pas';

type
  call_p_t = ^call_t;                  {pointer to data about nested syntax call}

  symbol_data_p_t = ^symbol_data_t;
  symbol_data_t = record               {data in private symbol table per symbol}
    sym_p: sst_symbol_p_t;             {pointer to SST symbol for procedure}
    name_p: string_var_p_t;            {points to SYN file symbol name}
    call_p: call_p_t;                  {points to chain of called syntaxes}
    end;

  call_t = record                      {data about one nested syntax reference}
    next_p: call_p_t;                  {points to next called syntax reference}
    data_p: symbol_data_p_t;           {points to hash entry data of called symbol}
    end;

  jflag_k_t = (                        {independent flags in jump target descriptor}
    jflag_fall_k,                      {fall thru, no jump required}
    jflag_mfset_k,                     {MFLAG variable must be set before jump}
    jflag_indir_k);                    {indirect, resolve with INDIR_P field}
  jflag_t = set of jflag_k_t;

  jtarg_k_t = (                        {all the possible jump target types}
    jtarg_yes_k,                       {syntax matched}
    jtarg_no_k,                        {syntax didn't match}
    jtarg_err_k);                      {error end of syntax on reparse}
  jtarg_t = set of jtarg_k_t;

  jump_target_p_t = ^jump_target_t;
  jump_target_t = record               {describes where execution should jump to}
    flags: jflag_t;                    {set of modifier flags}
    case integer of
      1:(lab_p: sst_symbol_p_t);       {points to label sym, optional for fall thru}
      2:(indir_p: jump_target_p_t);    {points to real jump target descriptor to use}
    end;

  jump_targets_t = record              {jump targets for each possible block end}
    case integer of
      1: (
        yes: jump_target_t;            {syntax matched}
        no: jump_target_t;             {syntax did not match}
        err: jump_target_t);           {error end of syntax encountered on reparse}
      2: (
        ar: array[firstof(jtarg_k_t)..lastof(jtarg_k_t)] of jump_target_t);
    end;

var (sst_r_syo)
  table_sym: string_hash_handle_t;     {handle to SYN symbols table}
  seq_mflag: sys_int_machine_t;        {sequence number of next MFLAG variable}
  seq_label: sys_int_machine_t;        {sequence number of next YES label}
  seq_subr: sys_int_machine_t;         {sequence number of next syntax subroutine}
  seq_int: sys_int_machine_t;          {sequence number of next integer variable}
  lab_fall_k: sst_symbol_p_t;          {"constant" for fall thru jump target}
  lab_same_k: sst_symbol_p_t;          {"constant" for no change to jump target}
  prefix: string_var32_t;              {prefix for default subroutine names}
  def_syo_p: symbol_data_p_t;          {points to data about symbol curr defining}
{
*   Pointers to pre-defined subroutines we may want to reference.
}
  sym_start_routine_p: sst_symbol_p_t; {pnt to SYO_P_START_ROUTINE symbol}
  sym_end_routine_p: sst_symbol_p_t;   {pnt to SYO_P_END_ROUTINE symbol}
  sym_cpos_push_p: sst_symbol_p_t;     {pnt to SYO_P_CPOS_PUSH symbol}
  sym_cpos_pop_p: sst_symbol_p_t;      {pnt to SYO_P_CPOS_POP symbol}
  sym_tag_start_p: sst_symbol_p_t;     {pnt to SYO_P_TAG_START symbol}
  sym_tag_end_p: sst_symbol_p_t;       {pnt to SYO_P_TAG_END symbol}
  sym_charcase_p: sst_symbol_p_t;      {pnt to SYO_P_CHARCASE symbol}
  sym_get_ichar_p: sst_symbol_p_t;     {pnt to SYO_P_GET_ICHAR symbol}
  sym_test_eod_p: sst_symbol_p_t;      {pnt to SYO_P_TEST_EOD symbol}
  sym_test_eof_p: sst_symbol_p_t;      {pnt to SYO_P_TEST_EOF symbol}
  sym_test_string_p: sst_symbol_p_t;   {pnt to SYO_P_TEST_STRING symbol}
{
*   Pointers to pre-defined data types we may want to reference.
}
  sym_int_machine_t_p: sst_symbol_p_t; {pnt to SYS_INT_MACHINE_T symbol}
  sym_mflag_t_p: sst_symbol_p_t;       {pnt to SYO_MFLAG_K_T symbol}
  sym_charcase_t_p: sst_symbol_p_t;    {pnt to SYO_CHARCASE_K_T symbol}
{
*   Pointers to pre-defined constants we may want to use.
}
  sym_mflag_yes_p: sst_symbol_p_t;     {pnt to SYO_MFLAG_YES_K symbol}
  sym_mflag_no_p: sst_symbol_p_t;      {pnt to SYO_MFLAG_NO_K symbol}

  sym_charcase_down_p: sst_symbol_p_t; {pnt to SYO_CHARCASE_DOWN_K symbol}
  sym_charcase_up_p: sst_symbol_p_t;   {pnt to SYO_CHARCASE_UP_K symbol}
  sym_charcase_asis_p: sst_symbol_p_t; {pnt to SYO_CHARCASE_ASIS_K symbol}

  sym_ichar_eol_p: sst_symbol_p_t;     {pnt to SYO_ICHAR_EOL_K symbol}
  sym_ichar_eof_p: sst_symbol_p_t;     {pnt to SYO_ICHAR_EOF_K symbol}
  sym_ichar_eod_p: sst_symbol_p_t;     {pnt to SYO_ICHAR_EOD_K symbol}
{
*   Pointers to pre-defined variables we may want to use.
}
  sym_error_p: sst_symbol_p_t;         {pnt to ERROR symbol}
{
*********************************************
*
*   Private entry points for SYO front end.
}
procedure sst_r_syo_opc_assign (       {make opcode to assign symbol to variable}
  in      sym1: sst_symbol_t;          {variable to assign value to}
  in      sym2: sst_symbol_t);         {symbol with value to assign}
  extern;

procedure sst_r_syo_comp_var_int (     {create exp comparing var to integer constant}
  in      sym1: sst_symbol_t;          {variable for first term in comparison}
  in      val: sys_int_machine_t;      {integer value to compare against}
  in      op: sst_op2_k_t;             {comparison operator}
  out     exp_p: sst_exp_p_t);         {returned pointer to new expression}
  val_param; extern;

procedure sst_r_syo_comp_var_sym (     {create exp comparing var to other symbol}
  in      sym1: sst_symbol_t;          {variable for first term in comparison}
  in      sym2: sst_symbol_t;          {symbol for second term in comparison}
  in      op: sst_op2_k_t;             {comparison operator}
  out     exp_p: sst_exp_p_t);         {returned pointer to new expression}
  val_param; extern;

procedure sst_r_syo_doit (             {do SYN language front end phase}
  in      fnam: univ string_var_arg_t; {raw input file name}
  in out  gnam: univ string_var_arg_t; {returned as generic name of input file}
  out     stat: sys_err_t);            {completion status code}
  extern;

procedure sst_r_syo_goto (             {go to jump targets, as required}
  in out  jtarg: jump_targets_t;       {indicates where execution is to end up}
  in      flags: jtarg_t;              {indicates which jump targets to use}
  in      sym_mflag: sst_symbol_t);    {handle to MFLAG symbol}
  val_param; extern;

procedure sst_r_syo_int (              {make new integer variable}
  out     sym_p: sst_symbol_p_t);      {pointer to symbol descriptor of new var}
  extern;

procedure sst_r_syo_jtarget_sym (      {get or make symbol for jump target label}
  in out  jt: jump_target_t;           {descriptor for this jump target}
  out     sym_p: sst_symbol_p_t);      {will point to label symbol descriptor}
  extern;

procedure sst_r_syo_jtargets_done (    {write implicit labels created by jump targs}
  in      targ: jump_targets_t);       {jump targets descriptor now done with}
  extern;

procedure sst_r_syo_jtargets_make (    {make new jump targets from old and modifiers}
  in      targ_in: jump_targets_t;     {old jump targets}
  out     targ_out: jump_targets_t;    {resulting new jump targets}
  in      mod_yes: sst_symbol_p_t;     {modifier for YES branch}
  in      mod_no: sst_symbol_p_t;      {modifier for NO branch}
  in      mod_err: sst_symbol_p_t);    {modifier for ERR branch}
  val_param; extern;

procedure sst_r_syo_set_mflag (        {set MFLAG YES if comparison TRUE, else NO}
  in      sym1: sst_symbol_t;          {variable for first term in comparison}
  in      sym2: sst_symbol_t;          {symbol for second term in comparison}
  in      op: sst_op2_k_t;             {comparison operator}
  in      use_err: boolean;            {TRUE if OR error flag to comparison}
  in      sym_mflag: sst_symbol_t);    {handle to MFLAG symbol}
  val_param; extern;
{
*
*   Entry points for routines that have a one-to-one match with syntaxes.
}
procedure sst_r_syo_command (          {process COMMAND syntax}
  out     stat: sys_err_t);            {completion status code}
  extern;

procedure sst_r_syo_declare;           {process DECLARE syntax}
  extern;

procedure sst_r_syo_define;            {process DEFINE syntax}
  extern;

procedure sst_r_syo_expression (       {process EXPRESSION syntax}
  in out  jtarg: jump_targets_t;       {execution block jump targets info}
  in      sym_mflag: sst_symbol_t);    {desc of parent MFLAG variable symbol}
  val_param; extern;

procedure sst_r_syo_item (             {process ITEM syntax}
  in out  jtarg: jump_targets_t;       {execution block jump targets info}
  in      sym_mflag: sst_symbol_t);    {desc of parent MFLAG variable symbol}
  val_param; extern;

procedure sst_r_syo_utitem (           {process UTITEM syntax}
  in out  jtarg: jump_targets_t;       {execution block jump targets info}
  in      sym_mflag: sst_symbol_t);    {desc of parent MFLAG variable symbol}
  val_param; extern;
{
*   Entry points of routines resulting from the SYN.SYN file that we
*   call explicitly.
}
procedure sst_r_syo_sy_command (       {process top level SYN file command}
  out     mflag: syo_mflag_k_t);       {syntax matched yes/no, use SYO_MFLAG_xxx_K}
  extern;
