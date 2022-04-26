{   Private include file for SYN front end to SST.
}
%include 'sys.ins.pas';
%include 'util.ins.pas';
%include 'string.ins.pas';
%include 'file.ins.pas';
%include 'fline.ins.pas';
%include 'syn.ins.pas';
%include 'syo.ins.pas';
%include 'sst.ins.pas';

const
  sst_r_syn_nbuck_k = 64;              {number of buckets in syntax constr names table}

type
  call_p_t = ^call_t;                  {pointer to data about nested syntax call}

  symbol_data_p_t = ^symbol_data_t;
  symbol_data_t = record               {data in private symbol table per symbol}
    name_p: string_var_p_t;            {points to SYN file symbol name}
    sym_p: sst_symbol_p_t;             {pointer to SST symbol for procedure}
    call_p: call_p_t;                  {points to chain of called syntaxes}
    end;

  call_t = record                      {data about one nested syntax reference}
    next_p: call_p_t;                  {points to next called syntax reference}
    data_p: symbol_data_p_t;           {points to hash entry data of called symbol}
    end;

  jflag_k_t = (                        {independent flags in jump target descriptor}
    jflag_fall_k,                      {fall thru, no jump required}
    jflag_indir_k);                    {indirect, resolve with INDIR_P field}
  jflag_t = set of jflag_k_t;

  jump_target_p_t = ^jump_target_t;
  jump_target_t = record               {describes where execution should jump to}
    flags: jflag_t;                    {set of modifier flags}
    case integer of
      1:(lab_p: sst_symbol_p_t);       {points to label sym, optional for fall thru}
      2:(indir_p: jump_target_p_t);    {points to real jump target descriptor to use}
    end;

  jump_targets_t = record              {jump targets for each possible block end}
    yes: jump_target_t;                {where to go on syntax matched}
    no: jump_target_t;                 {where to go on syntax did not match}
    end;

  jtarg_k_t = (                        {all the possible jump target types}
    jtarg_yes_k,                       {syntax matched}
    jtarg_no_k);                       {syntax didn't match}
  jtarg_t = set of jtarg_k_t;

var (sst_r_syn)
  syn_p: syn_p_t;                      {points to our SYN library use state}
  table_sym: string_hash_handle_t;     {handle to SYN symbols symbol table}
  prefix: string_var32_t;              {prefix for default subroutine names}
  seq_subr: sys_int_machine_t;         {sequence number of next syntax subroutine}
  seq_label: sys_int_machine_t;        {sequence number of next YES label}
  seq_int: sys_int_machine_t;          {sequence number of next integer variable}
  lab_fall_k: sst_symbol_p_t;          {"constant" for fall thru jump target}
  lab_same_k: sst_symbol_p_t;          {"constant" for no change to jump target}
{
*   Pointers to symbols, variables, expressions, and the like in the current
*   syntax parsing function being built.  These are set by SST_R_SYN_DEFINE when
*   the parsing function is first created.
}
  def_syn_p: symbol_data_p_t;          {to sym data for syntax being defined}
  exp_syn_p: sst_exp_p_t;              {pnt to expression for SYN call argument}
  sym_error_p: sst_exp_p_t;            {pnt to exp for error reparse hit end}
  match_var_p: sst_var_p_t;            {pnt to local MATCH var in curr subroutine}
  match_exp_p: sst_exp_p_t;            {pnt to expression for reading MATCH value}
  match_not_exp_p: sst_exp_p_t;        {pnt to expression for NOT MATCH}
  label_err_p: sst_symbol_p_t;         {pnt to error case label, NIL until used}
  var_ichar_p: sst_var_p_t;            {pnt to SYN_P_ICHAR variable reference}
  exp_ichar_p: sst_exp_p_t;            {pnt to exp for SYN_P_ICHAR return value}
{
*   Pointers to pre-defined subroutines we may want to reference.
}
  sym_constr_start_p: sst_symbol_p_t;  {pnt to SYN_P_CONSTR_START symbol}
  sym_constr_end_p: sst_symbol_p_t;    {pnt to SYN_P_CONSTR_END symbol}
  sym_cpos_push_p: sst_symbol_p_t;     {pnt to SYN_P_CPOS_PUSH symbol}
  sym_cpos_pop_p: sst_symbol_p_t;      {pnt to SYN_P_CPOS_POP symbol}
  sym_cpos_get_p: sst_symbol_p_t;      {pnt to SYN_P_CPOS_GET symbol}
  sym_cpos_set_p: sst_symbol_p_t;      {pnt to SYN_P_CPOS_SET symbol}
  sym_tag_start_p: sst_symbol_p_t;     {pnt to SYN_P_TAG_START symbol}
  sym_tag_end_p: sst_symbol_p_t;       {pnt to SYN_P_TAG_END symbol}
  sym_ichar_p: sst_symbol_p_t;         {pnt to SYN_P_ICHAR symbol}
  sym_test_string_p: sst_symbol_p_t;   {pnt to SYN_P_TEST_STRING symbol}
  sym_test_eol_p: sst_symbol_p_t;      {pnt to SYN_P_TEST_EOL symbol}
  sym_test_eof_p: sst_symbol_p_t;      {pnt to SYN_P_TEST_EOF symbol}
  sym_test_eod_p: sst_symbol_p_t;      {pnt to SYN_P_TEST_EOD symbol}
  sym_charcase_p: sst_symbol_p_t;      {pnt to SYN_P_CHARCASE symbol}
{
*   Pointers to pre-defined data types we may want to reference.
}
  sym_syn_t_p: sst_symbol_p_t;         {pnt to SYN library use state symbol}
  sym_charcase_t_p: sst_symbol_p_t;    {pnt to SYN_CHARCASE_K_T symbol}
  sym_int_p: sst_symbol_p_t;           {pnt to machine integer data type}
{
*   Pointers to pre-defined constants we may want to use.
}
  sym_charcase_down_p: sst_symbol_p_t; {pnt to SYN_CHARCASE_DOWN_K symbol}
  sym_charcase_up_p: sst_symbol_p_t;   {pnt to SYN_CHARCASE_UP_K symbol}
  sym_charcase_asis_p: sst_symbol_p_t; {pnt to SYN_CHARCASE_ASIS_K symbol}

  sym_ichar_eol_p: sst_symbol_p_t;     {pnt to SYN_ICHAR_EOL_K symbol}
  sym_ichar_eof_p: sst_symbol_p_t;     {pnt to SYN_ICHAR_EOF_K symbol}
  sym_ichar_eod_p: sst_symbol_p_t;     {pnt to SYN_ICHAR_EOD_K symbol}
{
*   Expressions with fixed values we will use.
}
  exp_true_p: sst_exp_p_t;             {pnt to expression with fixed TRUE value}
  exp_false_p: sst_exp_p_t;            {pnt to expression with fixed FALSE value}
{
*********************************************
*
*   Private entry points for SYN front end.
}
procedure sst_r_syn_arg_match;         {add MATCH as next call argument}
  val_param; extern;

procedure sst_r_syn_arg_syn;           {add SYN as next call argument}
  val_param; extern;

procedure sst_r_syn_assign_exp (       {make opcode to assign expression to variable}
  in      var v: sst_var_t;            {variable to assign value to}
  in      var exp: sst_exp_t);         {expression to assign to the variable}
  val_param; extern;

procedure sst_r_syn_assign_match (     {make opcode to assign true/false to MATCH}
  in      tf: boolean);                {the value to assign to MATCH}
  val_param; extern;

function sst_r_syn_char_get (          {get the result of the SYN CHAR syntax}
  out     ccode: sys_int_machine_t)    {0-N character code}
  :boolean;                            {success, no syntax error encountered}
  val_param; extern;

procedure sst_r_syn_comp_var_int (     {create exp comparing var to integer constant}
  in      sym: sst_symbol_t;           {variable for first term in comparison}
  in      ival: sys_int_machine_t;     {integer value to compare against}
  in      op: sst_op2_k_t;             {comparison operator}
  out     exp_p: sst_exp_p_t);         {returned pointer to new expression}
  val_param; extern;

procedure sst_r_syn_doit (             {do SYN language front end phase}
  in      fnam: univ string_var_arg_t; {raw input file name}
  in out  gnam: univ string_var_arg_t; {returned as generic name of input file}
  out     stat: sys_err_t);            {completion status code}
  extern;

procedure sst_r_syn_err_check;         {check for err reparse end in syntax checking code}
  val_param; extern;

function sst_r_syn_exp_ichar:          {create SST expression for SYN_P_ICHAR value}
  sst_exp_p_t;                         {pointer to the new expression descriptor}
  val_param; extern;

function sst_r_syn_exp_pfunc (         {create expression for value of syn parsing func}
  in var  sym: sst_symbol_t)           {syntax parsing funtion symbol}
  :sst_exp_p_t;                        {pointer to expression referencing the function}
  val_param; extern;

procedure sst_r_syn_int (              {make new interger variable}
  out     sym_p: sst_symbol_p_t);      {pointer to symbol descriptor of new var}
  val_param; extern;

procedure sst_r_syn_jtarg_goto (       {go to jump targets, as required}
  in out  jtarg: jump_targets_t;       {where to go for each case}
  in      flags: jtarg_t);             {which cases to write code for}
  val_param; extern;

procedure sst_r_syn_jtarg_here (       {write implicit labels created by jump targs}
  in      jtarg: jump_targets_t);      {jump targets descriptor now done with}
  val_param; extern;

procedure sst_r_syn_jtarg_init (       {initialize jump targets}
  out     jtarg: jump_targets_t);      {the set of jump targets to initialize}
  val_param; extern;

procedure sst_r_syn_jtarg_sub (        {make new jump targets from old and modifiers}
  in var  jtarg: jump_targets_t;       {old jump targets}
  out     subtarg: jump_targets_t;     {resulting new jump targets}
  in      lab_yes_p: sst_symbol_p_t;   {label to jump to for YES case, or LAB_xxx_K}
  in      lab_no_p: sst_symbol_p_t);   {label to jump to for NO case, or LAB_xxx_K}
  val_param; extern;

procedure sst_r_syn_jtarg_sym (        {get or make symbol for jump target label}
  in out  jt: jump_target_t;           {descriptor for this jump target}
  out     sym_p: sst_symbol_p_t);      {returned pointing to jump label symbol}
  val_param; extern;

procedure sst_r_syn_sym_add (          {add new symbol to table, bomb if already exist}
  in      name: univ string_var_arg_t; {name of symbol to add, case-insensitive}
  out     data_p: symbol_data_p_t);    {pointer to data for new symbol in the table}
  val_param; extern;

procedure sst_r_syn_sym_called (       {symbol is called from curr syn parsing function}
  in      name: univ string_var_arg_t); {name of called symbol, case-insensitive}
  val_param; extern;

procedure sst_r_syn_sym_delete;        {delete the symbol table, dealloc resources}
  val_param; extern;

procedure sst_r_syn_sym_init;          {create and initialize the SYN symbol table}
  val_param; extern;

procedure sst_r_syn_sym_lookup (       {look up name in table, bomb if not found}
  in      name: univ string_var_arg_t; {name of symbol to look up, case-insensitive}
  out     data_p: symbol_data_p_t);    {pointer to symbol data}
  val_param; extern;

procedure sst_r_syn_sym_loop_init;     {init loop over sym table entries, one thread only}
  val_param; extern;

function sst_r_syn_sym_loop_next (     {get next symbol table entry, one thread only}
  out     data_p: symbol_data_p_t)     {pointer to next entry, NIL at end}
  :boolean;                            {returning with entry, not hit end of table}
  val_param; extern;

function sst_r_syn_var_proc (          {make "variable" reference to a procedure}
  in var  sym: sst_symbol_t)           {the procedure symbol}
  :sst_var_p_t;                        {pointer to the new variable reference}
  val_param; extern;
{
*
*   Entry points for routines that have a one-to-one match with syntaxes.
}
procedure sst_r_syn_command (          {process COMMAND syntax}
  out     stat: sys_err_t);            {completion status code}
  extern;

procedure sst_r_syn_declare;           {process DECLARE syntax}
  extern;

procedure sst_r_syn_define;            {process DEFINE syntax}
  extern;

procedure sst_r_syn_expression (       {process EXPRESSION syntax}
  in out  jtarg: jump_targets_t);      {execution block jump targets info}
  val_param; extern;

procedure sst_r_syn_item (             {process ITEM syntax}
  in out  jtarg: jump_targets_t);      {execution block jump targets info}
  val_param; extern;

procedure sst_r_syn_utitem (           {process UNTAGGED_ITEM syntax}
  in out  jtarg: jump_targets_t);      {execution block jump targets info}
  val_param; extern;
