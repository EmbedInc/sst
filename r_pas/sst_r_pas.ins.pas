{   Private include file for the Pascal front end to the source to source
*   translator.
}
%include 'sys.ins.pas';
%include 'util.ins.pas';
%include 'string.ins.pas';
%include 'file.ins.pas';
%include 'syo.ins.pas';
%include 'sst.ins.pas';

const
  max_cvar_length = 32;                {max allowable compiler variable length}

type
{
*   Intrinsic functions in Domain Pascal.
}
  ifunc_k_t = (                        {list of the Pascal intrinsic functions}
    ifunc_abs_k,                       {absolute value of arg1}
    ifunc_addr_k,                      {universal pointer to arg1}
    ifunc_arctan_k,                    {arctangent of arg1}
    ifunc_arshft_k,                    {arithmetic shift arg1 right by arg2 bits}
    ifunc_chr_k,                       {convert integer to CHAR, use low 8 bits}
    ifunc_cos_k,                       {cosine of arg1}
    ifunc_exp_k,                       {E**arg1}
    ifunc_firstof_k,                   {first possible value of arg1 data type}
    ifunc_lastof_k,                    {last possible value of arg1 data type}
    ifunc_ln_k,                        {natural log of arg1}
    ifunc_lshft_k,                     {shift arg1 left arg2 bits}
    ifunc_max_k,                       {maximum of args 1-N, two args minimum}
    ifunc_min_k,                       {minimum of args 1-N, two args minimum}
    ifunc_odd_k,                       {TRUE if arg1 is odd}
    ifunc_ord_k,                       {ordinal value of arg1}
    ifunc_pred_k,                      {next lower (predecessor) value of arg1}
    ifunc_round_k,                     {convert arg1 to integer, round to nearest}
    ifunc_rshft_k,                     {shift arg1 right arg2 bits}
    ifunc_sin_k,                       {sine of arg1}
    ifunc_sizeof_k,                    {align-padded size of arg1 data type}
    ifunc_sqr_k,                       {square of arg1}
    ifunc_sqrt_k,                      {square root of arg1}
    ifunc_succ_k,                      {next higher (successor) value of arg1}
    ifunc_trunc_k,                     {convert arg1 to integer, round towards zero}
    ifunc_xor_k,                       {excluse or of args 1-N, two args minimum}
{
*   Other intrinsic functions that are "extensions" to Domain Pascal.
}
    ifunc_alignof_k,                   {minimum alignment needed by arg1 data type}
    ifunc_arctan2_k,                   {arctangent of arg1/arg2}
    ifunc_offset_k,                    {machine address offset of field in record}
    ifunc_shift_k,                     {shift arg1 right arg2 bits, arg2 signed}
    ifunc_szchar_k,                    {num chars storable in arg1 data type}
    ifunc_szmin_k,                     {unpadded size of arg1 data type}
    ifunc_setof_k,                     {creates explicitly typed set expression}
{
*   Temporary intrinsic functions that help test the translator.
}
    ifunc_val_k);                      {returns argument value as direct constant}

  top_block_k_t = (                    {what kind of structure is top nested block}
    top_block_none_k,                  {undetermined, or not in top block yet}
    top_block_prog_k,                  {top block is PROGRAM}
    top_block_module_k);               {top block is MODULE}

  sst_sym_front_t = record             {data for our private symbols}
    ifunc: ifunc_k_t;
    end;

var (sst_r_pas)
  error_syo_found: boolean;            {TRUE on syntax error}
  addr_of: boolean;                    {TRUE if processing ADDR function argument}
  top_block: top_block_k_t;            {what kind of structure is top nested block}
  nest_level: sys_int_machine_t;       {nesting level, module/program = 1}
{
*   Pointers to data type definition blocks for the PASCAL pre-defined data types.
}
  dtype_i16_p: sst_dtype_p_t;          {INTEGER16}
  dtype_i32_p: sst_dtype_p_t;          {INTEGER32}
  dtype_str_p: sst_dtype_p_t;          {STRING}
{
*   Entry point declarations.
}
procedure sst_r_pas_data_type (        {process DATA_TYPE}
  in out  dtype_p: sst_dtype_p_t);     {returned pointer to new or reused data type}
  extern;

procedure sst_r_pas_dtype_record (     {process RECORD_DATA_TYPE}
  in out  d: sst_dtype_t);             {returned pointer to newly created data type}
  extern;

procedure sst_r_pas_doit (             {read input source code into in-memory data}
  in      fnam: univ string_var_arg_t; {raw input file name}
  in out  gnam: univ string_var_arg_t; {returned as generic name of input file}
  out     stat: sys_err_t);            {completion status code}
  extern;

procedure sst_r_pas_exp (              {create compiled expression from input stream}
  in      exp_str_h: syo_string_t;     {SYO string handle for whole EXPRESSION syntax}
  in      nval_err: boolean;           {unknown value at compile time is err if TRUE}
  out     exp_p: sst_exp_p_t);         {returned pointer to new expression def}
  extern;

procedure sst_r_pas_exp_eval (         {find constant value of EXPRESSION syntax}
  out     val: sst_var_value_t);       {value of EXPRESSION}
  extern;

procedure sst_r_pas_exp_sym (          {point to symbol that is EXPRESSION}
  out     sym_p: sst_symbol_p_t);      {error if EXPRESSION is not just one symbol}
  extern;

procedure sst_r_pas_exp_term (         {read and process next term in expression}
  in      term_str_h: syo_string_t;    {SYO string handle for whole term}
  in      nval_err: boolean;           {unknown value at compile time is err if TRUE}
  out     term: sst_exp_term_t);       {term descriptor to fill in}
  extern;

procedure sst_r_pas_ifunc (            {process intrinsic function call}
  in      sym: sst_symbol_t;           {symbol descriptor for intrinsic function}
  in out  term: sst_exp_term_t);       {filled in descriptor for term in expression}
  extern;

procedure sst_r_pas_integer (          {read UNSIGNED_LIT_INTEGER and return value}
  out     ival: sys_int_max_t);        {returned integer value}
  extern;

procedure sst_r_pas_item (             {create compiled item from input stream}
  out     term: sst_exp_term_t);       {expression term descriptor to fill in}
  extern;

procedure sst_r_pas_item_eval (        {find constant value of ITEM syntax}
  out     val: sst_var_value_t);       {returned value of ITEM}
  extern;

procedure sst_r_pas_lit_string (       {read LIT_STRING syntax and return string}
  in out  str: univ string_var_arg_t); {returned string}
  extern;

procedure sst_r_pas_preproc (          {pre-processor before syntaxer interpretation}
  out     line_p: syo_line_p_t;        {points to descriptor for line chars are from}
  out     start_char: sys_int_machine_t; {starting char within line, first = 1}
  out     n_chars: sys_int_machine_t); {number of characters returned by this call}
  extern;

procedure sst_r_pas_preproc_init;      {init our pre-syntaxer processor}
  extern;

procedure sst_r_pas_proc_args (        {process PARAMETER_DECLARATION syntax}
  in out  proc: sst_proc_t);           {top level procedure descriptor}
  extern;

procedure sst_r_pas_raw_sment;         {build opcodes from RAW_STATEMENT syntax}
  extern;

procedure sst_r_pas_routine (          {create routine descriptor}
  in      str_rout_h: syo_string_t;    {string handle to whole call}
  in      v: sst_var_t;                {"variable" descriptor for routine name}
  in      args_here: boolean;          {TRUE if FUNCTION_ARGUMENTS syntax exists}
  out     proc_p: sst_proc_p_t);       {will point to new routine descriptor}
  extern;

procedure sst_r_pas_statement (        {process STATEMENT syntax}
  out     stat: sys_err_t);
  extern;

procedure sst_r_pas_statements;        {build opcodes from STATEMENTS syntax}
  extern;

procedure sst_r_pas_sment_case (       {CASE statement, inside RAW_STATEMENT syntax}
  in      str_all_h: syo_string_t);    {string handle for whole CASE statement}
  extern;

procedure sst_r_pas_sment_const;       {process CONST_STATEMENT syntax}
  extern;

procedure sst_r_pas_sment_define;      {process DEFINE_STATEMENT syntax}
  extern;

procedure sst_r_pas_sment_label;       {process LABEL_STATEMENT syntax}
  extern;

procedure sst_r_pas_sment_module (     {proces MODULE_STATEMENT syntax}
  in      str_mod_h: syo_string_t);    {string handle to MODULE_STATEMENT syntax}
  extern;

procedure sst_r_pas_sment_prog (       {process PROGRAM_STATEMENT syntax}
  in      str_prog_h: syo_string_t);   {string handle to PROGRAM_STATEMENT syntax}
  extern;

procedure sst_r_pas_sment_rout (       {process ROUTINE_HEADING syntax}
  in      str_all_h: syo_string_t);    {string handle for whole statement}
  extern;

procedure sst_r_pas_sment_type;        {process TYPE_STATEMENT syntax}
  extern;

procedure sst_r_pas_sment_var;         {process VAR_STATEMENT syntax}
  extern;

procedure sst_r_pas_syn_pad (          {implements PAD syntax}
  out     mflag: syo_mflag_k_t);       {syntax matched yes/no, use SYO_MFLAG_xxx_K}
  extern;

procedure sst_r_pas_syn_type (         {run TYPE_SUBSTATEMENT syntax}
  out     mflag: syo_mflag_k_t);       {syntax matched yes/no, use SYO_MFLAG_xxx_K}
  extern;

procedure sst_r_pas_syn_var (          {run VAR_SUBSTATEMENT syntax}
  out     mflag: syo_mflag_k_t);       {syntax matched yes/no, use SYO_MFLAG_xxx_K}
  extern;

procedure sst_r_pas_var_init (         {process VAR_INITIALIZER syntax}
  in      dtype: sst_dtype_t;          {data type that init value must match}
  out     exp_p: sst_exp_p_t);         {returned pointing to initial value expression}
  extern;

procedure sst_r_pas_variable (         {process VARIABLE syntax}
  out     var_p: sst_var_p_t);         {returned pointer to VAR descriptor}
  extern;

procedure sst_r_pas_vparam (           {apply VAL_PARAM to routine template}
  in out  proc: sst_proc_t);           {routine descriptor that will have args fixed}
  extern;

procedure sst_r_pas_write;             {process WRITE and WRITELN statements}
  extern;
