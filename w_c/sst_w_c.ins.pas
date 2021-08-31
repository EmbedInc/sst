{   Private inlude file for the C back end to the source  to source
*   translator.
*
*   This include file was originally converted from the Pascal back end include
*   file.  Therefore, some of the symbol names reflect Pascal nomenclature.
}
%natural_alignment;
%include 'sys.ins.pas';
%include 'util.ins.pas';
%include 'string.ins.pas';
%include 'file.ins.pas';
%include 'syo.ins.pas';
%include 'sst.ins.pas';

type
  decl_k_t = (                         {list of implicit declarations}
    decl_nil_k,                        {nil pointer value}
    decl_true_k,                       {logical TRUE}
    decl_false_k,                      {logical FALSE}
    decl_nullset_k,                    {empty set value}
    decl_unspec_int_k,                 {initial integer var val when unspecified}
    decl_unspec_enum_k,                {initial enumerated var val when unspecified}
    decl_unspec_float_k,               {initial floating point var val when unspec}
    decl_unspec_bool_k,                {initial boolean var val when unspecified}
    decl_unspec_char_k,                {initial char var val when unspecified}
    decl_unspec_set_k,                 {initial set var val when unspecified}
    decl_unspec_pnt_k,                 {initial pointer var val when unspecified}
    decl_stddef_k,                     {includes STDDEF.H standard header file}
    decl_stdlib_k,                     {includes STDLIB.H standard header file}
    decl_stdio_k,                      {includes STDIO.H standard header file}
    decl_string_k,                     {includes STRING.H standard header file}
    decl_math_k);                      {includes MATH.H standard header file}

  decl_set_t = set of decl_k_t;

  intr_k_t = (                         {list of intrinsic symbols we assume exist}
    intr_nil_k,                        {NIL pointer value}
    intr_true_k,                       {logical TRUE}
    intr_false_k,                      {logical FALSE}
    intr_nullset_k,                    {empty set value}
    intr_unspec_int_k,                 {initial integer var val when unspecified}
    intr_unspec_enum_k,                {initial enumerated var val when unspecified}
    intr_unspec_float_k,               {initial floating point var val when unspec}
    intr_unspec_bool_k,                {initial boolean var val when unspecified}
    intr_unspec_char_k,                {initial char var val when unspecified}
    intr_unspec_set_k,                 {initial set var val when unspecified}
    intr_unspec_pnt_k,                 {initial pointer var val when unspecified}
    intr_abs_k,                        {absolute value, integer}
{
*   Math functions.
}
    intr_atan_k,                       {arctangent, one argument}
    intr_atan2_k,                      {arctangent, two arguments}
    intr_ceil_k,                       {to integer, round toward +infinity}
    intr_cos_k,                        {cosine}
    intr_exp_k,                        {E ** arg}
    intr_fabs_k,                       {absolute value, floating point}
    intr_floor_k,                      {to integer, round toward -infinity}
    intr_log_k,                        {natural log}
    intr_pow_k,                        {arg1 ** arg2}
    intr_sin_k,                        {sine}
    intr_sqrt_k,                       {square root}
    intr_tan_k);                       {tangent}

  enclose_k_t = (                      {indicates enclose expression in ()}
    enclose_yes_k,                     {enclose in () if compound expression}
    enclose_no_k);                     {don't enclose expression in () at all}

  sment_type_k_t = (                   {type of statement currently being written}
    sment_type_declg_k,                {writing global declarations}
    sment_type_decll_k,                {writing local declarations}
    sment_type_exec_k);                {executable, within braces after declarations}

  scope_type_k_t = (                   {what type of entity owns the current scope}
    scope_type_global_k,               {root scope}
    scope_type_prog_k,                 {top level program}
    scope_type_module_k,               {module containing executable units}
    scope_type_rout_k);                {routine (subroutine or function)}

  sment_type_t =                       {any combination of the statement types}
    set of sment_type_k_t;

  frame_scope_p_t =                    {pointer to scope stack frame}
    ^frame_scope_t;

  frame_sment_p_t =                    {pointer to executable statement stack frame}
    ^frame_sment_t;

  frame_posp_p_t =                     {pointer to saved write position stack frame}
    ^frame_posp_t;

  frame_array_p_t =                    {pointer to save array context information}
    ^frame_array_t;

  frame_scope_t = record               {stack frame for each scope}
    prev_p: frame_scope_p_t;           {pointer to previous scope stack frame}
    pos_decll: sst_out_dyn_t;          {pos for new local declaration statements}
    pos_exec: sst_out_dyn_t;           {position for new executable statements}
    scope_p: sst_scope_p_t;            {points to scope descriptor for this scope}
    funcval_sym_p: sst_symbol_p_t;     {points to function return value variable}
    const_p: sst_symbol_p_t;           {points to start of const vars chain}
    scope_type: scope_type_k_t;        {what kind of structure owns this scope}
    sment_type: sment_type_k_t;        {type of statement currently being written}
    end;

  frame_sment_t = record               {stack frame for each executable statement}
    pos_before: sst_out_dyn_t;         {output position right before this statement}
    prev_p: frame_sment_p_t;           {points to previous statement frame}
    end;

  frame_posp_t = record                {stack frame to remember writing position}
    dyn_p: sst_out_dyn_p_t;            {saved dynamic output writing state pointer}
    sment_type: sment_type_k_t;        {saved current statement type}
    end;

  array_k_t = (                        {method for dealing with array identifiers}
    array_whole_k,                     {identifier represents data in whole array}
    array_pnt_whole_k,                 {identifier represents pointer to whole array}
    array_pnt_first_k);                {identifier represents pointer to first ele}

  frame_array_t = record               {context for interpreting array variables}
    addr_cnt: sys_int_machine_t;       {extra "address of" for array variables}
    mode: array_k_t;                   {mode how identifier is interpreted}
    end;

var (sst_w_c)
  frame_scope_p: frame_scope_p_t;      {points to current scope stack frame}
  frame_sment_p: frame_sment_p_t;      {points to current statement stack frame}
  frame_array_p: frame_array_p_t;      {stack frame for saved array context}
  pos_declg: sst_out_dyn_t;            {pos for new global declaration statements}
  decl_done: decl_set_t;               {set of implicit constants already declared}
  addr_cnt_ar: sys_int_machine_t;      {extra "address of"s for array variables}
  array_mode: array_k_t;               {mode for interpreting array identifiers}
  path_to_here: boolean;               {TRUE if executable path to after last opc}
  no_ibm_str_kluge: boolean;           {TRUE if inhibit special IBM string kluge}
{
*   Entry point declarations.
}
procedure sst_w_c_armode_pop;          {restore previous array interpretation mode}
  extern;

procedure sst_w_c_armode_push (        {set new array interpret mode, save old}
  in      mode: array_k_t);            {new array identifier interpretation mode}
  extern;

procedure sst_w_c_adrcnt_ar_push (     {set new ADDR_CNT_AR, save old on stack}
  in      addr_cnt: sys_int_machine_t); {new value to set ADDR_CNT_AR to}
  extern;

procedure sst_w_c_arg (                {write argument in function/subroutine call}
  in out  arg_p: sst_proc_arg_p_t;     {points to arg descriptor, will be advanced}
  in out  argt_p: sst_proc_arg_p_t);   {points to arg template, will be advanced}
  extern;

procedure sst_w_c_assign (             {write complete assignment statement}
  in      v: sst_var_t;                {descriptor for variable to assign to}
  in      exp: sst_exp_t);             {descriptor for assignment value expression}
  extern;

procedure sst_w_c_cont_def;            {does continuation line in DEFINE statement}
  extern;

procedure sst_w_c_decl_sym_exp (       {declare symbols reference by an expression}
  in      exp: sst_exp_t);             {expression that may reference symbols}
  extern;

procedure sst_w_c_decl_sym_dtype (     {declare symbols referenced by a data type}
  in      dtype: sst_dtype_t);         {data type descriptor that may reference syms}
  extern;

procedure sst_w_c_decl_sym_rout (      {declare symbols referenced by routine call}
  in      proc: sst_proc_t);           {routine descriptor}
  extern;

procedure sst_w_c_declare (            {write out one of the implicit declarations}
  in      decl: decl_k_t);             {selects which implicit declaration to write}
  extern;

procedure sst_w_c_doit (               {do the whole back end phase}
  in      gnam: univ string_var_arg_t; {raw output file name}
  out     stat: sys_err_t);            {completion status code}
  extern;

procedure sst_w_c_dtype (              {write data type definition}
  in      dtype: sst_dtype_t;          {data type descriptor block}
  in      name: univ string_var_arg_t; {name of symbol to declare with this dtype}
  in      pack: boolean);              {TRUE if part of packed record}
  val_param; extern;

procedure sst_w_c_dtype_simple (       {write data type def, forced to be simple}
  in      dtype: sst_dtype_t;          {data type descriptor block}
  in      name: univ string_var_arg_t; {name of symbol to declare with this dtype}
  in      pack: boolean);              {TRUE if part of packed record}
  val_param; extern;

procedure sst_w_c_dtype_vrec (         {write data type definition of variant record}
  in out  field_p: sst_symbol_p_t;     {pnt to first field in variant, updated}
  in      pack: boolean);              {TRUE if variant part of packed record}
  val_param; extern;

procedure sst_w_c_exec (               {process executable opcode chain}
  in      first_p: sst_opc_p_t);       {pointer to first opcode, may be NIL}
  extern;

procedure sst_w_c_exp (                {write expression}
  in      exp: sst_exp_t;              {expression descriptor}
  in      addr_cnt: sys_int_machine_t; {number of times to take address of}
  in      dt_out_p: sst_dtype_p_t;     {desired output data type, NIL = as is}
  in      enclose: enclose_k_t);       {enclose in () yes/no}
  extern;

function sst_w_c_exp_adrable (         {determine whether "address of" exp is legal}
  in      exp: sst_exp_t)              {descriptor for expression to examine}
  :boolean; extern;                    {TRUE if "address of" expression is legal}

procedure sst_w_c_exp_array (          {write exp value, must be array constant}
  in      exp: sst_exp_t);             {expression descriptor}
  extern;

procedure sst_w_c_exp_const (          {write exp value, const value must be known}
  in      exp: sst_exp_t;              {expression descriptor}
  in      addr_cnt: sys_int_machine_t; {number of times to take address of}
  in      dt_out_p: sst_dtype_p_t;     {desired output data type, NIL = as is}
  in      enclose: enclose_k_t);       {enclose in () yes/no}
  extern;

procedure sst_w_c_exp_explicit (       {always create variable for expression value}
  in      exp: sst_exp_t;              {descriptor for expression}
  in      dtype: sst_dtype_t;          {data type descriptor for variable}
  out     sym_p: sst_symbol_p_t);      {will point to variable's symbol descriptor}
  extern;

procedure sst_w_c_exp_implicit (       {create implicit var for exp value, if needed}
  in      exp: sst_exp_t;              {descriptor for expression}
  out     sym_p: sst_symbol_p_t);      {will pnt to implicit var, or NIL for unused}
  extern;

procedure sst_w_c_exp_rec (            {write exp value, must be record constant}
  in      exp: sst_exp_t);             {expression descriptor}
  extern;

procedure sst_w_c_exp2 (               {write 2 expressions with operator in between}
  in      term_first1: sst_exp_term_t; {first term in expression 1}
  in      n_terms1: sys_int_machine_t; {number of terms in expression 1}
  in      term_first2: sst_exp_term_t; {first term in expression 2}
  in      n_terms2: sys_int_machine_t; {number of terms in expression 2}
  in      op: sst_op2_k_t;             {operator acting between the terms}
  in      enclose: enclose_k_t);       {enclose in () yes/no}
  extern;

procedure sst_w_c_header_decl (        {if needed, set up for declaration statement}
  in      stype: sment_type_k_t;       {desired type of declaration statement}
  out     pop_needed: boolean);        {TRUE if caller must pop position when done}
  extern;

procedure sst_w_c_implicit_const (     {create/reuse implicit constant variable}
  in      dtype: sst_dtype_t;          {data type descriptor for variable}
  in      val: sst_var_value_t;        {descriptor for variable's value}
  out     sym_p: sst_symbol_p_t);      {returned pointing to reused/new variable}
  extern;

procedure sst_w_c_implicit_var (       {create an implicit variable}
  in      dtype: sst_dtype_t;          {data type descriptor for new variable}
  out     v: sst_var_t);               {filled in "variable" desc for new variable}
  extern;

procedure sst_w_c_intrinsic (          {write name of intrinsic symbol}
  in      intr: intr_k_t);             {selects symbol, will be declared if needed}
  extern;

procedure sst_w_c_ival_unspec (        {write "unspecified" initial variable value}
  in      dtype: sst_dtype_t);         {data type descriptor for variable}
  extern;

procedure sst_w_c_name_com_var (       {set output name for var in common block}
  in out  sym: sst_symbol_t);          {symbol to set output name for}
  extern;

procedure sst_w_c_opcodes (            {read opcode chain and write output lines}
  in      first_p: sst_opc_p_t);       {points to first opcode in chain, may be NIL}
  extern;

procedure sst_w_c_pos_pop;             {pop old position from stack and restore}
  extern;

procedure sst_w_c_pos_push (           {push curr pos, set to a previous position}
  in      stype: sment_type_k_t);      {statement type that will be written}
  extern;

procedure sst_w_c_rearrange;           {rearrange data structures for C conventions}
  extern;

procedure sst_w_c_scope_pop;           {restore previous scope as current scope}
  extern;

procedure sst_w_c_scope_push (         {set a new scope as current}
  in      scope: sst_scope_t;          {scope to set current}
  in      scope_type: scope_type_k_t); {the type of the new scope}
  extern;

procedure sst_w_c_sment_end;           {done writing statement, close line}
  extern;

procedure sst_w_c_sment_end_nclose;    {done writing statement, leave line as is}
  extern;

procedure sst_w_c_sment_start;         {set for writing start of a new statement}
  extern;

procedure sst_w_c_symbol (             {declare symbol and others depended on}
  in out  sym: sst_symbol_t);          {symbol to declare}
  extern;

procedure sst_w_c_symbols (            {declare all used symbols not decl before}
  in      global_only: boolean);       {declare only global symbols def here if TRUE}
  extern;

procedure sst_w_c_term (               {write a term in an expression}
  in      term: sst_exp_term_t;        {descriptor for term to write}
  in      addr_cnt: sys_int_machine_t; {number of times to take address of}
  in      enclose: enclose_k_t);       {enclose in () yes/no}
  extern;

procedure sst_w_c_terms_implicit (     {make implicit var from terms list if needed}
  in      term1: sst_exp_term_t;       {descriptor for first term in chain}
  in      n_terms: sys_int_machine_t;  {number of terms in chain to consider}
  in      dtype: sst_dtype_t;          {descriptor for data type of implicit var}
  out     sym_p: sst_symbol_p_t);      {will pnt to implicit var, or NIL for unused}
  extern;

procedure sst_w_c_value (              {write the value of a constant}
  in      val: sst_var_value_t;        {value descriptor}
  in      enclose: enclose_k_t);       {enclose in () yes/no}
  extern;

procedure sst_w_c_var (                {write variable reference}
  in      v: sst_var_t;                {variable reference descriptor block}
  in      addr_cnt: sys_int_machine_t); {number of times to take address of}
  extern;
