{   Routines to create various SST expressions for use in syntax parsing
*   functions.
}
module sst_r_syn_exp;
define sst_r_syn_exp_ichar;
define sst_r_syn_exp_pfunc;
%include 'sst_r_syn.ins.pas';
{
********************************************************************************
*
*   Function SST_R_SYN_EXP_ICHAR
*
*   Create the SST expression that is the value of the function SYN_P_ICHAR.
*   The function returns a pointer to the newly-created expression.  This
*   function must be called when a syntax parsing function is being written,
*   since it references that functions call argument.
}
function sst_r_syn_exp_ichar:          {create SST expression for SYN_P_ICHAR value}
  sst_exp_p_t;                         {pointer to the new expression descriptor}
  val_param;

var
  exp_p: sst_exp_p_t;                  {pointer to expression being built}
  proc_p: sst_proc_p_t;                {pointer to called procedure descriptor}
  arg_p: sst_proc_arg_p_t;             {pointer to call argument}

begin
{
*   Create the call argument descriptor.
}
  sst_mem_alloc_scope (                {create the empty descriptor}
    sizeof(arg_p^), arg_p);

  arg_p^ := sym_ichar_p^.proc.first_arg_p^; {init from template}

  arg_p^.next_p := nil;                {there is no following argument}
  arg_p^.exp_p := exp_syn_p;           {pointer to SYN argument value expression}
{
*   Create desciptor for the function call.
}
  sst_mem_alloc_scope (                {create called routine descriptor}
    sizeof(proc_p^), proc_p);

  proc_p^ := sym_ichar_p^.proc;        {initialize called function from template}
  proc_p^.first_arg_p := arg_p;        {point to first (and only) call argument}
{
*   Create the expression for the value of the called function.
}
  sst_mem_alloc_scope (                {create expression descriptor}
    sizeof(exp_p^), exp_p);

  exp_p^.str_h.first_char.crange_p := nil;
  exp_p^.dtype_p := sym_int_p^.dtype_dtype_p; {expression type is machine integer}
  exp_p^.dtype_hard := true;           {data type is known and fixed}
  exp_p^.val_eval := true;             {attempted to resolve value}
  exp_p^.val_fnd := false;             {fixed value not found}
  exp_p^.rwflag := [sst_rwflag_read_k]; {expression is read-only}

  exp_p^.term1.next_p := nil;          {there is no next term}
  exp_p^.term1.op2 := sst_op2_none_k;  {no operator with previous term}
  exp_p^.term1.op1 := sst_op1_none_k;  {no unary operator applying to this term}
  exp_p^.term1.ttype := sst_term_func_k; {term is the value of a function}
  exp_p^.term1.str_h.first_char.crange_p := nil;
  exp_p^.term1.dtype_p := exp_p^.dtype_p; {same data type as whole expression}
  exp_p^.term1.dtype_hard := true;     {data type is known and fixed}
  exp_p^.term1.val_eval := true;       {tried to resolve value}
  exp_p^.term1.val_fnd := false;       {not a known fixed value}
  exp_p^.term1.rwflag := [sst_rwflag_read_k]; {term is read-only}
  exp_p^.term1.func_var_p := var_ichar_p; {"variable" reference for calling the function}
  exp_p^.term1.func_proc_p := proc_p;  {point to function call descriptor}
  exp_p^.term1.func_proct_p :=         {point to the procedure template}
    addr(sym_ichar_p^.proc);

  sst_r_syn_exp_ichar := exp_p;        {return pointer to the function result expression}
  end;
{
********************************************************************************
*
*   Function SST_R_SYN_EXP_PFUNC (SYM)
*
*   Create the expression that is the value of a syntax parsing function.  SYM
*   is the function symbol.  Such functions always take the SYN argument, and
*   return TRUE of FALSE depending on whether the input stream matched the
*   syntax template.
}
function sst_r_syn_exp_pfunc (         {create expression for value of syn parsing func}
  in var  sym: sst_symbol_t)           {syntax parsing funtion symbol}
  :sst_exp_p_t;                        {pointer to expression referencing the function}
  val_param;

var
  exp_p: sst_exp_p_t;                  {pointer to expression being built}
  proc_p: sst_proc_p_t;                {pointer to called procedure descriptor}
  arg_p: sst_proc_arg_p_t;             {pointer to call argument}

begin
{
*   Create the call argument descriptor.
}
  sst_mem_alloc_scope (                {create the empty descriptor}
    sizeof(arg_p^), arg_p);

  arg_p^ := sym.proc.first_arg_p^;     {init from template}

  arg_p^.next_p := nil;                {there is no following argument}
  arg_p^.exp_p := exp_syn_p;           {pointer to SYN argument value expression}
{
*   Create desciptor for the function call.
}
  sst_mem_alloc_scope (                {create called routine descriptor}
    sizeof(proc_p^), proc_p);

  proc_p^ := sym.proc;                 {initialize called function from template}
  proc_p^.first_arg_p := arg_p;        {point to first (and only) call argument}
{
*   Create the expression for the value of the called function.
}
  sst_mem_alloc_scope (                {create expression descriptor}
    sizeof(exp_p^), exp_p);

  exp_p^.str_h.first_char.crange_p := nil;
  exp_p^.dtype_p := sym.proc_dtype_p;  {expression dtype is function return dtype}
  exp_p^.dtype_hard := true;           {data type is known and fixed}
  exp_p^.val_eval := true;             {attempted to resolve value}
  exp_p^.val_fnd := false;             {fixed value not found}
  exp_p^.rwflag := [sst_rwflag_read_k]; {expression is read-only}

  exp_p^.term1.next_p := nil;          {there is no next term}
  exp_p^.term1.op2 := sst_op2_none_k;  {no operator with previous term}
  exp_p^.term1.op1 := sst_op1_none_k;  {no unary operator applying to this term}
  exp_p^.term1.ttype := sst_term_func_k; {term is the value of a function}
  exp_p^.term1.str_h.first_char.crange_p := nil;
  exp_p^.term1.dtype_p := exp_p^.dtype_p; {same data type as whole expression}
  exp_p^.term1.dtype_hard := true;     {data type is known and fixed}
  exp_p^.term1.val_eval := true;       {tried to resolve value}
  exp_p^.term1.val_fnd := false;       {not a known fixed value}
  exp_p^.term1.rwflag := [sst_rwflag_read_k]; {term is read-only}
  exp_p^.term1.func_var_p :=           {"variable" reference for calling the function}
    sst_r_syn_var_proc (sym);
  exp_p^.term1.func_proc_p := proc_p;  {point to function call descriptor}
  exp_p^.term1.func_proct_p :=         {point to the procedure template}
    addr(sym.proc);

  sst_r_syn_exp_pfunc := exp_p;        {return pointer to the function value expression}
  end;
