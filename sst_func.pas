module sst_func;
define sst_func_exp;
define sst_func_arg;
%include 'sst2.ins.pas';
{
********************************************************************************
*
*   Function SST_FUNC_EXP (FUNC)
*
*   Create the expression for the returned value of a function.  FUNC is the
*   function symbol.  The function will not be passed any call arguments.  These
*   can be added later with SST_FUNC_ARG and related routines.
}
function sst_func_exp (                {make function value expression, no args}
  in var  func: sst_symbol_t)          {function symbol}
  :sst_exp_p_t;                        {pointer to new expression}
  val_param;

var
  proc_p: sst_proc_p_t;                {pointer to called procedure descriptor}
  var_p: sst_var_p_t;                  {pnt to "variable" descriptor for func ref}
  exp_p: sst_exp_p_t;                  {pointer to expression being built}

begin
  if func.symtype <> sst_symtype_proc_k then begin
    writeln ('INTERNAL ERROR: FUNC argument to SST_EXP_FUNC not a function symbol.');
    sys_bomb;
    end;
{
*   Create desciptor for the function call.
}
  sst_mem_alloc_scope (                {create called routine descriptor}
    sizeof(proc_p^), proc_p);

  proc_p^ := func.proc;                {initialize called function from template}
  proc_p^.n_args := 0;                 {init to no call arguments}
  proc_p^.first_arg_p := nil;          {init call arguments list to empty}
{
*   Create the "variable" reference to the function.
}
  sst_mem_alloc_scope (                {create variable reference descriptor}
    sizeof(var_p^), var_p);

  var_p^.mod1.next_p := nil;           {this is last modifier in chain}
  var_p^.mod1.modtyp := sst_var_modtyp_top_k; {this is top level modifier}
  var_p^.mod1.top_str_h.first_char.crange_p := nil;
  var_p^.mod1.top_sym_p := addr(func); {symbol being referenced}

  var_p^.dtype_p := func.proc.dtype_func_p; {resulting data type of the reference}
  var_p^.rwflag := [sst_rwflag_read_k]; {reference is read-only}
  var_p^.vtype := sst_vtype_rout_k;    {reference is to called routine}
  var_p^.rout_proc_p := addr(func.proc); {template for calling the routine}
{
*   Create the expression for the value of the called function.
}
  sst_mem_alloc_scope (                {create expression descriptor}
    sizeof(exp_p^), exp_p);

  exp_p^.str_h.first_char.crange_p := nil;
  exp_p^.dtype_p := func.proc.dtype_func_p; {function return value data type}
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
  exp_p^.term1.func_var_p := var_p;    {"variable" reference for calling the function}
  exp_p^.term1.func_proc_p := proc_p;  {point to function call descriptor}
  exp_p^.term1.func_proct_p :=         {point to the procedure template}
    addr(func.proc);

  sst_func_exp := exp_p;               {return pointer to the new expression}
  end;
{
********************************************************************************
*
*   Subroutine SST_FUNC_ARG (EXP, ARG)
*
*   Add an argument to the function call expression EXP.  ARG is the argument
*   value expression.  EXP must be the expression for the result of calling a
*   function, such as returned by SST_FUNC_EXP.
}
procedure sst_func_arg (               {add argument to function value expression}
  in out  exp: sst_exp_t;              {function reference exp to add argument to}
  in var  arg: sst_exp_t);             {argument expression}
  val_param;

var
  proc_p: sst_proc_p_t;                {pointer to the called procedure descriptor}
  narg: sys_int_machine_t;             {number of arguments found}
  argt_p: sst_proc_arg_p_t;            {pointer to argument template}
  arg_p: sst_proc_arg_p_t;             {pointer to call argument}
  arg_pp: ^sst_proc_arg_p_t;           {pnt to where to link new arg descriptor}

label
  notemplate;

begin
  if exp.term1.ttype <> sst_term_func_k then begin
    writeln ('INTERNAL ERROR: Expression is not a function call in SST_FUNC_ARG.');
    sys_bomb;
    end;

  proc_p := exp.term1.func_proc_p;     {get pointer to call descriptor}
{
*   Find the end of the arguments chain.  The following state is set:
*
*     NARG  -  Number of existing arguments found.
*
*     ARG_PP  -  Points to where to link new argument to end of arguments chain.
*
*     ARGT_P  -  Pointer to the template for this argument in the function
*       descriptor.
}
  narg := 0;                           {init number of existing arguments found}
  arg_pp := addr(proc_p^.first_arg_p); {init pointer to end of chain link}
  arg_p := proc_p^.first_arg_p;        {init to first argument}
  argt_p := exp.term1.func_proct_p^.first_arg_p; {init to first argument template}
  if argt_p = nil then goto notemplate; {no template exists for a new argument ?}

  while arg_p <> nil do begin          {loop until hit end of chain}
    narg := narg + 1;                  {count one more existing argument}
    arg_pp := addr(arg_p^.next_p);     {update end of chain link pointer}
    arg_p := arg_p^.next_p;            {advance to next argument in chain}
    argt_p := argt_p^.next_p;          {advance to template for next argument}
    if argt_p = nil then goto notemplate; {no template for new argument ?}
    end;
{
*   Create the new call argument descriptor.
}
  sst_mem_alloc_scope (                {create the empty descriptor}
    sizeof(arg_p^), arg_p);

  arg_p^ := argt_p^;                   {init the new argument from the template}
  arg_p^.next_p := nil;                {this will be last argument in chain}
  arg_p^.exp_p := addr(arg);           {point to expression for this argument}
  arg_p^.dtype_p := arg.dtype_p;       {data type of this argument}
{
*   Update the function call.
}
  arg_pp^ := arg_p;                    {link new argument to end of chain}
  proc_p^.n_args := narg + 1;          {update number of args function called with}
  return;

notemplate:
  writeln ('INTERNAL ERROR: Attempt to add more arguments than allowed by function.');
  writeln ('  in SST_FUNC_ARG.');
  sys_bomb;
  end;
