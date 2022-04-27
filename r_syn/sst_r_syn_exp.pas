{   Routines to create various SST expressions for use in syntax parsing
*   functions.
}
module sst_r_syn_exp;
define sst_r_syn_exp_ichar;
define sst_r_syn_exp_pfunc;
define sst_r_syn_exp_exp2;
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
{
********************************************************************************
*
*   Subrtoutine SST_R_SYN_EXP_EXP2 (EXP1, EXP1, OP, EXP_P)
*
*   Create the expression resulting from combining two sub-expressions.  EXP1
*   and EXP2 are the two sub-expressions.  OP is the operator between them.
*   EXP_P is returned pointing to the new combined expression.
}
procedure sst_r_syn_exp_exp2 (         {exp from operation on two sub-exp}
  in var  exp1: sst_exp_t;             {first sub-expression}
  in var  exp2: sst_exp_t;             {second sub-expression}
  in      op: sst_op2_k_t;             {operator between the two sub-expressions}
  out     exp_p: sst_exp_p_t);         {returned pointer to new combined expression}
  val_param;

var
  dtype_p: sst_dtype_p_t;              {pnt to data type of combined expression}
  term_p: sst_exp_term_p_t;            {second term of combined expression}

begin
  case op of                           {which operator ?}
sst_op2_eq_k,
sst_op2_ne_k,
sst_op2_ge_k,
sst_op2_le_k,
sst_op2_lt_k,
sst_op2_and_k,
sst_op2_or_k,
sst_op2_andthen_k,
sst_op2_orelse_k,
sst_op2_in_k: begin                    {operators that always result in BOOLEAN}
      dtype_p := sst_dtype_bool_p;
      end;
otherwise
    writeln ('INTERNAL ERROR: Operator with ID ', ord(op), ' not supported');
    writeln ('in SST_R_SYN_EXP_EXP2.');
    sys_bomb;
    end;

  sst_mem_alloc_scope (                {create combined expression descriptor}
    sizeof(exp_p^), exp_p);

  exp_p^.str_h.first_char.crange_p := nil;
  exp_p^.dtype_p := dtype_p;           {data type of whole expression}
  exp_p^.dtype_hard := true;           {data type is known and fixed}
  exp_p^.val_eval := true;             {indicated attempted to evaluate}
  exp_p^.val_fnd := false;             {no fixed value found}
  exp_p^.rwflag := [sst_rwflag_read_k]; {expression is read-only}

  exp_p^.term1.op2 := sst_op2_none_k;  {no operation with previous term}
  exp_p^.term1.op1 := sst_op1_none_k;  {no unary operation on this term}
  exp_p^.term1.ttype := sst_term_exp_k; {this term is an expression}
  exp_p^.term1.str_h.first_char.crange_p := nil;
  exp_p^.term1.dtype_p := exp1.dtype_p; {data type of this term}
  exp_p^.term1.dtype_hard := true;     {data type is known and fixed}
  exp_p^.term1.val_eval := true;       {tried to evaluate fixed value}
  exp_p^.term1.val_fnd := false;       {no fixed value}
  exp_p^.term1.rwflag := [sst_rwflag_read_k]; {term is read-only}
  exp_p^.term1.exp_exp_p := addr(exp1); {the expression that is this term}

  sst_mem_alloc_scope (                {create descriptor for second term}
    sizeof(term_p^), term_p);
  exp_p^.term1.next_p := term_p;       {link second term to after first}

  term_p^.next_p := nil;               {this is last term in expression}
  term_p^.op2 := op;                   {operation between prev term and this}
  term_p^.op1 := sst_op1_none_k;       {no unary operation on this term}
  term_p^.ttype := sst_term_exp_k;     {this term is an expression}
  term_p^.str_h.first_char.crange_p := nil;
  term_p^.dtype_p := exp2.dtype_p;     {data type of this term}
  term_p^.dtype_hard := true;          {data type is known and fixed}
  term_p^.val_eval := true;            {tried to evaluate fixed value}
  term_p^.val_fnd := false;            {no fixed value}
  term_p^.rwflag := [sst_rwflag_read_k]; {term is read-only}
  term_p^.exp_exp_p := addr(exp2);     {the expression that is this term}
  end;
