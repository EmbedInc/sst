{   Module of routines that add call arguments a subroutine call opcode.
}
module sst_call_arg;
define sst_call_arg_enum;
define sst_call_arg_int;
define sst_call_arg_str;
define sst_call_arg_var;
%include 'sst2.ins.pas';
{
***********************************************************
*
*   Local subroutine MAKE_ARG_TEMPLATE (OPC, ARGT_P, ARG_P)
*
*   Given the opcode OPC for a subroutine call, return pointer to
*   next call argument template (ARGT_P) and new called argument
*   descriptor (ARG_P).  Some obvious errors are checked.
}
procedure make_arg_template (
  in      opc: sst_opc_t;              {opcode for subroutine call}
  out     argt_p: sst_proc_arg_p_t;    {returned pointer to argument template}
  out     arg_p: sst_proc_arg_p_t);    {returned pointer to new called arg desc}
  val_param;

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  arg_pp: sst_proc_arg_pp_t;           {points to next argument chain pointer}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

begin
  if opc.opcode <> sst_opc_call_k then begin {opcode is not a call ?}
    sys_msg_parm_int (msg_parm[1], ord(opc.opcode));
    sys_message_bomb ('sst', 'opcode_unexpected', msg_parm, 1);
    end;

  argt_p := opc.call_proct_p^.first_arg_p; {init pointer to first argument template}
  arg_pp := addr(opc.call_proc_p^.first_arg_p); {point to start of call args chain}
  arg_p := arg_pp^;                    {init pointer to first call argument}

  while arg_p <> nil do begin          {existing call argument found here ?}
    if argt_p = nil then begin         {too many call arguments ?}
      sys_message_bomb ('sst', 'args_too_many', nil, 0);
      end;
    arg_pp := addr(arg_p^.next_p);     {update pointer to chain link pointer}
    argt_p := argt_p^.next_p;          {advance to next call argument template}
    arg_p := arg_p^.next_p;            {advance to next call argument descriptor}
    end;                               {back and check this new argument}

  if argt_p = nil then begin           {no room for another call argument ?}
    sys_message_bomb ('sst', 'args_too_many_add', nil, 0);
    end;

  sst_mem_alloc_scope (sizeof(arg_p^), arg_p); {alloc mem for new argument desc}
  arg_pp^ := arg_p;                    {add new argument to end of list}
  opc.call_proc_p^.n_args :=           {count one more argument in call}
    opc.call_proc_p^.n_args + 1;

  arg_p^.next_p := nil;                {initialize call argument descriptor}
  arg_p^.sym_p := nil;
  arg_p^.name_p := nil;
  arg_p^.exp_p := nil;
  arg_p^.dtype_p := nil;
  arg_p^.pass := argt_p^.pass;
  arg_p^.rwflag_int := argt_p^.rwflag_int;
  arg_p^.rwflag_ext := argt_p^.rwflag_ext;
  arg_p^.univ := argt_p^.univ;
  end;
{
***********************************************************
}
procedure sst_call_arg_enum (          {add constant enum call arg to call}
  in      opc: sst_opc_t;              {CALL opcode to add argument to}
  in      sym: sst_symbol_t);          {symbol descriptor for enumerated value}
  val_param;

var
  arg_p: sst_proc_arg_p_t;             {points to call argument descriptor}
  argt_p: sst_proc_arg_p_t;            {points to call argument template}

begin
  make_arg_template (opc, argt_p, arg_p); {create and init argument descriptor}
  sst_exp_const_enum (sym, arg_p^.exp_p); {create constant enum expression}
  arg_p^.dtype_p := arg_p^.exp_p^.dtype_p;
  sst_exp_useage_check (               {check exp compatibility with arg template}
    arg_p^.exp_p^,                     {expression to check}
    argt_p^.rwflag_ext,                {neccessary read/write access to expression}
    argt_p^.dtype_p^);                 {dtype expression must be compatible with}
  end;
{
***********************************************************
}
procedure sst_call_arg_int (           {add constant integer call arg to call}
  in      opc: sst_opc_t;              {CALL opcode to add argument to}
  in      ival: sys_int_max_t);        {integer value for argument}
  val_param;

var
  arg_p: sst_proc_arg_p_t;             {points to call argument descriptor}
  argt_p: sst_proc_arg_p_t;            {points to call argument template}

begin
  make_arg_template (opc, argt_p, arg_p); {create and init argument descriptor}
  sst_exp_const_int (ival, arg_p^.exp_p); {create constant integer expression}
  arg_p^.dtype_p := arg_p^.exp_p^.dtype_p;
  sst_exp_useage_check (               {check exp compatibility with arg template}
    arg_p^.exp_p^,                     {expression to check}
    argt_p^.rwflag_ext,                {neccessary read/write access to expression}
    argt_p^.dtype_p^);                 {dtype expression must be compatible with}
  end;
{
***********************************************************
}
procedure sst_call_arg_str (           {add constant string call arg to call}
  in      opc: sst_opc_t;              {CALL opcode to add argument to}
  in      str: univ string;            {string for argument value}
  in      len: sys_int_machine_t);     {string length}
  val_param;

var
  arg_p: sst_proc_arg_p_t;             {points to call argument descriptor}
  argt_p: sst_proc_arg_p_t;            {points to call argument template}
  dtlen: sys_int_machine_t;            {string length of target data type}
  dt_p: sst_dtype_p_t;                 {base data type of argument template}

begin
  make_arg_template (opc, argt_p, arg_p); {create and init argument descriptor}

  if argt_p^.univ
    then begin                         {no need to match target argument}
      dtlen := len;
      end
    else begin                         {must match target argument data type}
      dt_p := argt_p^.dtype_p;         {make base argument templat data type}
      while dt_p^.dtype = sst_dtype_copy_k do dt_p := dt_p^.copy_dtype_p;
      if dt_p^.dtype <> sst_dtype_array_k then begin {AR_IND_N not valid ?}
        sys_message_bomb ('sst', 'dtype_not_array', nil, 0);
        end;
      dtlen := dt_p^.ar_ind_n;         {set required length of target string}
      end
    ;

  sst_exp_const_str (                  {create string constant expression}
    str, len,                          {the string and string length}
    dtlen,                             {length of target string data type}
    arg_p^.exp_p);                     {returned pointer to expression descriptor}
  arg_p^.dtype_p := arg_p^.exp_p^.dtype_p; {copy data type to argument descriptor}

  sst_exp_useage_check (               {check exp compatibility with arg template}
    arg_p^.exp_p^,                     {expression to check}
    argt_p^.rwflag_ext,                {neccessary read/write access to expression}
    argt_p^.dtype_p^);                 {dtype expression must be compatible with}
  end;
{
***********************************************************
}
procedure sst_call_arg_var (           {add variable call arg to call}
  in      opc: sst_opc_t;              {CALL opcode to add argument to}
  in      sym: sst_symbol_t);          {variable to add as call argument}
  val_param;

var
  arg_p: sst_proc_arg_p_t;             {points to call argument descriptor}
  argt_p: sst_proc_arg_p_t;            {points to call argument template}

begin
  make_arg_template (opc, argt_p, arg_p); {create and init argument descriptor}

  arg_p^.exp_p := sst_exp_make_var(sym); {make expression referencing variable}
  arg_p^.dtype_p := arg_p^.exp_p^.dtype_p;

  sst_exp_useage_check (               {check exp compatibility with arg template}
    arg_p^.exp_p^,                     {expression to check}
    argt_p^.rwflag_ext,                {neccessary read/write access to expression}
    argt_p^.dtype_p^);                 {dtype expression must be compatible with}
  end;
