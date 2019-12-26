{   Subroutine SST_R_PAS_SMENT_DEFINE
*
*   Process DEFINE_STATEMENT syntax.  The tag for this syntax has just been read.
*   This statement indicates that the named symbol will be globally know to the
*   binder, and will be defined here.
}
module sst_r_pas_SMENT_DEFINE;
define sst_r_pas_sment_define;
%include 'sst_r_pas.ins.pas';

procedure sst_r_pas_sment_define;      {process DEFINE_STATEMENT syntax}

const
  max_msg_parms = 2;                   {max parameters we can pass to a message}

var
  tag: sys_int_machine_t;              {syntax tag ID}
  str_h: syn_string_t;                 {handle to string associated with TAG}
  sym_p: sst_symbol_p_t;               {points to descriptor for this symbol}
  lnum: sys_int_machine_t;             {input file line number}
  fnam: string_treename_t;             {input file name}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;
  stat: sys_err_t;                     {completion status code}

label
  leave;

begin
  fnam.max := sizeof(fnam.str);        {init local var string}

  syn_level_down;                      {down into DEFINE_STATEMENT syntax}
  syn_get_tag_msg (                    {get tag for symbol name}
    tag, str_h, 'sst_pas_read', 'define_statement_bad', nil, 0);
  sst_symbol_new (                     {make new symbol or find old sym descriptor}
    str_h, syn_charcase_asis_k, sym_p, stat);
  sym_p^.flags := sym_p^.flags +       {flag symbol as globally known}
    [sst_symflag_global_k, sst_symflag_used_k];
  sym_p^.flags := sym_p^.flags -       {symbol lives right here}
    [sst_symflag_extern_k];

  syn_get_tag_msg (                    {get tag for optional initial value}
    tag, str_h, 'sst_pas_read', 'define_statement_bad', nil, 0);
  if tag = syn_tag_end_k then goto leave; {no optional initial value present ?}
{
*   An initial value was supplied for this symbol.  It had better be a
*   variable with no previous initial value.  TAG is for the
*   VAR_INITIALIZER syntax.
}
  if
      (sym_p^.symtype <> sst_symtype_var_k) or else {symbol not a variable ?}
      (sym_p^.var_proc_p <> nil)       {dummy argument or function return value ?}
      then begin
    syn_error (str_h, 'sst_pas_read', 'initial_value_not_var', nil, 0);
    end;

  if sym_p^.var_val_p <> nil then begin {already has an initial value ?}
    sst_charh_info (                   {find where initial value was declared}
      sym_p^.var_val_p^.str_h.first_char, {handle to first char of previous init val}
      fnam,                            {file name of previous initial value}
      lnum);                           {line number of previous initial value}
    sys_msg_parm_int (msg_parm[1], lnum);
    sys_msg_parm_vstr (msg_parm[2], fnam);
    syn_error (str_h, 'sst_pas_read', 'initial_value_already', msg_parm, 2);
    end;

  if sym_p^.var_dtype_p = nil then begin {this variable has no data type yet ?}
    syn_error (str_h, 'sst_pas_read', 'initial_value_no_dtype', nil, 0);
    end;
{
*   Done error checking.  Everything looks OK for giving this variable an
*   initial value.
}
  sst_r_pas_var_init (                 {process VAR_INITIALIZER syntax}
    sym_p^.var_dtype_p^,               {data type initial value must conform to}
    sym_p^.var_val_p);                 {returned pointer to initial value expression}

leave:                                 {common exit point}
  syn_level_up;                        {back up to caller's syntax level}
  end;
