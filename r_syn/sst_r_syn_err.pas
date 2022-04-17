{   Routines for handling errors in parsing code.
}
module sst_r_syn_err;
define sst_r_syn_err_check;
%include 'sst_r_syn.ins.pas';
{
********************************************************************************
*
*   Subroutine SST_R_SYN_ERR_CHECK
*
*   Check for end of error re-parse has been encountered.  If so, then a jump
*   will be taken to the error label, pointed to by LABEL_ERR_P.  This label is
*   created here the first time an error check is performed.  The error exit
*   code at the label is not written later if the label was not created.
*
*   If end of error re-parse is detected, then the syntax parsing function will
*   be aborted and will return FALSE.
}
procedure sst_r_syn_err_check;         {check for err reparse end in syntax checking code}
  val_param;

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  name: string_var32_t;                {label name}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;
  stat: sys_err_t;                     {completion status}

begin
  name.max := size_char(name.str);     {init local var string}
{
*   Make sure the label for the error exit code exists.  It is created here if
*   not.
}
  if label_err_p = nil then begin      {label to jump to doesn't exist yet ?}
    string_vstring (name, 'err'(0), -1); {set the label name}
    sst_symbol_new_name (              {create the label symbol}
      name,                            {label name}
      label_err_p,                     {returned pointer to the new symbol}
      stat);
    sys_msg_parm_vstr (msg_parm[1], name);
    sys_error_abort (stat, 'sst_syn_read', 'symbol_label_create', msg_parm, 1);

    label_err_p^.symtype := sst_symtype_label_k; {this symbol is a label}
    label_err_p^.label_opc_p := nil;   {opcode for this label not written yet}
    end;
{
*   Write the conditional goto to the error label.
}
  sst_opcode_new;                      {create opcode for the conditional}
  sst_opc_p^.opcode := sst_opc_if_k;   {opcode type}
  sst_opc_p^.str_h.first_char.crange_p := nil;
  sst_opc_p^.str_h.first_char.ofs := 0;
  sst_opc_p^.str_h.last_char := sst_opc_p^.str_h.first_char;
  sst_opc_p^.if_exp_p := sym_error_p;  {expression to evaluate}
  sst_opc_p^.if_false_p := nil;        {no FALSE case code}

  sst_opcode_pos_push (sst_opc_p^.if_true_p); {set up for writing TRUE case code}

  sst_opcode_new;                      {create GOTO opcode}
  sst_opc_p^.opcode := sst_opc_goto_k;
  sst_opc_p^.str_h.first_char.crange_p := nil;
  sst_opc_p^.str_h.first_char.ofs := 0;
  sst_opc_p^.str_h.last_char := sst_opc_p^.str_h.first_char;
  sst_opc_p^.goto_sym_p := label_err_p; {label to jump to}

  sst_opcode_pos_pop;                  {done writing TRUE case opcodes}
  end;
