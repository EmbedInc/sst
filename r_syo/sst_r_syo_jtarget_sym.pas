{   Subroutine SST_R_SYO_JTARGET_SYM (JT, SYM_P)
*
*   Return pointer to descriptor of label corresponding to jump target JT.
*   A label is implicitly created, if one didn't already exist.
}
module sst_r_syo_jtarget_sym;
define sst_r_syo_jtarget_sym;
%include 'sst_r_syo.ins.pas';

procedure sst_r_syo_jtarget_sym (      {get or make symbol for jump target label}
  in out  jt: jump_target_t;           {descriptor for this jump target}
  out     sym_p: sst_symbol_p_t);      {will point to label symbol descriptor}

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  jt_p: jump_target_p_t;               {pointer to base jump target descriptor}
  name: string_var32_t;                {label name}
  token: string_var32_t;               {scratch token for number conversion}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;
  stat: sys_err_t;

begin
  name.max := sizeof(name.str);        {init local var strings}
  token.max := sizeof(token.str);

  jt_p := addr(jt);                    {init base descriptor to first descriptor}
  while jflag_indir_k in jt_p^.flags do begin {this is an indirect descriptor ?}
    jt_p := jt_p^.indir_p;             {resolve one level of indirection}
    end;                               {back to resolve next level of indirection}
{
*   JT_P is pointing to the base jump target descriptor.
}
  if jt_p^.lab_p <> nil then begin     {a label symbol already exists here ?}
    sym_p := jt_p^.lab_p;              {fetch label symbol pointer from jump desc}
    return;
    end;
{
*   No label symbol exists for this jump target.  Create one and pass back
*   the pointer to it.
}
  string_vstring (name, 'lab', 3);     {set static part of label name}
  string_f_int (token, seq_label);     {make sequence number string}
  seq_label := seq_label + 1;          {update sequence number for next time}
  string_append (name, token);         {make full label name}
  sst_symbol_new_name (name, sym_p, stat); {add symbol to symbol table}
  sys_error_abort (stat, 'sst_syo_read', 'symbol_label_create', msg_parm, 1);

  sym_p^.symtype := sst_symtype_label_k; {fill in new label symbol descriptor}
  sym_p^.label_opc_p := nil;
  jt_p^.lab_p := sym_p;                {save symbol pointer in jump descriptor}
  end;
