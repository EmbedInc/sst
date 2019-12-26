{   Subroutine SST_EXP_USEAGE_CHECK (EXP,RW,DTYPE)
*
*   Check that the attributes of an expression are consistant with its useage.
*   It is assumed that the expression descriptor is completely filled in and
*   evaluated.  An expression may be evaluated with routine SST_EXP_EVAL.
*
*   EXP  -  Expression descriptor.
*
*   RW  -  Indicates the read/write access needed to the expression value.
*     RW is a SET.  The possible element values are:
*
*       SST_RWFLAG_READ_K  -  Expression value must be readable.
*       SST_RWFLAG_WRITE_K  -  Expression value must be writeable.
*
*   DTYPE  -  A data type descriptor declaring the data type the expression
*     value must be convertable to/from.  The convert direction(s) that are
*     checked depend on the value of RW.  Any expression data type is legal
*     if DTYPE is set to data type ID of SST_DTYPE_UNDEF_K.
}
module sst_EXP_USEAGE_CHECK;
define sst_exp_useage_check;
%include 'sst2.ins.pas';

procedure sst_exp_useage_check (       {check expression attributes for given useage}
  in      exp: sst_exp_t;              {expression to check}
  in      rw: sst_rwflag_t;            {read/write expression useage}
  in      dtype: sst_dtype_t);         {required data type of expression}

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;
  stat: sys_err_t;

label
  has_value;

begin
  if rw <> [] then begin               {expression must have a value ?}
    if exp.val_fnd then goto has_value; {expression has known constant value ?}
    if exp.term1.next_p <> nil then goto has_value; {compound expression ?}
    case exp.term1.ttype of            {what type is the term}
sst_term_const_k,                      {these term types definately have a value}
sst_term_func_k,
sst_term_ifunc_k,
sst_term_type_k,
sst_term_set_k: goto has_value;
sst_term_var_k: begin                  {term is a "variable" reference}
        case exp.term1.var_var_p^.vtype of {what kind of "variable" is this ?}
sst_vtype_var_k,                       {these "variable" types definately have val}
sst_vtype_rout_k,
sst_vtype_const_k: goto has_value;
sst_vtype_dtype_k: begin               {variable is a data type}
            syn_error (exp.str_h, 'sst', 'dtype_useage_bad', nil, 0);
            end;
sst_vtype_com_k: begin                 {variable is a common block}
            syn_error (exp.str_h, 'sst', 'common_block_useage_bad', nil, 0);
            end;
          end;                         {end of variable descriptor type cases}
        end;                           {end of term is a variable descriptor case}
sst_term_exp_k: begin                  {term is a nested expression}
        sst_exp_useage_check (exp.term1.exp_exp_p^, rw, dtype);
        return;
        end;
otherwise
      sys_msg_parm_int (msg_parm[1], ord(exp.term1.ttype));
      syn_error (exp.str_h, 'sst', 'term_type_unknown', msg_parm, 1);
      end;                             {end of term type cases}
has_value:                             {jump here if exp definately has a value}
    end;                               {done with case where exp needs a value}

  sst_rwcheck (rw, exp.rwflag, stat);  {check read/write permission}
  syn_error_abort (stat, exp.str_h, '', '', nil, 0);

  if dtype.dtype = sst_dtype_undef_k   {data type indicates universal match ?}
    then return;

  if sst_rwflag_read_k in rw then begin {check exp dtype convertable to DTYPE}
    if not sst_dtype_convertable (
        exp.dtype_p^,                  {input data type}
        dtype)                         {data type must be convertable to}
        then begin
      syn_error (exp.str_h, 'sst', 'dtype_exp_mismatch', nil, 0);
      end;
    end;

  if sst_rwflag_write_k in rw then begin {check DTYPE convertable to exp dtype}
    if not sst_dtype_convertable (
        dtype,                         {input data type}
        exp.dtype_p^)                  {data type must be convertable to}
        then begin
      syn_error (exp.str_h, 'sst', 'dtype_exp_mismatch', nil, 0);
      end;
    end;
  end;
