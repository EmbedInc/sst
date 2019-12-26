{   Function SST_TERM_SIMPLE(TERM)
*
*   Returns TRUE if the term is "simple".  This means no computes are needed
*   to evaluate it, other than just to go fetch its value.
}
module sst_TERM_SIMPLE;
define sst_term_simple;
%include 'sst2.ins.pas';

function sst_term_simple (             {check for term is simple/complicated}
  in      term: sst_exp_term_t)        {term descriptor to check}
  : boolean;                           {TRUE if no computes needed to evaluate term}

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  ele_p: sst_ele_exp_p_t;              {points to current set element descriptor}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

begin
  sst_term_simple := false;            {init to term is not simple}
  if                                   {unary operator and term not a constant ?}
      (term.ttype <> sst_term_const_k) and
      (term.op1 <> sst_op1_none_k)
    then return;

  case term.ttype of                   {what kind of term is it ?}

sst_term_const_k: ;

sst_term_var_k: begin
      case term.var_var_p^.vtype of    {what kind of var descriptor is this ?}
sst_vtype_var_k: ;
sst_vtype_dtype_k: ;
sst_vtype_rout_k: ;
sst_vtype_const_k: ;
sst_vtype_com_k: ;
otherwise
        sys_msg_parm_int (msg_parm[1], ord(term.var_var_p^.vtype));
        sys_message_bomb ('sst', 'vtype_unexpected', msg_parm, 1);
        end;
      end;

sst_term_type_k: begin                 {term is a type-transfer function}
      sst_term_simple := sst_exp_simple(term.type_exp_p^); {process nested exp}
      return;
      end;

sst_term_set_k: begin
      ele_p := term.set_first_p;       {set curr set element to first set element}
      while ele_p <> nil do begin      {once for each set element}
        if not sst_exp_simple(ele_p^.first_p^) then return;
        if ele_p^.last_p <> nil then begin
          if not sst_exp_simple(ele_p^.last_p^) then return;
          end;
        ele_p := ele_p^.next_p;        {advance to next set element descriptor}
        end;                           {back and process this next set element}
      end;

sst_term_exp_k: begin
      sst_term_simple := sst_exp_simple(term.exp_exp_p^); {process nested exp}
      return;
      end;

sst_term_func_k,
sst_term_ifunc_k,
sst_term_field_k,
sst_term_arele_k: return;

otherwise
    sys_msg_parm_int (msg_parm[1], ord(term.ttype));
    syn_error (term.str_h, 'sst', 'term_type_unknown', msg_parm, 1);
    end;
  sst_term_simple := true;             {we now know that expression is simple}
  end;
