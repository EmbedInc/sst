{   Subroutine SST_EXP_SIMPLE (EXP)
*
*   Return TRUE if the expression is "simple".  This means it has only one term
*   and contains no function references.
}
module sst_EXP_SIMPLE;
define sst_exp_simple;
%include 'sst2.ins.pas';

function sst_exp_simple (              {check for expression is simple/complicated}
  in      exp: sst_exp_t)              {expression descriptor to check}
  : boolean;                           {TRUE if no computes needed to evaluate exp}

begin
  if exp.term1.next_p <> nil then begin {more than one term ?}
    sst_exp_simple := false;
    return;
    end;
  sst_exp_simple := sst_term_simple(exp.term1);
  end;
