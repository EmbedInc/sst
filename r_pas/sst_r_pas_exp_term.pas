{   Subroutine SST_R_PAS_EXP_TERM (TERM_STR_H,NVAL_ERR,TERM)
*
*   The tag for a nested EXPRESSION2 - EXPRESSION4 or ITEM syntax has just been
*   read.  Fill in TERM to reflect this nested expression.  A nested expression
*   will only be created if necessary, otherwise the term will be an item.
*   STR_H is the string handle to the nested term.  If NVAL_ERR is TRUE, then
*   it will be an error if the term does not have a resolable constant value.
}
module sst_r_pas_EXP_TERM;
define sst_r_pas_exp_term;
%include 'sst_r_pas.ins.pas';

procedure sst_r_pas_exp_term (         {read and process next term in expression}
  in      term_str_h: syn_string_t;    {SYN string handle for whole term}
  in      nval_err: boolean;           {unknown value at compile time is err if TRUE}
  out     term: sst_exp_term_t);       {term descriptor to fill in}

var
  tag: sys_int_machine_t;              {syntax tag ID}
  str_h: syn_string_t;                 {handle to string associated with TAG}
  tag2: sys_int_machine_t;             {to avoid corrupting TAG}
  str2_h: syn_string_t;                {handle to string associated with TAG2}
  levels_down: sys_int_machine_t;      {number of syntax levels currently down}

label
  exp_loop, leave;

begin
  levels_down := 0;                    {init number of syntax levels below caller}

exp_loop:                              {back here if just contains one expression}
  syn_push_pos;                        {save syn position at old level}
  syn_level_down;                      {down into this EXPRESSIONn syntax level}
  levels_down := levels_down + 1;      {one more syntax level down from caller}
  syn_get_tag_msg (tag, str_h, 'sst_pas_read', 'exp_bad', nil, 0); {item or nested exp}

  syn_push_pos;                        {save position at nested expression}
  syn_get_tag_msg (tag2, str2_h, 'sst_pas_read', 'exp_bad', nil, 0); {operator tag, if any}
  syn_pop_pos;                         {restore current pos to nested expression}

  if tag2 <> syn_tag_end_k then begin  {term is more than just one ITEM ?}
    syn_pop_pos;                       {back to start of this expression level}
    levels_down := levels_down - 1;    {one less syntax level below caller}
    term.next_p := nil;                {init to this is last term in expression}
    term.op1 := sst_op1_none_k;        {init to no preceeding unadic operator}
    term.ttype := sst_term_exp_k;      {term is a nested expression}
    term.str_h := str_h;               {save term source stream handle}
    term.val_eval := false;            {inidicate not tried to resolve constant value}
    sst_r_pas_exp (term_str_h, nval_err, term.exp_exp_p); {process nested expression}
    sst_term_eval (term, nval_err);    {fully evaluate this term}
    goto leave;                        {TERM all filled in}
    end;
{
*   There is only one tagged syntax in the current syntax level.  Its tag is
*   in TAG.
}
  case tag of
{
*   The tagged syntax is another nested expression.
}
1: begin
      goto exp_loop;                   {back for another level down}
      end;
{
*   The tagged syntax is an ITEM.
}
2: begin
      term.str_h := str_h;             {save source stream handle to this item}
      sst_r_pas_item (term);           {read and process ITEM syntax}
      end;
{
*   Unexpected TAG value.
}
otherwise
    syn_error_tag_unexp (tag, str_h);
    end;                               {end of cases for first tag in EXPRESSION}

leave:                                 {common exit point}
  while levels_down > 0 do begin       {once for each nested syntax level}
    syn_pop_pos;                       {back up one level}
    levels_down := levels_down - 1;    {keep track of current nesting level}
    end;                               {back to pop another level up}
  end;
