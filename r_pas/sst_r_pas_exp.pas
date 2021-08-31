{   Subroutine SST_R_PAS_EXP (EXP_STR_H,NVAL_ERR,EXP_P)
*
*   Process EXPRESSION syntax.  EXP_P will be returned pointing to the
*   compiled expression descriptor.  EXP_STR_H is the SYN string handle for the
*   whole expression.  If NVAL_ERR is TRUE, then it will be an error if the
*   expression can not be evaluated to a constant.
*
*   This subroutine is set up to handle any of the EXPRESSIONn syntaxes,
*   regardless of which one the previous tag is for.
}
module sst_r_pas_EXP;
define sst_r_pas_exp;
%include 'sst_r_pas.ins.pas';

procedure sst_r_pas_exp (              {create compiled expression from input stream}
  in      exp_str_h: syo_string_t;     {SYN string handle for whole EXPRESSION syntax}
  in      nval_err: boolean;           {unknown value at compile time is err if TRUE}
  out     exp_p: sst_exp_p_t);         {returned pointer to new expression def}

var
  tag: sys_int_machine_t;              {syntax tag ID}
  str_h: syo_string_t;                 {handle to string associated with TAG}
  tag2: sys_int_machine_t;             {to avoid corrupting TAG}
  str2_h: syo_string_t;                {handle to string associated with TAG2}
  term_p: sst_exp_term_p_t;            {points to current term in expression}
  levels_down: sys_int_machine_t;      {number of syntax levels currently down}

label
  exp_loop, term_loop, leave;

begin
  sst_mem_alloc_namesp (               {allocate memory for expression descriptor}
    sizeof(exp_p^), exp_p);
  levels_down := 0;                    {init number of syntax levels down from call}
  exp_p^.str_h := exp_str_h;           {save string handle for whole expression}
  exp_p^.val_eval := false;            {init to not tried to evaluate expression}

exp_loop:                              {back here if just contains one nested exp}
  syo_level_down;                      {down into this EXPRESSIONn syntax level}
  levels_down := levels_down + 1;      {one more syntax level down from caller}
  syo_get_tag_msg (tag, str_h, 'sst_pas_read', 'exp_bad', nil, 0); {item or nested exp}
  case tag of
{
*************************************
*
*   First term is nested expression.  If this is the only term in the whole
*   expression, then we will compress out this level.
}
1: begin
  syo_push_pos;                        {save position at nested expression}
  syo_get_tag_msg (tag2, str2_h, 'sst_pas_read', 'exp_bad', nil, 0); {operator tag, if any}
  syo_pop_pos;                         {restore current pos to nested expression}
  if tag2 = syo_tag_end_k              {only one nested expression at this level ?}
    then goto exp_loop;                {don't create structure for "pass thru" level}
{
*   The expression here contains more than just one nested expression.
*   This means we will be filling in the data structures at this level.
}
  exp_p^.term1.op2 := sst_op2_none_k;  {first term has no diadic operator before it}
  sst_r_pas_exp_term (str_h, nval_err, exp_p^.term1); {fill in first term}
  end;                                 {end of first term is expression case}
{
*************************************
*
*   First term is an ITEM syntax.
}
2: begin
  exp_p^.term1.str_h := str_h;         {save string handle to whole ITEM}
  sst_r_pas_item (exp_p^.term1);       {read and process ITEM syntax}
  exp_p^.term1.next_p := nil;          {init to first term is last in expression}
  exp_p^.term1.op2 := sst_op2_none_k;  {first term has no diadic operator before it}
  end;                                 {end of first term is ITEM case}
{
*************************************
*
*   Unexpected TAG value for first tag in EXPRESSION.
}
otherwise
    syo_error_tag_unexp (tag, str_h);
    end;                               {end of cases for first tag in EXPRESSION}
  term_p := addr(exp_p^.term1);        {init address to last term in expression}
{
*   The first term in the expression has been processed, and the expression
*   has been initialized as if this were the only term.  We now loop back here
*   once for every new operator/term pair in the expression.  TERM_P
*   is pointing to the descriptor for the previous term.
}
term_loop:
  syo_get_tag_msg (tag, str_h, 'sst_pas_read', 'exp_bad', nil, 0); {get operator tag}
  if tag = syo_tag_end_k               {hit end of expression ?}
    then goto leave;
  sst_mem_alloc_namesp (               {allocate memory for new term descriptor}
    sizeof(term_p^.next_p^), term_p^.next_p);
  term_p := term_p^.next_p;            {make new expression descriptor current}
  with term_p^: term do begin          {TERM is descriptor for this new term}
  term.next_p := nil;                  {init to this is last term in expression}
  case tag of                          {which operator between this and prev term ?}
1: begin                               {+}
      term.op2 := sst_op2_add_k;
      end;
2: begin                               {-}
      term.op2 := sst_op2_sub_k;
      end;
3: begin                               {**}
      term.op2 := sst_op2_pwr_k;
      end;
4: begin                               {*}
      term.op2 := sst_op2_mult_k;
      end;
5: begin                               {/}
      term.op2 := sst_op2_div_k;
      end;
6: begin                               {div}
      term.op2 := sst_op2_divi_k;
      end;
7: begin                               {mod}
      term.op2 := sst_op2_rem_k;
      end;
8: begin                               {&}
      term.op2 := sst_op2_btand_k;
      end;
9: begin                               {!}
      term.op2 := sst_op2_btor_k;
      end;
10: begin                              {=}
      term.op2 := sst_op2_eq_k;
      end;
11: begin                              {<>}
      term.op2 := sst_op2_ne_k;
      end;
12: begin                              {>=}
      term.op2 := sst_op2_ge_k;
      end;
13: begin                              {>}
      term.op2 := sst_op2_gt_k;
      end;
14: begin                              {<=}
      term.op2 := sst_op2_le_k;
      end;
15: begin                              {<}
      term.op2 := sst_op2_lt_k;
      end;
16: begin                              {and}
      term.op2 := sst_op2_and_k;
      end;
17: begin                              {or}
      term.op2 := sst_op2_or_k;
      end;
18: begin                              {in}
      term.op2 := sst_op2_in_k;
      end;
19: begin                              {and then}
      term.op2 := sst_op2_andthen_k;
      end;
20: begin                              {or else}
      term.op2 := sst_op2_orelse_k;
      end;
otherwise
    syo_error_tag_unexp (tag, str_h);
    end;                               {end of operator tag cases}
  syo_get_tag_msg (tag, str_h, 'sst_pas_read', 'exp_bad', nil, 0); {get tag for new term}
  case tag of                          {what kind of term is this ?}
{
*   This new term in the expression is a nested expression.
}
1: begin
  sst_r_pas_exp_term (str_h, nval_err, term); {process nested expression as new term}
  end;                                 {end of new term is expression}
{
*   This new term in the expression is an item.
}
2: begin
  term.str_h := str_h;                 {save string handle to whole ITEM}
  sst_r_pas_item (term);               {read and process ITEM syntax}
  end;                                 {end of new term is item}
{
*   Unexpected TAG value for new term in expression.
}
otherwise
    syo_error_tag_unexp (tag, str_h);
    end;                               {end of new term type cases}
{
*   Some of the Pascal operators server dual functions.  Now look at the
*   data type of the resulting term and alter the diadic operator, if neccessary.
}
  case term.val.dtype of

sst_dtype_set_k: begin                 {term data type is SET}
      case term.op2 of
sst_op2_add_k: term.op2 := sst_op2_union_k;
sst_op2_sub_k: term.op2 := sst_op2_remov_k;
sst_op2_mult_k: term.op2 := sst_op2_isect_k;
sst_op2_ge_k: term.op2 := sst_op2_superset_eq_k;
sst_op2_gt_k: term.op2 := sst_op2_superset_k;
sst_op2_le_k: term.op2 := sst_op2_subset_eq_k;
sst_op2_lt_k: term.op2 := sst_op2_subset_k;
        end;
      end;

    end;                               {end of term data type cases}
  goto term_loop;                      {back for next operator/term pair in this exp}
  end;                                 {done with TERM abbreviation}
{
*   Common exit point.  We skipped over any expression levels if they only
*   contained another nested expression.  LEVELS_DOWN indicates how many syntax
*   levels we are down from the caller's level.  We must return to the caller's
*   syntax level.
}
leave:
  while levels_down > 0 do begin       {once for each nested syntax level}
    syo_level_up;                      {pop back one syntax level}
    levels_down := levels_down - 1;    {keep track of current nesting level}
    end;                               {back to pop another level up}
  sst_exp_eval (exp_p^, nval_err);     {find data types and evaluate if possible}
  end;
