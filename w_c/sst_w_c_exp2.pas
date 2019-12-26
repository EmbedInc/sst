{   Subroutine SST_W_C_EXP2 (TERM_FIRST1, N_TERMS1, TERM_FIRST2, N_TERMS2, OP,
*                            ENCLOSE)
*
*   Write two expressions with an operator in between.  TERM_FIRST1 is the
*   descriptor for the first term in the expression before the operator.
*   N_TERMS1 is the number of terms in the chain starting at TERM_FIRST1.
*   TERM_FIRST2 and N_TERMS2 work in a likewise fashion.  OP identifies the
*   operator between the two expressions.  ENCLOSE
*   indicates whether the final expression should be enclosed in parentheses.
*   Value of ENCLOSE are:
*
*     ENCLOSE_YES_K  -  Enclose in parentheses, if needed to make the entire
*       expression be one term.
*
*     ENCLOSE_NO_K  -  Don't enclose expression in parentheses, even if is is
*       written as more than one term with operators in between.
}
module sst_W_C_EXP2;
define sst_w_c_exp2;
%include 'sst_w_c.ins.pas';

procedure sst_w_c_exp2 (               {write 2 expressions with operator in between}
  in      term_first1: sst_exp_term_t; {first term in expression 1}
  in      n_terms1: sys_int_machine_t; {number of terms in expression 1}
  in      term_first2: sst_exp_term_t; {first term in expression 2}
  in      n_terms2: sys_int_machine_t; {number of terms in expression 2}
  in      op: sst_op2_k_t;             {operator acting between the terms}
  in      enclose: enclose_k_t);       {enclose in () yes/no}

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  term_p: sst_exp_term_p_t;            {scratch pointer to a term in expression}
  i: sys_int_machine_t;                {scratch integer and loop counter}
  dt_p: sst_dtype_p_t;                 {scratch pointer to data type descriptor}
  sym1_p, sym2_p: sst_symbol_p_t;      {pointers to implicit variables}
  dt: sst_dtype_k_t;                   {scratch data type ID}
  opname: string_var4_t;               {operator name when direct C operator exists}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

label
  op2_eq_ne, op_map_direct, leave;
{
********************************
*
*   Local subroutine WRITE_EXP (T,N,ENC)
*
*   Write an expression starting at term T, with N terms in it.  ENC is
*   the standard parentheses enclose yes/no selector.  It must be either
*   ENCLOSE_YES_K, or ENCLOSE_NO_K.  These have the same meaning as for the
*   ENCLOSE argument of the main routine.  See comment header for main routine.
}
procedure write_exp (
  in      t: sst_exp_term_t;           {descriptor for first term in expression}
  in      n: sys_int_machine_t;        {number of terms in expression chain}
  in      enc: enclose_k_t);           {enclose in () yes/no flag}

var
  last_p: sst_exp_term_p_t;            {points to last term in expression}
  i: sys_int_machine_t;                {loop counter}

begin
  if n <= 1
    then begin                         {the expression is just one term}
      sst_w_c_term (t, 0, enc);        {write the expression as a term}
      end
    else begin                         {this is a compound expression}
      last_p := t.next_p;              {init last term pointer to second term in exp}
      for i := 3 to n do begin         {once for each time to advance LAST_P}
        last_p := last_p^.next_p;      {advance LAST_P to point to next term in exp}
        end;
      sst_w_c_exp2 (                   {write exp as 2 expressions with operator}
        t,                             {first term in expression before operator}
        n - 1,                         {number of terms in expression before op}
        last_p^,                       {first term in expression after operator}
        1,                             {number of terms in expression after op}
        last_p^.op2,                   {operator between the two expressions}
        enc);                          {enclose in () yes/no flag}
      end
    ;
  end;
{
********************************
*
*   Start of main routine.
}
begin
  opname.max := sizeof(opname.str);    {init local var string}
  opname.len := 0;                     {init operator name string to empty}

  case op of                           {what is the operator between expressions ?}
{
*   The following operators map directly to target language operators.
*   The cases for these operators will fall thru to the general code that
*   assumes OPNAME is set to the name of the operator in the target language.
}
sst_op2_add_k: string_appendn (opname, '+', 1); {term1 + term2}
sst_op2_sub_k: string_appendn (opname, '-', 1); {term1 - term2}
sst_op2_mult_k: string_appendn (opname, '*', 1); {term1 * term2}
sst_op2_rem_k: string_appendn (opname, '%', 1); {remainder of term1 / term2}
sst_op2_btand_k: string_appendn (opname, '&', 1); {bitwise and}
sst_op2_btor_k: string_appendn (opname, '|', 1); {bitwise or}
sst_op2_ge_k: string_appendn (opname, '>=', 2); {TRUE if term1 greater than or equal to term2}
sst_op2_gt_k: string_appendn (opname, '>', 1); {TRUE if term1 greater than term2}
sst_op2_le_k: string_appendn (opname, '<=', 2); {TRUE if term1 less than or equal to term2}
sst_op2_lt_k: string_appendn (opname, '<', 1); {TRUE if term1 less than term2}
sst_op2_and_k: string_appendn (opname, '&&', 2); {logical AND}
sst_op2_or_k: string_appendn (opname, '||', 2); {logical OR}
sst_op2_andthen_k: string_appendn (opname, '&&', 2); {logical AND, first op evaluated first}
sst_op2_orelse_k: string_appendn (opname, '||', 2); {logical OR, first op evaluated first}
sst_op2_union_k: string_appendn (opname, '|', 1); {term1 UNION term2}
sst_op2_isect_k: string_appendn (opname, '&', 1); {term1 INTERSECTION term2}
{
*   The following operators do not map directly to target language operators in
*   all cases.  When the mapping IS direct, set OPNAME to the name of the target
*   language operator and either fall thru to after the CASE statement or jump
*   to OP_MAP_DIRECT.  When the operator does not map directly, each case
*   must handle this itself, and jump to LEAVE when all done.
}
{
**********
}
sst_op2_eq_k: begin                    {TRUE if term1 = term2}
  string_appendn (opname, '==', 2);
  goto op2_eq_ne;
  end;
sst_op2_ne_k: begin                    {TRUE if term1 <> term2}
  string_appendn (opname, '!=', 2);
op2_eq_ne:                             {doing = or <> operator}
  dt_p := term_first2.dtype_p;         {determine base data type of second term}
  while dt_p^.dtype = sst_dtype_copy_k do dt_p := dt_p^.copy_dtype_p;
  case dt_p^.dtype of
sst_dtype_rec_k,
sst_dtype_array_k: begin
      syn_error (term_first2.str_h, 'sst_c_write', 'op_compare_dtype_bad', nil, 0);
      end;
    end;
  end;
{
**********
*
*   Real number divide.  The C compiler will do an integer divide if both
*   terms have integer data type.  If both TERM1 and TERM2 have at least one
*   component that does not have floating point type then we will explicitly
*   type-cast the second term to force a real number divide.
}
sst_op2_div_k: begin                   {term1 / term2, all treated as real numbers}
  term_p := addr(term_first1);         {init curr term to first in exp1}
  for i := 1 to n_terms1 do begin      {scan all the terms in first expression}
    sst_dtype_resolve (                {resolve term's base data type}
      term_p^.dtype_p^, dt_p, dt);
    if dt <> sst_dtype_float_k then exit; {not all terms have floating point type ?}
    if i = n_terms1 then begin         {all terms have floating point data type ?}
      string_appendn (opname, '/', 1); {we can use regular "/" operator}
      goto op_map_direct;
      end;
    term_p := term_p^.next_p;          {advance to next term in first expression}
    end;
{
*   At least one term in the first expression does not have floating point data
*   type.  This means we need to check second expression.
}
  term_p := addr(term_first2);         {init curr term to first in exp2}
  for i := 1 to n_terms2 do begin      {scan all the terms in second expression}
    sst_dtype_resolve (                {resolve term's base data type}
      term_p^.dtype_p^, dt_p, dt);
    if dt <> sst_dtype_float_k then exit; {not all terms have floating point type ?}
    if i = n_terms1 then begin         {all terms have floating point data type ?}
      string_appendn (opname, '/', 1); {we can use regular "/" operator}
      goto op_map_direct;
      end;
    term_p := term_p^.next_p;          {advance to next term in first expression}
    end;
{
*   At least one term in both expressions does not have floating point data
*   type.  We now definately need to type-cast at least one of the expressions
*   to prevent the C compiler from doing an integer divide.  We will type cast
*   the second expression.
}
  if enclose = enclose_yes_k then begin {enclose expression in () to make 1 term ?}
    sst_w.appendn^ ('(', 1);           {write leading paren}
    end;
  write_exp (term_first1, n_terms1, enclose_yes_k); {write first expression}
  sst_w.delimit^;
  sst_w.appendn^ ('/', 1);
  sst_w.delimit^;
  sst_w.appendn^ ('(double)', 8);      {force second expression to be floating point}
  write_exp (term_first2, n_terms2, enclose_yes_k); {write second expression}
  if enclose = enclose_yes_k then begin {enclose expression in () to make 1 term ?}
    sst_w.appendn^ (')', 1);           {write trailing paren}
    end;
  goto leave;
  end;
{
**********
}
sst_op2_divi_k: begin                  {term1 / term2, term2 and result are integer}
  term_p := addr(term_first1);         {init curr term to first in exp1}
  for i := 1 to n_terms1 do begin      {scan all the terms in first expression}
    sst_dtype_resolve (                {resolve term's base data type}
      term_p^.dtype_p^, dt_p, dt);
    if dt <> sst_dtype_int_k then exit; {not all terms have integer value ?}
    if i = n_terms1 then begin         {all terms have integer data type ?}
      string_appendn (opname, '/', 1); {we can use regular "/" operator}
      goto op_map_direct;
      end;
    term_p := term_p^.next_p;          {advance to next term in first expression}
    end;
{
*   At least one term in the first expression does not have an integer value.
*   This means we have to explicitly cast the first expression as an integer
*   before using the "/" operator.
}
  if enclose = enclose_yes_k then begin {enclose expression in () to make 1 term ?}
    sst_w.appendn^ ('(', 1);           {write leading paren}
    end;
  sst_w.appendn^ ('(int)', 5);         {integer type-case operator}
  write_exp (term_first1, n_terms1, enclose_yes_k); {write first expression}
  sst_w.delimit^;
  sst_w.appendn^ ('/', 1);
  sst_w.delimit^;
  write_exp (term_first2, n_terms2, enclose_yes_k); {write second expression}
  if enclose = enclose_yes_k then begin {enclose expression in () to make 1 term ?}
    sst_w.appendn^ (')', 1);           {write trailing paren}
    end;
  goto leave;
  end;
{
**********
}
sst_op2_pwr_k: begin                   {term1 to-power-of term2}
  sst_w_c_intrinsic (intr_pow_k);      {write POW function name}
  sst_w.appendn^ ('(', 1);
  sst_w.allow_break^;
  write_exp (term_first1, n_terms1, enclose_no_k); {write first expression}
  sst_w.appendn^ (',', 1);
  sst_w.allow_break^;
  write_exp (term_first2, n_terms2, enclose_no_k); {write second expression}
  sst_w.appendn^ (')', 1);
  goto leave;
  end;
{
**********
*
*   True if EXP1 is member of EXP2.  Form will be:
*
*     ((1 << exp1) & exp2) != 0
}
sst_op2_in_k: begin                    {TRUE if term1 is member of term2}
  if enclose = enclose_yes_k then begin {enclose expression in () to make 1 term ?}
    sst_w.appendn^ ('(', 1);           {write leading paren}
    end;
  sst_w.appendn^ ('((1', 3);
  sst_w.delimit^;
  sst_w.appendn^ ('<<', 2);
  sst_w.delimit^;
  write_exp (term_first1, n_terms1, enclose_yes_k); {write first expression}
  sst_w.appendn^ (')', 1);
  sst_w.delimit^;
  sst_w.appendn^ ('&', 1);
  sst_w.delimit^;
  write_exp (term_first2, n_terms2, enclose_yes_k); {write second expression}
  sst_w.appendn^ (')', 1);
  sst_w.delimit^;
  sst_w.appendn^ ('!=', 2);
  sst_w.delimit^;
  sst_w.appendn^ ('0', 1);
  if enclose = enclose_yes_k then begin {enclose expression in () to make 1 term ?}
    sst_w.appendn^ (')', 1);           {write trailing paren}
    end;
  goto leave;
  end;
{
**********
*
*   Remove all members in EXP2 from EXP1.
*
*     exp1 & ~exp2
}
sst_op2_remov_k: begin                 {REMOVE all members of term2 from term1}
  if enclose = enclose_yes_k then begin {enclose expression in () to make 1 term ?}
    sst_w.appendn^ ('(', 1);           {write leading paren}
    end;
  write_exp (term_first1, n_terms1, enclose_yes_k); {write first expression}
  sst_w.delimit^;
  sst_w.appendn^ ('&', 1);
  sst_w.delimit^;
  sst_w.appendn^ ('~', 1);             {indicate one's complement of expression 2}
  write_exp (term_first2, n_terms2, enclose_yes_k); {write second expression}
  if enclose = enclose_yes_k then begin {enclose expression in () to make 1 term ?}
    sst_w.appendn^ (')', 1);           {write trailing paren}
    end;
  goto leave;
  end;
{
**********
*
*   TRUE if EXP1 is a proper subset of EXP2.  This means that EXP1 <> EXP2.
*
*     (exp1 & exp2) == exp1 && exp1 != exp2
*
*   This may require implicit variables be created for exp1 and exp2.
}
sst_op2_subset_k: begin                {TRUE if term1 is proper subset of term2}
  if enclose = enclose_yes_k then begin {enclose expression in () to make 1 term ?}
    sst_w.appendn^ ('(', 1);           {write leading paren}
    end;
  sst_w_c_terms_implicit (             {make implicit var for exp1 if needed}
    term_first1,                       {first term in expression}
    n_terms1,                          {number of terms in expression}
    term_first1.dtype_p^,              {data type for implicit variable}
    sym1_p);                           {returned pointer to implicit var, if any}
  sst_w_c_terms_implicit (             {make implicit var for exp2 if needed}
    term_first2,                       {first term in expression}
    n_terms2,                          {number of terms in expression}
    term_first2.dtype_p^,              {data type for implicit variable}
    sym2_p);                           {returned pointer to implicit var, if any}

  sst_w.appendn^ ('(', 1);
  if sym1_p = nil                      {write value of expression 1}
    then write_exp (term_first1, n_terms1, enclose_yes_k)
    else sst_w.append_sym_name^ (sym1_p^);
  sst_w.delimit^;
  sst_w.appendn^ ('&', 1);
  sst_w.delimit^;
  if sym2_p = nil                      {write value of expression 2}
    then write_exp (term_first2, n_terms2, enclose_yes_k)
    else sst_w.append_sym_name^ (sym2_p^);
  sst_w.appendn^ (')', 1);
  sst_w.delimit^;
  sst_w.appendn^ ('==', 2);
  sst_w.delimit^;
  if sym1_p = nil                      {write value of expression 1}
    then write_exp (term_first1, n_terms1, enclose_yes_k)
    else sst_w.append_sym_name^ (sym1_p^);
  sst_w.delimit^;
  sst_w.appendn^ ('&&', 2);
  sst_w.delimit^;
  if sym1_p = nil                      {write value of expression 1}
    then write_exp (term_first1, n_terms1, enclose_yes_k)
    else sst_w.append_sym_name^ (sym1_p^);
  sst_w.delimit^;
  sst_w.appendn^ ('!=', 2);
  sst_w.delimit^;
  if sym2_p = nil                      {write value of expression 2}
    then write_exp (term_first2, n_terms2, enclose_yes_k)
    else sst_w.append_sym_name^ (sym2_p^);
  if enclose = enclose_yes_k then begin {enclose expression in () to make 1 term ?}
    sst_w.appendn^ (')', 1);           {write trailing paren}
    end;
  goto leave;
  end;
{
**********
*
*   TRUE if EXP1 is a subset of EXP2.  This means that EXP1 may equal EXP2.
*
*     (exp1 & exp2) == exp1
*
*   This may require an implicit variable be created for EXP1.
}
sst_op2_subset_eq_k: begin             {TRUE if term1 is subset or equal to term2}
  if enclose = enclose_yes_k then begin {enclose expression in () to make 1 term ?}
    sst_w.appendn^ ('(', 1);           {write leading paren}
    end;
  sst_w_c_terms_implicit (             {make implicit var for exp1 if needed}
    term_first1,                       {first term in expression}
    n_terms1,                          {number of terms in expression}
    term_first1.dtype_p^,              {data type for implicit variable}
    sym1_p);                           {returned pointer to implicit var, if any}

  sst_w.appendn^ ('(', 1);
  if sym1_p = nil                      {write value of expression 1}
    then write_exp (term_first1, n_terms1, enclose_yes_k)
    else sst_w.append_sym_name^ (sym1_p^);
  sst_w.delimit^;
  sst_w.appendn^ ('&', 1);
  sst_w.delimit^;
  write_exp (term_first2, n_terms2, enclose_yes_k); {write value of expression 2}
  sst_w.appendn^ (')', 1);
  sst_w.delimit^;
  sst_w.appendn^ ('==', 2);
  sst_w.delimit^;
  if sym1_p = nil                      {write value of expression 1}
    then write_exp (term_first1, n_terms1, enclose_yes_k)
    else sst_w.append_sym_name^ (sym1_p^);
  if enclose = enclose_yes_k then begin {enclose expression in () to make 1 term ?}
    sst_w.appendn^ (')', 1);           {write trailing paren}
    end;
  goto leave;
  end;
{
**********
*
*   TRUE if EXP1 is a proper superset of EXP2.  This means that EXP1 <> EXP2.
*
*     (exp1 & exp2) == exp2 && exp1 != exp2
*
*   This may require implicit variables be created for exp1 and exp2.
}
sst_op2_superset_k: begin              {TRUE if term1 is proper superset of term2}
  if enclose = enclose_yes_k then begin {enclose expression in () to make 1 term ?}
    sst_w.appendn^ ('(', 1);           {write leading paren}
    end;
  sst_w_c_terms_implicit (             {make implicit var for exp1 if needed}
    term_first1,                       {first term in expression}
    n_terms1,                          {number of terms in expression}
    term_first1.dtype_p^,              {data type for implicit variable}
    sym1_p);                           {returned pointer to implicit var, if any}
  sst_w_c_terms_implicit (             {make implicit var for exp2 if needed}
    term_first2,                       {first term in expression}
    n_terms2,                          {number of terms in expression}
    term_first2.dtype_p^,              {data type for implicit variable}
    sym2_p);                           {returned pointer to implicit var, if any}

  sst_w.appendn^ ('(', 1);
  if sym1_p = nil                      {write value of expression 1}
    then write_exp (term_first1, n_terms1, enclose_yes_k)
    else sst_w.append_sym_name^ (sym1_p^);
  sst_w.delimit^;
  sst_w.appendn^ ('&', 1);
  sst_w.delimit^;
  if sym2_p = nil                      {write value of expression 2}
    then write_exp (term_first2, n_terms2, enclose_yes_k)
    else sst_w.append_sym_name^ (sym2_p^);
  sst_w.appendn^ (')', 1);
  sst_w.delimit^;
  sst_w.appendn^ ('==', 2);
  sst_w.delimit^;
  if sym2_p = nil                      {write value of expression 2}
    then write_exp (term_first2, n_terms2, enclose_yes_k)
    else sst_w.append_sym_name^ (sym2_p^);
  sst_w.delimit^;
  sst_w.appendn^ ('&&', 2);
  sst_w.delimit^;
  if sym1_p = nil                      {write value of expression 1}
    then write_exp (term_first1, n_terms1, enclose_yes_k)
    else sst_w.append_sym_name^ (sym1_p^);
  sst_w.delimit^;
  sst_w.appendn^ ('!=', 2);
  sst_w.delimit^;
  if sym2_p = nil                      {write value of expression 2}
    then write_exp (term_first2, n_terms2, enclose_yes_k)
    else sst_w.append_sym_name^ (sym2_p^);
  if enclose = enclose_yes_k then begin {enclose expression in () to make 1 term ?}
    sst_w.appendn^ (')', 1);           {write trailing paren}
    end;
  goto leave;
  end;
{
**********
*
*   TRUE if EXP1 is a superset of EXP2.  This means that EXP1 may equal EXP2.
*
*     (exp1 & exp2) == exp2
*
*   This may require an implicit variable be created for EXP2.
}
sst_op2_superset_eq_k: begin           {TRUE if term1 is superset or equal to term2}
  if enclose = enclose_yes_k then begin {enclose expression in () to make 1 term ?}
    sst_w.appendn^ ('(', 1);           {write leading paren}
    end;
  sst_w_c_terms_implicit (             {make implicit var for exp2 if needed}
    term_first2,                       {first term in expression}
    n_terms2,                          {number of terms in expression}
    term_first2.dtype_p^,              {data type for implicit variable}
    sym2_p);                           {returned pointer to implicit var, if any}

  sst_w.appendn^ ('(', 1);
  write_exp (term_first1, n_terms1, enclose_yes_k); {write value of expression 1}
  sst_w.delimit^;
  sst_w.appendn^ ('&', 1);
  sst_w.delimit^;
  if sym2_p = nil                      {write value of expression 2}
    then write_exp (term_first2, n_terms2, enclose_yes_k)
    else sst_w.append_sym_name^ (sym2_p^);
  sst_w.appendn^ (')', 1);
  sst_w.delimit^;
  sst_w.appendn^ ('==', 2);
  sst_w.delimit^;
  if sym2_p = nil                      {write value of expression 2}
    then write_exp (term_first2, n_terms2, enclose_yes_k)
    else sst_w.append_sym_name^ (sym2_p^);
  if enclose = enclose_yes_k then begin {enclose expression in () to make 1 term ?}
    sst_w.appendn^ (')', 1);           {write trailing paren}
    end;
  goto leave;
  end;
{
**********
}
otherwise                              {illegal or unimplemented operator}
    sys_msg_parm_int (msg_parm[1], ord(op));
    sys_message_bomb ('sst', 'operator_unknown_diadic', msg_parm, 1);
    end;                               {end of operator cases}
{
*   All the cases that fall thru here are where the SST operator maps directly
*   to a target language operator.  OPNAME is set to the operator name.
}
op_map_direct:                         {jump here when OPNAME is target language op}
  if enclose = enclose_yes_k then begin {enclose expression in () to make 1 term ?}
    sst_w.appendn^ ('(', 1);           {write leading paren}
    end;
  write_exp (                          {write expression before operator}
    term_first1, n_terms1, enclose_yes_k);
  sst_w.delimit^;
  sst_w.append^ (opname);              {write operator}
  sst_w.delimit^;
  write_exp (                          {write expression after operator}
    term_first2, n_terms2, enclose_yes_k);
  if enclose = enclose_yes_k then begin {enclose expression in () to make 1 term ?}
    sst_w.appendn^ (')', 1);           {write trailing paren}
    end;

leave:                                 {common exit point}
  end;
