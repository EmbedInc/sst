{   Subroutine SST_EXP_EVAL (EXP,NVAL_ERR)
*
*   Evaluate a compiled expression.  This means determining its data type, and
*   value if known at compile time.  EXP is the expression to evaluate.  If
*   NVAL_ERR is TRUE, then it will be considered an error if the expression can
*   not be evaluated to a constant value.
}
module sst_EXP_EVAL;
define sst_exp_eval;
%include 'sst2.ins.pas';

procedure sst_exp_eval (               {evaluate compiled expression}
  in out  exp: sst_exp_t;              {expression, fills in value and data type}
  in      nval_err: boolean);          {unknown value at compile time is err if TRUE}

const
  max_msg_parms = 1;                   {max paramters we can pass to a message}

var
  term_p: sst_exp_term_p_t;            {points to current term in expression}
  dt_p, dt2_p: sst_dtype_p_t;          {scratch data type pointers}
  dt_exp_p: sst_dtype_p_t;             {points to base dtype descriptor of exp}
  dt_term_p: sst_dtype_p_t;            {points to base dtype descriptor of term}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

label
  dtype_mismatch, bad_operator, not_readable, leave;

begin
  if exp.val_eval then goto leave;     {expression already evaluated before ?}
  sst_term_eval (exp.term1, nval_err); {evaluate first term}
  exp.dtype_p := exp.term1.dtype_p;    {init expression data type from first term}
  if exp.term1.next_p = nil            {this is a simple expression ?}
    then exp.dtype_hard := exp.term1.dtype_hard {copy hard/soft flag from term 1}
    else exp.dtype_hard := false;      {compound exp always have soft data type}
  exp.val_fnd := exp.term1.val_fnd;    {get constant value exists flag from term 1}
  if exp.val_fnd
    then begin                         {first term has a known constant value}
      exp.val := exp.term1.val;        {copy whole value descriptor from term}
      end
    else begin                         {first term is not a known constant}
      exp.val.dtype := exp.term1.val.dtype; {only need base data type ID}
      end
    ;
  exp.rwflag := exp.term1.rwflag;      {promote read/write permission flag}
  if                                   {compound exp, but first term not readable ?}
      (not (sst_rwflag_read_k in exp.rwflag)) and
      (exp.term1.next_p <> nil)
      then begin
    term_p := addr(exp.term1);         {set pointer to offending term}
    goto not_readable;
    end;
  exp.val_eval := true;                {flag expression as evaluated}
  term_p := exp.term1.next_p;          {set pointer for next term to process}
{
*   The expression value and data type have been initialized to the value and
*   data type of the first term.  We will loop back here each new term and
*   accumulate the resulting expression value and data type in EXP.  Once we
*   encounter a term that does not have a constant value, then we stop trying
*   to resolve the constant value of the expression, but continue resolving
*   the data type.
}
  while term_p <> nil do begin         {once for each remaining term in expression}
    with term_p^: term do begin        {TERM is abbreviation for current term}
      sst_term_eval (term, nval_err);  {evaluate this term}
      exp.val_fnd := exp.val_fnd and term.val_fnd; {TRUE if still have known value}
      if not (sst_rwflag_read_k in term.rwflag) {term not readable ?}
        then goto not_readable;
      exp.rwflag :=                    {expressions with > 1 term are not writeable}
        exp.rwflag - [sst_rwflag_write_k];
      dt_exp_p := exp.dtype_p;         {find pointer to base expression data type}
      while dt_exp_p^.dtype = sst_dtype_copy_k do begin
        dt_exp_p := dt_exp_p^.copy_dtype_p;
        end;
      dt_term_p := term.dtype_p;       {find pointer to base term data type}
      while dt_term_p^.dtype = sst_dtype_copy_k do begin
        dt_term_p := dt_term_p^.copy_dtype_p;
        end;
      case exp.val.dtype of            {cases for raw expression data type so far}
{
********************************************
*
*   Existing expression type is INTEGER.
}
sst_dtype_int_k: begin
  case term.val.dtype of               {cases for data type of new term}
{
*   INTEGER op INTEGER
}
sst_dtype_int_k: begin
  case term.op2 of
sst_op2_add_k: begin                   {+}
      exp.dtype_p := sst_dtype_int_max_p;
      if exp.val_fnd then begin
        exp.val.int_val := exp.val.int_val + term.val.int_val;
        end;
      end;
sst_op2_sub_k: begin                   {-}
      exp.dtype_p := sst_dtype_int_max_p;
      if exp.val_fnd then begin
        exp.val.int_val := exp.val.int_val - term.val.int_val;
        end;
      end;
sst_op2_mult_k: begin                  {*}
      exp.dtype_p := sst_dtype_int_max_p;
      if exp.val_fnd then begin
        exp.val.int_val := exp.val.int_val * term.val.int_val;
        end;
      end;
sst_op2_div_k: begin                   {divide, floating point}
      exp.dtype_p := sst_dtype_float_max_p;
      if exp.val_fnd then begin
        exp.val.float_val := exp.val.int_val / term.val.int_val;
        end;
      end;
sst_op2_divi_k: begin                  {divide, integer}
      exp.dtype_p := sst_dtype_int_max_p;
      if exp.val_fnd then begin
        exp.val.int_val := exp.val.int_val div term.val.int_val;
        end;
      end;
sst_op2_rem_k: begin                   {remainder}
      exp.dtype_p := sst_dtype_int_max_p;
      if exp.val_fnd then begin
        exp.val.int_val := exp.val.int_val mod term.val.int_val;
        end;
      end;
sst_op2_pwr_k: begin                   {**}
      exp.dtype_p := sst_dtype_int_max_p;
      if exp.val_fnd then begin
        exp.val.int_val := exp.val.int_val ** term.val.int_val;
        end;
      end;
sst_op2_btand_k: begin                 {bitwise AND}
      exp.dtype_p := sst_dtype_int_max_p;
      if exp.val_fnd then begin
        exp.val.int_val := exp.val.int_val & term.val.int_val;
        end;
      end;
sst_op2_btor_k: begin                  {bitwise OR}
      exp.dtype_p := sst_dtype_int_max_p;
      if exp.val_fnd then begin
        exp.val.int_val := exp.val.int_val ! term.val.int_val;
        end;
      end;
sst_op2_eq_k: begin                    {= comparison}
      exp.dtype_p := sst_dtype_bool_p;
      if exp.val_fnd then begin
        exp.val.bool_val := exp.val.int_val = term.val.int_val;
        end;
      end;
sst_op2_ne_k: begin                    {<> comparison}
      exp.dtype_p := sst_dtype_bool_p;
      if exp.val_fnd then begin
        exp.val.bool_val := exp.val.int_val <> term.val.int_val;
        end;
      end;
sst_op2_ge_k: begin                    {>= comparison}
      exp.dtype_p := sst_dtype_bool_p;
      if exp.val_fnd then begin
        exp.val.bool_val := exp.val.int_val >= term.val.int_val;
        end;
      end;
sst_op2_gt_k: begin                    {> comparison}
      exp.dtype_p := sst_dtype_bool_p;
      if exp.val_fnd then begin
        exp.val.bool_val := exp.val.int_val > term.val.int_val;
        end;
      end;
sst_op2_le_k: begin                    {<= comparison}
      exp.dtype_p := sst_dtype_bool_p;
      if exp.val_fnd then begin
        exp.val.bool_val := exp.val.int_val <= term.val.int_val;
        end;
      end;
sst_op2_lt_k: begin                    {< comparison}
      exp.dtype_p := sst_dtype_bool_p;
      if exp.val_fnd then begin
        exp.val.bool_val := exp.val.int_val < term.val.int_val;
        end;
      end;
otherwise
    goto bad_operator;
    end;                               {end of operator cases}
  end;                                 {end of term is integer case}
{
*   INTEGER op FLOATING POINT
}
sst_dtype_float_k: begin
  case term.op2 of
sst_op2_add_k: begin                   {+}
      exp.dtype_p := sst_dtype_float_max_p;
      if exp.val_fnd then begin
        exp.val.float_val := exp.val.int_val + term.val.float_val;
        end;
      end;
sst_op2_sub_k: begin                   {-}
      exp.dtype_p := sst_dtype_float_max_p;
      if exp.val_fnd then begin
        exp.val.float_val := exp.val.int_val - term.val.float_val;
        end;
      end;
sst_op2_mult_k: begin                  {*}
      exp.dtype_p := sst_dtype_float_max_p;
      if exp.val_fnd then begin
        exp.val.float_val := exp.val.int_val * term.val.float_val;
        end;
      end;
sst_op2_div_k: begin                   {divide, floating point}
      exp.dtype_p := sst_dtype_float_max_p;
      if exp.val_fnd then begin
        exp.val.float_val := exp.val.int_val / term.val.float_val;
        end;
      end;
sst_op2_pwr_k: begin                   {**}
      exp.dtype_p := sst_dtype_float_max_p;
      if exp.val_fnd then begin
        exp.val.float_val := exp.val.int_val ** term.val.float_val;
        end;
      end;
sst_op2_eq_k: begin                    {= comparison}
      exp.dtype_p := sst_dtype_bool_p;
      if exp.val_fnd then begin
        exp.val.bool_val := exp.val.int_val = term.val.float_val;
        end;
      end;
sst_op2_ne_k: begin                    {<> comparison}
      exp.dtype_p := sst_dtype_bool_p;
      if exp.val_fnd then begin
        exp.val.bool_val := exp.val.int_val <> term.val.float_val;
        end;
      end;
sst_op2_ge_k: begin                    {>= comparison}
      exp.dtype_p := sst_dtype_bool_p;
      if exp.val_fnd then begin
        exp.val.bool_val := exp.val.int_val >= term.val.float_val;
        end;
      end;
sst_op2_gt_k: begin                    {> comparison}
      exp.dtype_p := sst_dtype_bool_p;
      if exp.val_fnd then begin
        exp.val.bool_val := exp.val.int_val > term.val.float_val;
        end;
      end;
sst_op2_le_k: begin                    {<= comparison}
      exp.dtype_p := sst_dtype_bool_p;
      if exp.val_fnd then begin
        exp.val.bool_val := exp.val.int_val <= term.val.float_val;
        end;
      end;
sst_op2_lt_k: begin                    {< comparison}
      exp.dtype_p := sst_dtype_bool_p;
      if exp.val_fnd then begin
        exp.val.bool_val := exp.val.int_val < term.val.float_val;
        end;
      end;
otherwise
    goto bad_operator;
    end;                               {end of operator cases}
  end;                                 {end of term is floating point case}
{
*   INTEGER op SET
}
sst_dtype_set_k: begin
  case term.op2 of
sst_op2_in_k: begin                    {check for is member of SET}
      exp.dtype_p := sst_dtype_bool_p;
      exp.val_fnd := false;            {we don't evaluate this case currently}
      end;                             {end of INTEGER op SET case}
otherwise
    goto bad_operator;
    end;                               {end of operator cases}
  end;                                 {end of term is a SET case}
{
*   INTEGER op <incompatible data type>
}
otherwise
    goto dtype_mismatch;
    end;                               {end of term data type cases}
  end;                                 {end of old expression is INTEGER case}
{
********************************************
*
*   Existing expression type is ENUMERATED TYPE.
}
sst_dtype_enum_k: begin
  case dt_term_p^.dtype of             {cases for data type of new term}
{
*   ENUMERATED op ENUMERATED
}
sst_dtype_enum_k: begin
  if dt_term_p^.enum_first_p <> dt_exp_p^.enum_first_p
    then goto dtype_mismatch;          {not of same enumerated type ?}
  case term.op2 of
sst_op2_eq_k: begin                    {= comparison}
      exp.dtype_p := sst_dtype_bool_p;
      if exp.val_fnd then begin
        exp.val.bool_val := exp.val.enum_p = term.val.enum_p;
        end;
      end;
sst_op2_ne_k: begin                    {<> comparison}
      exp.dtype_p := sst_dtype_bool_p;
      if exp.val_fnd then begin
        exp.val.bool_val := exp.val.enum_p <> term.val.enum_p;
        end;
      end;
otherwise
    goto bad_operator;
    end;                               {end of operator cases}
  end;                                 {end of term is ENUMERATED case}
{
*   ENUMERATED op SET
}
sst_dtype_set_k: begin
  case term.op2 of
sst_op2_in_k: begin                    {check for is member of SET}
      exp.dtype_p := sst_dtype_bool_p;
      exp.val_fnd := false;            {we don't evaluate this case currently}
      end;                             {end of ENUMERATED op SET case}
otherwise
    goto bad_operator;
    end;                               {end of operator cases}
  end;                                 {end of term is a SET case}
{
*   ENUMERATED op <incompatible data type>
}
otherwise
    goto dtype_mismatch;
    end;                               {end of term data type cases}
  end;                                 {end of old expression is ENUMERATED case}
{
********************************************
*
*   Existing expression type is FLOATING POINT.
}
sst_dtype_float_k: begin
  case term.val.dtype of               {cases for data type of new term}
{
*   FLOAT op INTEGER
}
sst_dtype_int_k: begin
  case term.op2 of
sst_op2_add_k: begin                   {+}
      exp.dtype_p := sst_dtype_float_max_p;
      if exp.val_fnd then begin
        exp.val.float_val := exp.val.float_val + term.val.int_val;
        end;
      end;
sst_op2_sub_k: begin                   {-}
      exp.dtype_p := sst_dtype_float_max_p;
      if exp.val_fnd then begin
        exp.val.float_val := exp.val.float_val - term.val.int_val;
        end;
      end;
sst_op2_mult_k: begin                  {*}
      exp.dtype_p := sst_dtype_float_max_p;
      if exp.val_fnd then begin
        exp.val.float_val := exp.val.float_val * term.val.int_val;
        end;
      end;
sst_op2_div_k: begin                   {divide, floating point}
      exp.dtype_p := sst_dtype_float_max_p;
      if exp.val_fnd then begin
        exp.val.float_val := exp.val.float_val / term.val.int_val;
        end;
      end;
sst_op2_pwr_k: begin                   {**}
      exp.dtype_p := sst_dtype_float_max_p;
      if exp.val_fnd then begin
        exp.val.float_val := exp.val.float_val ** term.val.int_val;
        end;
      end;
sst_op2_eq_k: begin                    {= comparison}
      exp.dtype_p := sst_dtype_bool_p;
      if exp.val_fnd then begin
        exp.val.bool_val := exp.val.float_val = term.val.int_val;
        end;
      end;
sst_op2_ne_k: begin                    {<> comparison}
      exp.dtype_p := sst_dtype_bool_p;
      if exp.val_fnd then begin
        exp.val.bool_val := exp.val.float_val <> term.val.int_val;
        end;
      end;
sst_op2_ge_k: begin                    {>= comparison}
      exp.dtype_p := sst_dtype_bool_p;
      if exp.val_fnd then begin
        exp.val.bool_val := exp.val.float_val >= term.val.int_val;
        end;
      end;
sst_op2_gt_k: begin                    {> comparison}
      exp.dtype_p := sst_dtype_bool_p;
      if exp.val_fnd then begin
        exp.val.bool_val := exp.val.float_val > term.val.int_val;
        end;
      end;
sst_op2_le_k: begin                    {<= comparison}
      exp.dtype_p := sst_dtype_bool_p;
      if exp.val_fnd then begin
        exp.val.bool_val := exp.val.float_val <= term.val.int_val;
        end;
      end;
sst_op2_lt_k: begin                    {< comparison}
      exp.dtype_p := sst_dtype_bool_p;
      if exp.val_fnd then begin
        exp.val.bool_val := exp.val.float_val < term.val.int_val;
        end;
      end;
otherwise
    goto bad_operator;
    end;                               {end of operator cases}
  end;                                 {end of term is integer case}
{
*   FLOAT op FLOAT
}
sst_dtype_float_k: begin
  case term.op2 of
sst_op2_add_k: begin                   {+}
      exp.dtype_p := sst_dtype_float_max_p;
      if exp.val_fnd then begin
        exp.val.float_val := exp.val.float_val + term.val.float_val;
        end;
      end;
sst_op2_sub_k: begin                   {-}
      exp.dtype_p := sst_dtype_float_max_p;
      if exp.val_fnd then begin
        exp.val.float_val := exp.val.float_val - term.val.float_val;
        end;
      end;
sst_op2_mult_k: begin                  {*}
      exp.dtype_p := sst_dtype_float_max_p;
      if exp.val_fnd then begin
        exp.val.float_val := exp.val.float_val * term.val.float_val;
        end;
      end;
sst_op2_div_k: begin                   {divide, floating point}
      exp.dtype_p := sst_dtype_float_max_p;
      if exp.val_fnd then begin
        exp.val.float_val := exp.val.float_val / term.val.float_val;
        end;
      end;
sst_op2_pwr_k: begin                   {**}
      exp.dtype_p := sst_dtype_float_max_p;
      if exp.val_fnd then begin
        exp.val.float_val := exp.val.float_val ** term.val.float_val;
        end;
      end;
sst_op2_eq_k: begin                    {= comparison}
      exp.dtype_p := sst_dtype_bool_p;
      if exp.val_fnd then begin
        exp.val.bool_val := exp.val.float_val = term.val.float_val;
        end;
      end;
sst_op2_ne_k: begin                    {<> comparison}
      exp.dtype_p := sst_dtype_bool_p;
      if exp.val_fnd then begin
        exp.val.bool_val := exp.val.float_val <> term.val.float_val;
        end;
      end;
sst_op2_ge_k: begin                    {>= comparison}
      exp.dtype_p := sst_dtype_bool_p;
      if exp.val_fnd then begin
        exp.val.bool_val := exp.val.float_val >= term.val.float_val;
        end;
      end;
sst_op2_gt_k: begin                    {> comparison}
      exp.dtype_p := sst_dtype_bool_p;
      if exp.val_fnd then begin
        exp.val.bool_val := exp.val.float_val > term.val.float_val;
        end;
      end;
sst_op2_le_k: begin                    {<= comparison}
      exp.dtype_p := sst_dtype_bool_p;
      if exp.val_fnd then begin
        exp.val.bool_val := exp.val.float_val <= term.val.float_val;
        end;
      end;
sst_op2_lt_k: begin                    {< comparison}
      exp.dtype_p := sst_dtype_bool_p;
      if exp.val_fnd then begin
        exp.val.bool_val := exp.val.float_val < term.val.float_val;
        end;
      end;
otherwise
    goto bad_operator;
    end;                               {end of operator cases}
  end;                                 {end of term is FLOATING POINT case}
{
*   FLOATING POINT op <incompatible data type>
}
otherwise
    goto dtype_mismatch;
    end;                               {end of term data type cases}
  end;                                 {end of old expression is FLOAT case}
{
********************************************
*
*   Existing expression type is BOOLEAN.
}
sst_dtype_bool_k: begin
  case term.val.dtype of               {cases for data type of new term}
{
*   BOOLEAN op BOOLEAN
}
sst_dtype_bool_k: begin
  case term.op2 of
sst_op2_eq_k: begin                    {= comparison}
      exp.dtype_p := sst_dtype_bool_p;
      if exp.val_fnd then begin
        exp.val.bool_val := exp.val.bool_val = term.val.bool_val;
        end;
      end;
sst_op2_ne_k: begin                    {<> comparison}
      exp.dtype_p := sst_dtype_bool_p;
      if exp.val_fnd then begin
        exp.val.bool_val := exp.val.bool_val <> term.val.bool_val;
        end;
      end;
sst_op2_andthen_k,                     {logical AND, first arg evaluated first}
sst_op2_and_k: begin                   {logical AND}
      exp.dtype_p := sst_dtype_bool_p;
      if exp.val_fnd then begin
        exp.val.bool_val := exp.val.bool_val and term.val.bool_val;
        end;
      end;
sst_op2_orelse_k,                      {logical OR, first arg evaluated first}
sst_op2_or_k: begin                    {logical OR}
      exp.dtype_p := sst_dtype_bool_p;
      if exp.val_fnd then begin
        exp.val.bool_val := exp.val.bool_val or term.val.bool_val;
        end;
      end;
otherwise
    goto bad_operator;
    end;                               {end of operator cases}
  end;                                 {end of term is BOOLEAN case}
{
*   BOOLEAN op SET
}
sst_dtype_set_k: begin
  case term.op2 of
sst_op2_in_k: begin                    {check for is member of SET}
      exp.dtype_p := sst_dtype_bool_p;
      exp.val_fnd := false;            {we don't evaluate this case currently}
      end;                             {end of BOOLEAN op SET case}
otherwise
    goto bad_operator;
    end;                               {end of operator cases}
  end;                                 {end of term is a SET case}
{
*   BOOLEAN op <incompatible data type>
}
otherwise
    goto dtype_mismatch;
    end;                               {end of term data type cases}
  end;                                 {end of old expression is BOOLEAN case}
{
********************************************
*
*   Existing expression type is CHARACTER.
}
sst_dtype_char_k: begin
  case term.val.dtype of               {cases for data type of new term}
{
*   CHARACTER op CHARACTER
}
sst_dtype_char_k: begin
  case term.op2 of
sst_op2_eq_k: begin                    {= comparison}
      exp.dtype_p := sst_dtype_bool_p;
      if exp.val_fnd then begin
        exp.val.bool_val := exp.val.char_val = term.val.char_val;
        end;
      end;
sst_op2_ne_k: begin                    {<> comparison}
      exp.dtype_p := sst_dtype_bool_p;
      if exp.val_fnd then begin
        exp.val.bool_val := exp.val.char_val <> term.val.char_val;
        end;
      end;
sst_op2_ge_k: begin                    {>= comparison}
      exp.dtype_p := sst_dtype_bool_p;
      if exp.val_fnd then begin
        exp.val.bool_val := exp.val.char_val >= term.val.char_val;
        end;
      end;
sst_op2_gt_k: begin                    {> comparison}
      exp.dtype_p := sst_dtype_bool_p;
      if exp.val_fnd then begin
        exp.val.bool_val := exp.val.char_val > term.val.char_val;
        end;
      end;
sst_op2_le_k: begin                    {<= comparison}
      exp.dtype_p := sst_dtype_bool_p;
      if exp.val_fnd then begin
        exp.val.bool_val := exp.val.char_val <= term.val.char_val;
        end;
      end;
sst_op2_lt_k: begin                    {< comparison}
      exp.dtype_p := sst_dtype_bool_p;
      if exp.val_fnd then begin
        exp.val.bool_val := exp.val.char_val < term.val.char_val;
        end;
      end;
otherwise
    goto bad_operator;
    end;                               {end of operator cases}
  end;                                 {end of term is CHAR case}
{
*   CHARACTER op SET
}
sst_dtype_set_k: begin
  case term.op2 of
sst_op2_in_k: begin                    {check for is member of SET}
      exp.dtype_p := sst_dtype_bool_p;
      exp.val_fnd := false;            {we don't evaluate this case currently}
      end;                             {end of CHARACTER op SET case}
otherwise
    goto bad_operator;
    end;                               {end of operator cases}
  end;                                 {end of term is a SET case}
{
*   CHAR op <incompatible data type>
}
otherwise
    goto dtype_mismatch;
    end;                               {end of term data type cases}
  end;                                 {end of old expression is CHAR case}
{
********************************************
*
*   Existing expression type is RECORD.
}
sst_dtype_rec_k: begin
  if dt_term_p <> dt_exp_p             {term not same record type as expression ?}
    then goto dtype_mismatch;
  case term.op2 of
sst_op2_eq_k,                          {= comparison}
sst_op2_ne_k: begin                    {<> comparison}
      exp.dtype_p := sst_dtype_bool_p;
      exp.val_fnd := false;
      end;
otherwise
    goto bad_operator;
    end;                               {end of operator cases}
  end;
{
********************************************
*
*   Existing expression type is ARRAY.
}
sst_dtype_array_k: begin
  if dt_term_p <> dt_exp_p             {term not same record type as expression ?}
    then goto dtype_mismatch;
  case term.op2 of
sst_op2_eq_k,                          {= comparison}
sst_op2_ne_k: begin                    {<> comparison}
      exp.dtype_p := sst_dtype_bool_p;
      exp.val_fnd := false;
      end;
otherwise
    goto bad_operator;
    end;                               {end of operator cases}
  end;
{
********************************************
*
*   Existing expression type is SET.
}
sst_dtype_set_k: begin
  case term.val.dtype of               {cases for data type of new term}
{
*   SET op SET
}
sst_dtype_set_k: begin
  sst_set_dtypes_combine (dt_exp_p^, dt_term_p^, dt_p); {make composite data type}
  if dt_p = nil                        {sets have incompatible data types ?}
    then goto dtype_mismatch;
  case term.op2 of
sst_op2_union_k,
sst_op2_isect_k,
sst_op2_remov_k: begin
      exp.dtype_p := dt_p;
      exp.val_fnd := false;            {we don't evaluate this case currently}
      end;
sst_op2_eq_k,
sst_op2_ne_k,
sst_op2_subset_k,
sst_op2_subset_eq_k,
sst_op2_superset_k,
sst_op2_superset_eq_k: begin
      exp.dtype_p := sst_dtype_bool_p;
      exp.val_fnd := false;            {we don't evaluate this case currently}
      end;
otherwise
    goto bad_operator;
    end;                               {end of operator cases}
  end;                                 {end of SET op SET case}
{
*   SET op <incompatible data type>
}
otherwise
    goto dtype_mismatch;
    end;                               {end of term data type cases}
  end;                                 {end of old expression is SET case}
{
********************************************
*
*   Existing expression type is POINTER TYPE.
}
sst_dtype_pnt_k: begin
  case term.val.dtype of               {cases for data type of new term}
{
*   POINTER op POINTER
}
sst_dtype_pnt_k: begin
  if                                   {neither is a NIL pointer ?}
      (dt_exp_p^.pnt_dtype_p <> nil) and
      (dt_term_p^.pnt_dtype_p <> nil)
      then begin                       {need to check if pointing to same data type}
    dt_p := dt_exp_p^.pnt_dtype_p;     {find expressions's base pointed to data type}
    while dt_p^.dtype = sst_dtype_copy_k do begin
      dt_p := dt_p^.copy_dtype_p;
      end;
    dt2_p := dt_term_p^.pnt_dtype_p;   {find term's base pointed to data type}
    while dt2_p^.dtype = sst_dtype_copy_k do begin
      dt2_p := dt2_p^.copy_dtype_p;
      end;
    if dt_p <> dt2_p
      then goto dtype_mismatch;        {not pointing to same data type}
    end;
  case term.op2 of
sst_op2_eq_k, sst_op2_ne_k: begin      {= or <> comparison}
      exp.dtype_p := sst_dtype_bool_p;
      exp.val_fnd := false;            {can't resolve constant value at compile time}
      end;
otherwise
    goto bad_operator;
    end;                               {end of operator cases}
  end;                                 {end of term is POINTER case}
{
*   POINTER op <incompatible data type>
}
otherwise
    goto dtype_mismatch;
    end;                               {end of term data type cases}
  end;                                 {end of old expression is POINTER case}
{
********************************************
*
*   Data type of existing expression is illegal or unimplemented.
}
otherwise
        sys_msg_parm_int (msg_parm[1], ord(dt_exp_p^.dtype));
        syn_error (term.str_h, 'sst', 'dtype_unexpected_exp', msg_parm, 1);
        end;                           {end of previous expression data type cases}
      end;                             {done with TERM abbreviation}
    term_p := term_p^.next_p;          {point to next term in expression}
    sst_dtype_resolve (                {resolve base data types of expression}
      exp.dtype_p^, dt_p, exp.val.dtype);
    if                                 {data type is a set with fully known dtype ?}
        (dt_p^.dtype = sst_dtype_set_k) and
        (dt_p^.set_dtype_final)
        then begin
      exp.dtype_hard := true;
      end;
    end;                               {back and process next term in expression}

leave:                                 {common exit point}
  if nval_err and (not exp.val_fnd) then begin {need value but not possible ?}
    syn_error (exp.str_h, 'sst', 'exp_not_const_val', nil, 0);
    end;
  return;
{
*   The data type of the expression so far and the new term are incompatible.
}
dtype_mismatch:
  syn_error (term_p^.str_h, 'sst', 'dtype_term_mismatch', nil, 0);
{
*   The operator is incompatible with the data type of the expression and term.
}
bad_operator:
  syn_error (term_p^.str_h, 'sst', 'operator_mismatch', nil, 0);
{
*   The current term is not readable.  It is pointed to by TERM_P.
}
not_readable:
  syn_error (term_p^.str_h, 'sst', 'term_not_readable', nil, 0);
  end;
