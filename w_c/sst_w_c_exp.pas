{   Subroutine SST_W_C_EXP (EXP, ADDR_CNT, DT_OUT_P, ENCLOSE)
*
*   Write the expression from the expression descriptor EXP.
*   ADDR_CNT is the number of times to take the address of the expression.
*   It may be negative to indicate the number of times the expression
*   should be assumed to be a pointer and dereferenced.
*   DT_OUT_P indicates the desired data type of the resulting expression.
*   It may be set to NIL to indicate the expression's inherent data type
*   need not be altered.
*   ENCLOSE indicates whether the final expression should be enclosed in
*   parentheses.  Values of ENCLOSE can be:
*
*     ENCLOSE_YES_K  -  Enclose in parentheses, if neccessary, to make the
*       entire expression be one term.
*
*     ENCLOSE_NO_K  -  Don't enclose expression in parentheses, even if is is
*       written as more than one term with operators in between.
}
module sst_W_C_EXP;
define sst_w_c_exp;
%include 'sst_w_c.ins.pas';

procedure sst_w_c_exp (                {write expression}
  in      exp: sst_exp_t;              {expression descriptor}
  in      addr_cnt: sys_int_machine_t; {number of times to take address of}
  in      dt_out_p: sst_dtype_p_t;     {desired output data type, NIL = as is}
  in      enclose: enclose_k_t);       {enclose in () yes/no}

const
  max_str_comm = 40;                   {max length of string value in comment}

var
  term_p: sst_exp_term_p_t;            {points to current term in expression}
  n_terms: sys_int_machine_t;          {number of terms in top level of expression}
  dt_p, dt2_p: sst_dtype_p_t;          {scratch data type pointers}
  sym_p: sst_symbol_p_t;               {pointer to implicit variable for exp value}
  v: sst_var_t;                        {"variable" descriptor for implicit var}
  enc: enclose_k_t;                    {local copy of ENCLOSE flag}
  i: sys_int_machine_t;                {scratch integer and loop counter}
  bool_kluge: boolean;                 {TRUE if due hard TRUE/FALSE assign for bool}
  comm: string_var80_t;                {end of line comment string}
  token: string_var80_t;

label
  dtype_loop, dtype_match, implicit_var, fix_string;
{
********************************
*
*   Local subroutine DO_ADDR_CNT (CNT)
*
*   Write the appropriate number of "address of" (&) or pointer dereference (*)
*   operators preceeding the expression.
}
procedure do_addr_cnt (
  in      cnt: sys_int_machine_t);

var
  adrcnt: sys_int_machine_t;           {local copy of CNT}

begin
  if cnt = 0 then return;              {nothing to do ?}
  adrcnt := cnt;                       {make local copy of CNT argument}
  while adrcnt > 0 do begin            {"address of"s requested ?}
    sst_w.appendn^ ('&', 1);
    adrcnt := adrcnt - 1;
    end;
  while adrcnt < 0 do begin            {pointer dereferences requested ?}
    sst_w.appendn^ ('*', 1);
    adrcnt := adrcnt + 1;
    end;
  enc := enclose_yes_k;                {expression now needs to be enclosed in ()}
  end;
{
********************************
*
*   Start of main routine.
}
begin
  comm.max := sizeof(comm.str);        {init local var strings}
  token.max := sizeof(token.str);

  if exp.term1.ttype = sst_term_field_k then begin {exp is record constant ?}
    sst_w_c_exp_rec (exp);             {handle in separate routine}
    return;
    end;
  if exp.term1.ttype = sst_term_arele_k then begin {exp is array constant ?}
    sst_w_c_exp_array (exp);           {handle in separate routine}
    return;
    end;

  enc := enclose;                      {make local copy of ENCLOSE flag}

  n_terms := 1;                        {init number of terms counted}
  term_p := addr(exp.term1);           {init current term to first term in expression}
  while term_p^.next_p <> nil do begin {loop thru all the terms at this level}
    n_terms := n_terms + 1;            {count one more term in expression}
    term_p := term_p^.next_p;          {advance to next term in expression}
    end;
{
*   N_TERMS is the number of terms in the expression, and TERM_P is pointing
*   to the last term in the expression.
}
  if dt_out_p <> nil then begin        {expression must be of DT_OUT_P data type ?}
    dt_p := exp.dtype_p;               {resolve base expresion data type}
    dt2_p := dt_out_p;                 {resolve base requested data type}
dtype_loop:                            {back here after resolve pointer layer}
    while dt_p^.dtype = sst_dtype_copy_k do dt_p := dt_p^.copy_dtype_p;
    while dt2_p^.dtype = sst_dtype_copy_k do dt2_p := dt2_p^.copy_dtype_p;
    if dt_p = dt2_p                    {exactly the same data type descriptors ?}
      then goto dtype_match;
    if dt_p^.dtype <> dt2_p^.dtype     {base data types don't match ?}
      then goto implicit_var;
    if dt_p^.dtype = sst_dtype_pnt_k then begin {both data types are pointers ?}
      if dt_p^.pnt_dtype_p = dt2_p^.pnt_dtype_p {pointers to same data types ?}
        then goto dtype_match;
      if (dt_p^.pnt_dtype_p = nil) or (dt2_p^.pnt_dtype_p = nil) {one is NIL ptr ?}
        then goto implicit_var;
      dt_p := dt_p^.pnt_dtype_p;       {go one level deeper to pointed-to dtypes}
      dt2_p := dt2_p^.pnt_dtype_p;
      goto dtype_loop;                 {try again with pointed-to data types}
      end;
    goto implicit_var;                 {data types don't match}
    end;
dtype_match:                           {jump here if exp and required dtypes match}

  if                                   {can't do adr of exp, but asked to ?}
      (addr_cnt > 0) and               {caller wants address of expression ?}
      (not sst_w_c_exp_adrable(exp))   {but address of this exp not allowed ?}
    then goto implicit_var;            {assign exp to var, then take adr of var}
{
*   The expression may be written directly at the current position.  It
*   is not neccessary to create an implicit variable for the expression value.
}
  dt_p := exp.dtype_p;                 {resolve base expresion data type}
  while dt_p^.dtype = sst_dtype_copy_k do dt_p := dt_p^.copy_dtype_p;
  bool_kluge :=                        {need to force TRUE/FALSE value ?}
    (sst_config.manuf = sst_manuf_apollo_k) and {going to run on Apollos ?}
    (dt_p^.dtype = sst_dtype_bool_k) and {expression has BOOLEAN data type ?}
    (addr_cnt = 0) and                 {no address ofs or pointer dereference}
    (enclose = enclose_no_k);          {not a nested expression ?}

  if n_terms <= 1
    then begin                         {this expression contains only one term}
      if
          bool_kluge and               {kluge alert ?}
          (exp.term1.op1 <> sst_op1_none_k)
        then begin                     {special Apollo boolean kluge in effect}
          sst_w_c_term (exp.term1, addr_cnt, enclose_yes_k);
          if bool_kluge then begin
            sst_w.delimit^;
            sst_w.appendn^ ('?', 1);
            sst_w.delimit^;
            sst_w_c_intrinsic (intr_true_k);
            sst_w.delimit^;
            sst_w.appendn^ (':', 1);
            sst_w.delimit^;
            sst_w_c_intrinsic (intr_false_k);
            end;
          end
        else begin                     {process normally}
          sst_w_c_term (exp.term1, addr_cnt, enclose);
          end
        ;
      end
    else begin                         {this is a compound expression}
      do_addr_cnt (addr_cnt);          {write preceeding "&" or "*" operators}
      sst_w_c_armode_push (array_pnt_first_k); {force mode in compound expression}
      if bool_kluge then begin
        enc := enclose_yes_k;          {enclose in () if compound expression}
        end;
      sst_w_c_exp2 (                   {write top level operation in expression}
        exp.term1,                     {first term in expression before operator}
        n_terms - 1,                   {number of terms in exp before operator}
        term_p^,                       {first term in expression after operator}
        1,                             {number of terms in exp after operator}
        term_p^.op2,                   {operator between the expressions}
        enc);                          {enclose in () yes/no flag}
      if bool_kluge then begin
        sst_w.delimit^;
        sst_w.appendn^ ('?', 1);
        sst_w.delimit^;
        sst_w_c_intrinsic (intr_true_k);
        sst_w.delimit^;
        sst_w.appendn^ (':', 1);
        sst_w.delimit^;
        sst_w_c_intrinsic (intr_false_k);
        end;
      sst_w_c_armode_pop;              {restore array symbol interpretation mode}
      end
    ;

  return;
{
*   The expression can not be written directly at the current writing position.
*   An implicit variable will be created for the expression value.
}
implicit_var:
  if dt_out_p = nil                    {determine implicit variable's data type}
    then dt_p := exp.dtype_p
    else dt_p := dt_out_p;
  sst_w_c_armode_push (array_pnt_first_k); {override mode inside implicit expression}
  sst_w_c_exp_explicit (exp, dt_p^, sym_p); {create variable with expression value}
  sst_w_c_armode_pop;                  {restore array symbol interpretation mode}
  v.mod1.next_p := nil;                {fill in desc for implicit var reference}
  v.mod1.modtyp := sst_var_modtyp_top_k;
  v.mod1.top_str_h.first_char.crange_p := nil;
  v.mod1.top_str_h.first_char.ofs := 0;
  v.mod1.top_str_h.last_char.crange_p := nil;
  v.mod1.top_str_h.last_char.ofs := 0;
  v.mod1.top_sym_p := sym_p;
  v.dtype_p := sym_p^.var_dtype_p;
  v.rwflag := [sst_rwflag_read_k, sst_rwflag_write_k];
  v.vtype := sst_vtype_var_k;
{
*   Write value of expression as an end of line comment if expression has
*   constant value and is of a suitable type.
}
  if exp.val_fnd then begin            {expression has a constant value ?}
    comm.len := 0;                     {init to no comment generated}
    case exp.val.dtype of              {what is data type of value ?}
sst_dtype_int_k: begin
        string_f_int (comm, exp.val.int_val);
        end;
sst_dtype_enum_k: begin
        string_copy (exp.val.enum_p^.name_in_p^, comm);
        end;
sst_dtype_float_k: begin
        string_f_fp_free (             {make floating point value string}
          comm,                        {output string}
          exp.val.float_val,           {input floating point value}
          6);                          {minimum significant digits required}
        end;
sst_dtype_bool_k: begin
        if exp.val.bool_val
          then string_appendn (comm, 'TRUE', 4)
          else string_appendn (comm, 'FALSE', 5);
        end;
sst_dtype_char_k: begin
        string_append1 (comm, '"');
        string_append1 (comm, exp.val.char_val);
        string_append1 (comm, '"');
        goto fix_string;               {fix possible control characters in string}
        end;
sst_dtype_array_k: begin
        if exp.val.ar_str_p <> nil then begin {array is string, value exists ?}
          string_append1 (comm, '"');
          string_appendn (             {append string up to max allowed len}
            comm,                      {string to append to}
            exp.val.ar_str_p^.str,     {the string characters}
            min(exp.val.ar_str_p^.len, max_str_comm)); {max allowed str in comment}
          string_append1 (comm, '"');
          if exp.val.ar_str_p^.len > max_str_comm then begin {got truncated ?}
            string_appendn (comm, '...', 3);
            end;
fix_string:                            {common code with CHAR data type}
          for i := 2 to comm.len-1 do begin {scan range of possible control chars}
            if (comm.str[i] < ' ') or (comm.str[i] > '~') then begin {bad char ?}
              comm.str[i] := ' ';
              end;
            if (comm.str[i] = '*') and (comm.str[i+1] = '/') then begin {"*/" ?}
              comm.str[i] := '-';      {change "*/" to "-/"}
              end;
            end;                       {back and check next char in COMM}
          end;
        end;                           {done with ARRAY data type case}

      end;                             {end of expression value data type cases}
    sst_w.comment_set^ (comm);         {set this end of line comment}
    end;

  sst_w_c_var (v, addr_cnt);           {write reference to this variable}
  end;
