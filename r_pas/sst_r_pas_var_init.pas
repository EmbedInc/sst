{   Subroutine SST_R_PAS_VAR_INIT (DTYPE, EXP_P)
*
*   Process the VAR_INITIALIZER syntax.  The tag for this syntax has just been
*   read.  DTYPE is the data type descriptor for the variable being initialized.
*   EXP_P will be returned pointing to the initial value expression.
}
module sst_r_pas_VAR_INIT;
define sst_r_pas_var_init;
%include 'sst_r_pas.ins.pas';

procedure sst_r_pas_var_init (         {process VAR_INITIALIZER syntax}
  in      dtype: sst_dtype_t;          {data type that init value must match}
  out     exp_p: sst_exp_p_t);         {returned pointing to initial value expression}

const
  max_msg_parms = 2;                   {max parameters we can pass to a message}

var
  tag: sys_int_machine_t;              {syntax tag from .syn file}
  str_h: syn_string_t;                 {handle to string for a tag}
  dt_p: sst_dtype_p_t;                 {points to base data type descriptor for var}
  dt_junk_p: sst_dtype_p_t;            {unused dtype descriptor pointer}
  dt: sst_dtype_k_t;                   {base data type ID of var}
  term_p: sst_exp_term_p_t;            {points to current term in expression}
  name: string_var80_t;                {name of current field in record}
  ind_n: sys_int_machine_t;            {number of next array element}
  sym_p: sst_symbol_p_t;               {scratch symbol pointer}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

label
  loop_tag, field_found, done_varinit, error_syntax;

begin
  name.max := sizeof(name.str);        {init var string}

  sst_dtype_resolve (dtype, dt_p, dt); {resolve variable's base data types}
  syn_level_down;                      {down into VAR_INITIALIZER syntax}
  syn_get_tag_msg (tag, str_h, 'sst_pas_read', 'var_init_bad', nil, 0);
  case tag of
{
***************************
*
*   Tag is for a list of VAR_INIT_FIELD syntaxes separated by commas.
}
1: begin
  sst_mem_alloc_scope (sizeof(exp_p^), exp_p); {alloc mem for expression descriptor}
  exp_p^.str_h := str_h;               {save handle to source characters}
  exp_p^.dtype_p := dt_p;              {point to root data type for expression}
  exp_p^.dtype_hard := true;
  exp_p^.val_eval := true;             {expression will have known value (or else)}
  exp_p^.val_fnd := true;
  exp_p^.val.dtype := dt;              {set base data type ID for whole expression}
  exp_p^.rwflag := [sst_rwflag_read_k]; {expression is read-only}
  term_p := nil;                       {init to no current term yet}
  ind_n := 0;                          {init to next array index number, if used}

loop_tag:                              {back here each new tag in VAR_INITIALIZER}
  syn_get_tag_msg (                    {get tag for next VAR_INIT_FIELD syntax}
    tag, str_h, 'sst_pas_read', 'var_init_bad', nil, 0);
  case tag of
1: ;                                   {regular VAR_INIT_FIELD syntax}
syn_tag_end_k: begin                   {end of VAR_INITIALIZER syntax}
      goto done_varinit;               {done with VAR_INITIALIZER syntax}
      end;
otherwise                              {unexpected TAG value}
    syn_error_tag_unexp (tag, str_h);
    end;

  if term_p = nil
    then begin                         {no current term, init to first term}
      term_p := addr(exp_p^.term1);
      end
    else begin                         {make new term and chain to old term}
      sst_mem_alloc_scope (sizeof(term_p^), term_p^.next_p); {grab mem for new term}
      term_p := term_p^.next_p;        {make new term the current term}
      end
    ;
  term_p^.next_p := nil;               {init descriptor for this term}
  term_p^.op2 := sst_op2_none_k;
  term_p^.op1 := sst_op1_none_k;
  term_p^.str_h := str_h;
  term_p^.dtype_hard := true;
  term_p^.val_eval := true;
  term_p^.val_fnd := true;
  term_p^.rwflag := [sst_rwflag_read_k];

  syn_level_down;                      {down into VAR_INIT_FIELD syntax}
  case dt_p^.dtype of                  {what is var base data type ?}
{
*   The variable being initialized is a RECORD.
}
sst_dtype_rec_k: begin
  syn_get_tag_msg (                    {get field name tag}
    tag, str_h, 'sst_pas_read', 'var_init_bad', nil, 0);
  case tag of                          {check for proper tag value}
1: ;
2: goto error_syntax;
otherwise
    syn_error_tag_unexp (tag, str_h);
    end;

  syn_get_tag_string (str_h, name);    {get name of this field}
  string_downcase (name);              {make lower case for matching}
  sym_p := dt_p^.rec_first_p;          {init to first field in record}
  while sym_p <> nil do begin          {loop thru the fields in this record}
    if string_equal(sym_p^.name_in_p^, name) {found symbol descriptor for this field ?}
      then goto field_found;
    sym_p := sym_p^.field_next_p;      {advance to next field in this record}
    end;                               {back and check this new field in record}
  sys_msg_parm_vstr (msg_parm[1], name);
  sys_msg_parm_vstr (msg_parm[2], dtype.symbol_p^.name_in_p^);
  syn_error (str_h, 'sst_pas_read', 'field_name_not_found', msg_parm, 2);

field_found:                           {SYM_P is pointing to selected field in rec}
  term_p^.ttype := sst_term_field_k;   {this term describes value of field in record}
  term_p^.dtype_p := sym_p^.field_dtype_p; {point to data type for this field}
  term_p^.field_sym_p := sym_p;        {point to symbol for this field}

  syn_get_tag_msg (                    {get VAR_INITIALIZER value tag}
    tag, str_h, 'sst_pas_read', 'var_init_bad', nil, 0);
  if tag <> 2 then syn_error_tag_unexp (tag, str_h);
  sst_r_pas_var_init (                 {get initializer expression for this field}
    term_p^.dtype_p^, term_p^.field_exp_p);
  end;                                 {done with initialized var is a RECORD}
{
*   The variable being initialized is an ARRAY.
}
sst_dtype_array_k: begin
  syn_get_tag_msg (                    {get VAR_INITIALIZER value tag}
    tag, str_h, 'sst_pas_read', 'var_init_bad', nil, 0);
  case tag of                          {check for proper tag value}
1: goto error_syntax;
2: ;
otherwise
    syn_error_tag_unexp (tag, str_h);
    end;

  term_p^.ttype := sst_term_arele_k;   {this term describes array element value}
  if dt_p^.ar_dtype_rem_p = nil
    then begin                         {array has only one subscript}
      term_p^.dtype_p := dt_p^.ar_dtype_ele_p; {"next" dtype is for the elements}
      end
    else begin                         {array has more than one subscript}
      term_p^.dtype_p := dt_p^.ar_dtype_rem_p; {"next" dtype is for rest of array}
      end
    ;
  if ind_n >= dt_p^.ar_ind_n then begin {more values than array indicies ?}
    sys_msg_parm_int (msg_parm[1], dt_p^.ar_ind_n);
    syn_error (str_h, 'sst_pas_read', 'var_init_index_too_many', msg_parm, 1);
    end;
  term_p^.arele_start := ind_n;        {first index value}
  ind_n := ind_n + 1;                  {make index ordinal for next value}
  term_p^.arele_n := 1;                {number of indicies covered by this value}
  sst_r_pas_var_init (                 {get initializer expression for this field}
    term_p^.dtype_p^, term_p^.arele_exp_p);
  term_p^.val := term_p^.arele_exp_p^.val; {copy ele value back up to this term}
  end;
{
*   The variable being initialized is a SET.
}
sst_dtype_set_k: begin
  syn_error (str_h, 'sst_pas_read', 'var_init_set_unimp', nil, 0);
  end;
{
*   The data type of the variable does not match the syntax of the initializer
*   declaration.
}
otherwise
    goto error_syntax;
    end;
  syn_level_up;                        {back up from VAR_INIT_FIELD syntax}
  sst_dtype_resolve (term_p^.dtype_p^, dt_junk_p, term_p^.val.dtype);
  goto loop_tag;                       {back for next tag in VAR_INITIALIZER}
{
*   Done with the whole VAR_INITIALIZER syntax.
}
done_varinit:
  if dt_p^.dtype = sst_dtype_array_k then begin {initialization was for array ?}
    if ind_n <> dt_p^.ar_ind_n then begin {not right number of values found ?}
      sys_msg_parm_int (msg_parm[1], dt_p^.ar_ind_n);
      sys_msg_parm_int (msg_parm[2], ind_n);
      syn_error (
        term_p^.str_h, 'sst_pas_read', 'var_init_index_too_few', msg_parm, 2);
      end;

    if dt_p^.ar_string then begin      {array is a character string ?}
      sst_mem_alloc_scope (            {allocate memory for string constant value}
        string_size(dt_p^.ar_ind_n),   {amount of memory to allocate}
        exp_p^.val.ar_str_p);          {returned pointer to the new memory}
      exp_p^.val.ar_str_p^.max := dt_p^.ar_ind_n; {set var string max length}
      exp_p^.val.ar_str_p^.len := 0;   {init string to empty}
      term_p := addr(exp_p^.term1);    {init pointer to first}
      while term_p <> nil do begin     {once for each character in string}
        string_append1 (exp_p^.val.ar_str_p^, term_p^.val.char_val); {append char}
        term_p := term_p^.next_p;      {advance to next array value}
        end;                           {back and process this new array value}
      end;                             {end of array is a character string}
    end;                               {end of variable was an array special case}

  end;                                 {end of list of VAR_INIT_FIELD case}
{
***************************
*
*   Tag is for an EXPRESSION syntax.  This must be for a "simple" variable.
}
2: begin
  case dt_p^.dtype of                  {what is var base data type ?}
sst_dtype_int_k,
sst_dtype_enum_k,
sst_dtype_float_k,
sst_dtype_bool_k,
sst_dtype_char_k,
sst_dtype_array_k,
sst_dtype_range_k,
sst_dtype_pnt_k: begin                 {all the legal data types for EXRESSION}
      sst_r_pas_exp (str_h, true, exp_p); {get descriptor for this expression}
      sst_exp_useage_check (           {check expression attributes for this useage}
        exp_p^,                        {expression to check}
        [sst_rwflag_read_k],           {read/write access needed to expression value}
        dt_p^);                        {data type value must be compatible with}
      end;
otherwise
    goto error_syntax;
    end;
  end;
{
***************************
*
*   Unexpected TAG value.
}
otherwise
    syn_error_tag_unexp (tag, str_h);
    end;                               {end of TAG value cases}

  syn_level_up;                        {back up from VAR_INITIALIZER syntax}
  return;                              {normal return}

error_syntax:                          {jump here on syntax error}
  syn_error (str_h, 'sst_pas_read', 'var_init_bad', nil, 0);
  end;
