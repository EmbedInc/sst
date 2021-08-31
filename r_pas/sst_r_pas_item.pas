{   Subroutine SST_R_PAS_ITEM (TERM)
*
*   Read and process ITEM syntax.  The appropriate fields in the expression term
*   descriptor TERM are filled in.
}
module sst_r_pas_ITEM;
define sst_r_pas_item;
%include 'sst_r_pas.ins.pas';

procedure sst_r_pas_item (             {create compiled item from input stream}
  out     term: sst_exp_term_t);       {expression term descriptor to fill in}

var
  tag: sys_int_machine_t;              {syntax tag ID}
  str_h: syo_string_t;                 {handle to string associated with TAG}
  tag2: sys_int_machine_t;             {extra syntax tag to avoid corrupting TAG}
  str2_h: syo_string_t;                {handle to string associated with TAG2}
  token: string_var8192_t;             {scratch token for number conversion, etc}
  sz: sys_int_adr_t;                   {amount of memory to allocate}
  sym_p: sst_symbol_p_t;               {points to symbol descriptor}
  dt_p: sst_dtype_p_t;                 {scratch pointer to data type descriptor}
  var_p: sst_var_p_t;                  {points to full variable descriptor}
  exp_p: sst_exp_p_t;                  {points to new expression descriptor}
  ifarg_p: sst_exp_chain_p_t;          {points to intrinsic function args chain}
  set_ele_pp: ^sst_ele_exp_p_t;        {points to set elements chain pointer}
  set_ele_p: sst_ele_exp_p_t;          {points to curr set element/range val desc}
  args_here: boolean;                  {TRUE if function reference has arguments}
  stat: sys_err_t;

label
  not_ifunc, isa_routine, loop_set, leave;

begin
  token.max := sizeof(token.str);      {init var string}

  term.val_eval := false;              {init to not attempted to evaluate this term}
  term.dtype_p := nil;                 {init to data type is not known yet}
  syo_level_down;                      {down into ITEM syntax level}
  syo_get_tag_msg (                    {get unadic operator tag}
    tag, str_h, 'sst_pas_read', 'exp_bad', nil, 0);
  case tag of                          {unadic operator cases}
1:  term.op1 := sst_op1_none_k;        {none}
2:  term.op1 := sst_op1_plus_k;        {+}
3:  term.op1 := sst_op1_minus_k;       {-}
4:  term.op1 := sst_op1_not_k;         {not}
5:  term.op1 := sst_op1_1comp_k;       {~}
otherwise
    syo_error_tag_unexp (tag, str_h);
    end;                               {end of unadic operator cases}
  syo_get_tag_msg (                    {get item type tag}
    tag, str_h, 'sst_pas_read', 'exp_bad', nil, 0);
  case tag of
{
*************************************
*
*   Item is a literal floating point number.
}
1: begin
  term.ttype := sst_term_const_k;      {this item is a constant}
  syo_get_tag_string (str_h, token);   {get floating point number string}
  string_t_fp2 (token, term.val.float_val, stat);
  syo_error_abort (stat, str_h, 'sst_pas_read', 'exp_bad', nil, 0);
  term.val.dtype := sst_dtype_float_k;
  end;
{
*************************************
*
*   Item is a literal integer number.
}
2: begin
  term.ttype := sst_term_const_k;      {this item is a constant}
  sst_r_pas_integer (term.val.int_val); {get integer value}
  term.val.dtype := sst_dtype_int_k;
  end;
{
*************************************
*
*   Item is a literal string.
}
3: begin
  term.ttype := sst_term_const_k;      {term is is CONSTANT}
  sst_r_pas_lit_string (token);        {get value of this literal string}
  if token.len = 1
    then begin                         {string is only one character}
      term.val.dtype := sst_dtype_char_k; {term value is CHARACTER data type}
      term.val.char_val := token.str[1]; {save character value}
      end
    else begin                         {string is not just one character}
      sz :=                            {amount of memory needed for whole var string}
        sizeof(token) - sizeof(token.str) + {overhead for var string}
        (sizeof(token.str[1]) * token.len); {storage for raw string}
      sst_mem_alloc_scope (sz, term.val.ar_str_p); {allocate mem for string}
      term.val.ar_str_p^.max := token.len; {set new var string}
      string_copy (token, term.val.ar_str_p^);
      term.val.dtype := sst_dtype_array_k; {term value is of ARRAY data type}
      end
    ;
  end;
{
*************************************
*
*   Item is a nested expression in parenthesis.
}
4: begin
  term.ttype := sst_term_exp_k;        {indicate item is nested expression}
  sst_r_pas_exp (str_h, false, term.exp_exp_p); {process nested expression}
  end;
{
*************************************
*
*   Item is a SET expression.
}
5: begin
  term.ttype := sst_term_set_k;        {indicate item is a SET expression}
  syo_level_down;                      {down into SET_VALUE syntax}
  term.set_first_p := nil;             {init to no element expressions in set}
  set_ele_pp := addr(term.set_first_p); {set pointer to current end of ranges chain}

loop_set:                              {back here each new tag in SET_VALUE syntax}
  syo_get_tag_msg (tag, str_h, 'sst_pas_read', 'exp_bad', nil, 0); {get next syntax tag}
  case tag of
{
*   Tag is for new SET_VALUE_RANGE syntax.
}
1: begin
  sst_mem_alloc_scope (sizeof(set_ele_p^), set_ele_p); {alloc set element/range desc}
  set_ele_pp^ := set_ele_p;            {link new descriptor to end of chain}
  set_ele_p^.next_p := nil;            {indicate new descriptor is at end of chain}
  set_ele_pp := addr(set_ele_p^.next_p); {update pointer to end of chain pointer}
  syo_level_down;                      {down into SET_VALUE_RANGE syntax}

  syo_get_tag_msg                      {get tag for ele value or start val of range}
    (tag, str_h, 'sst_pas_read', 'exp_bad', nil, 0);
  if tag <> 1 then syo_error_tag_unexp (tag, str_h);
  sst_r_pas_exp (str_h, false, set_ele_p^.first_p); {process start val expression}

  syo_get_tag_msg                      {get tag for end of ele range expression}
    (tag, str_h, 'sst_pas_read', 'exp_bad', nil, 0);
  case tag of
1: begin                               {tag is for end of range expression}
      sst_r_pas_exp (str_h, false, set_ele_p^.last_p);
      end;
syo_tag_end_k: begin                   {descriptor is for one value, not a range}
      set_ele_p^.last_p := nil;        {indicate no end of range expression present}
      end;
otherwise
    syo_error_tag_unexp (tag, str_h);
    end;

  syo_level_up;                        {back up from SET_VALUE_RANGE syntax}
  end;
{
*   Tag incidates end of SET_VALUE syntax.
}
syo_tag_end_k: begin
  syo_level_up;                        {back up from SET_VALUE syntax}
  goto leave;                          {all done processing set value expression}
  end;
{
*   Unexepected TAG value in SET_VALUE syntax.
}
otherwise
    syo_error_tag_unexp (tag, str_h);
    end;
  goto loop_set;                       {back for next tag in SET_VALUE syntax}
  end;
{
*************************************
*
*   Item is a symbol.  This could be a variable, function, enumerated constant,
*   data type, or named constant.  We will first check for an intrinsic function,
*   since it will be handled differently.
}
6: begin
  syo_push_pos;                        {save current syntax position on stack}
  syo_level_down;                      {down into VARIABLE syntax}
  syo_get_tag_msg (                    {get tag for intrinsic function name}
    tag, str_h, 'sst_pas_read', 'exp_bad', nil, 0);
  syo_get_tag_msg (                    {must be syntax end for intrinsic function}
    tag2, str2_h, 'sst_pas_read', 'exp_bad', nil, 0);
  if tag2 <> syo_tag_end_k then goto not_ifunc; {not intrinsic function ?}
  sst_symbol_lookup (str_h, sym_p, stat); {look up symbol name}
  syo_error_abort (stat, str_h, '', '', nil, 0);
  if sym_p^.symtype <> sst_symtype_front_k {not right sym type for intrinsic func ?}
    then goto not_ifunc;
  syo_pop_pos;                         {pop old syntax position from stack}
{
*   This item is an intrinsic function reference.  SYM_P is pointing to the
*   symbol descriptor for the intrinsic function.  The syntax position is
*   right after the tag for the VARIABLE syntax was read in ITEM.  The next
*   tag should be for the function arguments.
}
  sst_r_pas_ifunc (sym_p^, term);      {handle intrinsic function call}
  goto leave;
{
*   The item is not an intrinsic function.
}
not_ifunc:                             {go here if "variable" wasn't intrinsic func}
  syo_pop_pos;                         {restore syntax parsing position}
  sst_r_pas_variable (var_p);          {process VARIABLE syntax and build descriptor}
  sst_var_funcname (var_p^);           {call func instead of stuff return value}
  syo_get_tag_msg (tag, str_h, 'sst_pas_read', 'exp_bad', nil, 0); {var/func tag}
  case tag of
{
*   Item looks syntactically like a variable reference.  No () follows.
}
1: begin                               {syntactically just a variable reference}
      case var_p^.vtype of             {what kind of "variable" references is this}
sst_vtype_var_k,                       {regular variable}
sst_vtype_dtype_k,                     {data type}
sst_vtype_const_k: begin               {named constant}
          term.ttype := sst_term_var_k; {indicate term is variable reference}
          term.var_var_p := var_p;     {point to variable descriptor}
          end;
sst_vtype_rout_k: begin                {routine name}
          args_here := false;          {no arguments were supplied for function}
          goto isa_routine;
          end;
otherwise
        syo_error (term.str_h, 'sst_pas_read', 'exp_symbol_type_bad', nil, 0);
        end;
      end;                             {end of syntactic variable reference}
{
*   Item has () following it.
}
2: begin                               {function reference}
      case var_p^.vtype of             {what type of "variable" is this ?}
sst_vtype_dtype_k: begin               {item is a type-casting function}
          term.ttype := sst_term_type_k;
          term.type_dtype_p := var_p^.dtype_p;
          syo_level_down;              {down into FUNCTION_ARGUMENTS syntax}
          syo_get_tag_msg              {get tag to argument expression}
            (tag, str_h, 'sst_pas_read', 'exp_bad', nil, 0);
          if tag <> 1 then begin
            syo_error_tag_unexp (tag, str_h);
            end;
          sst_r_pas_exp (str_h, false, term.type_exp_p); {make expression descriptor}
          syo_get_tag_msg              {get tag to "next" argument (should be none)}
            (tag, str_h, 'sst_pas_read', 'exp_bad', nil, 0);
          if tag <> syo_tag_end_k then begin
            syo_error_tag_unexp (tag, str_h);
            end;
          syo_level_up;                {back up from FUNCTION_ARGUMENTS syntax}
          end;
sst_vtype_rout_k: begin                {item is a function reference}
          args_here := true;           {this function definately has arguments}
          goto isa_routine;            {to common code for all functions}
          end;
otherwise                              {wrong item type to have () following}
        syo_error (var_p^.mod1.top_str_h, 'sst_pas_read', 'exp_symbol_not_func', nil, 0);
        end;                           {end of "variable" type cases followed by ()}
      end;                             {end of item has () following case}
{
*   Unexpected syntax tag value.
}
otherwise
    syo_error_tag_unexp (tag, str_h);
    end;
  goto leave;                          {all done with item is a symbol}

isa_routine:                           {item is a routine, ARGS_HERE has been set}
  if
      (var_p^.rout_proc_p^.dtype_func_p = nil) or {routine is not a function ?}
      addr_of                          {this is argument to ADDR function ?}
      then begin
{
*   Pass routine, instead of function value.  This happens to all ADDR
*   arguments.
}
    if args_here then begin            {definately bad if arguments exists}
      syo_error (term.str_h, 'sst_pas_read', 'exp_rout_not_func', nil, 0);
      end;
    term.ttype := sst_term_var_k;      {flag term as a variable reference}
    term.dtype_p := var_p^.dtype_p;    {term takes on variable's data type}
    term.dtype_hard := true;
    term.val_eval := true;             {prevent re-evaluation later}
    term.val_fnd := false;             {this term has no known constant value}
    term.val.dtype := sst_dtype_proc_k; {base data type is PROCEDURE}
    term.rwflag := [];                 {no read/write access allowed to this term}
    term.var_var_p := var_p;           {set pointer to routine name var descriptor}
    goto leave;
    end;                               {end of routine is not a function case}

  term.ttype := sst_term_func_k;       {term is a function reference}
  term.func_var_p := var_p;            {point to descriptor for function reference}
  sst_r_pas_routine (                  {create descriptor for this routine reference}
    str_h,                             {string handle for routine reference}
    term.func_var_p^,                  {"variable" descriptor for routine name}
    args_here,                         {TRUE if arguments were supplied for function}
    term.func_proc_p);                 {returned routine call descriptor}
  term.func_proct_p := var_p^.rout_proc_p; {save pointer to called routine template}
  goto leave;                          {all done with VARIABLE/FUNCTION}
  end;
{
*************************************
*
*   Item is boolean constant TRUE
}
7: begin
  term.ttype := sst_term_const_k;      {this item is a constant}
  term.val.bool_val := true;
  term.val.dtype := sst_dtype_bool_k;
  end;
{
*************************************
*
*   Item is boolean constant FALSE
}
8: begin
  term.ttype := sst_term_const_k;      {this item is a constant}
  term.val.bool_val := false;
  term.val.dtype := sst_dtype_bool_k;
  end;
{
*************************************
*
*   Item is pointer value NIL.
}
9: begin
  term.ttype := sst_term_const_k;      {this item is a constant}
  term.val.dtype := sst_dtype_pnt_k;   {constant is of POINTER type}
  term.val.pnt_dtype_p := sst_dtype_uptr_p; {point to dtype descriptor for univ ptr}
  term.val.pnt_exp_p := nil;
  end;
{
*************************************
*
*   Unexpected TAG value.
}
otherwise
    syo_error_tag_unexp (tag, str_h);
    end;                               {end of item type TAG cases}

leave:                                 {common exit point}
  syo_level_up;                        {back up from ITEM syntax level}

  if term.op1 = sst_op1_1comp_k then begin {term preceeded by ~ operator ?}
    term.op1 := sst_op1_none_k;        {temporarily disable unary operator}
    if term.dtype_p = nil then begin   {need to find data type to check unary op ?}
      sst_term_eval (term, false);     {determine term's data type}
      end;
    dt_p := term.dtype_p;              {resolve term's base data type}
    while dt_p^.dtype = sst_dtype_copy_k do dt_p := dt_p^.copy_dtype_p;
    if dt_p^.dtype = sst_dtype_set_k
{
*   The term was preceeded by the "~" unary operator, and has the SET data
*   type.  This indicates set inversion, but is handled with the SETINV
*   intrinisic function instead of a unary operator.  The term that was just
*   created will be set at the argument to a SETINV intrinsic function.
}
      then begin                       {term has the SET data type}
        sst_mem_alloc_scope (sizeof(exp_p^), exp_p);
        exp_p^.term1 := term;          {fill in mandatory part of ifunc arg exp}
        exp_p^.term1.next_p := nil;
        exp_p^.term1.op2 := sst_op2_none_k;
        exp_p^.str_h := exp_p^.term1.str_h;
        exp_p^.val_eval := false;

        sst_mem_alloc_scope (sizeof(ifarg_p^), ifarg_p); {get ifunc arg descriptor}
        ifarg_p^.next_p := nil;        {this ifunc has only one argument}
        ifarg_p^.exp_p := exp_p;       {point to expression for this argument}

        term.ttype := sst_term_ifunc_k; {caller's term no refers to intrinsic func}
        term.val_eval := false;
        term.ifunc_id := sst_ifunc_setinv_k;
        term.ifunc_args_p := ifarg_p;
        end
{
*   The term does not represent a set inversion.
}
      else begin                       {term is not a SET}
        term.op1 := sst_op1_1comp_k;   {restore unary operator}
        term.val_eval := false;        {reset term to not evaluated yet}
        end
      ;
    end;                               {done handling term has preceeding "~"}

  sst_term_eval (term, false);         {evaluate term, if possible}
  end;
