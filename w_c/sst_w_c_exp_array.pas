{   Subroutine SST_W_C_EXP_ARRAY (EXP)
*
*   Write the value of the expression EXP.  EXP is a constant array expression.
*   This is allowed only in the initialization of an array variable.
}
module sst_w_c_EXP_ARRAY;
define sst_w_c_exp_array;
%include 'sst_w_c.ins.pas';

procedure sst_w_c_exp_array (          {write exp value, must be array constant}
  in      exp: sst_exp_t);             {expression descriptor}

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  dt_exp_p: sst_dtype_p_t;             {points to base data type of array}
  dt_ele_p: sst_dtype_p_t;             {points to base data type of array elements}
  ind: sys_int_max_t;                  {seq number of current array ele, first = 0}
  ind_max: sys_int_max_t;              {seq number of last element with initial val}
  term_p: sst_exp_term_p_t;            {points to current term in expression}
  term_start_p: sst_exp_term_p_t;      {first term with vals at or after curr ele}
  default: string_var8192_t;           {default element value expression}
  vert: boolean;                       {TRUE if expand elements vertically}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

label
  term_next, ele_next;

begin
  default.max := sizeof(default.str);  {init local var string}
{
*  Resolve and check base data type of array.
}
  dt_exp_p := exp.dtype_p;             {resolve expression's base data type}
  while dt_exp_p^.dtype = sst_dtype_copy_k
    do dt_exp_p := dt_exp_p^.copy_dtype_p;
  if dt_exp_p^.dtype <> sst_dtype_array_k then begin {expression dtype not ARRAY ?}
    sys_msg_parm_int (msg_parm[1], ord(dt_exp_p^.dtype));
    sys_message_bomb ('sst', 'dtype_unexpected', msg_parm, 1);
    end;
{
*   Resolve base data type of array elements after this subscript.
}
  if dt_exp_p^.ar_dtype_rem_p = nil    {make raw pointer to elements data type}
    then dt_ele_p := dt_exp_p^.ar_dtype_ele_p
    else dt_ele_p := dt_exp_p^.ar_dtype_rem_p;
  while dt_ele_p^.dtype = sst_dtype_copy_k {resolve base elements data type}
    do dt_ele_p := dt_ele_p^.copy_dtype_p;
{
*   Set the default element value and decide on vertical/horizontal element list
*   expansion.
}
  vert := false;                       {init to horizontal element expansion}
  default.len := 0;                    {init default value string}
  case dt_ele_p^.dtype of              {what is data type of elements}
sst_dtype_int_k,                       {integer}
sst_dtype_enum_k,                      {enumerated (names for each value)}
sst_dtype_range_k,                     {subrange of a simple data type}
sst_dtype_pnt_k: begin                 {pointer}
        string_appendn (default, '0', 1);
        end;
sst_dtype_rec_k,                       {record}
sst_dtype_set_k: begin                 {set of an enumerated type}
        vert := true;                  {indicate vertical expansion}
        string_appendn (default, '0', 1);
        end;
sst_dtype_bool_k: begin                {TRUE/FALSE (Boolean)}
        string_appendn (default, 'false', 5);
        sst_w_c_declare (decl_false_k);
        sst_w_c_declare (decl_true_k);
        end;
sst_dtype_float_k: begin               {floating point}
        string_appendn (default, '0.0', 3);
        end;
sst_dtype_char_k: begin                {character}
        string_appendn (default, ''' ''', 3);
        end;
sst_dtype_array_k: begin               {array}
        vert := true;                  {indicate vertical expansion}
        if dt_ele_p^.ar_string
          then begin                   {array is a string of characters}
            string_appendn (default, '""', 2);
            end
          else begin
            string_appendn (default, '{0}', 3);
            end
          ;
        end;
otherwise
      sys_msg_parm_int (msg_parm[1], ord(dt_ele_p^.dtype));
      sys_message_bomb ('sst', 'dtype_unexpected', msg_parm, 1);
      end;
{
*   Find the number of the last element that has an initial value.
}
  ind_max := 0;                        {init to max initialized ele is first ele}
  term_p := addr(exp.term1);           {init current term to first in expression}
  while term_p <> nil do begin         {once for every term in expression}
    ind_max :=                         {update max element number initialized here}
      max(ind_max, term_p^.arele_start + term_p^.arele_n - 1);
    term_p := term_p^.next_p;          {advance to next term in expression}
    end;
{
*   The current state has been set up as follows:
*
*     VERT  -  TRUE if the elements list should be expanded vertically
*       (one per line).  Otherwise the elements list will be written sequentially
*       without line breaks, and wrapping will occurr when lines are filled.
*
*     DEFAULT  -  This is a string of the element value to write when it
*       is not explicitly initialized.
*
*     IND_MAX  -  This identifies the maximum element that has an initial
*       value.  This is used to decide when to stop, since all the remaining
*       elements are allowed to take on arbitrary initial values.  IND_MAX
*       is 0 to indicate the first element, regardless of the ordinal value
*       of the start of the subscript range.  This is also how the elements
*       are identified in the term descriptor when they are tagged with an
*       initial value.
*
*   Now write the element value from the first element in the array, to the
*   last initialized element.  For each element sequentially, the initial
*   values expression is searched to determine whether this element has an
*   initial value.  If so, the value is written out.  If not, DEFAULT is
*   written.
*
*   To make the search for the current element number more efficient,
*   TERM_START_P will be maintained to point to the first term that contains
*   any values for elements at or after the current element.  This will make
*   the search much more efficient when the elements are listed in subscript
*   order.
}
  sst_w.appendn^ ('{', 1);             {start of initial values for an array}
  term_start_p := addr(exp.term1);     {init first term to search for curr element}

  for ind := 0 to ind_max do begin     {loop up to max initialized array element}
    if vert then begin                 {each element goes on a new line ?}
      sst_w.line_close^;               {start a new line}
      sst_w.tab_indent^;
      sst_w.indent^;                   {extra indent for wrapped lines}
      end;
    term_p := term_start_p;            {init curr term to first term to search}
    while term_p <> nil do begin       {search all terms from here to end of exp}
      if (term_p^.arele_start + term_p^.arele_n) <= ind then begin {before element ?}
        term_start_p := term_p^.next_p; {next time start one term after here}
        goto term_next;                {try again with next term}
        end;
      if term_p^.arele_start > ind then begin {this term is after current element ?}
        goto term_next;                {try again with next term}
        end;
{
*   This term describes the initial value for the current array element.
}
      sst_w_c_exp_const (term_p^.arele_exp_p^, 0, dt_ele_p, enclose_no_k); {ele value}
      goto ele_next;                   {done writing value for current element}

term_next:                             {jump here to check next term in expression}
      term_p := term_p^.next_p;        {advance to next term in expression}
      end;                             {back and check new term for element value}
{
*   All the terms were checked and none of them had the initial value for the
*   current element.  Write the default value.
}
    sst_w.append^ (default);

ele_next:                              {jump here to advance to next array element}
    if ind <> ind_max then begin       {this was not last element in list ?}
      sst_w.appendn^ (',', 1);
      if not vert then begin           {writing elements horizontally ?}
        sst_w.delimit^;
        end;
      end;
    if vert then begin                 {writing elements vertically ?}
      sst_w.undent^;                   {undo extra indent for wrapped lines}
      end;
    end;                               {back and process next element of array}
{
*   Done writing initial values for all the elements.
}
  sst_w.appendn^ ('}', 1);
  end;
