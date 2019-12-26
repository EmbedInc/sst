{   Subroutine SST_W_C_DTYPE (DTYPE, NAME, PACK)
*
*   Write the data type definition from the data type descriptor DTYPE.
*   The full data type definition will be written, whether simple or not.
*   This routine would be used when declaring the data type itself.
*   When PACK is TRUE, the data type declaration is for a field in a packed
*   record.  This causes some data types to be written as bit fields.
*   NAME is the name of the symbol being declared with the data type.
*   It will be inserted in the appropriate place.
}
module sst_w_c_dtype;
define sst_w_c_dtype;
%include 'sst_w_c.ins.pas';

procedure sst_w_c_dtype (              {write data type definition}
  in      dtype: sst_dtype_t;          {data type descriptor block}
  in      name: univ string_var_arg_t; {name of symbol to declare with this dtype}
  in      pack: boolean);              {TRUE if part of packed record}
  val_param;

const
  max_msg_parms = 2;                   {max parameters we can pass to a message}

type
  int_sign_t = (                       {flag to select signed/unsigned integer}
    int_signed_k,                      {integer is signed}
    int_unsigned_k);                   {integer is unsigned}

var
  i: sys_int_machine_t;                {scratch integer and loop counter}
  fw: sys_int_machine_t;               {field width specifier for packed field}
  sym_p: sst_symbol_p_t;               {points to current symbol}
  token: string_var80_t;               {scratch for number conversion}
  dt_p: sst_dtype_p_t;                 {scratch pointer to data type descriptor}
  arg_p: sst_proc_arg_p_t;             {points to current routine arg descriptor}
  name_ele: string_var132_t;           {input name of element in union}
  scope_old_p: sst_scope_p_t;          {saved current scope pointer}
  names_old_p: sst_scope_p_t;          {saved current namespace pointer}
  int_sign: int_sign_t;                {flag to select signed/unsigned integer}
  name_written: boolean;               {TRUE if NAME already written}
  variants: boolean;                   {TRUE if record has any variants (overlays)}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

label
  do_int, done_case;

begin
  token.max := sizeof(token.str);      {init local var strings}
  name_ele.max := sizeof(name_ele.str);

  fw := 0;                             {init field width specifier}
  name_written := false;               {init to NAME not written yet}

  case dtype.dtype of
{
************************************
*
*   Data type is INTEGER.
}
sst_dtype_int_k: begin
  int_sign := int_signed_k;            {numeric integers are signed}

do_int:                                {jump here from other dtypes that are ints}
  case int_sign of
int_signed_k: sst_w.appendn^ ('signed', 6);
int_unsigned_k: sst_w.appendn^ ('unsigned', 8);
    end;
  sst_w.delimit^;

  if pack then begin                   {this is field in a packed record ?}
    sst_w.appendn^ ('int', 3);         {field width will be set explicitly later}
    fw := dtype.bits_min;              {set field width for this field in record}

    if                                 {SGI full size packed field kluge ?}
        (sst_config.manuf = sst_manuf_sgi_k) and {will be an SGI compiler ?}
        (fw = sst_config.int_machine_p^.bits_min) {same size as INT ?}
        then begin
      fw := 0;                         {disable field width specifier}
      end;

    goto done_case;
    end;
{
*   Look for target machine integer sizes to match the size of this data type.
*   The match must be exact, because the front end may have assumed the size
*   was known.  This would be the case, for example, if a SIZE_MIN function
*   was used on this data type.
}
  for i := 1 to sst_config.n_size_int do begin {once for each avail integer size}
    with sst_config.size_int[i].dtype_p^: dt do begin {DT is dtype desc for this int}
      if dt.size_used = dtype.size_used then begin {found the right one ?}
        sst_w.append_sym_name^ (dt.symbol_p^); {write int dtype name of matching size}
        goto done_case;
        end;
      end;                             {done with DT abbreviation}
    end;                               {back to check next native out integer type}
{
*   None of the native integers are of the right size.  This is a hard error.
}
  if
      (dtype.symbol_p = nil) or else
      (dtype.symbol_p^.name_in_p = nil)
    then begin                         {no name available for input data type}
      sys_msg_parm_str (msg_parm[1], '');
      end
    else begin                         {input data type does have a name}
      sys_msg_parm_vstr (msg_parm[1], dtype.symbol_p^.name_in_p^);
      end
    ;
  sys_msg_parm_int (msg_parm[2], dtype.bits_min);
  sys_message_bomb ('sst', 'dtype_int_unavailable', msg_parm, 2);
  end;
{
************************************
*
*   Data type is ENUMERATED TYPE.
}
sst_dtype_enum_k: begin
  if                                   {OK to use native ENUM data type ?}
      (not pack) and
      (sst_config.size_enum = sst_config.size_enum_native)
{
*   We are allowed to use the native ENUM data type.
}
    then begin
      sst_w.appendn^ ('enum', 4);
      sst_w.delimit^;
      sst_w.appendn^ ('{', 1);
      sym_p := dtype.enum_first_p;     {init current symbol to first symbol}
      while sym_p <> nil do begin      {once for each enumerated value}
        sst_w.line_close^;             {put each name on a new line}
        sst_w.tab_indent^;
        sst_w.append_sym_name^ (sym_p^); {write this enumerated value name}
        if sym_p <> dtype.enum_last_p then begin {this is not last name in list ?}
          sst_w.appendn^ (',', 1);
          end;
        sym_p := sym_p^.enum_next_p;   {advance to next enumerated name in list}
        end;
      sst_w.appendn^ ('}', 1);
      end
{
*   We are not allowed to use the native ENUM data type because it is a different
*   size than what we have been told to use for ENUMs.  The data type will
*   be an integer of the appropriate size.  All the mnemonics of the enumerated
*   type will be declared to their values in #define statements.
}
    else begin
      int_sign := int_unsigned_k;      {this integer type is always unsigned}
      fw := dtype.bits_min;            {field width if field in packed record}

      sst_w_c_pos_push (frame_scope_p^.sment_type); {switch to before curr statement}
      sym_p := dtype.enum_first_p;     {init current symbol to first symbol}
      while sym_p <> nil do begin      {once for each enumerated value}
        sst_w.tab_indent^;
        sst_w.appendn^ ('#define ', 8);
        sst_w.append_sym_name^ (sym_p^); {write this enumerated value name}
        sst_w.appendn^ (' ', 1);
        string_f_int (token, sym_p^.enum_ordval);
        sst_w.append^ (token);         {write value of this enumerated name}
        sst_w.line_close^;
        sym_p := sym_p^.enum_next_p;   {advance to next enumerated name in list}
        end;
      sst_w_c_pos_pop;                 {pop back to old writing position}
      goto do_int;                     {handle data type like other integers}
      end;
    ;
  end;
{
************************************
*
*   Data type is FLOATING POINT.
}
sst_dtype_float_k: begin
  for i := 1 to sst_config.n_size_float do begin {once for each avail float size}
    with sst_config.size_float[i].dtype_p^: dt do begin {DT is dtype for this float}
      if dt.bits_min < dtype.bits_min  {not big enough ?}
        then next;
      if dt.bits_min = dtype.bits_min then begin {exact right size ?}
        sst_w.append_sym_name^ (dt.symbol_p^); {write data type's name}
        goto done_case;
        end;
      end;                             {done with DT abbreviation}
    exit;                              {exact match not available}
    end;                               {back to check next native out float type}
{
*   None of the native output floating point formats match.  This is a hard
*   error.
}
  if
      (dtype.symbol_p = nil) or else
      (dtype.symbol_p^.name_in_p = nil)
    then begin                         {no name available for input data type}
      sys_msg_parm_str (msg_parm[1], '');
      end
    else begin                         {input data type does have a name}
      sys_msg_parm_vstr (msg_parm[1], dtype.symbol_p^.name_out_p^);
      end
    ;
  sys_msg_parm_int (msg_parm[2], dtype.bits_min);
  sys_message_bomb ('sst', 'dtype_float_unavailable', msg_parm, 2);
  end;
{
************************************
*
*   Data type is BOOLEAN.
}
sst_dtype_bool_k: begin
  if pack then begin                   {this is field in a packed record ?}
    sst_w.appendn^ ('unsigned int', 12);
    fw := 1;
    goto done_case;
    end;

  sst_w.append^ (sst_config.name_bool);
  end;
{
************************************
*
*   Data type is CHARACTER.
}
sst_dtype_char_k: begin
  sst_w.append^ (sst_config.name_char);
  end;
{
************************************
*
*   Data type is a RECORD.
}
sst_dtype_rec_k: begin
{
*   Determine whether this record has any variants (overlays).  Only
*   records without any variants are handled here directly.  Records
*   with variants are handled with the help of routine SST_W_C_DTYPE_VREC.
*
*   Variant records are written as UNIONs at the top level, while records
*   without variants are written as STRUTs.
}
  if sst_rec_variant(dtype)
    then begin                         {the record DOES have variants}
      variants := true;
      sst_w.appendn^ ('union', 5);
      end
    else begin                         {the record has no variants}
      variants := false;
      sst_w.appendn^ ('struct', 6);
      end
    ;
  sst_w.delimit^;
  sst_w.append^ (name);
  sst_w.delimit^;
  sst_w.appendn^ ('{', 1);
  sst_w.line_close^;

  scope_old_p := sst_scope_p;          {save old scope/namespace context}
  names_old_p := sst_names_p;
  sst_scope_p := dtype.rec_scope_p;    {temp swap in scope for this record}
  sst_names_p := sst_scope_p;

  sym_p := dtype.rec_first_p;          {init curr field symbol to first field symbol}
  while sym_p <> nil do begin          {once for each field name in record}
    if variants
      then begin                       {this field is in a variant record}
        sst_w_c_dtype_vrec (           {write all the fields in this variant}
          sym_p,                       {pointer to next field, will be updated}
          dtype.align_nat = 0);        {TRUE if within a packed record}
        end
      else begin                       {this field is not in a variant record}
        sst_w.tab_indent^;             {go to proper indentation level}
        sst_w.indent^;                 {indent wrapped lines}
        sst_w_c_dtype_simple (         {write data type declaration}
          sym_p^.field_dtype_p^,       {descriptor for data type to write}
          sym_p^.name_out_p^,          {name of field to declare}
          dtype.align_nat = 0);        {TRUE if this is a packed record}
        sst_w.undent^;                 {undo indent for wrapped lines}
        sst_w.appendn^ (';', 1);
        sst_w.line_close^;
        sym_p := sym_p^.field_next_p;  {advance to next field in this record}
        end
      ;
    end;                               {back and process this new field}

  sst_w.tab_indent^;
  sst_w.appendn^ ('}', 1);
  sst_scope_p := scope_old_p;          {restore old scope/namespace}
  sst_names_p := names_old_p;
  end;
{
************************************
*
*   Data type is an ARRAY.
*
*   The C declaration for an array looks like this:
*
*     <element data type> <array name>[s1][s2] ... [sn]
*
*   The current writing position is at the start of <element data type>.
}
sst_dtype_array_k: begin
  string_copy (name, name_ele);        {init symbol "name" to declare}
  dt_p := addr(dtype);                 {init to outermost data type}
  repeat                               {once for each subscript in array}
    string_append1 (name_ele, '[');
    string_f_int (token, max(dt_p^.ar_ind_n, 1)); {string for dimension of this subscr}
    string_append (name_ele, token);
    string_append1 (name_ele, ']');
    dt_p := dt_p^.ar_dtype_rem_p;      {advance to data type after this subscript}
    until dt_p = nil;                  {just did last subscript ?}
  sst_w_c_dtype_simple (dtype.ar_dtype_ele_p^, name_ele, false); {do the declare}
  name_written := true;                {indicate NAME already written}
  end;
{
************************************
*
*   Data type is a SET.
*
*   The C language has no SET data type.  These will be emulated with unsigned
*   integers.  Each element of the set will have a 2**n value.  The first element
*   will have value 1.  For now, this implementation only allows set sizes
*   limited to the number of bits in the largest available integer.
}
sst_dtype_set_k: begin
  int_sign := int_unsigned_k;          {emulate with unsigned integers}
  goto do_int;
  end;
{
************************************
*
*   Data type is a RANGE.
*
*   The C language has no subrange data type.  These will be emulated with
*   either signed or unsigned integers, depending on whether the range includes
*   any negative numbers.
}
sst_dtype_range_k: begin
  if dtype.range_ord_first >= 0
    then int_sign := int_unsigned_k    {no negative values in subrange}
    else int_sign := int_signed_k;     {range does cover negative values}
  goto do_int;
  end;
{
************************************
*
*   Data type is a PROCEDURE
*
*   The C declaration for a pointer to a routine looks like this:
*
*     <function value data type> (*<symbol name>) (<arguments template>)
*
*   Since this translator data type is a routine, not a pointer to a routine,
*   the call argument NAME already contains the leading "*".
}
sst_dtype_proc_k: begin
  name_ele.len := 0;                   {empty string for writing data type only}
{
*   Write data type of function return, if any.
}
  if dtype.proc_p^.dtype_func_p = nil
    then begin                         {subroutine, returns no value}
      sst_w.appendn^ ('void', 4);
      end
    else begin                         {routine is function, returns a value}
      sst_w_c_dtype_simple (dtype.proc_p^.dtype_func_p^, name_ele, false);
      end
    ;
  sst_w.delimit^;
{
*   Write name of symbol to declare.
}
  sym_p := dtype.proc_p^.sym_p;        {get pointer to routine name symbol}
  if
      (sym_p <> nil) and then          {we have routine name symbol ?}
      (sst_symflag_global_k in sym_p^.flags) and {this is a global function ?}
      (sst_config.os = sys_os_win32_k) {writing for Win32 API ?}
      then begin
    sst_w.appends^ ('__stdcall'(0));   {specify linkage conventions}
    sst_w.delimit^;
    end;

  if name.str[1] = '*'
    then begin                         {name must be enclosed in parenthesis}
      sst_w.appendn^ ('(', 1);
      sst_w.append^ (name);
      sst_w.appendn^ (')', 1);
      end
    else begin
      sst_w.append^ (name);
      end
    ;
  sst_w.delimit^;
  name_written := true;                {indicate symbol name already written}
{
*   Write arguments template.
}
  sst_w.appendn^ ('(', 1);
  arg_p := dtype.proc_p^.first_arg_p;  {init pointer to first call argument}
  if arg_p = nil
    then begin                         {this routine takes no call arguments}
      sst_w.appendn^ ('void', 4);
      end
    else begin                         {this routine does take call arguments}
      while arg_p <> nil do begin      {once for each call argument}
        sst_w.line_close^;             {each argument goes on a new line}
        sst_w.tab_indent^;
        sst_w.indent^;                 {for wrapped argument definition}
        case arg_p^.pass of            {how is this argument passed ?}

sst_pass_ref_k: begin                  {argument is passed by reference}
  if
      arg_p^.univ and                  {any calling data type allowed to match ?}
      (sst_ins or sst_writeall)        {declaration not be seen by definition code ?}
    then begin                         {don't force caller to write type cast}
      sst_w.appends^ ('void *'(0));
      end
    else begin                         {write declaration to match definition}
      sst_w_c_dtype_simple (arg_p^.dtype_p^, name_ele, false); {write arg dtype name}
      dt_p := arg_p^.dtype_p;          {get argument's base data type}
      while dt_p^.dtype = sst_dtype_copy_k
        do dt_p := dt_p^.copy_dtype_p;
      if                               {not already passed as pointer anyway ?}
          dt_p^.dtype <> sst_dtype_array_k
          then begin
        sst_w.delimit^;
        sst_w.appendn^ ('*', 1);
        end;
      end
    ;
  end;

sst_pass_val_k: begin                  {argument is passed by value}
  sst_w_c_dtype_simple (arg_p^.dtype_p^, name_ele, false); {write arg dtype name}
  end;

otherwise
          sys_msg_parm_int (msg_parm[1], ord(arg_p^.pass));
          sys_message_bomb ('sst_c_write', 'arg_pass_method_bad', msg_parm, 1);
          end;

        arg_p := arg_p^.next_p;        {advance to next argument in list}
        if arg_p <> nil then begin     {another argument follows this one ?}
          sst_w.appendn^ (',', 1);
          sst_w.delimit^;
          end;
        sst_w.undent^;                 {back from extra level for this argument}
        end;                           {back and process next call argument}
      end
    ;                                  {done with body of call args template}
  sst_w.appendn^ (')', 1);
  end;
{
************************************
*
*   Data type is a POINTER.
}
sst_dtype_pnt_k: begin
  name_ele.len := 0;
  string_append1 (name_ele, '*');
  string_append (name_ele, name);
  sst_w_c_dtype_simple (dtype.pnt_dtype_p^, name_ele, false);
  name_written := true;                {indicate symbol name already written}
  end;
{
************************************
*
*   Data type is a COPY of another data type.
}
sst_dtype_copy_k: begin
  sst_w_c_dtype_simple (dtype.copy_dtype_p^, name, pack);
  name_written := true;                {indicate symbol name already written}
  end;
{
************************************
*
*   Unexpected data type.
}
otherwise
    sys_msg_parm_int (msg_parm[1], ord(dtype.dtype));
    sys_message_bomb ('sst', 'dtype_unexpected', msg_parm, 1);
    end;                               {end of data type cases}
done_case:                             {jump here if done with any one case}
{
*   Done with all the data type cases.  For most data types in C, the symbol
*   being declared comes after the data type.  The symbol must now be written
*   if NAME_WRITTEN is false.  Also, the field width for a packed record field
*   must be written, if appropriate.
}
  if (not name_written) and (name.len > 0) then begin {need to write symbol name ?}
    sst_w.delimit^;
    sst_w.append^ (name);
    end;

  if pack and (fw > 0) then begin      {need to write field width specifier ?}
    sst_w.appendn^ (':', 1);
    sst_w.delimit^;
    string_f_int (token, fw);          {make field width string}
    sst_w.append^ (token);
    end;
  end;
