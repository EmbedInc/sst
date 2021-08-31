{   Subroutine SST_W_C_VAR (V, ADDR_CNT)
*
*   Write the variable reference from the variable descriptor V.
*   ADDR_CNT is the number of times the resulting expression should be
*   the address of the variable descriptor.  A value of 0 causes the
*   variable reference to be written as is.  Values above 0 cause
*   "*"s to be removed or "&"s added.  Values below 0 have the reverse
*   affect.
}
module sst_w_c_var;
define sst_w_c_var;
%include 'sst_w_c.ins.pas';

procedure sst_w_c_var (                {write variable reference}
  in      v: sst_var_t;                {variable reference descriptor block}
  in      addr_cnt: sys_int_machine_t); {number of times to take address of}

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}
  max_insert = 2;                      {max modifiers can be inserted after top}

var
  mod_p: sst_var_mod_p_t;              {points to current variable modifier}
  quit_p: sst_var_mod_p_t;             {pointer to start of mod chain to ignore}
  dtype_p: sst_dtype_p_t;              {pointer to data type at current modifier}
  dt_p: sst_dtype_p_t;                 {scratch data type pointer}
  dt_nc_p: sst_dtype_p_t;              {data type before resolve copies}
  sym_p: sst_symbol_p_t;               {scratch pointer to symbol descriptor}
  ordval: sys_int_max_t;               {ordinal value for array subscript range}
  addr_cnt_adj: sys_int_machine_t;     {adjusted internal address-of count}
  since_arrow: sys_int_machine_t;      {number of modifiers since last "->"}
  since_deref: sys_int_machine_t;      {num of modifiers since last derefernce mod}
  mod_insert:                          {modifiers may be inserted after top ref}
    array[1..max_insert] of sst_var_mod_t;
  insert_cnt: sys_int_machine_t;       {number of modifiers currently inserted}
  old_next_p: sst_var_mod_p_t;         {saved copy of V.MOD1.NEXT_P}
  pnt_cnt: sys_int_machine_t;          {levels of pointers before base data type}
  i: sys_int_machine_t;                {scratch integer and loop counter}
  token: string_var16_t;               {scratch string for number conversion}
  paren: boolean;                      {leading paren written, need close paren}
  enclose: boolean;                    {enclose everything when start with spec char}
  spchar_first: boolean;               {next special leading char is the first}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;
  stat: sys_err_t;                     {error status code}

label
  resolve_dtype;
{
****************************************
*
*   Local subroutine DEREF_TOP
*
*   Edit modifier chain so that the top modifier is dereferenced one level
*   more than it otherwise would be.  INSERT_CNT is updated to indicate the
*   total number of modifiers inserted after the top modifier.
}
procedure deref_top;

var
  mod_p: sst_var_mod_p_t;              {scratch modifier pointer}

begin
  insert_cnt := insert_cnt + 1;        {make MOD_INSERT index for new modifier}
  if insert_cnt > max_insert then begin
    sys_message_bomb ('sst_write_c', 'var_mode_too_many', nil, 0);
    end;
  mod_insert[insert_cnt].next_p := v.mod1.next_p; {fill in new modifier}
  mod_insert[insert_cnt].modtyp := sst_var_modtyp_unpnt_k;
  mod_p := addr(v.mod1);               {subvert IN declaration for V argument}
  mod_p^.next_p := addr(mod_insert[insert_cnt]); {insert new modifier after top sym}
  end;
{
****************************************
*
*   Local function DEREF_FIELD (M)
*
*   Return true if the modifier M is a pointer dereference of a record
*   pointer followed by a field in that record.  It is assumed that MOD
*   is a pointer dereference modifier.
}
function deref_field (
  in      m: sst_var_mod_t)            {modifier to check out}
  :boolean;                            {TRUE if rec pointer deref followed by field}
  val_param;

begin
  deref_field := false;                {init to not dereference before field name}
  if m.next_p = nil then return;       {no modifier follows ?}
  if m.next_p^.modtyp <> sst_var_modtyp_field_k {next modifier is not a field ?}
    then return;
  deref_field := true;
  end;
{
****************************************
*
*   Local subroutine LEADING_DEREF (M)
*
*   Handle all pointer dereferences that are not from record pointers followed
*   by fields.  Each such pointer dereference requires a "*" be written at the
*   start of the variable reference expression.  The modifier chain starting
*   at M is followed.  QUIT_P will be updated to point to the first modifier
*   of a continuous block of dereference modifiers at the end of the variable
*   descriptor chain.  This allows the main routine to stop when that modifier
*   is reached.  A leading parenthesis will be written before the "*"s for
*   all but the outermost group.
}
procedure leading_deref (
  in      m: sst_var_mod_t);           {modifier at start of chain to check out}
  val_param;

var
  m_p: sst_var_mod_p_t;                {pointer to current modifier}
  deref_cnt: sys_int_machine_t;        {number of dereferences at end of mod chain}

label
  write_deref;

begin
  deref_cnt := 0;                      {init number of dereferences found}
  quit_p := nil;                       {indicate no dereference block found at end}

  m_p := addr(m);                      {init current modifier to first in chain}
  while m_p <> nil do begin            {loop thru all the modifiers}
    if
        (m_p^.modtyp = sst_var_modtyp_unpnt_k) and {dereference modifier ?}
        (not deref_field(m_p^))        {not deref from rec pointer to field ?}
      then begin                       {this is dereference modifier we care about}
        if quit_p = nil then begin     {first consecutive dereference ?}
          quit_p := m_p;               {save start of consecutive dereference chain}
          end;
        deref_cnt := deref_cnt + 1;    {count one more consecutive dereference}
        end
      else begin                       {not deref modifier we are looking for}
        if deref_cnt <> 0 then begin   {first mod after block of dereferences ?}
          leading_deref (m_p^);        {process rest of modifier chain recursively}
          sst_w.appendn^ ('(', 1);
          spchar_first := false;       {next special char will not be at var start}
          goto write_deref;            {write the dereferences}
          end;
        end
      ;
    m_p := m_p^.next_p;                {advance to next modifier in chain}
    end;                               {back and process this new modifier}
{
*   DEREF_CNT is set to the number of consecutive dereferences found at the end
*   of the modifier chain that was not handled recursively.  This routine calls
*   itself recursively in such a way that the first invocation to come thru here
*   represents the end of the modifier chain.  Therefore ADDR_CNT_ADJ will be
*   used only by the first invocation.
}
write_deref:                           {jump here if derefs not at end of chain}
  deref_cnt := deref_cnt - addr_cnt_adj; {add in caller's dereferences}
  addr_cnt_adj := 0;                   {insure caller's dereferences used only once}

  if                                   {need leading parenthesis ?}
      (deref_cnt <> 0) and             {will write some special character ?}
      spchar_first and                 {this will be start of everything written}
      enclose                          {need to enclose on leading special char ?}
      then begin
    sst_w.appendn^ ('(', 1);           {write leading parenthesis}
    paren := true;                     {we will need close parenthesis later}
    end;
  spchar_first := false;               {no longer at start of whole var}

  while deref_cnt > 0 do begin         {once for each dereference to write}
    sst_w.appendn^ ('*', 1);
    deref_cnt := deref_cnt - 1;        {one less dereference to write}
    end;

  while deref_cnt < 0 do begin         {once for each "address of" to write}
    sst_w.appendn^ ('&', 1);
    deref_cnt := deref_cnt + 1;        {one less dereference to write}
    end;
  end;
{
****************************************
*
*   Start of main routine.
}
begin
  token.max := sizeof(token.str);      {init local var string}

  enclose := false;                    {init to don't enclose everything in ()}
  paren := false;                      {init to no close paren needed}
  spchar_first := true;                {next char will be first in var reference}
{
*   Init DTYPE_P to point to the data type of the top symbol.  It will be
*   kept up to date as each modifier is processed.  DTYPE_P is set to NIL
*   to indicate no data type exists.
}
  dtype_p := nil;                      {init to no current data type exists}
  case v.mod1.top_sym_p^.symtype of    {what kind of symbol do we have ?}
sst_symtype_const_k: dtype_p := v.mod1.top_sym_p^.const_exp_p^.dtype_p;
sst_symtype_enum_k: ;
sst_symtype_dtype_k: dtype_p := v.mod1.top_sym_p^.dtype_dtype_p;
sst_symtype_field_k: dtype_p := v.mod1.top_sym_p^.field_dtype_p;
sst_symtype_var_k: dtype_p := v.mod1.top_sym_p^.var_dtype_p;
sst_symtype_abbrev_k: dtype_p := v.mod1.top_sym_p^.abbrev_var_p^.dtype_p;
sst_symtype_proc_k: ;
sst_symtype_com_k: ;
otherwise
    sys_msg_parm_int (msg_parm[1], ord(v.mod1.top_sym_p^.symtype));
    sys_message_bomb ('sst', 'symbol_type_unknown', msg_parm, 1);
    end;
  if dtype_p <> nil then begin         {there is a data type to resovle ?}
    while dtype_p^.dtype = sst_dtype_copy_k do dtype_p := dtype_p^.copy_dtype_p;
    end;
{
*   Set ADDR_CNT_ADJ.  This indicates the number of pointer derefernces to ignore
*   at the end of the modifier string.  Some types of variables are implicitly
*   pointers instead of representing the thing they are pointing to.
}
  addr_cnt_adj := addr_cnt;            {init to number of address-ofs caller wants}
  if v.dtype_p <> nil then begin       {this variable reference has a data type ?}
    dt_p := v.dtype_p;                 {resolve base pointed-to data type}
    dt_nc_p := dt_p;                   {save pointer to dtype before resolve copy}
    pnt_cnt := 0;                      {init pointed-to count for base data type}
resolve_dtype:
    case dt_p^.dtype of
sst_dtype_pnt_k: begin
        if dt_p^.pnt_dtype_p <> nil then begin {not NIL pointer ?}
          dt_p := dt_p^.pnt_dtype_p;   {get pointed-to data type}
          dt_nc_p := dt_p;             {save dtype pointer before resolve copies}
          pnt_cnt := pnt_cnt + 1;      {count one more level of pointer data type}
          goto resolve_dtype;
          end;
        end;
sst_dtype_copy_k: begin
        dt_p := dt_p^.copy_dtype_p;
        goto resolve_dtype;
        end;

sst_dtype_array_k: begin               {ultimate pointed-to data type is an array}
        if                             {array/pointer special case ?}
            (pnt_cnt = 0) or           {var descriptor represents array directly ?}
            (pnt_cnt + addr_cnt_adj = 0) {caller want direct array ?}
            then begin
          pnt_cnt := pnt_cnt + addr_cnt_adj; {num of final "address of"s array dtype}
          addr_cnt_adj :=              {add in possible implicit pointer dereference}
            addr_cnt_adj + addr_cnt_ar;
          token.len := 0;              {init to no "pointer to" operators}
          case array_mode of           {how will this array name be interpreted}
array_whole_k: ;                       {identifier is exactly what we assume it is}
array_pnt_whole_k: ;                   {array symbol is pointer to whole array}
array_pnt_first_k: begin               {pointer to first ele, need to re-cast}
  for i := 1 to pnt_cnt do begin       {once for each "pointer to" final array dtype}
    string_append1 (token, '*');
    end;
  sst_w.appendn^ ('(', 1);             {start of type-casting operator}
  sst_w_c_dtype_simple (dt_nc_p^, token, false); {write final desired data type}
  sst_w.appendn^ (')', 1);             {end the type-casting operator}
  end;
otherwise
            sys_msg_parm_int (msg_parm[1], ord(array_mode));
            syo_error (
              v.mod1.top_str_h, 'sst_c_write', 'array_mode_unexpeted', msg_parm, 1);
            end;
          end;
        end;                           {done with pointed to data type is ARRAY}

sst_dtype_proc_k: begin                {ultimate pointed-to data type is ROUTINE}
        if (pnt_cnt + addr_cnt_adj) = 0 then begin {actually calling the routine ?}
          enclose := true;             {enclose in parens if using special chars}
          end;
        end;

      end;                             {end of data type cases}
    end;                               {end of var descriptor has a data type}
{
*   Determine whether the top modifier must be dereferenced before use.  This
*   can happen sometimes when it is a dummy argument passed by reference,
*   or an abbreviation symbol reference.
*   In that case, an extra dereference modifier is temporarily inserted in the
*   chain.  The original NEXT_P in the top modifier is saved in OLD_NEXT_P.
}
  old_next_p := v.mod1.next_p;         {save original NEXT_P of top modifier}
  insert_cnt := 0;                     {init to no modifiers inserted after top sym}
  sym_p := v.mod1.top_sym_p;           {init pointer to top symbol}

  case sym_p^.symtype of               {what is top symbol type ?}

sst_symtype_var_k: begin               {top symbol is a variable}
      if
          (sym_p^.var_arg_p <> nil) and then {variable is a dummy arg ?}
          (sym_p^.var_arg_p^.pass = sst_pass_ref_k) {passed by ref ?}
          then begin
        dt_p := sym_p^.var_dtype_p;    {resolve base data type of variable}
        while dt_p^.dtype = sst_dtype_copy_k do dt_p := dt_p^.copy_dtype_p;
        if dt_p^.dtype <> sst_dtype_array_k then begin {dummy arg is not an array ?}
          deref_top;                   {edit chain to dereference top symbol}
          end;
        end;
      end;

sst_symtype_abbrev_k: begin            {top symbol is an abbreviation}
      deref_top;
      end;

    end;                               {end of top symbol type cases}
{
*   Initialize for looping thru all the modifiers.
}
  quit_p := nil;                       {init MOD_P value at which to quit}
  since_arrow := 1;                    {init to not right after writing "->"}
  since_deref := 1;                    {init num of modifiers since last deref mod}
  mod_p := addr(v.mod1);               {make current modifier the first}
  while mod_p <> quit_p do begin       {once for each modifier}
    if dtype_p <> nil then begin       {resolve base data type, if there is one}
      while dtype_p^.dtype = sst_dtype_copy_k do dtype_p := dtype_p^.copy_dtype_p;
      end;
    since_arrow := since_arrow + 1;    {one more modifier since writing "->"}
    since_deref := since_deref + 1;    {one more mod since last defef modifier}
    case mod_p^.modtyp of
{
*   Modifier indicates top name token of variable.
}
sst_var_modtyp_top_k: begin
  leading_deref (mod_p^);              {write leading dereference operators}
  spchar_first := false;               {next special char won't be the first}
  if                                   {symbol is variable in a common block ?}
      (mod_p^.top_sym_p^.symtype = sst_symtype_var_k) and
      (mod_p^.top_sym_p^.var_com_p <> nil)
      then begin
    sst_w.append_sym_name^ (mod_p^.top_sym_p^.var_com_p^); {write common block name}
    sst_w.appendn^ ('.', 1);           {var name is really a field in a STRUCT}
    end;
  sst_w.append_sym_name^ (mod_p^.top_sym_p^); {write this top name}
  end;
{
*   Modifier indicates pointer dereference.  All the dereference operators
*   except those on a record pointer followed by a field have been written.
*   These will be written here using the "->" operator.  A close parenthesis
*   must be written for each continuous group of the other pointer dereferences.
}
sst_var_modtyp_unpnt_k: begin
  if deref_field(mod_p^)
    then begin                         {this dereference is for record pnt to field}
      sst_w.appendn^ ('->', 2);
      since_arrow := 0;                {number of modifiers since writing last "->"}
      end
    else begin                         {this dereference written before}
      if since_deref > 1 then begin    {this is start of new block of dereferences ?}
        sst_w.appendn^ (')', 1);
        end;
      since_deref := 0;                {number of modifiers since last deref mod}
      end
    ;
  if                                   {not temp deref inserted after top modifier ?}
      (insert_cnt = 0) or              {no temporary deref was inserted ?}
      (v.mod1.next_p <> mod_p)         {not deref immediately after top mod ?}
      then begin
    dtype_p := dtype_p^.pnt_dtype_p;   {update current data type}
    end;
  end;
{
*   Modifier indicates next array subscript.
}
sst_var_modtyp_subscr_k: begin
  sst_w.appendn^ ('[', 1);
  sst_ordval (                         {get ordinal value of subscript range start}
    dtype_p^.ar_ind_first_p^.val, ordval, stat);
  if ordval = 0
    then begin                         {subscript range starts at 0}
      sst_w_c_exp                      {write complete expression for subscript}
        (mod_p^.subscr_exp_p^, 0, nil, enclose_no_k);
      end
    else begin                         {range doesn't start where C assumed}
      sst_w_c_exp                      {write raw expression value}
        (mod_p^.subscr_exp_p^, 0, nil, enclose_yes_k);
      sst_w.delimit^;
      if ordval >= 0
        then begin                     {need to subtract off ORDVAL}
          sst_w.appendn^ ('-', 1);
          end
        else begin                     {need to add on -ORDVAL}
          sst_w.appendn^ ('+', 1);
          ordval := -ordval;           {make unsigned value to add/subtract}
          end
        ;
      sst_w.delimit^;
      string_f_int_max_base (          {convert ORDVAL to string}
        token,                         {output string}
        ordval,                        {input integer}
        10,                            {number base}
        0,                             {use free format}
        [string_fi_unsig_k],           {input number is unsigned}
        stat);                         {returned error status code}
      sys_error_abort (stat, '', '', nil, 0);
      sst_w.append^ (token);           {write offset for this array subscript}
      end
    ;
  sst_w.appendn^ (']', 1);
  if dtype_p^.ar_dtype_rem_p = nil
    then begin                         {this was last possible subscript}
      dtype_p := dtype_p^.ar_dtype_ele_p;
      end
    else begin                         {there may be more subscripts}
      dtype_p := dtype_p^.ar_dtype_rem_p;
      end
    ;
  end;
{
*   Modifier indicates field in a record.
}
sst_var_modtyp_field_k: begin
  if since_arrow > 1 then begin        {last modifier wasn't "->" ?}
    sst_w.appendn^ ('.', 1);
    end;
  sst_w.append_sym_name^ (mod_p^.field_sym_p^); {write this field name}
  dtype_p := mod_p^.field_sym_p^.field_dtype_p; {update current data type}
  end;
{
*   Unexpected modifier type.
}
otherwise
      sys_msg_parm_int (msg_parm[1], ord(mod_p^.modtyp));
      sys_message_bomb ('sst', 'var_modifier_unknown', msg_parm, 1);
      end;                             {end of modifier type cases}
    sst_w.allow_break^;                {allow a line break after each modifier}
    mod_p := mod_p^.next_p;            {advance to next modifier in chain}
    end;                               {back and process this new modifier}
  mod_p := addr(v.mod1);               {allow writing to IN argument}
  mod_p^.next_p := old_next_p;         {restore original NEXT_P of top modifier}
  if paren then begin                  {need to write close parenthesis ?}
    sst_w.appendn^ (')', 1);
    end;
  end;
