{   Subroutine SST_W_C_EXEC (FIRST_P)
*
*   Process a chain of executable opcodes.  The opcodes will be translated
*   to C.  FIRST_P is pointing to the first opcode in the chain.  It may
*   be NIL to indicate an empty chain.  This subroutine returns when the
*   end of chain is reached.  It is an error if the chain contains any
*   non-executable opcodes.
}
module sst_w_c_EXEC;
define sst_w_c_exec;
%include 'sst_w_c.ins.pas';

var
  int_def_name: string_var4_t :=       {name of default integer}
    [str := 'int', len := 3, max := 4];

procedure sst_w_c_exec (               {process executable opcode chain}
  in      first_p: sst_opc_p_t);       {pointer to first opcode, may be NIL}

const
  n_args_same_line = 1;                {max args on same line before expand vertical}
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  opc_p: sst_opc_p_t;                  {pointer to current opcode descriptor}
  sym_p, sym2_p, sym3_p: sst_symbol_p_t; {pointers to implicit symbols}
  arg_p: sst_proc_arg_p_t;             {pointer to subroutine argument descriptor}
  argt_p: sst_proc_arg_p_t;            {pointer to subroutine arg template}
  dt_p: sst_dtype_p_t;                 {scratch pointer to data type descriptor}
  case_opc_p: sst_case_opc_p_t;        {points to descriptor for current case}
  case_val_p: sst_case_val_p_t;        {points to descriptor for current case choice}
  token: string_var32_t;               {for number conversion}
  fw: sys_int_machine_t;               {explicit field width value}
  n: sys_int_machine_t;                {scratch counter}
  i: sys_int_machine_t;                {scratch integer and loop counter}
  ordval: sys_int_max_t;               {scratch ordinal value}
  fw1_param, fw2_param: boolean;       {TRUE if field widths passed as parameters}
  did_eol: boolean;                    {TRUE when already did next WRITE EOL opcode}
  unsig: boolean;                      {TRUE if argument is unsigned}
  cast_arg: boolean;                   {TRUE if need to type-cast argument}
  c: char;                             {scratch character}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;
  stat: sys_err_t;                     {completion status code}

label
  write_new_dtype, write_int, write_int_default, write_done_float, done_opcode;
{
*********************************************
*
*   Local subroutine PRECEEDING_BLANKS (DSIZE, EXP_P, FW)
*
*   Emit code that writes blanks before a character string or character.
*   This is neccessary when the desired field width is either unknown or
*   exceeds the size of the available data.  DSIZE is the number of characters
*   available in the data.  EXP_P points to the expression for the desired
*   field width.  EXP_P may be NIL to indicate that no explicit field width
*   was requested.  FW is returned to indicate the remaining field width.
*   When the remaining field width is known, FW is >= zero.  In this case
*   the remaining field width is MAX(MIN(EXP,DSIZE),0).  When the remaining
*   field width is not a known constant FW < 0.
*
*   If N is the known constant number of padding characters:
*
*     printf ("%Ns","");
*
*   When the number of padding characters is not a known constant, EXP is the
*   desired field width expression, and DSIZE is the data size number:
*
*     if (EXP > DSIZE) printf ("%*s", EXP - DSIZE, "");
*
*   An implicit variable will be created for EXP if it is not "simple".  In
*   that case, SYM_P will be left pointing to the implicit variable, otherwise
*   SYM_P will be NIL.
}
procedure preceeding_blanks (
  in      dsize: sys_int_machine_t;    {number of characters available in the data}
  in      exp_p: sst_exp_p_t;          {NIL or points to field width expression}
  out     fw: sys_int_machine_t);      {returned to indicate remaining field width}
  val_param;

var
  token: string_var32_t;               {scratch string for number conversion}

begin
  token.max := sizeof(token.str);      {init local var string}
  sym_p := nil;                        {init to no implicit variable created}

  if exp_p = nil then begin            {default field width requires no padding}
    fw := dsize;
    return;
    end;

  if exp_p^.val_fnd
    then begin                         {desired field width is a known constant val}
      if exp_p^.val.int_val <= dsize then begin {no padding needed ?}
        fw := exp_p^.val.int_val;
        return;
        end;
      sst_w.appendn^ ('printf ("%', 10);
      string_f_int (token, exp_p^.val.int_val - dsize); {make num pad chars string}
      sst_w.append^ (token);
      sst_w.appendn^ ('s","")', 6);
      fw := dsize;
      end
    else begin                         {desired field width value is not known}
      sst_w_c_exp_implicit (exp_p^, sym_p); {create implicit var if needed}
      sst_w.appendn^ ('if (', 4);
      if sym_p = nil                   {write field width expression value}
        then sst_w_c_exp (exp_p^, 0, nil, enclose_yes_k)
        else sst_w.append_sym_name^ (sym_p^);
      sst_w.delimit^;
      sst_w.appendn^ ('>', 1);
      sst_w.delimit^;
      string_f_int (token, dsize);     {make DSIZE constant string}
      sst_w.append^ (token);
      sst_w.appendn^ (')', 1);
      sst_w.delimit^;
      sst_w.appendn^ ('printf (', 8);
      sst_w.allow_break^;
      sst_w.appendn^ ('"%*s",', 6);
      sst_w.delimit^;
      if sym_p = nil                   {write field width expression value}
        then sst_w_c_exp (exp_p^, 0, nil, enclose_yes_k)
        else sst_w.append_sym_name^ (sym_p^);
      sst_w.delimit^;
      sst_w.appendn^ ('-', 1);
      sst_w.delimit^;
      sst_w.append^ (token);
      sst_w.appendn^ (',', 1);
      sst_w.delimit^;
      sst_w.appendn^ ('"")', 3);
      if dsize <= 0
        then fw := 0                   {definately nothing left to write}
        else fw := -1;                 {remaining field width not known}
      end
    ;
  sst_w_c_sment_end;
  sst_w_c_sment_start;
  end;
{
*********************************************
*
*   Local subroutine CHECK_EOL
*
*   Check whether the next opcode is WRITE EOL.  If so, write the neccessary
*   characters to the PRINTF format string at the current position to cause
*   the end of line to be written, and set DID_EOL to TRUE.  The characters
*   written are "\n".
}
procedure check_eol;

begin
  if                                   {next opcode is WRITE EOL ?}
      (opc_p^.next_p <> nil) and then
      (opc_p^.next_p^.opcode = sst_opc_write_eol_k)
      then begin
    sst_w.appendn^ ('\n', 2);
    did_eol := true;                   {remember that next opcode already done}
    end;
  end;
{
*********************************************
*
*   Local subroutine FIELD_WIDTH (FW_P, PARAM)
*
*   Write the field width specifier in a PRINTF format string.  FW_P points to the
*   field width expression descriptor.  The field width will be written
*   directly into the format string when it is a known constant.  Otherwise
*   "*" will be written into the format string, and PARAM will be set to
*   TRUE to indicate that the field width must be explicitly passed as
*   a parameter after the format string.
}
procedure field_width (
  in      fw_p: sst_exp_p_t;           {expression descriptor for field width}
  out     param: boolean);             {TRUE when field width must be passed later}
  val_param;

begin
  param := false;                      {init to don't write FW as parameter later}
  if fw_p = nil then return;           {no field width to write ?}
  if not fw_p^.val_fnd then begin      {field width has no known constant value ?}
    sst_w.appendn^ ('*', 1);           {tells C that field width coming as param}
    param := true;
    return;
    end;
  sst_w_c_value (fw_p^.val, enclose_yes_k); {write field width value into fmt string}
  end;
{
*********************************************
*
*   Start of main routine.
}
begin
  token.max := sizeof(token.str);      {init local var string}
  path_to_here := true;                {init to path exists to after opcodes}

  opc_p := first_p;                    {init current opcode to first opcode}
  while opc_p <> nil do begin          {keep looping until end of opcode chain}
    with opc_p^: opc do begin          {OPC is the current opcode descriptor}
      case opc.opcode of               {what kind of opcode is this ?}
{
***************************************
*
*   Label at current position.
}
sst_opc_label_k: begin
  path_to_here := true;                {executable path exists to after this opcode}
  if not (sst_symflag_used_k in opc.label_sym_p^.flags) {label not used }
    then goto done_opcode;             {skip over unused label}
  sst_w.append_sym_name^ (opc.label_sym_p^); {write label name}
  sst_w.appendn^ (': ;', 3);
  sst_w.line_close^;                   {label reference is always on its own line}
  end;
{
***************************************
*
*   Subroutine call.
}
sst_opc_call_k: begin
  sst_w_c_sment_start;                 {start a new statement}
  sst_w_c_var (opc.call_var_p^, 0);    {write subroutine name reference}
  sst_w.delimit^;
  sst_w.appendn^ ('(', 1);             {open paren of argument list}
  arg_p := opc.call_proc_p^.first_arg_p; {init pointer to first call argument}
  argt_p := opc.call_proct_p^.first_arg_p; {init pointer to first call arg template}
  while (arg_p <> nil) or (argt_p <> nil) do begin {once for each call argument}
    if opc.call_proc_p^.n_args > n_args_same_line then begin {expand vertically ?}
      sst_w.line_close^;               {start this arg on a new line}
      sst_w.tab_indent^;
      end;
    sst_w_c_arg (arg_p, argt_p);       {write this call argument}
    if (arg_p <> nil) or (argt_p <> nil) then begin {another arg after this one ?}
      sst_w.appendn^ (',', 1);
      sst_w.delimit^;
      end;
    end;                               {back and process new argument}
  sst_w.appendn^ (')', 1);             {close paren of argument list}
  sst_w_c_sment_end;
  path_to_here := not                  {path continues if routine might return}
    (sst_procflag_noreturn_k in opc.call_proc_p^.flags);
  end;
{
***************************************
*
*   Assignment statement.
}
sst_opc_assign_k: begin
  sst_w_c_assign (opc.assign_var_p^, opc.assign_exp_p^);
  path_to_here := true;                {executable path exists to after this opcode}
  end;
{
***************************************
*
*   GOTO statment.
}
sst_opc_goto_k: begin
  sst_w_c_sment_start;                 {start a new statement}
  sst_w.appendn^ ('goto', 4);
  sst_w.delimit^;
  sst_w.append_sym_name^ (opc.goto_sym_p^); {write label name}
  sst_w_c_sment_end;                   {finish this statement}
  path_to_here := false;               {no path exists to after this opcode}
  end;
{
***************************************
*
*   CASE statement.  This will have the form:
*
*   switch (<expression>) [
*     case <expression>:
*     case <expression>:
*       <statements for these cases>
*       break;
*     case <expression>:
*       .
*       .
*       break;
*     default:
*       <statements>
*       break;
*     ]
*
*   If the switch expression has a known value, then only the code for the
*   appropriate case is written out.
}
sst_opc_case_k: begin
  if opc.case_exp_p^.val_fnd then begin {value of switch expression is known ?}
    sst_ordval (opc.case_exp_p^.val, ordval, stat); {get ordinal value of switch exp}
    syn_error_abort (stat, opc.case_exp_p^.str_h, '', '', nil, 0);
    case_val_p := opc.case_val_p;      {init pointer to first choice value in list}
    while case_val_p <> nil do begin   {loop thru the list of choice values}
      if case_val_p^.val = ordval then begin {this is the selected choice ?}
        sst_w_c_exec (case_val_p^.opc_p^.code_p); {write code for selected case}
        goto done_opcode;
        end;
      case_val_p := case_val_p^.next_val_p; {advance to next value in CASE}
      end;                             {back and check this new choice value}
    sst_w_c_exec (opc.case_none_p);    {switch value didn't match any choice}
    goto done_opcode;
    end;                               {done handling switch value is known}
{
*   switch <expression> [
}
  sst_w_c_sment_start;                 {start a new statement}
  sst_w.indent^;                       {extra indent for continued lines}
  sst_w.appendn^ ('switch (', 8);
  sst_w_c_exp (opc.case_exp_p^, 0, nil, enclose_no_k); {write case selection expression}
  sst_w.appendn^ (')', 1);
  sst_w.delimit^;
  sst_w.appendn^ ('{', 1);
  sst_w.line_close^;
  sst_w.undent^;                       {back for extra indentation level}
{
*   Done writing statement header.  Now loop over the cases in this
*   statement.
}
  case_opc_p := opc.case_opc_p;        {init current case to first case in chain}
  while case_opc_p <> nil do begin     {once for each case in CASE statement}
{
*   Write the choice values for the current case.  These have the form:
*
*     case <expression>:
}
    case_val_p := case_opc_p^.val_first_p; {init curr choice to first for this case}
    while case_val_p <> nil do begin   {once for each choice value for this case}
      sst_w.tab_indent^;
      sst_w.indent^;
      sst_w.appendn^ ('case', 4);
      sst_w.delimit^;
      sst_w_c_exp (case_val_p^.exp_p^, 0, nil, enclose_no_k); {write choice expression}
      sst_w.appendn^ (':', 1);
      sst_w.line_close^;
      sst_w.undent^;
      case_val_p := case_val_p^.next_opc_p; {advance to next choice for this case}
      end;
{
*   Done writing the last choice value for the current case.
*   Now write the code for this case.
}
    sst_w.indent^;                     {code is indented one more level from choice}
    sst_w_c_exec (case_opc_p^.code_p); {write the statements for this case}
    if path_to_here then begin         {path exists to after opcodes for this case ?}
      sst_w.tab_indent^;
      sst_w.appendn^ ('break;', 6);
      sst_w.line_close^;
      end;
    sst_w.undent^;                     {back to level for next choice}

    case_opc_p := case_opc_p^.next_p;  {advance to next case in CASE statement}
    end;                               {back and process this new case}
{
*   Done handling all the explicit cases in the CASE statement.  Now write
*   the DEFAULT clause, if exists.
}
  if opc.case_none_p <> nil then begin {DEFAULT case exists ?}
    sst_w.tab_indent^;
    sst_w.appendn^ ('default:', 8);
    sst_w.line_close^;
    sst_w.indent^;                     {code is indented one more level from choice}
    sst_w_c_exec (opc.case_none_p);    {write the statements for the default case}
    sst_w.tab_indent^;
    sst_w.appendn^ ('break;', 6);
    sst_w.line_close^;
    sst_w.undent^;                     {back to level of case choices}
    end;
  sst_w.tab_indent^;
  sst_w.appendn^ ('}', 1);             {end of SWITCH statement block}
  sst_w_c_sment_end;                   {end the SWITCH statement}
  path_to_here := true;                {executable path exists to after this opcode}
  end;
{
***************************************
*
*   If statement.  This will have the simple form:
*
*     if (conditional expression) [
*       .
*       .
*       ]
*
*   or the more complicated form:
*
*     if (conditional expression)
*       [
*         .
*         .
*         ]
*       else [
*         .
*         .
*         ]
*       ;
*
*   The decision logic will be removed completely if the conditional expression
*   has a known value.
}
sst_opc_if_k: begin
  if opc.if_exp_p^.val_fnd then begin  {conditional expression value is known ?}
    if opc.if_exp_p^.val.bool_val
      then begin                       {conditional expression is TRUE}
        sst_w_c_exec (opc.if_true_p);
        end
      else begin                       {conditional expression is FALSE}
        sst_w_c_exec (opc.if_false_p);
        end
      ;
    goto done_opcode;                  {all done with this conditional opcode}
    end;

  sst_w_c_sment_start;                 {start a new statement}
  sst_w.indent^;                       {indent header wrapping one extra level}
  sst_w.appendn^ ('if', 2);
  sst_w.delimit^;
  sst_w.appendn^ ('(', 1);
  sst_w_c_exp (opc.if_exp_p^, 0, nil, enclose_no_k); {write the conditional expression}
  sst_w.appendn^ (')', 1);
  if opc.if_false_p = nil
{
*   Statement has the simple form.
}
    then begin
      sst_w.delimit^;
      sst_w.appendn^ ('{', 1);
      sst_w.undent^;                   {done with extra header wrap indent}
      sst_w.line_close^;
      sst_w_c_exec (opc.if_true_p);
      sst_w.tab_indent^;
      sst_w.appendn^ ('}', 1);
      end
{
*   Statement has the more complicated form.
}
    else begin
      sst_w.undent^;                   {done with extra header wrap indent}

      sst_w.line_close^;
      sst_w.tab_indent^;
      sst_w.appendn^ ('{', 1);
      sst_w.indent^;                   {indent for TRUE case statements}
      sst_w.line_close^;
      sst_w_c_exec (opc.if_true_p);    {write TRUE case statements}
      sst_w.tab_indent^;
      sst_w.appendn^ ('}', 1);         {end of TRUE case clause}
      sst_w.undent^;                   {back from TRUE case clause indentation}

      sst_w.line_close^;
      sst_w.tab_indent^;
      sst_w.appendn^ ('else {', 6);
      sst_w.indent^;                   {indent for FALSE case statements}
      sst_w.line_close^;
      sst_w_c_exec (opc.if_false_p);   {write the FALSE case statements}
      sst_w.tab_indent^;
      sst_w.appendn^ ('}', 1);
      sst_w.undent^;                   {back from FALSE case clause indentation}

      sst_w.line_close^;
      sst_w.tab_indent^;
      end
    ;
  sst_w_c_sment_end;                   {finish this statement}
  path_to_here := true;                {executable path exists to after this opcode}
  end;
{
***************************************
*
*   Counted loop statement.  This will take one of three forms.
*   In all cases, the loop limit will be assigned to an implicit
*   variable if it is not known to be a constant.  This is because
*   C re-evaluates the loop limit every iteration.
*
*   The loop running variable that the compiler is told about will always
*   be of at least machine integer size.  This makes it far less likely that
*   we will be going up to the last, or down to the first possible value
*   of the running variable (in which case the loop will never terminate).
*
*   SYM3_P will be set to point at the substitue running variable, if one
*   is created.  SYM3_P will be NIL if none is created.
}
sst_opc_loop_cnt_k: begin
  sst_w_c_sment_start;                 {start a new statement}
  if opc.lpcn_exp_end_p^.val_fnd
    then begin                         {limit expression has constant value}
      sym2_p := nil;                   {indicate to use expression directly}
      end
    else begin                         {limit expression may change}
      sst_w_c_exp_explicit (           {assign limit expression to a variable}
        opc.lpcn_exp_end_p^,           {descriptor for expression value}
        opc.lpcn_exp_end_p^.dtype_p^,  {data type for variable}
        sym2_p);                       {returned pointing to the variable}
      end
    ;
  sym3_p := nil;                       {init to running variable not substituted}
  dt_p := opc.lpcn_var_p^.dtype_p;     {resolve running variable base data type}
  while dt_p^.dtype = sst_dtype_copy_k do dt_p := dt_p^.copy_dtype_p;
  if dt_p^.bits_min < sst_config.int_machine_p^.bits_min then begin {substitute ?}
    sst_sym_var_new_out (              {create substitue running variable}
      sst_config.int_machine_p^,       {data type for new running variable}
      sym3_p);                         {returned pointer to new variable symbol}
    sst_w_c_symbol (sym3_p^);          {declare the new variable}
    end;
  case opc.lpcn_inc_dir of
{
*   Case 1 - The loop variable is counting up.
*
*     for (var = initial value; var <= limit; var += increment) [
*       <statements>
*       ];
*
*   Case 2 - The loop variable is counting down.
*
*     for (var = initial value; var >= limit; var += increment) [
*       <statements>
*       ];
*
*
*   If a substitute runing variable is created an up-counting loop will be:
*
*     for (var = subvar = initial value; subvar <= limit; subvar += increment,
*         var = subvar) [
*       <statements>
*       ];
}
sst_incdir_up_k,                       {loop variable is counting up}
sst_incdir_down_k: begin               {loop variable is counting down}
  sst_w.indent^;                       {indent header wrapping one extra level}
  sst_w.appendn^ ('for', 3);
  sst_w.delimit^;
  sst_w.appendn^ ('(', 1);

  sst_w_c_var (opc.lpcn_var_p^, 0);
  sst_w.delimit^;
  sst_w.appendn^ ('=', 1);
  sst_w.delimit^;
  if sym3_p <> nil then begin          {substitute running variable in use ?}
    sst_w.append_sym_name^ (sym3_p^);
    sst_w.delimit^;
    sst_w.appendn^ ('=', 1);
    sst_w.delimit^;
    end;
  sst_w_c_exp (opc.lpcn_exp_start_p^, 0, nil, enclose_no_k); {initial value expression}
  sst_w.appendn^ (';', 1);
  sst_w.delimit^;

  if sym3_p = nil
    then sst_w_c_var (opc.lpcn_var_p^, 0)
    else sst_w.append_sym_name^ (sym3_p^);
  sst_w.delimit^;
  case opc.lpcn_inc_dir of             {write end condition comparison operator}
    sst_incdir_up_k: sst_w.appendn^ ('<=', 2);
    sst_incdir_down_k: sst_w.appendn^ ('>=', 2);
    end;
  sst_w.delimit^;
  if sym2_p = nil                      {write loop limit value}
    then sst_w_c_exp (opc.lpcn_exp_end_p^, 0, nil, enclose_yes_k)
    else sst_w.append_sym_name^ (sym2_p^);
  sst_w.appendn^ (';', 1);
  sst_w.delimit^;

  if sym3_p = nil
    then sst_w_c_var (opc.lpcn_var_p^, 0)
    else sst_w.append_sym_name^ (sym3_p^);
  sst_w.delimit^;
  sst_w.appendn^ ('+=', 2);
  sst_w.delimit^;
  sst_w_c_exp (opc.lpcn_exp_inc_p^, 0, nil, enclose_yes_k); {increment value expression}

  if sym3_p <> nil then begin          {using a substitute running variable ?}
    sst_w.appendn^ (',', 1);
    sst_w.delimit^;
    sst_w_c_var (opc.lpcn_var_p^, 0);  {write real variable reference}
    sst_w.delimit^;
    sst_w.appendn^ ('=', 1);
    sst_w.delimit^;
    sst_w.append_sym_name^ (sym3_p^);
    end;

  sst_w.appendn^ (')', 1);
  sst_w.delimit^;
  sst_w.appendn^ ('{', 1);
  sst_w.undent^;
  sst_w.line_close^;
  end;                                 {end of UP or DOWN increment direction case}
{
*
*   Case 3 - The loop variable counting direction is not known.
*
*     for (
*         var = initial value;
*         increment > 0 ? var >= limit : var <= limit;
*         var += increment) [
*       <statements>
*       ];
*
*     This form requires an implicit variable be created for the increment
*     value if it is not a simple expression.
}
sst_incdir_unk_k: begin
  sst_w_c_exp_implicit (opc.lpcn_exp_inc_p^, sym_p); {make implicit var if needed}

  sst_w.indent^;                       {indent header wrapping one extra level}
  sst_w.appendn^ ('for (', 5);
  sst_w.line_close^;

  sst_w.tab_indent^;
  sst_w_c_var (opc.lpcn_var_p^, 0);
  sst_w.delimit^;
  sst_w.appendn^ ('=', 1);
  sst_w.delimit^;
  if sym3_p <> nil then begin          {substitute running variable in use ?}
    sst_w.append_sym_name^ (sym3_p^);
    sst_w.delimit^;
    sst_w.appendn^ ('=', 1);
    sst_w.delimit^;
    end;
  sst_w_c_exp (opc.lpcn_exp_start_p^, 0, nil, enclose_no_k); {initial value expression}
  sst_w.appendn^ (';', 1);
  sst_w.line_close^;

  sst_w.tab_indent^;
  if sym_p = nil                       {write increment value expression}
    then sst_w_c_exp (opc.lpcn_exp_inc_p^, 0, nil, enclose_yes_k)
    else sst_w.append_sym_name^ (sym_p^);
  sst_w.delimit^;
  sst_w.appendn^ ('>', 1);
  sst_w.delimit^;
  sst_w.appendn^ ('0', 1);
  sst_w.delimit^;
  sst_w.appendn^ ('?', 1);
  sst_w.delimit^;
  if sym3_p = nil
    then sst_w_c_var (opc.lpcn_var_p^, 0)
    else sst_w.append_sym_name^ (sym3_p^);
  sst_w.delimit^;
  sst_w.appendn^ ('>=', 2);
  sst_w.delimit^;
  if sym2_p = nil                      {write loop limit value}
    then sst_w_c_exp (opc.lpcn_exp_end_p^, 0, nil, enclose_yes_k)
    else sst_w.append_sym_name^ (sym2_p^);
  sst_w.delimit^;
  sst_w.appendn^ (':', 1);
  sst_w.delimit^;
  if sym3_p = nil
    then sst_w_c_var (opc.lpcn_var_p^, 0)
    else sst_w.append_sym_name^ (sym3_p^);
  sst_w.delimit^;
  sst_w.appendn^ ('<=', 2);
  sst_w.delimit^;
  if sym2_p = nil                      {write loop limit value}
    then sst_w_c_exp (opc.lpcn_exp_end_p^, 0, nil, enclose_yes_k)
    else sst_w.append_sym_name^ (sym2_p^);
  sst_w.appendn^ (';', 1);
  sst_w.line_close^;

  sst_w.tab_indent^;
  if sym3_p = nil
    then sst_w_c_var (opc.lpcn_var_p^, 0)
    else sst_w.append_sym_name^ (sym3_p^);
  sst_w.delimit^;
  sst_w.appendn^ ('+=', 2);
  sst_w.delimit^;
  if sym_p = nil                       {write increment value expression}
    then sst_w_c_exp (opc.lpcn_exp_inc_p^, 0, nil, enclose_yes_k)
    else sst_w.append_sym_name^ (sym_p^);

  if sym3_p <> nil then begin          {using a substitute running variable ?}
    sst_w.appendn^ (',', 1);
    sst_w.delimit^;
    sst_w_c_var (opc.lpcn_var_p^, 0);  {write real variable reference}
    sst_w.delimit^;
    sst_w.appendn^ ('=', 1);
    sst_w.delimit^;
    sst_w.append_sym_name^ (sym3_p^);
    end;

  sst_w.appendn^ (')', 1);
  sst_w.delimit^;
  sst_w.appendn^ ('{', 1);
  sst_w.undent^;
  sst_w.line_close^;
  end;                                 {end of unknown increment direction case}
    end;                               {end of increment direction cases}
{
*   The FOR statement header has been written.  Now write the loop body
*   and close the FOR statement.  The syntax of this part is not dependent
*   on the loop increment direction.
}
  sst_w_c_exec (opc.lpcn_code_p);      {write body of loop}
  sst_w.tab_indent^;
  sst_w.appendn^ ('}', 1);
  sst_w_c_sment_end;                   {finish FOR statement}
  path_to_here := true;                {executable path exists to after this opcode}
  end;
{
***************************************
*
*   Loop with the test at the top.  The loop continues if the descision
*   expression is TRUE.  This takes the form:
*
*     FOR (; expression;) [
*       <statements>
*       ];
}
sst_opc_loop_ttop_k: begin
  sst_w_c_sment_start;
  sst_w.indent^;                       {indent wrapped characters one more level}
  sst_w.appendn^ ('for (;', 6);
  sst_w.delimit^;
  sst_w_c_exp (opc.lptp_exp_p^, 0, nil, enclose_no_k); {write loop decision expression}
  sst_w.appendn^ (';) {', 4);
  sst_w.undent^;                       {back to normal indentation level}
  sst_w.line_close^;                   {end of FOR statement line}
  sst_w_c_exec (opc.lptp_code_p);      {write the statements inside the loop}
  sst_w.tab_indent^;
  sst_w.appendn^ ('}', 1);             {write end of whole FOR statement}
  sst_w_c_sment_end;                   {close the FOR statement}
  path_to_here := true;                {executable path exists to after this opcode}
  end;
{
***************************************
*
*   Loop with test at the bottom.  The loop continues if the decision
*   expression is FALSE.  This takes the form:
*
*     DO [
*       <statements>
*       ] WHILE (! expression);
}
sst_opc_loop_tbot_k: begin
  sst_w_c_sment_start;
  sst_w.appendn^ ('do {', 4);
  sst_w.line_close^;
  sst_w_c_exec (opc.lpbt_code_p);      {write the statements inside the loop}
  sst_w.tab_indent^;
  sst_w.indent^;                       {indent wrapped characters an extra level}
  sst_w.appendn^ ('} while (!', 10);
  sst_w_c_exp (opc.lpbt_exp_p^, 0, nil, enclose_yes_k); {write loop decision expression}
  sst_w.appendn^ (')', 1);
  sst_w.undent^;                       {restore indentation level}
  sst_w_c_sment_end;
  path_to_here := true;                {executable path exists to after this opcode}
  end;
{
***************************************
*
*   Go to start of next time around loop.
}
sst_opc_loop_next_k: begin
  sst_w_c_sment_start;
  sst_w.appendn^ ('continue', 8);
  sst_w_c_sment_end;
  path_to_here := false;
  end;
{
***************************************
*
*   Unconditionally exit loop.
}
sst_opc_loop_exit_k: begin
  sst_w_c_sment_start;
  sst_w.appendn^ ('break', 5);
  sst_w_c_sment_end;
  path_to_here := false;
  end;
{
***************************************
*
*   Return from subroutine or function.  If this is a function then the function
*   return value must follow the RETURN statement.
}
sst_opc_return_k: begin
  sst_w_c_sment_start;
  sst_w.appendn^ ('return', 6);
  if frame_scope_p^.funcval_sym_p <> nil then begin {function return var exists ?}
    sst_w.delimit^;
    sst_w.append_sym_name^ (frame_scope_p^.funcval_sym_p^);
    end;
  if frame_scope_p^.scope_type = scope_type_prog_k then begin {in top program ?}
    sst_w.delimit^;
    sst_w.appendn^ ('0', 1);
    end;
  sst_w_c_sment_end;
  path_to_here := false;
  end;
{
***************************************
*
*   Opcode declares abbreviation in effect over a specific block of code.
*   The abbreviation symbol has already been declared as a variable with
*   a data type of pointer to the abbreviation expansion.
}
sst_opc_abbrev_k: begin
  if opc.abbrev_code_p = nil then goto done_opcode; {no code here ?}
  n := 0;                              {init number of abbreviations actually used}
  sym_p := opc.abbrev_sym_first_p;     {init curr abbrev to first in list}
  while sym_p <> nil do begin          {once for each abbrev in list}
    if sst_symflag_used_k in sym_p^.flags then begin {this abbrev actually used ?}
      n := n + 1;                      {count one more used abbreviation}
      end;
    sym_p := sym_p^.next_p;            {advance to next abbreviation in list}
    end;                               {back to check this new abbreviation}

  if n <= 0 then begin                 {no abbreviations were actually used ?}
    sst_w_c_exec (opc.abbrev_code_p);  {write code as if no abbrevs were declared}
    goto done_opcode;                  {all done with the abbrev opcode}
    end;
{
*   This ABBREV opcode has a code section and at least one abbreviation that
*   is actually used.
}
  sst_w.tab_indent^;                   {write start bracket for new scope}
  sst_w.appendn^ ('{', 1);
  sst_w.line_close^;
  sst_w_c_scope_push (                 {enter abbrev scope and declare all symbols}
    opc.abbrev_scope_p^, scope_type_rout_k);

  sym_p := opc.abbrev_sym_first_p;     {init curr abbrev to first in list}
  sst_w_c_armode_push (                {array vars are pointers to their first ele}
    array_pnt_first_k);
  while sym_p <> nil do begin          {once for each abbreviation in list}
    if sst_symflag_used_k in sym_p^.flags then begin {this abbrev actually used ?}
      sst_w_c_sment_start;             {start pointer assignment statement}
      sst_w.append_sym_name^ (sym_p^); {write abbreviation pointer name}
      sst_w.delimit^;
      sst_w.appendn^ ('=', 1);
      sst_w.delimit^;
      sst_w_c_var (                    {write value to assign to abbrev pointer}
        sym_p^.abbrev_var_p^,          {abbreviation expansion variable reference}
        1);                            {number of times to take addr of var ref}
      sst_w_c_sment_end;               {close abbrev pointer definition statement}
      end;
    sym_p := sym_p^.next_p;            {advance to next abbreviation in list}
    end;
  sst_w_c_armode_pop;

  sst_w_c_exec (opc.abbrev_code_p);    {write code that uses the abbreviations}

  sst_w_c_scope_pop;                   {pop back from abbreviations scope}
  sst_w.tab_indent^;                   {write close bracket to end C scope}
  sst_w.appendn^ ('}', 1);
  sst_w.line_close^;
  end;
{
***************************************
*
*   Call function and discard result.
}
sst_opc_discard_k: begin
  sst_w_c_sment_start;
  sst_w_c_exp (opc.discard_exp_p^, 0, nil, enclose_no_k);
  sst_w_c_sment_end;
  path_to_here := true;                {executable path exists to after this opcode}
  end;
{
***************************************
*
*   Write value of expression to standard output.
}
sst_opc_write_k: begin
  sst_w_c_declare (decl_stdio_k);      {make sure PRINTF and FWRITE declared}
  sst_w_c_sment_start;                 {start this statement}
  fw1_param := false;                  {init to field widths not passed as params}
  fw2_param := false;
  did_eol := false;                    {init to not already did following opcode}

  dt_p := opc.write_exp_p^.dtype_p;    {init data type of value to print}
  cast_arg := false;                   {init to not type-cast argument}
write_new_dtype:                       {back here to re-try with new data type}
  case dt_p^.dtype of                  {what data type is this ?}

sst_dtype_int_k: begin
      unsig := false;                  {integer is signed}
write_int:                             {common code with SUBRANGE data type}
      sst_w.appendn^ ('printf ("%', 10);
      field_width (opc.write_width_exp_p, fw1_param); {write field width, if any}
{
*   Determine if any prefix character needs to be written to specify a
*   non-default integer argument size.  Unfortunately, these characters
*   are system-specific.  By default, PRINTF assumes its arguments are INTs.
}
      for i := 1 to sst_config.n_size_int do begin {once for each integer type}
        if string_equal (sst_config.size_int[i].name, int_def_name) then begin
          if dt_p^.size_used = sst_config.size_int[i].size {write default size int ?}
            then goto write_int_default;
          end;
        end;                           {back and check next available integer name}
{
*   The integer value we are trying to write is not of the size assumed by
*   PRINTF.  An special size specification character is therefore required.
}
      c := ' ';                        {init to no suitable size char exists}
      case sst_config.os of            {what target environment writing code for ?}

sys_os_domain_k: begin
          case dt_p^.size_used of
            2: c := 'h';
            end;
          end;
sys_os_hpux_k: ;                       {no size characters available in this OS}
sys_os_aix_k: ;                        {no size characters available in this OS}
sys_os_irix_k: ;                       {no size characters available in this OS}
sys_os_solaris_k: begin
          if sst_config.int_machine_p^.size_used = 1 then begin {8051 special case ?}
            case dt_p^.size_used of
              1: c := 'b';
              4: c := 'l';
              end;
            end;                       {end of Franklin C51 compiler special case}
          end;                         {end of SOLARIS OS case}
        end;                           {end of OS type cases}
{
*   The character C is set to non-blank if an appropriate integer argument size
*   is available in this compiler.
}
      cast_arg := true;                {init to must type-case argument later}
      if c <> ' ' then begin           {applicable int arg size char was found ?}
        sst_w.appendn^ (c, 1);         {write the char}
        cast_arg := false;             {no need to type-cast argument later}
        end;
write_int_default:                     {skip to here if writing default int size}

      if unsig
        then begin                     {integer is unsigned}
          sst_w.appendn^ ('u', 1);
          end
        else begin                     {integer is signed}
          sst_w.appendn^ ('i', 1);
          end
        ;
      check_eol;
      end;

sst_dtype_float_k: begin
      sst_w.appendn^ ('printf ("%', 10);
      if opc.write_width_exp_p <> nil then begin {first field width exists ?}
        field_width (opc.write_width_exp_p, fw1_param);
        if opc.write_width2_exp_p <> nil then begin {second field width exists ?}
          sst_w.appendn^ ('.', 1);
          field_width (opc.write_width2_exp_p, fw2_param);
          sst_w.appendn^ ('f', 1);
          goto write_done_float;
          end;
        end;
      sst_w.appendn^ ('g', 1);
write_done_float:
      check_eol;
      end;

sst_dtype_char_k: begin
      preceeding_blanks (1, opc.write_width_exp_p, fw); {handle any blank padding}
      sst_w.appendn^ ('printf ("%c', 11);
      check_eol;
      end;

sst_dtype_array_k: begin
      if not dt_p^.ar_string then begin {this array is not a string of characters ?}
        writeln ('Writing non-character array values not implemented.');
        sys_bomb;
        end;
      preceeding_blanks (              {handle any blank padding}
        dt_p^.ar_ind_n, opc.write_width_exp_p, fw);
      if fw = 0 then begin             {definately nothing more to write ?}
        sst_w_c_sment_end_nclose;      {close statement started for this opcode}
        path_to_here := true;          {executable path exists to after this opcode}
        goto done_opcode;
        end;
      sst_w_c_armode_push (array_whole_k); {array identifiers represent whole array}
      sst_w.appendn^ ('fwrite (', 8);
      sst_w_c_exp (opc.write_exp_p^, 0, nil, enclose_no_k); {write string value}
      sst_w.appendn^ (',', 1);
      sst_w.delimit^;
      string_f_int (token, sst_config.size_char); {size of one character}
      sst_w.append^ (token);           {write size of each element (one character)}
      sst_w.appendn^ (',', 1);
      sst_w.delimit^;
      if fw >= 0
        then begin                     {remaining field width is a known constant}
          string_f_int (token, fw);
          sst_w.append^ (token);
          end
{
*   The remaining field width is not a known constant.  This makes the
*   remaining field width the value of the field width expression clipped to
*   0 and the number of characters in the string.  If EXP is the field width
*   expression, and SLEN is the string size, the expression will have the form:
*
*     EXP > 0 ? EXP < DSIZE ? EXP : DSIZE : 0
*
*   An implicit variable will be created for EXP if it is not "simple".  This
*   may have already been done in routine PRECEEDING_BLANKS.  If so, SYM_P
*   is pointing to the implicit variable, otherwise SYM_P is NIL.
}
        else begin
          if sym_p = nil then begin    {not already made implicit variable ?}
            sst_w_c_exp_implicit (opc.write_width_exp_p^, sym_p);
            end;
          if sym_p = nil
            then sst_w_c_exp (opc.write_width_exp_p^, 0, nil, enclose_yes_k)
            else sst_w.append_sym_name^ (sym_p^);
          sst_w.delimit^;
          sst_w.appendn^ ('>', 1);
          sst_w.delimit^;
          sst_w.appendn^ ('0', 1);
          sst_w.delimit^;
          sst_w.appendn^ ('?', 1);
          sst_w.delimit^;
          if sym_p = nil
            then sst_w_c_exp (opc.write_width_exp_p^, 0, nil, enclose_yes_k)
            else sst_w.append_sym_name^ (sym_p^);
          sst_w.delimit^;
          sst_w.appendn^ ('<', 1);
          sst_w.delimit^;
          string_f_int (token, dt_p^.ar_ind_n); {make data size expression}
          sst_w.append^ (token);
          sst_w.delimit^;
          sst_w.appendn^ ('?', 1);
          sst_w.delimit^;
          if sym_p = nil
            then sst_w_c_exp (opc.write_width_exp_p^, 0, nil, enclose_yes_k)
            else sst_w.append_sym_name^ (sym_p^);
          sst_w.delimit^;
          sst_w.appendn^ (':', 1);
          sst_w.delimit^;
          sst_w.append^ (token);
          sst_w.delimit^;
          sst_w.appendn^ (':', 1);
          sst_w.delimit^;
          sst_w.appendn^ ('0', 1);
          end
        ;                              {done writing number of elements to write}
      sst_w.appendn^ (',', 1);
      sst_w.delimit^;
      sst_w.appendn^ ('stdout)', 7);
      sst_w_c_armode_pop;              {restore previous array name interpret mode}
      sst_w_c_sment_end;
      path_to_here := true;            {executable path exists to after this opcode}
      goto done_opcode;
      end;

sst_dtype_range_k: begin
      unsig := dt_p^.range_ord_first >= 0; {true if subrange is unsigned}
      goto write_int;                  {to common code with INTEGER data type}
      end;

sst_dtype_copy_k: begin
      dt_p := dt_p^.copy_dtype_p;      {resolve to copied data type}
      goto write_new_dtype;
      end;

otherwise
    sys_msg_parm_int (msg_parm[1], ord(dt_p^.dtype));
    syn_error (
      opc.write_exp_p^.str_h, 'sst_c_write', 'dtype_exp_unexpected', msg_parm, 1);
    end;
{
*   The format string has been written up to and including the closing
*   character identifying the data type.  FW1_PARAM and FW2_PARAM are TRUE
*   if the first and second field width specifiers need to be passed as
*   arguments.  DID_EOL is TRUE if the next opcode has already been processed.
}
  sst_w.appendn^ ('",', 2);            {close the format string}
  sst_w.delimit^;
  if fw1_param then begin              {pass field width 1 as parameter ?}
    sst_w_c_exp (opc.write_width_exp_p^, 0, nil, enclose_no_k);
    sst_w.appendn^ (',', 1);
    sst_w.delimit^;
    end;
  if fw2_param then begin              {pass field width 2 as parameter ?}
    sst_w_c_exp (opc.write_width2_exp_p^, 0, nil, enclose_no_k);
    sst_w.appendn^ (',', 1);
    sst_w.delimit^;
    end;
  if cast_arg then begin               {need to type-cast argument ?}
    sst_w.appendn^ ('(', 1);
    if unsig then begin                {unsigned integer ?}
      sst_w.appendn^ ('unsigned', 8);
      sst_w.delimit^;
      end;
    sst_w.appendn^ ('int)', 4);
    sst_w.allow_break^;
    end;
  sst_w_c_exp (opc.write_exp_p^, 0, nil, enclose_no_k);
  sst_w.appendn^ (')', 1);
  sst_w_c_sment_end;

  if did_eol then begin                {already handled next opcode ?}
    opc_p := opc_p^.next_p;            {skip over since we already did this one}
    end;
  path_to_here := true;                {executable path exists to after this opcode}
  end;
{
***************************************
*
*   Write end-of-line to standard output.
}
sst_opc_write_eol_k: begin
  sst_w_c_declare (decl_stdio_k);      {make sure PRINTF is declared}
  sst_w_c_sment_start;                 {start this statement}
  sst_w.appendn^ ('printf ("\n")', 13);
  sst_w_c_sment_end;
  path_to_here := true;                {executable path exists to after this opcode}
  end;
{
***************************************
*
*   Unrecognized or unexpected opcode.
}
otherwise
        sys_msg_parm_int (msg_parm[1], ord(opc.opcode));
        sys_message_bomb ('sst', 'opcode_unexpected', msg_parm, 1);
        end;                           {end of opcode type cases}
      end;                             {done with OPC abbreviation}

done_opcode:                           {jump here if done with current opcode}
    if opc_p <> nil then begin         {advance to next opcode descriptor in chain}
      opc_p := opc_p^.next_p;
      end;
    end;                               {back and process new opcode}
  end;
