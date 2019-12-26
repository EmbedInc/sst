{   Module containing the default SST library routines for manipulating the
*   output character stream.  Pointers to these routines are installed in the
*   SST_W call table by SST_INIT.  All the state that these routines manipulate
*   other than the output lines themselves is in the global record SST_OUT.
*   This is declared in SST.INS.PAS.
}
module sst_out_module;
define sst_out_allow_break;
define sst_out_append;
define sst_out_appendn;
define sst_out_appends;
define sst_out_append_sym_name;
define sst_out_blank_line;
define sst_out_break;
define sst_out_comment_end;
define sst_out_comment_set;
define sst_out_comment_start;
define sst_out_delimit;
define sst_out_indent;
define sst_out_line_close;
define sst_out_line_insert;
define sst_out_line_new;
define sst_out_line_new_cont;
define sst_out_name;
define sst_out_name_sym;
define sst_out_notify_src_range;
define sst_out_tab_indent;
define sst_out_undent;
define sst_out_undent_all;
define sst_out_write;
%include 'sst2.ins.pas';
{
**************************************************************************
*
*   Subroutine SST_OUT_ALLOW_BREAK
*
*   Indicate that a line break is allowed at the current position, if needed
*   later.  The line break will occurr at this position if more characters are
*   appended to the current line and the resulting line length would exceed
*   the WRAP_LEN parameter.
}
procedure sst_out_allow_break;         {allow line break at current position}

begin
  case sst_out.dyn_p^.wpos of          {where is write position rel to curr line ?}
sst_wpos_before_k,                     {before start of current line}
sst_wpos_after_k: begin                {after end of current line}
      return;                          {nothing to break}
      end;
    end;                               {done with special handling write pos cases}

  if sst_out.dyn_p^.str_p^.s.len > 0 then begin {anything here to break ?}
    sst_out.dyn_p^.break_len :=        {save where this line would end}
      sst_out.dyn_p^.str_p^.s.len;
    sst_out.dyn_p^.break_start :=      {save source for first char on new line}
      sst_out.dyn_p^.break_len + 1;
    end;
  end;
{
**************************************************************************
*
*   Subroutine SST_OUT_APPEND (STR_H, STR)
*
*   Append more characters to the end of the current line.  If the new line
*   length would exceed the WRAP_LEN parameter, then the line is broken at
*   the last break set.  It is an error if the line can't be broken and its
*   length would exceed its max length (characters would be lost).
*
*   STR_H is the handle to a range of source characters that these output
*   characters are related to.  STR is the string of output characters to
*   append to the current line.
}
procedure sst_out_append (             {append string to current output line}
  in      str: univ string_var_arg_t); {the string to append}

begin
  sst_w.appendn^ (str.str, str.len);   {append the characters}
  end;
{
**************************************************************************
*
*   Subroutine SST_OUT_APPENDN (CHARS, N_CHARS)
*
*   Append characters at the current write position.  If the new line
*   length would exceed the WRAP_LEN parameter, then the line is broken at
*   the last break set.  It is an error if the line can't be broken and its
*   length would exceed its max length (characters would be lost).
*
*   CHARS is the array of characters to append to the current line.
*   N_CHARS is the number of characters in CHARS.
}
procedure sst_out_appendn (            {append N characters to current output line}
  in      chars: univ string;          {the characters to append}
  in      n_chars: string_index_t);    {the number of characters to append}

var
  olen: string_index_t;                {length of resulting output line}
  wl: sys_int_machine_t;               {WRAP_LEN after taking comment into account}

begin
  with sst_out.dyn_p^: dyn do begin    {DYN is dynamic output writing state block}

    case dyn.wpos of                   {where is write position rel to curr line ?}
sst_wpos_before_k,                     {before start of current line}
sst_wpos_after_k: begin                {after end of current line}
        sst_w.line_insert^;            {create new line to append characters to}
        end;
      end;                             {done with special handling pos cases}

    olen := dyn.str_p^.s.len + n_chars; {length if just did append}
    wl := sst_out.wrap_len;            {init max line length before wrapping}
    if dyn.comm.len > 0 then begin     {this line has an end of line comment ?}
      wl := min(wl, sst_out.comm_pos); {adjust wrap length for comment}
      end;
    if olen > wl then begin            {need to break line before new characters ?}
      sst_w.break^;                    {break curr line onto continuation line}
      olen :=                          {update resulting line len after doing break}
        dyn.str_p^.s.len + n_chars;
      end;                             {done breaking to continuation line}
    if olen > dyn.str_p^.s.max then begin {current line overflow ?}
      sys_message_bomb ('sst', 'out_line_too_long', nil, 0);
      end;
    string_appendn (dyn.str_p^.s, chars, n_chars); {append the chars to line}
    end;                               {done with DYN abbreviation}
  end;
{
**************************************************************************
*
*   Subroutine SST_OUT_APPENDS (S)
*
*   Append the string S to the current output line.  Trailing blanks in S
*   will be ignored.
}
procedure sst_out_appends (            {append string to current output line}
  in      s: string);                  {string to append, trailing blanks ignored}

var
  s2: string_var8192_t;                {scratch string}

begin
  s2.max := sizeof(s2.str);            {init local var string}

  string_vstring (s2, s, sizeof(s));   {convert input string to a var string}
  sst_w.appendn^ (s2.str, s2.len);
  end;
{
**************************************************************************
*
*   Subroutine SST_OUT_APPEND_SYM_NAME (SYM)
*
*   Append the output name of the symbol SYM at the current position.
*   It is an error if the output name does not already exist.
}
procedure sst_out_append_sym_name (    {append symbol output name to curr position}
  in      sym: sst_symbol_t);          {symbol descriptor to write name of}

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  str: string_var4_t;                  {scratch output name when blank}
  sym_p: sst_symbol_p_t;
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

begin
  if sym.name_out_p = nil then begin   {symbol has no output name ?}
    str.max := sizeof(str.str);        {init local var string}
    if sym.name_in_p <> nil
      then begin                       {input name exists}
        sys_msg_parm_vstr (msg_parm[1], sym.name_in_p^);
        end
      else begin                       {no input name exists}
        str.len := 0;
        sys_msg_parm_vstr (msg_parm[1], str);
        end
      ;
    sys_message_parms ('sst', 'out_name_none', msg_parm, 1);

    writeln ('Symbol flags are:');
    if sst_symflag_def_k in sym.flags then writeln ('  def');
    if sst_symflag_used_k in sym.flags then writeln ('  used');
    if sst_symflag_following_k in sym.flags then writeln ('  following');
    if sst_symflag_following_dt_k in sym.flags then writeln ('  following_dt');
    if sst_symflag_followed_k in sym.flags then writeln ('  followed');
    if sst_symflag_writing_k in sym.flags then writeln ('  writing');
    if sst_symflag_writing_dt_k in sym.flags then writeln ('  writing_dt');
    if sst_symflag_written_k in sym.flags then writeln ('  written');
    if sst_symflag_created_k in sym.flags then writeln ('  created');
    if sst_symflag_intrinsic_in_k in sym.flags then writeln ('  intrinsic_in');
    if sst_symflag_intrinsic_out_k in sym.flags then writeln ('  intrinsic_out');
    if sst_symflag_global_k in sym.flags then writeln ('  global');
    if sst_symflag_extern_k in sym.flags then writeln ('  extern');
    if sst_symflag_defnow_k in sym.flags then writeln ('  defnow');
    if sst_symflag_ok_sname_k in sym.flags then writeln ('  ok_sname');
    sym_p := addr(sym);
    sst_w.name_sym^ (sym_p^);          {make output name for symbol}
    end;

  sst_w.append^ (sym.name_out_p^);     {append the symbol's output name}
  end;
{
**************************************************************************
*
*   Subroutine SST_OUT_BREAK
*
*   Cause a break at the current break point of the current output line.
*   A new continuation line will be created, and initialized with
*   the characters on the current line after the break point.  The new line
*   will be initialized to not have a break point.  A call to this routine
*   will have no effect if there is no break point on the current line.
}
procedure sst_out_break;               {break line at current position}

var
  comm_ofs: sys_int_machine_t;         {commented char offset from first broken char}
  comm: string_var80_t;                {comment if moved to new line}
  moved: string_var256_t;              {string moved to next line}

begin
  comm.max := sizeof(comm.str);        {init local var strings}
  moved.max := sizeof(moved.str);

  with sst_out.dyn_p^: dyn do begin    {DYN is dynamic output state for this line}

  case dyn.wpos of                     {where is write position rel to curr line ?}
sst_wpos_before_k,                     {before start of current line}
sst_wpos_after_k: begin                {after end of current line}
      return;                          {nothing to break}
      end;
    end;                               {done with special handling write pos cases}

  if dyn.break_len <= 0 then return;   {no break point exists ?}
{
*   The line will definately be broken.
}
  comm.len := 0;                       {init to no comment moved from old line}
  comm_ofs :=                          {offset of commented char into moved chars}
    dyn.commented_pos - dyn.break_start;
  if                                   {need to move comment to new line ?}
      (dyn.comm.len > 0) and           {comment exists on old line ?}
      (comm_ofs >= 0)                  {comment belongs to char that will be moved ?}
      then begin
    string_copy (dyn.comm, comm);      {save comment string}
    dyn.comm.len := 0;                 {pretend old line has no comment}
    end;

  moved.len := 0;                      {init moved characters to empty}
  string_appendn (                     {save characters to move to next line}
    moved,                             {string to save characters in}
    dyn.str_p^.s.str[dyn.break_start], {start of string to move}
    dyn.str_p^.s.len - dyn.break_start + 1); {number of characters to save}
  dyn.str_p^.s.len := dyn.break_len;   {truncate line to before break}
  dyn.break_len := 0;                  {reset to no break on this line}
  dyn.break_start := 0;
{
*   The current line has been truncated to before the break.  The characters
*   to wrap to the next line are in MOVED.
}
  sst_w.line_new_cont^;                {create and init continuation line}

  comm_ofs := dyn.str_p^.s.len + comm_ofs; {make char pos of commented char}
  sst_w.append^ (moved);               {copy moved characters to new line}

  if comm.len > 0 then begin           {need to restore comment to new line ?}
    string_copy (comm, dyn.comm);      {set comment text}
    dyn.commented_pos := comm_ofs;     {set index of commented character}
    end;
  end;                                 {done with DYN abbreviation}
  end;
{
**************************************************************************
*
*   Subroutine SST_OUT_BLANK_LINE
*
*   Make sure the current position is immediately following a blank line.
*   A new blank line will be created only if one is not already there.
*   Although this will create a blank line at the start of the file, the
*   SST_OUT_WRITE routine will ignore such blank lines.  A blank line needs
*   to be put there, since other text may be inserted before it later.
*
*   This routine is useful when you want to leave a blank line, but aren't
*   sure if there may already be one there.
}
procedure sst_out_blank_line;          {make sure preceeding line is blank}

label
  wpos_end;

begin
  with sst_out.dyn_p^: dyn do begin    {DYN is dynamic part of current output state}
    case dyn.wpos of
{
*   The writing position is before the start of the current line.
}
sst_wpos_before_k: begin
  if
      (dyn.str_p <> nil) and then      {a current line exists ?}
      (dyn.str_p^.prev_p <> nil) and then {a previous line exists ?}
      (dyn.str_p^.prev_p^.s.len <= 0)  {previous line is blank ?}
    then return;                       {nothing to do, blank line already there}
  sst_w.line_insert^;                  {insert blank line before curr position}
  dyn.wpos := sst_wpos_after_k;        {position to after the blank line}
  end;
{
*   The writing position is at the end of the current line.
}
sst_wpos_end_k: begin
wpos_end:                              {common code for write pos end/after cases}
  if dyn.str_p^.next_p <> nil then begin {"next" line really exists ?}
    if dyn.str_p^.next_p^.s.len <= 0 then begin {next line is blank ?}
      dyn.str_p := dyn.str_p^.next_p;  {position to after end of the blank line}
      dyn.break_start := 0;
      dyn.break_len := 0;
      dyn.wpos := sst_wpos_after_k;
      return;
      end;
    end;
  end;
{
*   The writing position is after the end of the current line.
}
sst_wpos_after_k: begin
  if dyn.str_p^.s.len <= 0 then return; {already positioned after a blank line ?}
  goto wpos_end;                       {to common code with write position at end}
  end;

      end;                             {end of writing position cases}
{
*   The current position is not immediately following a blank line.  Create
*   a blank line and put the current write position after the end of it.
}
    sst_w.line_insert^;                {create new blank line}
    dyn.wpos := sst_wpos_after_k;      {position to after the blank line}
    end;                               {end of DYN abbreviation}
  end;
{
**************************************************************************
*
*   Subroutine SST_OUT_COMMENT_END
*
*   Write whatever characters are required to close an end of line comment.
*   The back end may replace this routine if complicated logic is required.
*   This version just writes the comment end string found in the
*   static write position block.
}
procedure sst_out_comment_end;         {write end of comment string}

begin
  sst_w.append^ (sst_out.comm_end);
  end;
{
**************************************************************************
*
*   Subroutine SST_OUT_COMMENT_SET (S)
*
*   Set the body of the end of line comment for this line.  The comment
*   will be associated with the next character written on this line.
}
procedure sst_out_comment_set (        {set comment to correspond to next char}
  in      s: univ string_var_arg_t);   {body of comment string}

var
  wl_save: sys_int_machine_t;          {saved copy of WRAP_LEN}

begin
  if s.len <= 0 then return;           {no comment to set ?}
  with sst_out.dyn_p^: dyn do begin    {DYN is dynamic output writing state block}

  wl_save := sst_out.wrap_len;         {save existing WRAP_LEN value}
  sst_out.wrap_len := sst_out.comm_pos; {set to where we would like comment to start}
  sst_w.appendn^ ('', 0);              {do break, if needed to obey WRAP_LEN}
  sst_out.wrap_len := wl_save;         {restore old WRAP_LEN value}
{
*   The current line has been broken, if possible, to not stick out past
*   where we would like to start the comment.  The writing position is
*   definately set to END.
}
  if dyn.comm.len > 0 then begin       {there is already a comment for this line ?}
    sst_w.line_new_cont^;              {create new continuation line for comment}
    end;

  string_copy (s, dyn.comm);           {save comment body for this line}
  dyn.commented_pos := dyn.str_p^.s.len + 1; {save index of commented character}

  end;                                 {done with DYN abbreviation}
  end;
{
**************************************************************************
*
*   Subroutine SST_OUT_COMMENT_START
*
*   Write whatever characters are required to start an end of line comment.
*   The back end may replace this routine if complicated logic is required.
*   This version just writes the comment start string found in the
*   static write position block.
}
procedure sst_out_comment_start;       {write start of comment string}

begin
  sst_w.append^ (sst_out.comm_start);
  end;
{
**************************************************************************
*
*   Subroutine SST_OUT_DELIMIT
*
*   Indicate that either a delimiter is to be written to the current position,
*   or a line break should occur here.  Multiple consecutive calls to this
*   routine will have no effect.
}
procedure sst_out_delimit;             {write delim or break line before next output}

const
  delim_string = ' ';                  {delimiter string}

var
  wl: sys_int_machine_t;               {WRAP_LEN after taking comment into account}

begin
  with sst_out.dyn_p^: dyn do begin    {DYN is dynamic part of current output state}
    case dyn.wpos of                   {where is write position rel to curr line ?}
sst_wpos_before_k,                     {before start of current line}
sst_wpos_after_k: begin                {after end of current line}
        return;                        {already at a line break}
        end;
      end;                             {done with special handling write pos cases}

    if                                 {delimiter/break already flagged ?}
        (dyn.break_len > 0) and        {break exists on this line ?}
        (dyn.break_start - dyn.break_len > 1) and {break is a delimiter ?}
        (dyn.break_start > dyn.str_p^.s.len) {break is at end of line ?}
      then return;
    sst_w.allow_break^;                {allow break at current end of line}
    wl := sst_out.wrap_len;            {init max line length before wrapping}
    if dyn.comm.len > 0 then begin     {this line has an end of line comment ?}
      wl := min(wl, sst_out.comm_pos); {adjust wrap length for comment}
      end;
    if dyn.str_p^.s.len < wl then begin {won't automatically break ?}
      string_appendn (dyn.str_p^.s, delim_string, sizeof(delim_string)); {add delim}
      dyn.break_start := dyn.str_p^.s.len + 1; {break will skip over delimiter}
      end;
    end;                               {done with DYN abbreviation}
  end;
{
**************************************************************************
*
*   Subroutine SST_OUT_INDENT
*
*   Increase the current indentation level by one.  The number of spaces per
*   indentation level is SST_OUT.INDENT_SIZE.
}
procedure sst_out_indent;              {increase indentation by one level}

begin
  sst_out.dyn_p^.indent_level := sst_out.dyn_p^.indent_level + 1; {set new indentation level}
  if sst_out.dyn_p^.indent_level > 0 then begin
    sst_out.dyn_p^.indent_chars :=     {indent more chars for this level}
      sst_out.dyn_p^.indent_chars + sst_out.indent_size;
    end;
  end;
{
**************************************************************************
*
*   Subroutine SST_OUT_LINE_CLOSE
*
*   Close out the current line.  Future append operations will go onto a
*   new line.
}
procedure sst_out_line_close;          {close current line}

begin
  with sst_out.dyn_p^: dyn do begin    {DYN is dynamic part of current output state}
    case dyn.wpos of
sst_wpos_before_k,                     {position is before or after current line}
sst_wpos_after_k: begin
        sst_w.line_insert^;
        end;
      end;                             {end of special handling write pos cases}
    dyn.wpos := sst_wpos_after_k;      {write position is after end of this line}
    end;                               {done with DYN abbreviation}
  end;
{
**************************************************************************
*
*   Subroutine SST_OUT_LINE_INSERT
*
*   Insert a new line at the current position.  The new line will be made
*   current, and the writing position will be set at the end of the new line.
}
procedure sst_out_line_insert;         {insert line after curr, new becomes curr}

var
  ent_p: string_chain_ent_p_t;         {points to chain entry for new line}

begin
  with sst_out.dyn_p^: dyn do begin    {DYN is dynamic part of current output state}
    util_mem_grab (sizeof(ent_p^), sst_out.mem_p^, false, ent_p); {alloc mem for new line}
    ent_p^.s.max := sizeof(ent_p^.s.str); {init new var string}
    ent_p^.s.len := 0;
    case dyn.wpos of
{
*   The current position is before the start of the current line.
*   Link the new line to before the current line.
}
sst_wpos_before_k: begin
  if dyn.str_p = nil
    then begin                         {new line is first one in file}
      ent_p^.prev_p := nil;
      ent_p^.next_p := nil;
      sst_out.first_str_p := ent_p;    {save pointer to first line in file}
      end
    else begin                         {there is an existing current line}
      ent_p^.prev_p := dyn.str_p^.prev_p;
      ent_p^.next_p := dyn.str_p;
      if ent_p^.prev_p = nil
        then begin                     {this line was inserted at start of list}
          sst_out.first_str_p := ent_p; {update pointer to first line in file}
          end
        else begin                     {there is an old line before the new line}
          ent_p^.prev_p^.next_p := ent_p;
          end
        ;
      dyn.str_p^.prev_p := ent_p;
      end
    ;
  end;
{
*   The current position is at or after the end of the current line.
*   Link the new line to after the current line.  Any pending comment
*   must be taken care of before moving to the new line.
}
sst_wpos_end_k,
sst_wpos_after_k: begin
  if dyn.comm.len > 0 then begin       {this line is tagged with a comment ?}
    dyn.break_len := 0;                {no more breaks allowed this line}
    dyn.break_start := 0;
    dyn.wpos := sst_wpos_end_k;        {allow appends to this line}
    string_append1 (dyn.str_p^.s, ' ');
    while (dyn.str_p^.s.len < (sst_out.comm_pos - 1)) do begin
      string_append1 (dyn.str_p^.s, ' '); {tab to comment start}
      end;
    sst_w.comment_start^;              {write start of comment syntax}
    sst_w.append^ (dyn.comm);          {write body of comment}
    sst_w.comment_end^;                {write end of comment syntax}
    end;

  ent_p^.next_p := dyn.str_p^.next_p;
  ent_p^.prev_p := dyn.str_p;
  if ent_p^.next_p <> nil then begin   {there is a line after new line ?}
    ent_p^.next_p^.prev_p := ent_p;
    end;
  dyn.str_p^.next_p := ent_p;
  end;
      end;                             {end of writing position cases}
{
*   The new line has been created and linked into the chain.
}
    dyn.str_p := ent_p;                {make the new line the current line}
    dyn.break_len := 0;                {init write position to end of new line}
    dyn.break_start := 0;
    dyn.wpos := sst_wpos_end_k;
    dyn.comm.max := sizeof(dyn.comm.str);
    dyn.comm.len := 0;
    dyn.commented_pos := 0;
    end;                               {done with DYN abbreviation}
  end;
{
**************************************************************************
*
*   Subroutine SST_OUT_LINE_NEW
*
*   Create a new line after the current line.  The new line will be initialized
*   as a non-continuation source code line.
}
procedure sst_out_line_new;            {set up so next char goes on next line}

begin
  sst_w.line_insert^;
  end;
{
**************************************************************************
*
*   Subroutine SST_OUT_LINE_NEW_CONT
*
*   Create a new line after the current line and initialize it ready to
*   put more characters on it from the statement already started on previous
*   lines.
}
procedure sst_out_line_new_cont;       {set up for next line is continuation line}

begin
  sst_w.line_insert^;                  {create raw new line}
  sst_w.tab_indent^;                   {tab to after indentation}
  end;
{
**************************************************************************
*
*   Subroutine SST_OUT_NAME (
*     NAME_IN, NAME_IN_LEN, EXT, EXT_LEN, RENAME, NAME_OUT, POS)
*
*   Convert the input source symbol name in NAME_IN,NAME_IN_LEN to the output
*   source symbol name in NAME_OUT.  EXT,EXT_LEN specifies the suffix, if any,
*   the output name must have.  RENAME indicates whether the name may be
*   changed to make it unique.  It may have one of the following values:
*
*     SST_RENAME_NONE_K
*
*       The name will not be changed from the one derived with the basic
*       rename rules.  It is an error if the name would have been renamed
*       under the rules for SST_RENAME_SCOPE_K, below.  This rename rule
*       is useful for "renaming" global symbols, since we aren't free to
*       change the name.
*
*     SST_RENAME_NCHECK_K
*
*       This renaming rule is the same as SST_RENAME_SCOPE_K, below, except
*       that no uniqueness check is done in the current scope.  Special
*       symbol names and reserved names are still checked.  POS will not
*       be valid.
*
*     SST_RENAME_SCOPE_K
*
*       The name will be changed, if necessary, to make it unique in the current
*       scope.  It will also be renamed to avoid matching the name of some
*       symbol types in the line of scopes from the current to the most global.
*       These special symbol types are data types, and intrinsic symbols to
*       the back end, and symbol names explicitly flagged as reserved in the
*       config file.
*
*     SST_RENAME_ALL_K
*
*       The name will be changed, if necessary, to make it unique in all the scopes
*       from the current in line to the most global.
*
*   POS is the returned position handle of where the name would go in the
*   symbol table for the current scope.  POS is not valid if RENAME is set to
*   SST_RENAME_NCHECK_K.
}
procedure sst_out_name (               {make output symbol name from input name}
  in      name_in: univ string;        {input symbol name characters}
  in      name_in_len: string_index_t; {number of characters in NAME_IN}
  in      ext: univ string;            {output name suffix, if any}
  in      ext_len: string_index_t;     {number of characters in EXT}
  in      rename: sst_rename_k_t;      {what kind of re-naming is allowed}
  in out  name_out: univ string_var_arg_t; {resulting output source symbol name}
  out     pos: string_hash_pos_t);     {pos handle where name goes in symbol table}

const
  max_msg_parms = 3;                   {max parameters we can pass to a message}

var
  seq: sys_int_machine_t;              {current sequence number for name}
  name_start: string_var80_t;          {name before sequence number}
  name_seq: string_var32_t;            {sequence number string}
  name_end: string_var80_t;            {name after sequence number}
  token: string_var80_t;               {scratch string for number conversion}
  chopt: sys_int_machine_t;            {total characters need to chop from name}
  choph: sys_int_machine_t;            {number chars need to chop here}
  i, j: sys_int_machine_t;             {loop counters and scratch integers}
  pos_local: string_hash_pos_t;        {scratch position handle}
  pos_p: ^string_hash_pos_t;           {pointer to position handle for synonym}
  scope_p: sst_scope_p_t;              {points to current scope trying name in}
  sym_pp: sst_symbol_pp_t;             {points to hash table user data area}
  sym_p: sst_symbol_p_t;               {points to symbol descriptor of hash entry}
  found: boolean;                      {TRUE if found name in hash table}
  c: char;                             {scratch character}
  str_h: syn_string_t;                 {handle to source chars for error message}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

label
  suffix_nfnd, lead_ok, next_name;

begin
  name_start.max := sizeof(name_start.str); {init local var strings}
  name_seq.max := sizeof(name_seq.str);
  name_end.max := sizeof(name_end.str);
  token.max := sizeof(token.str);

  string_vstring (name_start, name_in, name_in_len); {make raw var string input name}
  string_vstring (name_end, ext, ext_len); {make raw name suffix}
  if rename <> sst_rename_none_k then begin {allowed to change name at all ?}
    case sst_config.charcase of        {what is the output character case rule ?}
syn_charcase_down_k: begin
        string_downcase (name_start);
        string_downcase (name_end);
        end;
syn_charcase_up_k: begin
        string_upcase (name_start);
        string_upcase (name_end);
        end;
      end;                             {end of charcase cases}
    end;                               {done adjusting name's character case}

  if                                   {need to get rid of dollar signs ?}
      (sst_config.os = sys_os_aix_k) and {IBM AIX operating system ?}
      (sst_config.lang = sst_lang_c_k) {C output language ?}
      then begin
    for i := 1 to name_start.len do begin {once for each character in name}
      if name_start.str[i] = '$'
        then name_start.str[i] := '_';
      end;
    end;                               {done with IBM AIX C special case}

  if name_start.len > name_end.len then begin {NAME_START could have suffix ?}
    j := name_start.len;
    for i := name_end.len downto 1 do begin {scan backwards thru suffix}
      if name_start.str[j] <> name_end.str[i] {end of NAME_START not match suffix ?}
        then goto suffix_nfnd;
      j := j - 1;                      {advance NAME_START char index}
      end;                             {back here to check next suffix character}
    name_start.len :=                  {remove suffix from NAME_START}
      name_start.len - name_end.len;
    end;
suffix_nfnd:                           {jump here if NAME_START not ended in suffix}
{
*   NAME_START definately does not end in the suffix now.  It has been removed
*   if it was there before.  The suffix is now in NAME_END.
*
*   Now keep trying new names until one of them is found to be unique in the
*   line of scopes from the current to the root scope.  The first name will
*   be the name end and start stuck together.  After that, a sequence number
*   will be inserted between the name start and end.  If the name is too long,
*   then the start name will be shortened from the last character backwards,
*   until it is only one character in length.  After that, the suffix will
*   be shortened from its first character forwards.  If the remaining name
*   is still too long, then it is an error.  If the start name ends in an
*   underscore, then the sequence number will always be inserted.
*
*   This behavior may be modified by the RENAME flag.
*
*   Now check that leading name character is legal.
}
  if name_start.len < 1 then goto lead_ok; {prefix is empty ?}
  c := name_start.str[1];              {get name start character}
  if (c >= 'A') and (c <= 'Z') then goto lead_ok; {upper case letter}
  if (c >= 'a') and (c <= 'z') then goto lead_ok; {lower case letter}
  if c = '_' then goto lead_ok;
  if c = '$' then goto lead_ok;
{
*   The leading name character is not legal.
}
  if rename = sst_rename_none_k then begin {not allowed to change name ?}
    string_append (name_start, name_end); {make composite name for error message}
    sys_message_bomb ('sst', 'out_name_illegal', msg_parm, 1);
    end;

  string_copy (name_start, token);     {make temp copy of name start}
  case sst_config.charcase of          {what is the output character case rule ?}
syn_charcase_up_k: begin
      string_vstring (name_start, 'N', 1);
      end;
otherwise
    string_vstring (name_start, 'n', 1);
    end;
  string_append (name_start, token);   {make new name start with character inserted}
lead_ok:                               {done messing with leading character}

  if                                   {name indicates to always have seq number ?}
      (rename <> sst_rename_none_k) and
      (name_start.len >= 1) and then
      (name_start.str[name_start.len] = '_')
    then begin                         {there will always be a sequence number}
      seq := 0;                        {one less than first sequence number to try}
      name_start.len := name_start.len - 1; {truncate "_", will be added with seqnum}
      end
    else begin                         {first try will be without sequence number}
      seq := -1;
      end
    ;
  if name_start.len <= 0 then begin    {start of name is empty string ?}
    string_appendn (name_start, 'xxx', 3); {pick arbitrary start of name}
    end;

next_name:                             {back here to try name with new seq number}
  seq := seq + 1;                      {make sequence number for this try}
  name_seq.len := 0;                   {init inserted sequence number string length}
  if seq > 0 then begin                {need to make full sequence number string ?}
    if rename = sst_rename_none_k then begin {not allowed to make new name ?}
      string_hash_ent_atpos (          {get info about synonym entry}
        pos_p^,                        {position handle to hash table entry}
        sym_p,                         {unused pointer to hash entry name}
        sym_pp);                       {returned pointer to hash entry data area}
      sym_p := sym_pp^;                {get pointer to synonim symbol descriptor}
      str_h.first_char := sym_p^.char_h; {get handle to start of other symbol}
      str_h.last_char := str_h.first_char;
      sys_msg_parm_vstr (msg_parm[1], name_out);
      syn_error (str_h, 'sst', 'out_name_used', msg_parm, 1);
      end;
    string_append1 (name_seq, '_');
    string_f_int (token, seq);
    string_append (name_seq, token);
    end;
  chopt := max(0,                      {number of chars to shorten name by}
    name_start.len + name_seq.len + name_end.len - sst_config.sym_len_max);
{
*   If name is too long, try to chop characters from NAME_START.
}
  if chopt > 0 then begin              {need to shorten total name ?}
    choph := min(chopt, name_start.len - 1); {number of chars we can chop here}
    name_start.len :=                  {chop characters from name before seq number}
      name_start.len - choph;
    chopt := chopt - choph;            {less characters left to chop off}
    end;
{
*   If name is still too long, try to chop characters from NAME_END.
}
  if chopt > 0 then begin              {still need to shorten total name ?}
    choph := min(chopt, name_end.len); {number of chars we can chop here}
    name_end.len :=                    {set new length of NAME_END}
      name_end.len - choph;
    j := choph + 1;                    {init index of first source char}
    for i := 1 to name_end.len do begin {once for each char to move}
      name_end.str[i] := name_end.str[j]; {move this character}
      end;                             {back to move next char in NAME_END}
    chopt := chopt - choph;            {less characters left to chop off}
    end;

  string_copy (name_start, name_out);  {build full name string for this try}
  string_append (name_out, name_seq);
  string_append (name_out, name_end);
{
*   If name is still too long, then there is nothing more we can do to fix
*   it.  This is now an error.
}
  if chopt > 0 then begin              {name still too long ?}
    string_vstring (name_start, name_in, name_in_len); {make raw var string input name}
    string_vstring (name_end, ext, ext_len); {make raw name suffix}
    sys_msg_parm_vstr (msg_parm[1], name_start);
    sys_msg_parm_vstr (msg_parm[2], name_end);
    sys_msg_parm_vstr (msg_parm[3], name_out);
    sys_message_bomb ('sst', 'out_name_too_long', msg_parm, 3);
    end;
{
*   The current output name to try is in NAME_OUT.
}
  scope_p := sst_scope_p;              {init current scope to most local}
  while scope_p <> nil do begin        {loop thru the scopes from local to global}
    if scope_p = sst_scope_p
      then begin                       {we are looking for name in most local scope}
        if rename = sst_rename_ncheck_k
          then begin                   {no supposed to check current scope}
            found := false;
            end
          else begin                   {OK to check uniqueness in current scope}
            string_hash_pos_lookup (   {look up name in output symbol table}
              scope_p^.hash_out_h,     {hash table handle}
              name_out,                {symbol name}
              pos,                     {returned position handle, back to caller}
              found);                  {returned TRUE if name existed here}
            pos_p := addr(pos);        {set pointer to position handle used}
            end
          ;
        end
      else begin                       {not looking in table where symbol goes later}
        string_hash_pos_lookup (       {look up name in output symbol table}
          scope_p^.hash_out_h,         {hash table handle}
          name_out,                    {symbol name}
          pos_local,                   {returned hash table position handle}
          found);                      {returned TRUE if name existed here}
        pos_p := addr(pos_local);      {set pointer to position handle used}
        end
      ;
    if found then begin                {current name found in this scope ?}
      case rename of
sst_rename_none_k,
sst_rename_ncheck_k,
sst_rename_scope_k: begin
          if scope_p = sst_scope_p then begin {we just looked in current scope ?}
            goto next_name;            {must be completely unique in this scope}
            end;
          string_hash_ent_atpos (      {get data about this hash table entry}
            pos_local,                 {position handle to hash table entry}
            sym_p,                     {unused pointer to hash entry name}
            sym_pp);                   {returned pointer to hash entry data area}
          sym_p := sym_pp^;            {get pointer to descriptor for this symbol}
          if sym_p = nil               {this symbol is explicitly reserved name ?}
            then goto next_name;       {can't allow same name as reserved name}
          case sym_p^.symtype of       {what kind of symbol is this ?}
sst_symtype_dtype_k,                   {symbol is a data type}
sst_symtype_back_k: begin              {symbol is intrinsic to output language}
              goto next_name;          {can't allow conflict with these symbols}
              end;
            end;                       {OK to have same name as remaining sym types}
          end;
sst_rename_all_k: begin                {must be completely unique over all scopes}
          goto next_name;              {this name no good, try another}
          end;
otherwise
        sys_msg_parm_int (msg_parm[1], ord(rename));
        sys_message_bomb ('sst', 'rename_unknown', msg_parm, 1);
        end;                           {end of RENAME method cases}
      end;                             {done handling name was found in this scope}
    scope_p := scope_p^.parent_p;      {advance to next most global symbol table}
    end;                               {back and check for name in new scope}
  end;
{
**************************************************************************
*
*   Subroutine SST_OUT_NAME_SYM (SYM)
*
*   Set the output source name for the symbol SYM.
*   This subroutine has no effect if the output symbol name already exists.
}
procedure sst_out_name_sym (           {set output name for an existing symbol}
  in out  sym: sst_symbol_t);          {NOP if symbol already has output name}

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  suffix: string_var80_t;              {symbol name suffix, if any}
  pos: string_hash_pos_t;              {handle to output name symbol table position}
  sym_pp: sst_symbol_pp_t;             {pointer to hash entry user data area}
  dt_p: sst_dtype_p_t;                 {points to root data type descriptor}
  name_out: string_var132_t;           {final output symbol name}
  rename: sst_rename_k_t;              {symbol re-naming strategy flag}
  scope_old_p: sst_scope_p_t;          {saved copy of current scope pointer}
  names_old_p: sst_scope_p_t;          {saved copy of current namespace pointer}
  name_in: string_var32_t;             {used when no input name given}
  no_in_name: boolean;                 {TRUE if no input name existed}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

label
  name_dtype;

begin
  if sym.name_out_p <> nil then return; {output name already set ?}

  suffix.max := sizeof(suffix.str);    {init local var strings}
  name_out.max := sizeof(name_out.str);
  name_in.max := sizeof(name_in.str);

  no_in_name := false;                 {init to explicit input name was given}
  if sym.name_in_p = nil then begin    {no explicit input name exists}
    name_in.len := 0;
    case sym.symtype of
sst_symtype_const_k: string_appendn (name_in, 'const', 5);
sst_symtype_enum_k: string_appendn (name_in, 'enum', 4);
sst_symtype_dtype_k: begin             {symbol is name of a data type}
        dt_p := sym.dtype_dtype_p;     {resolve base data type}
name_dtype:                            {jump here to base name on data type}
        while dt_p^.dtype = sst_dtype_copy_k do dt_p := dt_p^.copy_dtype_p;
        case dt_p^.dtype of            {what data type is this ?}
sst_dtype_int_k: string_appendn (name_in, 'int', 3);
sst_dtype_enum_k: string_appendn (name_in, 'enum', 4);
sst_dtype_float_k: string_appendn (name_in, 'float', 5);
sst_dtype_bool_k: string_appendn (name_in, 'bool', 4);
sst_dtype_char_k: string_appendn (name_in, 'char', 4);
sst_dtype_rec_k: string_appendn (name_in, 'rec', 3);
sst_dtype_array_k: begin
            if dt_p^.ar_string
              then begin
                string_appendn (name_in, 'string', 6);
                string_f_int (suffix, dt_p^.ar_ind_n); {make string length number}
                string_append (name_in, suffix);
                end
              else begin
                string_appendn (name_in, 'array', 5);
                end
              ;
            end;
sst_dtype_set_k: string_appendn (name_in, 'set', 3);
sst_dtype_range_k: string_appendn (name_in, 'range', 5);
sst_dtype_proc_k: string_appendn (name_in, 'proc', 4);
sst_dtype_pnt_k: string_appendn (name_in, 'pnt', 3);
otherwise                              {none of the above data types}
          string_appendn (name_in, 'unk', 3);
          end;                         {end of data type cases}
        end;                           {end of symbol is data type case}
sst_symtype_field_k: string_appendn (name_in, 'field', 5);
sst_symtype_var_k: begin               {symbol is a variable}
        dt_p := sym.var_dtype_p;
        goto name_dtype;               {base name on variable's data type}
        end;
sst_symtype_abbrev_k: string_appendn (name_in, 'abbrev', 6);
sst_symtype_proc_k: string_appendn (name_in, 'proc', 4);
sst_symtype_prog_k: string_appendn (name_in, 'prog', 4);
sst_symtype_com_k: string_appendn (name_in, 'com', 3);
sst_symtype_module_k: string_appendn (name_in, 'module', 6);
sst_symtype_label_k: string_appendn (name_in, 'label', 5);
sst_symtype_front_k: string_appendn (name_in, 'front', 5);
sst_symtype_back_k: string_appendn (name_in, 'back', 4);
otherwise
      sys_msg_parm_int (msg_parm[1], ord(sym.symtype));
      sys_message_bomb ('sst', 'symbol_type_unknown', msg_parm, 1);
      end;
    sym.name_in_p := univ_ptr(addr(name_in));
    no_in_name := true;                {flag that we created input name}
    end;

  rename := sst_rename_all_k;          {init to want unique name in all scopes}
  case sym.symtype of                  {what kind of symbol is this ?}
sst_symtype_field_k: begin             {symbol is field name of a record}
      rename := sst_rename_scope_k;    {make unique only within its record}
      end;
sst_symtype_var_k: begin               {symbol is a variable}
      if  (sym.var_proc_p <> nil) and  {symbol is function return value ?}
          (sym.var_arg_p = nil)
          then begin
        if sym.var_proc_p^.sym_p^.name_out_p = nil then begin {rout not yet named ?}
          sst_w.name_sym^ (sym.var_proc_p^.sym_p^); {make output name for function}
          end;
        if sst_config.lang <> sst_lang_c_k then begin {not C language output ?}
          sym.name_out_p := sym.var_proc_p^.sym_p^.name_out_p; {use func's output name}
          return;
          end;
        end;                           {done handling function return value var}
      end;
    end;                               {end of symbol types with special handling}

  if sst_symflag_global_k in sym.flags then begin {symbol is globally known ?}
    rename := sst_rename_none_k;       {can't rename global symbols}
    end;

  if no_in_name then begin             {symbol output name was created here ?}
    rename := sst_rename_all_k;        {make these names unique everywhere}
    end;

  suffix.len := 0;                     {init to no special suffix required}
  if                                   {use special unique name in output sym name ?}
      (rename = sst_rename_all_k) and  {supposed to be unique over all scopes ?}
      (sst_symflag_created_k in sym.flags) and {symbol created by back end ?}
      (sst_oname_unique.len > 0)       {unique name suffix requested ?}
      then begin
    string_append1 (suffix, '_');
    string_append (suffix, sst_oname_unique);
    end;
  case sym.symtype of                  {check symbol types that require suffixes}
sst_symtype_const_k,
sst_symtype_enum_k: string_append (suffix, sst_config.suffix_const);
sst_symtype_dtype_k: string_append (suffix, sst_config.suffix_dtype);
    end;                               {end of special symbol type cases}

  scope_old_p := sst_scope_p;          {save current scope and namespace pointers}
  names_old_p := sst_names_p;
  sst_scope_p := sym.scope_p;          {save scope to that of symbol}
  sst_names_p := sst_scope_p;
  sst_w.name^ (                        {make final output symbol name}
    sym.name_in_p^.str, sym.name_in_p^.len, {input name and length}
    suffix.str, suffix.len,            {suffix string and length}
    rename,                            {TRUE if allowed to re-name symbol}
    name_out,                          {returned final output symbol name}
    pos);                              {output symbol table position handle}
  sst_scope_p := scope_old_p;          {restore current scope and namespace pointers}
  sst_names_p := names_old_p;
  if no_in_name then begin             {did we create input name here ?}
    sym.name_in_p := nil;              {restore symbol descriptor to the way it was}
    end;

  string_hash_ent_add (                {add name to output symbol table}
    pos, sym.name_out_p, sym_pp);
  sym_pp^ := addr(sym);                {point hash table entry to symbol descriptor}
  end;
{
**************************************************************************
*
*   Subroutine SST_OUT_NOTIFY_SRC_RANGE (STR_H)
*
*   Indicate the input source code character range that subsequent output
*   characters will be related to.  This currently does nothing, but is
*   intended to eventually handle bringing comments from the input to
*   the output files.
}
procedure sst_out_notify_src_range (   {declare source chars range for new output}
  in      str_h: syn_string_t);        {new out chars related to these in chars}

begin
  sst_out.dyn_p^.str_h := str_h;
  end;
{
**************************************************************************
*
*   Subroutine SST_OUT_TAB_INDENT
*
*   Tab the character position in the current line to after the indentation
*   characters.  The number of indentation characters is given by
*   SST_OUT.INDENT_CHARS.  This value is usually controlled thru subroutines
*   SST_W.INDENT^ and SST_W.UNDENT^.
}
procedure sst_out_tab_indent;          {tab to current indentation level}

const
  indent_char = ' ';                   {character used for indentation}

var
  tab_pos: sys_int_machine_t;          {final string length after tab}
  i: sys_int_machine_t;                {string write index}

begin
  with sst_out.dyn_p^: dyn do begin    {DYN is dynamic part of current output state}
    case dyn.wpos of
sst_wpos_before_k,                     {position is before or after current line}
sst_wpos_after_k: begin
        sst_w.line_insert^;
        end;
      end;                             {end of special handling write pos cases}
    tab_pos := max(dyn.str_p^.s.len, min(dyn.str_p^.s.max, {final string len}
      dyn.indent_chars));
    for i := dyn.str_p^.s.len+1 to tab_pos do begin {once for each char to add}
      dyn.str_p^.s.str[i] := indent_char;
      end;
    dyn.str_p^.s.len := tab_pos;       {set new string length}
    end;                               {done with DYN abbreviation}
  end;
{
**************************************************************************
*
*   Subroutine SST_OUT_UNDENT
*
*   Decrease the current indentation level by one.  The number of spaces per
*   indentation level is SST_OUT.INDENT_SIZE.
}
procedure sst_out_undent;              {decrease indentation by one level}

begin
  with sst_out.dyn_p^: dyn do begin    {DYN is dynamic part of current output state}
    if dyn.indent_level > 0 then begin
      dyn.indent_chars :=              {indent more chars for this level}
        dyn.indent_chars - sst_out.indent_size;
      end;
    dyn.indent_level := dyn.indent_level - 1; {set new indentation level}
    end;                               {done with DYN abbreviation}
  end;
{
**************************************************************************
*
*   Subroutine SST_OUT_UNDENT_ALL
*
*   Reset the indentation level to none, and the number of current indentation
*   characters to none.  This call destroys any previous indentation state.
}
procedure sst_out_undent_all;          {reset to no indentation level}

begin
  sst_out.dyn_p^.indent_level := 0;
  sst_out.dyn_p^.indent_chars := 0;
  end;
{
**************************************************************************
*
*   Subroutine SST_OUT_WRITE (CONN, STAT)
*
*   Write all the output lines to a file.  CONN is the connection handle of
*   the stream to write the lines to.
}
procedure sst_out_write (              {write output lines from memory to file}
  in out  conn: file_conn_t;           {connection handle to output file}
  out     stat: sys_err_t);            {completion status code}

var
  str_p: string_chain_ent_p_t;         {points to current output line}
  beginning: boolean;                  {TRUE if not written any line to file yet}

label
  next_line;

begin
  str_p := sst_out.first_str_p;        {init current line to first output line}
  beginning := true;                   {init to not written anything to file yet}

  while str_p <> nil do begin          {once for each line in linked list}
    string_unpad (str_p^.s);           {delete trailing spaces from this line}
    if beginning and (str_p^.s.len <= 0) {blank line at beginning of file ?}
      then goto next_line;             {ignore blank lines at beginning}
    file_write_text (str_p^.s, conn, stat); {write this line to output file}
    if sys_error(stat) then return;
    beginning := false;                {no longer at beginning of file}
next_line:                             {jump here to advance to line in chain}
    str_p := str_p^.next_p;            {advance to next output line}
    end;                               {back and process new output line}
  end;
