{   Module of routines that implement the application specific pre-processor
*   for the Pascal front end to the translator.  See the comment headers for
*   routine SST_R_PAS_PREPROC for more details (below).
}
module sst_r_pas_preproc;
define sst_r_pas_preproc_init;
define sst_r_pas_preproc;
%include 'sst_r_pas.ins.pas';

const
  n_dir_names = 29;                    {number of valid compiler directive names}
  max_dir_name_len = 17;               {max length of any compiler directive name}
  stack_file_block_size = 2048;        {minimum size of stack blocks for file state}
  stack_mode_block_size = 512;         {minimum size of stack blocks for parse mode}
  stack_align_block_size = 128;        {minimum size of stack blocks for align mode}
  max_msg_parms = 4;                   {max parameters we can pass to a message}

  dir_name_dim_len = max_dir_name_len + 1; {storage length for directive name}
  dir_names_len =                      {number of chars for directive names list}
    (dir_name_dim_len * n_dir_names) - 1;

type
  mode_k_t = (                         {parsing mode for current character}
    mode_open_k,                       {not within any restrictions}
    mode_quote_k,                      {within quoted string}
    mode_comment_k,                    {within a comment}
    mode_token_k,                      {looking for start of a token}
    mode_token2_k,                     {currently accumulating a token, unquoted}
    mode_token2q_k,                    {currently accumulating a token, quoted}
    mode_directive_k,                  {at start of pre-proc directive (after "%")}
    mode_directive_end_k,              {look for semicolon to end directive}
    mode_token_dir_k,                  {token just read is directive name}
    mode_token_incl_k,                 {just read include file name token}
    mode_token_incl2_k,                {just read ";" after INCLUDE directive}
    mode_token_lnum_k,                 {just got line number token}
    mode_token_lnum2_k,
    mode_token_ifpush_k,               {just got INFILE_PUSH file name}
    mode_token_ifpush2_k,
    mode_debug1_k,                     {part of DEBUG directive}
    mode_debug2_k);                    {part of DEBUG directive}

  frame_file_p_t =                     {pointer to input file state stack frame}
    ^frame_file_t;

  frame_mode_p_t =                     {pointer to parse mode state stack frame}
    ^frame_mode_t;

  frame_align_p_t =                    {pointer to stack frame for alignment state}
    ^frame_align_t;

  frame_file_t = record                {stack frame for state per input file}
    prev_p: frame_file_p_t;            {points to previous stack frame}
    line_p: syn_line_p_t;              {points to partially unsed line, if any}
    start_char: sys_int_machine_t;     {first unused character in the line}
    end;

  frame_mode_t = record                {stack frame for saving parsing mode state}
    mode: mode_k_t;                    {saved parsing mode}
    end;

  frame_align_t = record               {stack frame for saving curr alignment state}
    rule_align: sys_int_machine_t;     {save current default alignment rule}
    end;

  directive_name_t =                   {for storing one compiler directive name}
    array[1..dir_name_dim_len] of char;

var                                    {static storage local to this module}
  stack_file: util_stack_handle_t;     {stack for saving nested include file state}
  stack_mode: util_stack_handle_t;     {stack for saving nested parse mode state}
  stack_align: util_stack_handle_t;    {stack for saving curr alignment mode state}
  f_p: frame_file_p_t;                 {pointer to stack frame for current state}
  token_p: string_var_p_t;             {points to current token being built}
  comment: string_var4_t;              {end of comment string}
  comment_start: string_var4_t;        {string that started current comment}
  directive: string_var32_t;           {pre-processor directive name}
  parm: string_treename_t;             {pre-processor directive parameter}
  mode: mode_k_t;                      {current input char parsing mode}
  pop_when_empty: boolean;             {pop stack when current state exhausted}
  directive_names:                     {list of all the valid compiler directives}
    array[1..n_dir_names] of directive_name_t := [
      'BEGIN_INLINE     ',             {1}
      'BEGIN_NOINLINE   ',             {2}
      'DEBUG            ',             {3}
      'EJECT            ',             {4}
      'ELSE             ',             {5}
      'ELSEIF           ',             {6}
      'ELSEIFDEF        ',             {7}
      'ENABLE           ',             {8}
      'END_INLINE       ',             {9}
      'END_NOINLINE     ',             {10}
      'ENDIF            ',             {11}
      'ERROR            ',             {12}
      'EXIT             ',             {13}
      'IF               ',             {14}
      'INCLUDE          ',             {15}
      'LIST             ',             {16}
      'NATURAL_ALIGNMENT',             {17}
      'NOLIST           ',             {18}
      'SLIBRARY         ',             {19}
      'THEN             ',             {20}
      'POP_ALIGNMENT    ',             {21}
      'PUSH_ALIGNMENT   ',             {22}
      'VAR              ',             {23}
      'WARNING          ',             {24}
      'WORD_ALIGNMENT   ',             {25}
      'IFDEF            ',             {26}
      'LNUM             ',             {27}
      'INFILE_PUSH      ',             {28}
      'INFILE_POP       '              {29}
      ]
    ;
{
*********************************************
*
*   Subroutine SST_R_PAS_PREPROC_INIT
*
*   Initialize the pre-processor.  This is called before the first call to
*   PREPROC.
}
procedure sst_r_pas_preproc_init;

begin
  comment.max := sizeof(comment.str);  {init var strings in static storage}
  comment_start.max := sizeof(comment_start.str);
  directive.max := sizeof(directive.str);
  parm.max := sizeof(parm.str);

  syn_stack_alloc (stack_file);        {create our stack for nested include state}
  stack_file^.stack_len := stack_file_block_size; {we won't be using much stack space}
  util_stack_push                      {create stack frame for top level state}
    (stack_file, sizeof(f_p^), f_p);
  f_p^.prev_p := nil;                  {indicate this is the top stack frame}
  f_p^.line_p := nil;                  {indicate no unfinished line is waiting}

  syn_stack_alloc (stack_mode);        {create our stack for nested parse modes}
  stack_mode^.stack_len := stack_mode_block_size; {we won't be using much stack space}

  syn_stack_alloc (stack_align);       {create our stack for nested align rules}
  stack_align^.stack_len := stack_align_block_size; {we won't be using much stack space}

  mode := mode_open_k;                 {init mode for first char in first file}
  pop_when_empty := false;             {don't pop when stack when line is exhausted}
  end;
{
*********************************************
*
*   Subroutine SST_R_PAS_PREPROC (LINE_P,START_CHAR,N_CHARS)
*
*   Specific pre-processor for the SYN program.
}
procedure sst_r_pas_preproc (          {pre-processor before syntaxer interpretation}
  out     line_p: syn_line_p_t;        {points to descriptor for line chars are from}
  out     start_char: sys_int_machine_t; {starting char within line, first = 1}
  out     n_chars: sys_int_machine_t); {number of characters returned by this call}

var
  i: sys_int_machine_t;                {index to current input character}
  cval: sys_int_machine_t;             {integer value of current character}
  j, k: sys_int_machine_t;             {scratch integers and loop counters}
  end_char: sys_int_machine_t;         {index of last char to definately return}
  pick: sys_int_machine_t;             {number of token picked from list}
  frame_file_new_p: frame_file_p_t;    {points to new stack frame}
  frame_align_p: frame_align_p_t;      {points to alignment stack frame}
  fnam: string_treename_t;             {scratch file name}
  pass_char: boolean;                  {TRUE if curr char is to be passed back}
  pass_chunk: boolean;                 {TRUE if pass back chunk so far}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;
  stat: sys_err_t;                     {completion status code}

label
  loop_line_in, loop_char_in, char_open_mode, not_comment_end,
  not_comment_start, directive_end, include_not_local, include_open_ok,
  next_char_in;
{
*************************************
*
*   Local subroutine MODE_PUSH (NEW_MODE)
*   This subroutine is local to routine SST_R_PAS_PREPROC.
*
*   Push the current parsing mode onto the stack and then set the parsing mode
*   to NEW_MODE.
}
procedure mode_push (
  in      new_mode: mode_k_t);         {new parsing mode to set}

var
  frame_p: frame_mode_p_t;             {pointer to new stack frame}

begin
  util_stack_push (stack_mode, sizeof(frame_p^), frame_p); {make new stack frame}
  frame_p^.mode := mode;               {save current state on new stack frame}
  mode := new_mode;                    {set new current state}
  end;
{
*************************************
*
*   Local subroutine MODE_POP
*   This subroutine is local to routine SST_R_PAS_PREPROC.
*
*   Restore the current parsing state from the stack.  The state must have
*   been previously saved with routine MODE_PUSH.
}
procedure mode_pop;

var
  frame_p: frame_mode_p_t;             {pointer to new stack frame}

begin
  util_stack_last_frame (stack_mode, sizeof(frame_p^), frame_p); {get pnt to frame}
  mode := frame_p^.mode;               {restore state from stack frame}
  util_stack_pop (stack_mode, sizeof(frame_p^)); {remove this stack frame}
  end;
{
*************************************
*
*   Start of main routine (SST_R_PAS_PREPROC)
}
begin
  fnam.max := sizeof(fnam.str);        {init local var string}
{
*   Init the subroutine return arguments LINE_P, START_CHAR.
*   LINE_P and START_CHAR will be initialized to point to the very next
*   input stream character.  This may come from a partially processed input
*   source line left on the current stack frame, or from the next line from
*   the input files.  The local variable LAST_CHAR will be initialized to
*   indicate that no characters are definately to be passed back (yet) from
*   the new line.
*
*   We may also jump back here if the next character after the end of the
*   current line is needed.  In this case it is assumed that any part of the
*   current line to be passed back has been, and it is therefore OK to trash
*   the state associated with it.
}
loop_line_in:                          {back here for next input line}
  if f_p^.line_p <> nil
    then begin                         {a previously unfinished line exists}
      line_p := f_p^.line_p;           {restart where left off before}
      start_char := f_p^.start_char;
      f_p^.line_p := nil;              {indicate no longer any previous line left}
      end
    else begin                         {no previously unfinished line found}
      if pop_when_empty then begin     {need to pop to previous file ?}
        pop_when_empty := false;       {don't pop until next end of file}
        f_p := f_p^.prev_p;            {make previous stack frame current}
        util_stack_pop (stack_file, sizeof(f_p^)); {remove old stack frame from stack}
        goto loop_line_in;             {re-try with popped state}
        end;
      syn_infile_read (line_p, stat);  {read next line from input file}
      if file_eof(stat) then begin     {this is last "line" from this file ?}
        pop_when_empty :=              {pop stack after this line if not top file}
          f_p^.prev_p <> nil;
        end;
      if sys_error(stat) then begin    {hard error on trying to read input file ?}
        sys_msg_parm_int (msg_parm[1], line_p^.line_n);
        sys_msg_parm_vstr (msg_parm[2], line_p^.file_p^.conn_p^.tnam);
        sys_error_abort (stat, 'syn', 'syntax_error_msg', msg_parm, 2);
        end;
      start_char := 1;                 {start at first character on line}
      end
    ;                                  {LINE_P and START_CHAR have been initialized}

  i := start_char;                     {init index of last character processed}
  end_char := start_char - 1;          {init to no chars definately passed back yet}
  pass_chunk := false;                 {init to not force pass back of curr chunk}
{
*   Loop back here to process each new character.
}
loop_char_in:                          {jump here to process the current character}
  pass_char := true;                   {init to this character must be passed back}
  cval := ord(line_p^.c[i]);           {init integer character value}
  if cval > 127 then cval := cval ! ~127; {special flag char values are negative}
  case mode of                         {what parsing mode applies to this char ?}
{
*************************
*
*   This character is not within any restrictions.
}
mode_open_k: begin                     {not within any restrictions}
char_open_mode:                        {jump here for common code with other modes}
  case line_p^.c[i] of
'''': begin                            {start of a quoted string}
      if mode = mode_token2_k then begin {accumulating a token ?}
        mode_pop;                      {this ends the token}
        goto loop_char_in;             {re-process this char with popped mode}
        end;
      mode_push (mode_quote_k);        {we are now in a quoted string}
      end;
'{': begin                             {start of a comment}
      mode_push (mode_comment_k);
      comment_start.str[1] := '{';     {remember comment start string}
      comment_start.len := 1;
      comment.str[1] := '}';           {set close comment character}
      comment.len := 1;
      pass_char := false;
      pass_chunk := true;              {pass back chunk up to this comment}
      end;
'"': begin                             {start of a comment}
      mode_push (mode_comment_k);
      comment_start.str[1] := '"';     {remember comment start string}
      comment_start.len := 1;
      comment.str[1] := '"';           {set close comment character}
      comment.len := 1;
      pass_char := false;
      pass_chunk := true;              {pass back chunk up to this comment}
      end;
'(': begin                             {possibly the start of a comment}
      if  (i < line_p^.n_chars) and then {another character exists on this line ?}
          (line_p^.c[i+1] = '*')       {next char confirms comment start ?}
          then begin                   {this is definately a comment start}
        mode_push (mode_comment_k);
        comment_start.str[1] := '(';   {remember comment start string}
        comment_start.str[2] := '*';
        comment_start.len := 2;
        comment.str[1] := '*';         {set close comment string}
        comment.str[2] := ')';
        comment.len := 2;
        pass_char := false;
        i := i + 1;                    {skip over second comment start character}
        pass_chunk := true;            {pass back chunk up to this comment}
        end;
      end;
'%': begin                             {pre-processor directive follows}
      mode_push (mode_directive_k);
      pass_char := false;              {don't pass back the "%" character}
      pass_chunk := true;              {pass back chunk so far up to here}
      end;
    end;                               {done with OPEN mode character cases}
  if mode = mode_token_k               {looking for start of a token ?}
    then mode := mode_token2_k;        {this character starts the token}
  if                                   {currently accumulating a token ?}
      (mode = mode_token2_k) or
      (mode = mode_token2q_k)
      then begin
    if token_p^.len < token_p^.max then begin {room for another char in token ?}
      token_p^.len := token_p^.len + 1; {one more character in token}
      token_p^.str[token_p^.len] := line_p^.c[i]; {copy character into token}
      end;
    end;
  end;                                 {done processing character in OPEN mode}
{
*************************
*
*   We are looking for the start of a token as part of a pre-processor directive.
}
mode_token_k: begin                    {looking for the start of a directive token}
  pass_char := false;                  {definalely don't send this character on}
  if cval < 0 then goto next_char_in;  {skip over special characters}
  case line_p^.c[i] of
' ': ;                                 {skip over blanks}
'''': begin                            {token is a quoted string}
      mode := mode_token2q_k;
      end;
otherwise                              {not one of the characters above}
    mode := mode_token2_k;             {token is not a quoted string}
    goto loop_char_in;                 {back and re-process char with new mode}
    end;
  end;
{
*************************
*
*   We are currently accumulating a token.  The token is not quoted.
}
mode_token2_k: begin
  pass_char := false;                  {definately don't send this character on}
  if cval < 0 then begin               {special character ends token}
    mode_pop;
    goto loop_char_in;
    end;
  case line_p^.c[i] of
' ', ';': begin                        {these characters end tokens}
      mode_pop;
      goto loop_char_in;
      end;
    end;                               {end of character cases}
  goto char_open_mode;                 {to common code with OPEN mode}
  end;
{
*************************
*
*   We are currently accumulating a token.  The token IS quoted.
}
mode_token2q_k: begin                  {currently accumulating a directive token}
  pass_char := false;                  {definately don't send this character on}
  if cval < 0 then begin               {special character ends token}
    mode_pop;
    goto loop_char_in;
    end;
  case line_p^.c[i] of
'''': begin                            {end quote ends token}
      mode_pop;
      end;
otherwise
    goto char_open_mode;               {to common code with OPEN mode}
    end;                               {end of special handling character cases}
  end;
{
*************************
*
*   This character is within a quoted string.
}
mode_quote_k: begin                    {we are inside a quoted string}
  if
      (ord(line_p^.c[i]) > 127) or     {special character not allowed in quotes ?}
      (line_p^.c[i] = '''')            {explicit end of quoted string ?}
      then begin
    mode_pop;                          {end the quoted string, pop parsing mode}
    end;
  end;
{
*************************
*
*   This character is within a comment.
}
mode_comment_k: begin                  {we are inside a comment}
  pass_char := false;                  {don't pass back comment characters}
  if line_p^.c[i] = comment.str[1] then begin {could be start of end-comment ?}
    if (i + comment.len - 1) <= line_p^.n_chars then begin {end-comment could fit ?}
      k := i + 1;                      {init source line index of next comment char}
      for j := 2 to comment.len do begin {once for each remaining end-comment char}
        if line_p^.c[k] <> comment.str[j] {doesn't match comment end ?}
          then goto not_comment_end;
        k := k + 1;                    {advance source character index}
        end;                           {back and test next comment character}
      i := k - 1;                      {just got done with last comment end char}
      mode_pop;                        {no longer within a comment}
      goto next_char_in;               {back and process next input stream character}
      end;
    end;
not_comment_end:                       {jump here if definately still in a comment}

  if line_p^.c[i] = comment_start.str[1] then begin {could be start-comment ?}
    if (i + comment_start.len - 1) <= line_p^.n_chars then begin {could fit ?}
      k := i + 1;                      {init source line index of next comment char}
      for j := 2 to comment_start.len do begin {once for each remaining char}
        if line_p^.c[k] <> comment_start.str[j] {doesn't match comment start ?}
          then goto not_comment_start;
        k := k + 1;                    {advance source character index}
        end;                           {back and test next comment character}
      sys_msg_parm_int (msg_parm[1], line_p^.line_n);
      sys_msg_parm_vstr (msg_parm[2], line_p^.file_p^.conn_p^.tnam);
      sys_message_parms ('sst_pas_read', 'comment_start_start', msg_parm, 2);
      string_vstring (fnam, line_p^.c, line_p^.n_chars-1); {make writeable string}
      writeln (fnam.str:fnam.len);     {show the line with the error}
      writeln ('':i-1, '^');           {point to offending character}
      sys_bomb;
      end;
    end;
not_comment_start:                     {jump here if not hit illegal start of comm}

  if cval = syn_ichar_eof_k then begin {hit end of file within a comment ?}
    sys_msg_parm_vstr (msg_parm[1], line_p^.file_p^.conn_p^.tnam);
    sys_message_bomb ('sst_pas_read', 'comment_eof', msg_parm, 1);
    end;
  end;
{
*************************
*
*   This character is the first character of a directive name.
}
mode_directive_k: begin
  token_p := univ_ptr(addr(directive)); {point to token to accumulate}
  token_p^.len := 0;                   {init accumulated token length}
  mode := mode_token_dir_k;            {set mode to pop back to after done token}
  mode_push (mode_token_k);            {cause token to be accumulated}
  goto loop_char_in;                   {back and re-process char with new mode}
  end;
{
*************************
*
*   Skip to the ";" ending this compiler directive.
}
mode_directive_end_k: begin
  pass_char := false;                  {definately don't pass on this character}
  case cval of
ord(' '), syn_ichar_eol_k: ;           {skip over blanks and end of lines}
ord(';'): begin                        {this is what we are looking for}
      mode_pop;                        {pop back to previous parse state}
      end;
otherwise
    sys_msg_parm_int (msg_parm[1], line_p^.line_n);
    sys_msg_parm_vstr (msg_parm[2], line_p^.file_p^.conn_p^.tnam);
    sys_message_bomb ('sst_pas_read', 'directive_semicolon_missing', msg_parm, 2);
    end;
  end;
{
*************************
*
*   Current character is first character after directive name token.  The
*   directive name is in DIRECTIVE.
}
mode_token_dir_k: begin
  string_upcase (directive);           {make upper case for token matching}
  string_tkpick_s (                    {pick token from list of valid choices}
    directive,                         {token to pick from list}
    directive_names,                   {list of valid choices}
    dir_names_len,                     {number of characters in choice list}
    pick);                             {number of token picked from list}
  case pick of
{
*   All the legal but ignored compiler directives.
}
1,                                     {BEGIN_INLINE}
2,                                     {BEGIN_NOINLINE}
4,                                     {EJECT}
9,                                     {END_INLINE}
10,                                    {END_NOINLINE}
16,                                    {LIST}
18: begin                              {NOLIST}
directive_end:                         {jump here for common code to eat ";"}
  mode_pop;                            {set mode to pop back to after reading ";"}
  mode_push (mode_directive_end_k);    {look for ";" ending this compiler directive}
  goto loop_char_in;                   {back and re-process this char with new mode}
  end;
{
*   DEBUG
}
3: begin
  token_p := univ_ptr(addr(parm));     {point to token to accumulate}
  token_p^.len := 0;                   {init accumulated token length}
  mode := mode_debug1_k;               {mode to pop back to after done token}
  mode_push (mode_token_k);            {cause token to be accumulated}
  goto loop_char_in;                   {back and re-process this char with new mode}
  end;
{
*   INCLUDE <pathname>
}
15: begin
  token_p := univ_ptr(addr(parm));     {point to token to accumulate}
  token_p^.len := 0;                   {init accumulated token length}
  mode := mode_token_incl_k;           {set mode to pop back to after done token}
  mode_push (mode_token_k);            {cause token to be accumulated}
  goto loop_char_in;                   {back and re-process this char with new mode}
  end;
{
*   NATURAL_ALIGNMENT
}
17: begin
  sst_align := sst_align_natural_k;
  goto directive_end;
  end;
{
*   POP_ALIGNMENT
}
21: begin
  util_stack_last_frame                {get pointer to last alignment stack frame}
    (stack_align, sizeof(frame_align_p^), frame_align_p);
  sst_align := frame_align_p^.rule_align; {restore current alignment rule}
  util_stack_pop (stack_align, sizeof(frame_align_p^)); {remove last frame from stack}
  goto directive_end;
  end;
{
*   PUSH_ALIGNMENT
}
22: begin
  util_stack_push                      {create stack frame for saving state}
    (stack_align, sizeof(frame_align_p^), frame_align_p);
  frame_align_p^.rule_align := sst_align; {save alignment rule in stack frame}
  goto directive_end;
  end;
{
*   WORD_ALIGNMENT
}
25: begin
  sst_align := 2;
  goto directive_end;
  end;
{
*   LNUM line_number
}
27: begin
  token_p := univ_ptr(addr(parm));     {point to token to accumulate}
  token_p^.len := 0;                   {init accumulated token length}
  mode := mode_token_lnum_k;           {set mode to pop back to after done token}
  mode_push (mode_token_k);            {cause token to be accumulated}
  goto loop_char_in;                   {back and re-process this char with new mode}
  end;
{
*   INFILE_PUSH filename
}
28: begin
  token_p := univ_ptr(addr(parm));     {point to token to accumulate}
  token_p^.len := 0;                   {init accumulated token length}
  mode := mode_token_ifpush_k;         {set mode to pop back to after done token}
  mode_push (mode_token_k);            {cause token to be accumulated}
  goto loop_char_in;                   {back and re-process this char with new mode}
  end;
{
*   INFILE_POP
}
29: begin
  syn_infile_name_pop;
  goto directive_end;
  end;
{
*   Unrecognized compiler directive.
}
otherwise
    sys_msg_parm_vstr (msg_parm[1], directive);
    sys_msg_parm_int (msg_parm[2], line_p^.line_n);
    sys_msg_parm_vstr (msg_parm[3], line_p^.file_p^.conn_p^.tnam);
    sys_message_bomb ('sst_pas_read', 'directive_unrecognized', msg_parm, 3);
    end;                               {end of directive name choices}
  end;
{
*************************
*
*   The include file name has just been read into PARM.
}
mode_token_incl_k: begin
  mode := mode_token_incl2_k;          {mode to pop back to after found ";"}
  mode_push (mode_directive_end_k);    {look for ";" to end directive}
  goto loop_char_in;                   {back and re-process char with new mode}
  end;
mode_token_incl2_k: begin
  f_p^.line_p := line_p;               {save state of current file on stack}
  f_p^.start_char := i;
  util_stack_push                      {make new stack frame for nested file state}
    (stack_file, sizeof(frame_file_new_p^), frame_file_new_p);
  frame_file_new_p^.prev_p := f_p;     {point new frame back to its parent}
  f_p := frame_file_new_p;             {make the new frame current}
  f_p^.line_p := nil;                  {init to no partial line read in new file}
  if sst_local_ins then begin          {look for include file in local directory ?}
    string_generic_fnam (parm, '', fnam); {make leafname of include file in FNAM}
    syn_infile_push (fnam, '', stat);  {try for include file in current directory}
    if file_not_found(stat) then goto include_not_local;
    if not sys_error(stat) then goto include_open_ok;
    sys_msg_parm_vstr (msg_parm[1], parm);
    sys_msg_parm_int (msg_parm[2], line_p^.line_n);
    sys_msg_parm_vstr (msg_parm[3], line_p^.file_p^.conn_p^.tnam);
    sys_msg_parm_vstr (msg_parm[4], fnam);
    sys_error_abort (stat, 'sst_pas_read', 'directive_include_open_local', msg_parm, 4);
    end;                               {done handling LOCAL_INS switch ON}
include_not_local:                     {open include file name exactly as given}
  syn_infile_push (parm, '', stat);    {save state and switch input to new file}
  if sys_error(stat) then begin        {error opening include file ?}
    sys_msg_parm_vstr (msg_parm[1], parm);
    sys_msg_parm_int (msg_parm[2], line_p^.line_n);
    sys_msg_parm_vstr (msg_parm[3], line_p^.file_p^.conn_p^.tnam);
    sys_error_abort (stat, 'sst_pas_read', 'directive_include_open', msg_parm, 3);
    end;
include_open_ok:                       {done opening include file, was successful}
  mode_pop;                            {pop to state before INCLUDE directive}
  goto loop_line_in;                   {get first line from new file}
  end;
{
*************************
*
*   PARM now contains the optional debug level string.  It will be of zero
*   length if none was given.  For compatibility with the Pascal compiler,
*   this will be interpreted as debug level 1.
}
mode_debug1_k: begin
  if parm.len > 0
    then begin                         {a token was found}
      string_t_int (parm, j, stat);    {convert token to debug level in J}
      if sys_error(stat) or (j < 1) then begin {bad or out of range parameter ?}
        sys_msg_parm_vstr (msg_parm[1], parm);
        sys_msg_parm_vstr (msg_parm[2], directive);
        sys_msg_parm_int (msg_parm[3], line_p^.line_n);
        sys_msg_parm_vstr (msg_parm[4], line_p^.file_p^.conn_p^.tnam);
        sys_message_bomb ('sst_pas_read', 'directive_parm_bad', msg_parm, 4);
        end;
      end
    else begin                         {no token was found, use default}
      j := 1;                          {set default debug level}
      end
    ;                                  {J is now set to selected debug level}
  if sst_level_debug >= j
    then begin                         {debug code IS selected}
      mode_pop;                        {set state top pop back to after ";" is read}
      end
    else begin                         {debug code is NOT selected}
      mode := mode_debug2_k;           {mode to pop back to after ";" is read}
      end
    ;
  mode_push (mode_directive_end_k);    {look for ";" to end directive}
  goto loop_char_in;                   {back and re-process char with new mode}
  end;
mode_debug2_k: begin                   {skip over input stream until end of line}
  pass_char := false;                  {init to not pass on this character}
  if cval = syn_ichar_eol_k then begin {found end of this line ?}
    mode_pop;
    pass_char := true;
    end;
  end;
{
*************************
*
*   The LNUM line number has just been read into PARM.
}
mode_token_lnum_k: begin
  mode := mode_token_lnum2_k;          {mode to pop back to after found ";"}
  mode_push (mode_directive_end_k);    {look for ";" to end directive}
  goto loop_char_in;                   {back and re-process char with new mode}
  end;
mode_token_lnum2_k: begin
  string_t_int (parm, j, stat);        {convert token to integer value}
  if sys_error(stat) then begin
    sys_msg_parm_vstr (msg_parm[1], parm);
    sys_msg_parm_vstr (msg_parm[2], directive);
    sys_msg_parm_int (msg_parm[3], line_p^.line_n);
    sys_msg_parm_vstr (msg_parm[4], line_p^.file_p^.conn_p^.tnam);
    sys_error_abort (stat, 'sst_pas_read', 'directive_parm_bad', msg_parm, 4);
    end;
  syn_infile_name_lnum (j);            {set logical number of next input line}
  mode_pop;                            {pop to state before directive}
  end;
{
*************************
*
*   The INFILE_PUSH file name has just been read into PARM.
}
mode_token_ifpush_k: begin
  mode := mode_token_ifpush2_k;        {mode to pop back to after found ";"}
  mode_push (mode_directive_end_k);    {look for ";" to end directive}
  goto loop_char_in;                   {back and re-process char with new mode}
  end;
mode_token_ifpush2_k: begin
  syn_infile_name_push (parm);         {push new logical input file name}
  mode_pop;                            {pop to state before directive}
  end;
{
*************************
*
*   All done with the current character.
*
*   At this point LINE_P, START_CHAR, and END_CHAR indicate the range of
*   characters from this line that will definately be returned so far.  I
*   is the character index of the last character examined.
*
*   If I indicates the last character on the current input line, then we
*   will pass back the definate portion of the current input line and restart
*   next time with the next line.
*
*   PASS_CHAR indicates that the current character (at index I) is definately
*   to be passed back.  PASS_CHUNK indicates that the current chunk so far
*   is to be passed back now.
}
    end;                               {end of parsing mode cases}
next_char_in:                          {jump here to advance to next input character}
  if pass_char then begin              {current character must be passed back ?}
    if end_char < start_char then begin {this is first "solid" char this chunk ?}
      start_char := i;                 {reset start of chunk to this character}
      end;
    end_char := i;                     {this character will definately be passed back}
    end;
  pass_chunk :=                        {end of input line forces pass back of chunk}
    pass_chunk or (i >= line_p^.n_chars);
  if pass_chunk then begin             {need to pass back current chunk ?}
    if i < line_p^.n_chars then begin  {more left on line after this chunk ?}
      f_p^.line_p := line_p;           {save rest of line for next time}
      f_p^.start_char := i + 1;        {restart at next char after current}
      end;
    n_chars := end_char - start_char + 1; {number of characters to return}
    if n_chars <= 0 then goto loop_line_in; {nothing left to return on this line ?}
    return;                            {pass back last piece of this line}
    end;
  i := i + 1;                          {make index of next character on current line}
  goto loop_char_in;                   {back and process this new character}
  end;
