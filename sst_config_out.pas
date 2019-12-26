{   Subroutine SST_CONFIG_OUT (FNAM)
*
*   Set up the output source configuration and init the back end routines.
*   The configuration information is read from the file FNAM.
*
*   The file named in FNAM contains a set of commands, one per line.
*   "/*" starts an end of line comment.  Valid commands are:
*
*   BITS_ADR n
*
*     N is the number of bits in one machine address.
*
*   SIZE_INT n name
*
*     Declares that an integer is available directly in the target language
*     that is N machine addresses in size.  There may be several SIZE_INT commands
*     to indicate more than one available integer size.  If so, all SIZE_INT
*     commands must be in order from smallest to largest integer sizes.  NAME
*     is the name of this data type in the target language.
*
*   SIZE_INT_MACHINE n
*
*     Declares the size of the preferred "machine" integer.  This must be one
*     of the integer sizes previously declared with a SIZE_INT command.
*
*   SIZE_INT_ADR n
*
*     Declares the size of the integer needed to hold a machine address.
*     This must be one of the integer sizes previously declared with a
*     SIZE_INT command.
*
*   UNIV_POINTER name
*
*     Declare the name of the universal pointer data type in the target
*     language.
*
*   SIZE_FLOAT n name
*
*     Just like SIZE_INT, but declares the size of one available floating point
*     format.  NAME is the name of this data type in the target language.
*
*   SIZE_FLOAT_MACHINE n
*
*     Declares the size of the preferred "machine" floating point number.
*     This must be one of the floating point sizes previously declared with a
*     SIZE_FLOAT command.
*
*   SIZE_FLOAT_SINGLE n
*   SIZE_FLOAT_DOUBLE n
*
*     Declare the size of what is typically referred to as the "single" and
*     "double" precision floating numbers on the target machine.  The sizes
*     used must have been previously declared with a SIZE_FLOAT command.
*
*   SIZE_ENUMERATED_NATIVE n
*
*     Declare the size in machine addresses of the target compiler's enumerated
*     data type.  This will default to the smallest SIZE_INT declared.
*
*   SIZE_ENUMERATED_FORCE n
*
*     Declare what size variables of enumerated types should be, regardless of
*     whether this is the native size.  N must be either the same as the value
*     for SIZE_ENUMEREATED_NATIVE, or must be one of the sizes previously
*     declared with a SIZE_INT command.
*
*   SIZE_SET n
*
*     Declare an available size for SET data types.  There may be any number
*     of these commands, but they must be in order from smallest to largest
*     N.  N is a size in machine address increments.  If no SIZE_SET commands
*     are given, and the target compiler has a native SET data type, then
*     the set sizes will be set to match the native SET data type.  If no
*     SIZE_SET commands are given and the target compiler has no native SET
*     data type, then there will be one SIZE_SET for every integer size
*     declared with a SIZE_INT command.
*
*   SIZE_SET_MULTIPLE n
*
*     Declare the size in machine address units that large sets should be a
*     multiple of.  If the target compiler has a native SET data type, then
*     the default will be such that the native SET data type can be used.
*     Otherwise, this will default to the size declared in SIZE_INT_MACHINE.
*
*     Set data types will be the smallest size specified with a SIZE_SET
*     command that is still big enough to have one bit for every element.
*     If the set requires more bits than available in the largest SIZE_SET,
*     then the size will be a multiple of SIZE_SET_MULTIPLE.
*
*   SIZE_BOOLEAN n name
*
*     Declare the size in machine addresses of the standard BOOLEAN or LOGICAL
*     data type of the target compiler.  Variables of this data type can only
*     take on values of TRUE or FALSE.  NAME is the name of this data type in
*     the target language.
*
*   BITS_CHAR n name
*
*     Declare the number of bits of storage allocated for one CHARACTER.
*     NAME is the name of the basic character data type in the target language.
*
*   OS name
*
*     Declare the operating system name.  Valid names are
*     DOMAIN, HPUX, AIX, IRIX, and SOLARIS.
*
*   LANGUAGE name
*
*     Declare the target language name.  Valid names are C and PASCAL.
*
*   SUFFIX_FILE_NAME suffix
*
*     Declare the mandatory output file name suffix.  The output file name
*     will always end with this suffix.  If the output file name is specified
*     without this suffix, then it will be added.  Nothing will be done to it
*     if it already ends with this suffix.  The empty string (two quotes
*     in a row) specifies no particular suffix is required.  This is also
*     the default.
*
*   MAX_SYMBOL_LEN n
*
*     Declare the maximum allowable length of a symbol in the output source
*     code.  N is the maximum number of characters.
*
*   CONFIG string
*
*     Pass additional configuration information to the back end.  The string
*     may be interpreted uniquely for each back end.  Back end selection is
*     a function of the MANUFACTURER, OS, and LANGUAGE parameters.
*     This string must be enclosed in quotes if it contains any spaces.
*
*   CASE <character case>
*
*     Set the character case handling to be used for symbol names.  Valid
*     choices are:
*
*       UPPER  -  All symbol names will be written in upper case, regardless
*         of their case in the input source.
*
*       LOWER  -  All symbol names will be written in lower case, regardless
*         of their case in the input source.  This is the default.
*
*   RESERVED name
*
*     Declare the name of a reserved symbol.  No output symbol will have this
*     name, whether it originally came from the input source code or was
*     created implicitly by the translator.  Symbols will be renamed, if
*     necessary, to avoid these names.  There may be any number of these
*     commands.
*
*   SUFFIX_DATA_TYPE suffix
*
*     Declare the suffix that all data type names will end with.  The default
*     is no suffix (null string).
*
*   SUFFIX_CONSTANT suffix
*
*     Declare the suffix that all constant names will end with.  The default
*     is no suffix (null string).
*
*   PASS_VAL_SIZE_MAX n
*
*     Indicate the maximum size call argument that may be passed by value.
*
*   ALIGN_MIN_REC n
*
*     Set minimum alignment for all record data types.  Default is 1, meaning
*     arbitrary address alignment.
*
*   ALIGN_MIN_PACKED_REC n
*
*     Set minimum aligment of packed records.  This defaults to the
*     ALIGN_MIN_REC value.
}
module sst_CONFIG_OUT;
define sst_config_out;
%include 'sst2.ins.pas';

const
  align_not_set_k = 65535;             {alignment value to indicate align not set}
  n_cmds = 27;                         {number of possible commands}
  cmd_len_max = 22;                    {size of largest command name}
  max_msg_parms = 2;                   {max arguments we can pass to a message}

  cmd_len_alloc = cmd_len_max + 1;     {chars to allocate for each command}
  cmds_len = cmd_len_alloc * n_cmds;   {total number of chars in commands list}

type
  cmd_t =                              {one command name}
    array[1..cmd_len_alloc] of char;

  cmds_t = record                      {array of all the command names}
    max: string_index_t;               {simulate a var string}
    len: string_index_t;
    str: array[1..n_cmds] of cmd_t;
    end;

var
  cmds: cmds_t := [                    {all the command names}
    max := cmds_len, len := cmds_len, str := [
      'BITS_ADR              ',        {1}
      'SIZE_INT              ',        {2}
      'SIZE_INT_MACHINE      ',        {3}
      'SIZE_INT_ADR          ',        {4}
      'SIZE_FLOAT            ',        {5}
      'SIZE_FLOAT_MACHINE    ',        {6}
      'SIZE_FLOAT_SINGLE     ',        {7}
      'SIZE_FLOAT_DOUBLE     ',        {8}
      'SIZE_ENUMERATED_NATIVE',        {9}
      'SIZE_BOOLEAN          ',        {10}
      'BITS_CHAR             ',        {11}
      'OS                    ',        {12}
      'LANGUAGE              ',        {13}
      'SUFFIX_FILE_NAME      ',        {14}
      'MAX_SYMBOL_LEN        ',        {15}
      'CONFIG                ',        {16}
      'CASE                  ',        {17}
      'RESERVED              ',        {18}
      'SUFFIX_DATA_TYPE      ',        {19}
      'SUFFIX_CONSTANT       ',        {20}
      'UNIV_POINTER          ',        {21}
      'SIZE_ENUMERATED_FORCE ',        {22}
      'SIZE_SET              ',        {23}
      'SIZE_SET_MULTIPLE     ',        {24}
      'PASS_VAL_SIZE_MAX     ',        {25}
      'ALIGN_MIN_REC         ',        {26}
      'ALIGN_MIN_PACKED_REC  ',        {27}
      ]
    ];
  comment: string_var4_t :=            {end of line comment delimiter}
    [str := '/*', len := 2, max := 4];

procedure sst_config_out (             {configure and init back end}
  in      fnam: univ string_var_arg_t); {configuration file name}

var
  conn: file_conn_t;                   {connection handle to config file}
  buf: string_var8192_t;               {one line input buffer}
  p: string_index_t;                   {input line parse index}
  cmd: string_var32_t;                 {command name parsed from config file}
  pick: sys_int_machine_t;             {number of command picked from list}
  parm: string_var32_t;                {parameter to command from config file}
  i: sys_int_machine_t;                {loop counter and scratch integer}
  al: sys_int_machine_t;               {scratch alignment value}
  sz: sys_int_adr_t;                   {amount of memory needed to allocate}
  szi: sys_int_machine_t;              {mem size in SYS_INT_MACHINE_T format}
  ent_p: string_chain_ent_p_t;         {pointer to current string chain entry}
  sym_p: sst_symbol_p_t;               {scratch pointer to symbol descriptor}
  dt_p: sst_dtype_p_t;                 {scratch pointer to data type descriptor}
  char_none_h: syn_char_t;             {source stream char handle, set to invalid}
  uptr_given: boolean;                 {TRUE if UNIV_POINTER command given}
  os_given: boolean;                   {TRUE if OS command given}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;
  stat: sys_err_t;                     {status return code}

label
  next_line, loop_fp, done_fp, set_size_found, set_size_found2,
  no_err, parm_err, parm_range_err, read_err, eof, enum_done;
{
*********************************************
*
*   Local subroutine UNSPEC_ERR (CMD_NAME)
*
*   Complain that a value was unspecified.  CMD_NAME is the name of the
*   command that is used to specify the value.
}
procedure unspec_err (
  in      cmd_name: string);
  options (noreturn);

begin
  sys_msg_parm_str (msg_parm[1], cmd_name);
  sys_message_bomb ('sst', 'config_cmd_missing', msg_parm, 1);
  end;
{
*********************************************
*
*   Start of main routine.
}
begin
  buf.max := sizeof(buf.str);          {init local var strings}
  cmd.max := sizeof(cmd.str);
  parm.max := sizeof(parm.str);

  char_none_h.crange_p := nil;         {init invalid source character handle}
  char_none_h.ofs := 0;

  file_open_read_text (fnam, '', conn, stat); {open config file for reading text}
  if sys_error(stat) then begin        {error opening config file ?}
    sys_msg_parm_vstr (msg_parm[1], fnam);
    sys_error_print (stat, 'sst', 'config_open', msg_parm, 1);
    sys_bomb;
    end;
{
*   Init the configuration data before reading config file.
}
  sst_config.bits_adr := 0;

  sst_config.n_size_int := 0;
  sst_config.int_machine_p := nil;
  sst_config.int_adr_p := nil;
  sst_config.name_univ_ptr.max := sizeof(sst_config.name_univ_ptr.str);
  sst_config.name_univ_ptr.len := 0;

  sst_config.n_size_float := 0;
  sst_config.float_machine_p := nil;
  sst_config.float_single_p := nil;
  sst_config.float_double_p := nil;

  sst_config.n_size_set := 0;
  sst_config.size_set_multiple.size := 0;

  sst_config.align_min_rec := 1;
  sst_config.align_min_rec_pack := align_not_set_k;
  sst_config.pass_val_size_max := 0;
  sst_config.size_enum_native := 0;
  sst_config.size_enum := 0;
  sst_config.size_bool := 0;
  sst_config.name_bool.max := sizeof(sst_config.name_bool.str);
  sst_config.name_bool.len := 0;
  sst_config.bits_char := 0;
  sst_config.size_char := 0;
  sst_config.name_char.max := sizeof(sst_config.name_char.str);
  sst_config.name_char.len := 0;
  sst_config.manuf := sst_manuf_none_k;
  sst_config.lang := sst_lang_none_k;
  sst_config.suffix_fnam.max := sizeof(sst_config.suffix_fnam.str);
  sst_config.suffix_fnam.len := 0;
  sst_config.suffix_dtype.max := sizeof(sst_config.suffix_dtype.str);
  sst_config.suffix_dtype.len := 0;
  sst_config.suffix_const.max := sizeof(sst_config.suffix_const.str);
  sst_config.suffix_const.len := 0;
  sst_config.sym_len_max := 0;
  sst_config.config.max := sizeof(sst_config.config.str);
  sst_config.config.len := 0;
  sst_config.charcase := syn_charcase_down_k;
  sst_config.reserve_p := nil;

  uptr_given := false;                 {init to UNIV_POINTER command not given yet}
  os_given := false;                   {init to no OS command given yet}

next_line:                             {back here each new line from config file}
  file_read_text (conn, buf, stat);    {read next line from config file}
  if file_eof(stat) then goto eof;     {hit end of file ?}
  string_find (comment, buf, p);       {look for comment start}
  if p > 0 then begin                  {found start of comment ?}
    buf.len := p - 1;                  {truncate input line at comment start}
    end;
  p := 1;                              {init input line parse index}
  string_token (buf, p, cmd, stat);    {extract command name from this line}
  if string_eos(stat) then goto next_line; {no command at all on this line ?}
  if sys_error(stat) then goto read_err;
  string_upcase (cmd);                 {make upper case for name matching}
  string_tkpick (cmd, cmds, pick);     {pick command name from list}
  case pick of
{
*   BITS_ADR n
}
1: begin
  string_token_int (buf, p, sst_config.bits_adr, stat);
  if sys_error(stat) then goto parm_err;
  if sst_config.bits_adr < 1 then goto parm_range_err;
  end;
{
*   SIZE_INT n name
}
2: begin
  sst_config.n_size_int := sst_config.n_size_int + 1; {one more integer size}
  if sst_config.n_size_int > sst_size_list_max then begin
    sys_message ('sst', 'config_too_many_int');
    goto read_err;
    end;
  with sst_config.size_int[sst_config.n_size_int]: csize {CSIZE is SIZE_INT entry}
      do begin
    string_token_int (buf, p, csize.size, stat); {get size of this integer}
    if sys_error(stat) then goto parm_err; {error reading parameter ?}
    if csize.size <= 0 then goto parm_range_err; {value out of range ?}
    if sst_config.n_size_int > 1 then begin {there was a previous size ?}
      if  csize.size <=                {not bigger than previous integer size ?}
          sst_config.size_int[sst_config.n_size_int-1].size
          then begin
        sys_message ('sst', 'config_int_le_previous');
        goto read_err;
        end;
      end;
    csize.name.max := sizeof(csize.name.str); {init data type name var string}
    string_token (buf, p, csize.name, stat); {get data type name}
    sst_dtype_new (csize.dtype_p);     {create new data type}
    csize.dtype_p^.dtype := sst_dtype_int_k; {initialize data type descriptor}
    csize.dtype_p^.bits_min := csize.size * sst_config.bits_adr;
    csize.dtype_p^.align_nat := csize.size;
    csize.dtype_p^.align := csize.size;
    csize.dtype_p^.size_used := csize.size;
    csize.dtype_p^.size_align := csize.size;
    end;                               {done with CSIZE abbreviation}
  end;
{
*   SIZE_INT_MACHINE n
}
3: begin
  string_token_int (buf, p, szi, stat); {get size of this data type}
  if sys_error(stat) then goto parm_err;
  for i := 1 to sst_config.n_size_int do begin {loop thru the integer sizes}
    if sst_config.size_int[i].size = szi then begin
      sst_config.int_machine_p := sst_config.size_int[i].dtype_p;
      goto no_err;
      end;
    end;                               {back and try next integer size}
  sys_message ('sst', 'config_size_bad');
  goto read_err;
  end;
{
*   SIZE_INT_ADR n
}
4: begin
  string_token_int (buf, p, szi, stat); {get size of this data type}
  if sys_error(stat) then goto parm_err;
  for i := 1 to sst_config.n_size_int do begin {loop thru the integer sizes}
    if sst_config.size_int[i].size = szi then begin
      sst_config.int_adr_p := sst_config.size_int[i].dtype_p;
      goto no_err;
      end;
    end;                               {back and try next integer size}
  sys_message ('sst', 'config_size_bad');
  goto read_err;
  end;
{
*   SIZE_FLOAT n name [ALIGN n]
}
5: begin
  sst_config.n_size_float := sst_config.n_size_float + 1; {one more float size}
  if sst_config.n_size_float > sst_size_list_max then begin
    sys_message ('sst', 'config_too_many_float');
    goto read_err;
    end;
  with sst_config.size_float[sst_config.n_size_float]: csize {CSIZE is SIZE_FLOAT entry}
      do begin
    string_token_int (buf, p, csize.size, stat); {get size of this float}
    if sys_error(stat) then goto parm_err; {error reading parameter ?}
    if csize.size <= 0 then goto parm_range_err; {value out of range ?}
    if sst_config.n_size_float > 1 then begin {there was a previous size ?}
      if  csize.size <=                {not bigger than previous float size ?}
          sst_config.size_float[sst_config.n_size_float-1].size
          then begin
        sys_message ('sst', 'config_float_le_previous');
        goto read_err;
        end;
      end;
    csize.name.max := sizeof(csize.name.str); {init data type name var string}
    string_token (buf, p, csize.name, stat); {get data type name}
    al := csize.size;                  {init aligment to its own size}

loop_fp:                               {back here for each new optional keyword}
    string_token (buf, p, parm, stat); {get next keyword}
    if string_eos(stat) then goto done_fp; {exhausted optional keywords ?}
    string_upcase (parm);              {make upper case for keyword matching}
    string_tkpick80 (parm,
      'ALIGN',
      pick);                           {number of keyword picked from list}
    case pick of
1:    begin                            {ALIGN n}
        string_token_int (buf, p, al, stat); {get alignment of this data type}
        if sys_error(stat) then goto parm_err;
        end;
otherwise
      goto parm_err;
      end;
    goto loop_fp;                      {back for next subcommand}
done_fp:                               {done with all the subcommands}

    sst_dtype_new (csize.dtype_p);     {create new data type}
    csize.dtype_p^.dtype := sst_dtype_float_k; {initialize data type descriptor}
    csize.dtype_p^.bits_min := csize.size * sst_config.bits_adr;
    csize.dtype_p^.align_nat := al;
    csize.dtype_p^.align := al;
    csize.dtype_p^.size_used := csize.size;
    csize.dtype_p^.size_align :=
      ((csize.size + al - 1) div al) * al;
    end;                               {done with CSIZE abbreviation}
  end;
{
*   SIZE_FLOAT_MACHINE n
}
6: begin
  string_token_int (buf, p, szi, stat); {get size of this data type}
  if sys_error(stat) then goto parm_err;
  for i := 1 to sst_config.n_size_float do begin {loop thru the float sizes}
    if sst_config.size_float[i].size = szi then begin
      sst_config.float_machine_p := sst_config.size_float[i].dtype_p;
      goto no_err;
      end;
    end;                               {back and try next float size}
  sys_message ('sst', 'config_size_bad');
  goto read_err;
  end;
{
*   SIZE_FLOAT_SINGLE n
}
7: begin
  string_token_int (buf, p, szi, stat); {get size of this data type}
  if sys_error(stat) then goto parm_err;
  for i := 1 to sst_config.n_size_float do begin {loop thru the float sizes}
    if sst_config.size_float[i].size = szi then begin
      sst_config.float_single_p := sst_config.size_float[i].dtype_p;
      goto no_err;
      end;
    end;                               {back and try next float size}
  sys_message ('sst', 'config_size_bad');
  goto read_err;
  end;
{
*   SIZE_FLOAT_DOUBLE n
}
8: begin
  string_token_int (buf, p, szi, stat); {get size of this data type}
  if sys_error(stat) then goto parm_err;
  for i := 1 to sst_config.n_size_float do begin {loop thru the float sizes}
    if sst_config.size_float[i].size = szi then begin
      sst_config.float_double_p := sst_config.size_float[i].dtype_p;
      goto no_err;
      end;
    end;                               {back and try next float size}
  sys_message ('sst', 'config_size_bad');
  goto read_err;
  end;
{
*   SIZE_ENUMERATED_NATIVE n
}
9: begin
  string_token_int (buf, p, szi, stat);
  sst_config.size_enum_native := szi;
  if sys_error(stat) then goto parm_err;
  if sst_config.size_enum_native < 1 then goto parm_range_err;
  end;
{
*   SIZE_BOOLEAN n name
}
10: begin
  string_token_int (buf, p, szi, stat);
  sst_config.size_bool := szi;
  if sys_error(stat) then goto parm_err;
  if sst_config.size_bool < 1 then goto parm_range_err;
  string_token (buf, p, sst_config.name_bool, stat);
  end;
{
*   BITS_CHAR n name
}
11: begin
  string_token_int (buf, p, sst_config.bits_char, stat);
  if sys_error(stat) then goto parm_err;
  if sst_config.bits_char < 1 then goto parm_range_err;
  string_token (buf, p, sst_config.name_char, stat);
  end;
{
*   OS name
}
12: begin
  string_token (buf, p, parm, stat);
  if sys_error(stat) then goto parm_err;
  string_upcase (parm);
  string_tkpick80 (parm,
    'DOMAIN HPUX AIX IRIX SOLARIS WIN32',
    pick);
  case pick of
1: begin
      sst_config.os := sys_os_domain_k;
      sst_config.manuf := sst_manuf_apollo_k;
      end;
2: begin
      sst_config.os := sys_os_hpux_k;
      sst_config.manuf := sst_manuf_hp_k;
      end;
3: begin
      sst_config.os := sys_os_aix_k;
      sst_config.manuf := sst_manuf_ibm_k;
      end;
4: begin
      sst_config.os := sys_os_irix_k;
      sst_config.manuf := sst_manuf_sgi_k;
      end;
5: begin
      sst_config.os := sys_os_solaris_k;
      sst_config.manuf := sst_manuf_sun_k;
      end;
6: begin
      sst_config.os := sys_os_win32_k;
      sst_config.manuf := sst_manuf_none_k;
      end;
otherwise
    goto parm_range_err;
    end;                               {end of operating system name cases}
  os_given := true;                    {indicate OS command was given}
  end;
{
*   LANGUAGE name
}
13: begin
  string_token (buf, p, parm, stat);
  if sys_error(stat) then goto parm_err;
  string_upcase (parm);
  string_tkpick80 (parm,
    'PASCAL C',
    pick);
  case pick of
1: sst_config.lang := sst_lang_pas_k;
2: sst_config.lang := sst_lang_c_k;
otherwise
    goto parm_range_err;
    end;                               {end of language name cases}
  end;
{
*   SUFFIX_FILE_NAME suffix
}
14: begin
  string_token (buf, p, sst_config.suffix_fnam, stat);
  end;
{
*   MAX_SYMBOL_LEN n
}
15: begin
  string_token_int (buf, p, sst_config.sym_len_max, stat);
  if sys_error(stat) then goto parm_err;
  if sst_config.sym_len_max < 1 then goto parm_range_err;
  end;
{
*   CONFIG string
}
16: begin
  string_token (buf, p, sst_config.config, stat);
  end;
{
*   CASE <character case>
}
17: begin
  string_token (buf, p, parm, stat);
  if sys_error(stat) then goto parm_err;
  string_upcase (parm);                {make upper case for keyword list compare}
  string_tkpick80 (parm,
    'UPPER LOWER',
    pick);
  case pick of
1: sst_config.charcase := syn_charcase_up_k;
2: sst_config.charcase := syn_charcase_down_k;
otherwise
    goto parm_range_err;
    end;
  end;
{
*   RESERVED name
}
18: begin
  string_token (buf, p, parm, stat);
  if sys_error(stat) then goto parm_err;
  sz :=                                {amount of memory needed for this chain entry}
    sizeof(ent_p^) - sizeof(ent_p^.s.str) + parm.len;
  util_mem_grab (sz, sst_scope_root_p^.mem_p^, false, ent_p); {grab mem for this name}
  ent_p^.next_p := sst_config.reserve_p; {link new entry to start of chain}
  ent_p^.prev_p := nil;
  if ent_p^.next_p <> nil then begin   {there is a following entry ?}
    ent_p^.next_p^.prev_p := ent_p;    {point old first ent to new chain start}
    end;
  sst_config.reserve_p := ent_p;       {update pointer to new start of chain}
  ent_p^.s.max := parm.len;            {init new var string}
  string_copy (parm, ent_p^.s);        {copy string into new entry}
  end;
{
*   SUFFIX_DATA_TYPE suffix
}
19: begin
  string_token (buf, p, sst_config.suffix_dtype, stat);
  end;
{
*   SUFFIX_CONSTANT suffix
}
20: begin
  string_token (buf, p, sst_config.suffix_const, stat);
  end;
{
*   UNIV_POINTER name
}
21: begin
  string_token (buf, p, sst_config.name_univ_ptr, stat);
  uptr_given := true;
  end;
{
*   SIZE_ENUMERATED_FORCE n
}
22: begin
  string_token_int (buf, p, szi, stat);
  sst_config.size_enum := szi;
  if sys_error(stat) then goto parm_err;
  if sst_config.size_enum < 1 then goto parm_range_err;
  end;
{
*   SIZE_SET n
}
23: begin
  sst_config.n_size_set := sst_config.n_size_set + 1; {one more explicit SET size}
  if sst_config.n_size_set > sst_size_list_max then begin
    sys_message ('sst', 'config_too_many_set');
    goto read_err;
    end;
  with sst_config.size_set[sst_config.n_size_set]: ss do begin {SS is set size entry}
    string_token_int (buf, p, ss.size, stat);
    if sys_error(stat) then goto parm_err;
    if                                 {value not bigger than last SIZE_SET ?}
        (sst_config.n_size_set > 1) and then {there was previous SIZE_SET command ?}
        (ss.size <= sst_config.size_set[sst_config.n_size_set-1].size)
        then begin
      sys_message ('sst', 'config_set_le_previous');
      goto read_err;
      end;
    for i := 1 to sst_config.n_size_int do begin {loop thru the integer sizes}
      if sst_config.size_int[i].size = ss.size {found integer of the right size ?}
        then goto set_size_found;
      end;                             {back and try next integer size}
    sys_message ('sst', 'config_nosuch_int_size');
    goto read_err;

set_size_found:                        {I is index to integer size data}
    ss.dtype_p := sst_config.size_int[i].dtype_p; {copy pointer to data type}
    end;                               {done with SS abbreviation}
  end;
{
*   SIZE_SET_MULTIPLE n
}
24: begin
  string_token_int (buf, p, sst_config.size_set_multiple.size, stat);
  if sys_error(stat) then goto parm_err;
  for i := 1 to sst_config.n_size_int do begin {loop thru the integer sizes}
    if sst_config.size_int[i].size = sst_config.size_set_multiple.size
      then goto set_size_found2;
    end;                               {back and try next integer size}
  sys_message ('sst', 'config_nosuch_int_size');
  goto read_err;

set_size_found2:
  sst_config.size_set_multiple.dtype_p := sst_config.size_int[i].dtype_p;
  end;
{
*   PASS_VAL_SIZE_MAX n
}
25: begin
  string_token_int (buf, p, szi, stat);
  sst_config.pass_val_size_max := szi;
  end;
{
*   ALIGN_MIN_REC n
}
26: begin
  string_token_int (buf, p, szi, stat);
  sst_config.align_min_rec := szi;
  end;
{
*   ALIGN_MIN_PACKED_REC n
}
27: begin
  string_token_int (buf, p, szi, stat);
  sst_config.align_min_rec_pack := szi;
  end;
{
*   Illegal command name.
}
otherwise
    sys_msg_parm_vstr (msg_parm[1], cmd);
    sys_message_parms ('sst', 'config_cmd_bad', msg_parm, 1);
    goto read_err;
    end;                               {end of command name cases}
{
*   Done processing current command.  Now make sure there were no errors and
*   also make sure there are no unsed tokens remaining on the line.
}
  if sys_error(stat) then goto parm_err; {check for parameter error}
no_err:                                {jump here if no error processing command}
  string_token (buf, p, parm, stat);   {try to get one more token from this line}
  if not string_eos(stat) then begin   {there really was another token ?}
    sys_msg_parm_vstr (msg_parm[1], cmd);
    sys_msg_parm_vstr (msg_parm[2], parm);
    sys_message_parms ('sst', 'config_token_extraneous', msg_parm, 2);
    goto read_err;
    end;
  goto next_line;                      {back and process next input line}

parm_err:                              {jump here on error with command parameter}
  sys_msg_parm_vstr (msg_parm[1], cmd);
  sys_error_print (stat, 'sst', 'config_parm_err', msg_parm, 1);
  goto read_err;

parm_range_err:                        {jump here if parameter is out of range}
  sys_msg_parm_vstr (msg_parm[1], cmd);
  sys_message_parms ('sst', 'config_parm_range', msg_parm, 1);
  goto read_err;

read_err:                              {print file name and line number, then bomb}
  sys_msg_parm_int (msg_parm[1], conn.lnum);
  sys_msg_parm_vstr (msg_parm[2], conn.tnam);
  sys_message_bomb ('sst', 'config_fnam_lnum', msg_parm, 2);
{
*   We just encountered end of config file.
}
eof:
  file_close (conn);                   {close config file}
{
*   Make sure all the mandatory information was supplied.
}
  if sst_config.bits_adr = 0 then unspec_err ('BITS_ADR');

  if sst_config.n_size_int = 0 then unspec_err ('SIZE_INT');
  if sst_config.int_machine_p = nil then unspec_err ('SIZE_INT_MACHINE');
  if sst_config.int_adr_p = nil then unspec_err ('SIZE_INT_ADR');
  if not uptr_given then unspec_err ('UNIV_POINTER');

  if sst_config.n_size_float = 0 then unspec_err ('SIZE_FLOAT');
  if sst_config.float_machine_p = nil then unspec_err ('SIZE_FLOAT_MACHINE');
  if sst_config.float_single_p = nil then unspec_err ('SIZE_FLOAT_SINGLE');
  if sst_config.float_double_p = nil then unspec_err ('SIZE_FLOAT_DOUBLE');

  if sst_config.size_bool = 0 then unspec_err ('SIZE_BOOLEAN');
  if sst_config.bits_char = 0 then unspec_err ('BITS_CHAR');
  if not os_given then unspec_err ('OS');
  if sst_config.lang = sst_lang_none_k then unspec_err ('LANGUAGE');
  if sst_config.sym_len_max = 0 then unspec_err ('MAX_SYMBOL_LEN');

  sst_config.size_char :=
    (sst_config.bits_char + sst_config.bits_adr - 1) div sst_config.bits_adr;

  if sst_config.align_min_rec_pack = align_not_set_k then begin {align not speced ?}
    sst_config.align_min_rec_pack :=   {use default for this field}
      sst_config.align_min_rec;
    end;
{
*   Set up the pointers to the mandatory base data types the translator
*   requires internally.  The floating point and integer data types can just
*   reference the appropriate ones from the lists already created.  The others
*   must be created from scratch.
*
*   The data types and symbol descriptors will be created, but the symbols
*   will not be entered into any symbol table.  This will be done by the
*   front/back ends if appropriate.
}
  for i := 1 to sst_config.n_size_int do begin {once for each integer data type}
    with sst_config.size_int[i]: szent do begin {SZENT is this size array entry}
      sst_mem_alloc_scope (sizeof(sym_p^), sym_p); {allocate new symbol descriptor}
      sym_p^.name_in_p := nil;         {set up symbol descriptor}
      sym_p^.name_out_p := univ_ptr(addr(szent.name));
      sym_p^.next_p := nil;
      sym_p^.char_h := char_none_h;
      sym_p^.scope_p := sst_scope_p;
      sym_p^.symtype := sst_symtype_dtype_k;
      sym_p^.flags := [sst_symflag_def_k, sst_symflag_intrinsic_out_k];
      sym_p^.dtype_dtype_p := szent.dtype_p;
      szent.dtype_p^.symbol_p := sym_p; {point data type to symbol descriptor}
      end;                             {done with SZENT abbreviation}
    end;                               {back for next integer data type}

  for i := 1 to sst_config.n_size_float do begin {once for each float data type}
    with sst_config.size_float[i]: szent do begin {SZENT is this size array entry}
      sst_mem_alloc_scope (sizeof(sym_p^), sym_p); {allocate new symbol descriptor}
      sym_p^.name_in_p := nil;         {set up symbol descriptor}
      sym_p^.name_out_p := univ_ptr(addr(szent.name));
      sym_p^.next_p := nil;
      sym_p^.char_h := char_none_h;
      sym_p^.scope_p := sst_scope_p;
      sym_p^.symtype := sst_symtype_dtype_k;
      sym_p^.flags := [sst_symflag_def_k, sst_symflag_intrinsic_out_k];
      sym_p^.dtype_dtype_p := szent.dtype_p;
      szent.dtype_p^.symbol_p := sym_p; {point data type to symbol descriptor}
      end;                             {done with SZENT abbreviation}
    end;                               {back for next float data type}

  sst_mem_alloc_scope (sizeof(dt_p^), dt_p); {allocate new data type descriptor}
  dt_p^ := sst_config.float_machine_p^; {make separate copy for SYS_FP_MACHINE_T}
  sst_config.float_machine_p := dt_p;

  sst_dtype_int_max_p :=               {largest directly available integer}
    sst_config.size_int[sst_config.n_size_int].dtype_p;

  sst_dtype_float_max_p :=             {largest directly available floating point}
    sst_config.size_float[sst_config.n_size_float].dtype_p;

  sst_dtype_new (sst_dtype_uptr_p);    {create universal pointer dtype descriptor}
  sst_dtype_uptr_p^.dtype := sst_dtype_pnt_k;
  sst_dtype_uptr_p^.bits_min := sst_config.int_adr_p^.bits_min;
  sst_dtype_uptr_p^.align_nat := sst_config.int_adr_p^.align_nat;
  sst_dtype_uptr_p^.align := sst_config.int_adr_p^.align;
  sst_dtype_uptr_p^.size_used := sst_config.int_adr_p^.size_used;
  sst_dtype_uptr_p^.size_align := sst_config.int_adr_p^.size_align;
  sst_dtype_uptr_p^.pnt_dtype_p := nil;
  sst_mem_alloc_scope (sizeof(sym_p^), sym_p); {allocate new symbol descriptor}
  sym_p^.name_in_p := nil;             {set up symbol descriptor}
  sym_p^.name_out_p := univ_ptr(addr(sst_config.name_univ_ptr));
  sym_p^.next_p := nil;
  sym_p^.char_h := char_none_h;
  sym_p^.scope_p := sst_scope_p;
  sym_p^.symtype := sst_symtype_dtype_k;
  sym_p^.flags := [sst_symflag_def_k, sst_symflag_intrinsic_out_k];
  sym_p^.dtype_dtype_p := sst_dtype_uptr_p; {point symbol to data type descriptor}
  sst_dtype_uptr_p^.symbol_p := sym_p; {point data type to symbol descriptor}

  sst_dtype_new (sst_dtype_bool_p);    {create boolean data type descriptor}
  sst_dtype_bool_p^.dtype := sst_dtype_bool_k;
  sst_dtype_bool_p^.bits_min := 1;
  sst_dtype_bool_p^.align_nat := sst_config.size_bool;
  sst_dtype_bool_p^.align := sst_config.size_bool;
  sst_dtype_bool_p^.size_used := sst_config.size_bool;
  sst_dtype_bool_p^.size_align := sst_config.size_bool;
  sst_mem_alloc_scope (sizeof(sym_p^), sym_p); {allocate new symbol descriptor}
  sym_p^.name_in_p := nil;             {set up symbol descriptor}
  sym_p^.name_out_p := univ_ptr(addr(sst_config.name_bool));
  sym_p^.next_p := nil;
  sym_p^.char_h := char_none_h;
  sym_p^.scope_p := sst_scope_p;
  sym_p^.symtype := sst_symtype_dtype_k;
  sym_p^.flags := [sst_symflag_def_k, sst_symflag_intrinsic_out_k];
  sym_p^.dtype_dtype_p := sst_dtype_bool_p; {point symbol to data type descriptor}
  sst_dtype_bool_p^.symbol_p := sym_p; {point data type to symbol descriptor}

  sst_dtype_new (sst_dtype_char_p);    {create character data type descriptor}
  sst_dtype_char_p^.dtype := sst_dtype_char_k;
  sst_dtype_char_p^.bits_min := sst_config.size_char * sst_config.bits_adr;
  sst_dtype_char_p^.align_nat := sst_config.size_char;
  sst_dtype_char_p^.align := sst_config.size_char;
  sst_dtype_char_p^.size_used := sst_config.size_char;
  sst_dtype_char_p^.size_align := sst_config.size_char;
  sst_mem_alloc_scope (sizeof(sym_p^), sym_p); {allocate new symbol descriptor}
  sym_p^.name_in_p := nil;             {set up symbol descriptor}
  sym_p^.name_out_p := univ_ptr(addr(sst_config.name_char));
  sym_p^.next_p := nil;
  sym_p^.char_h := char_none_h;
  sym_p^.scope_p := sst_scope_p;
  sym_p^.symtype := sst_symtype_dtype_k;
  sym_p^.flags := [sst_symflag_def_k, sst_symflag_intrinsic_out_k];
  sym_p^.dtype_dtype_p := sst_dtype_char_p; {point symbol to data type descriptor}
  sst_dtype_char_p^.symbol_p := sym_p; {point data type to symbol descriptor}
{
*   Figure out how enumerated types will be handled.  The fields
*   SIZE_ENUM and SIZE_ENUM_NATIVE have been set if commands were supplied.
*   Otherwise they are set to zero to indicate no commands were supplied.
}
  if sst_config.size_enum = 0          {no SIZE_ENUMERATED_FORCE command given ?}
    then sst_config.size_enum := sst_config.size_enum_native; {use native size}
  if sst_config.size_enum = 0          {no SIZE_ENUMERATED_NATIVE command either ?}
    then sst_config.size_enum :=       {pick the size from the smallest integer}
      sst_config.size_int[1].size;
  sst_dtype_enum_p := nil;             {init to using native enum type directly}

  if sst_config.size_enum <> sst_config.size_enum_native then begin {use integer ?}
    for i := 1 to sst_config.n_size_int do begin {once for each size integer}
      if sst_config.size_int[i].size = sst_config.size_enum then begin {found it ?}
        sst_dtype_enum_p := sst_config.size_int[i].dtype_p;
        goto enum_done;
        end;
      end;                             {back and check next size integer for match}
    sys_msg_parm_int (msg_parm[1], sst_config.size_enum);
    sys_message_bomb ('sst', 'config_enum_size_bad', msg_parm, 1);
    end;                               {done handling not use native enum data type}
enum_done:                             {done handling enumerated type issues}

  if sst_config.pass_val_size_max = 0 then begin {no PASS_VAL_SIZE_MAX command ?}
    sst_config.pass_val_size_max :=    {default to size of maximum size integer}
      sst_config.size_int[sst_config.n_size_int].size;
    end;
  end;
