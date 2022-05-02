{   SST <input file name> [ options ]
*
*   Source-to-source translator program.  The input source code will be translated
*   to the output source code.  Valid command line options are:
*
*   -CONFIG <config file name>
*
*     Specify the target machine configuration file name.  This "option"
*     is mandatory.  There is no default.
*
*   -OUT <output file name>
*
*     Explicitly specify the output file name.  The default output file will
*     go into the current working directory with its generic name the same as
*     the input file's generic leafname.  The output file name will always
*     end with the suffix specified in the configuration file, even if
*     specified explicitly here.
*
*   -DEBUG level
*
*     Specify the desired conditional compilation debugging level.  Level 0
*     specifies to ignore any optional debugging source code.  Levels 1-n
*     specify successively more debugging code be used.  The exact behaviour
*     depends on the particular front ends.  The selected debug level number
*     is made available to the front end.  The default is 0.
*
*   -SHOW_UNUSED level
*   -SHOW_UNUSED ALL
*
*     Control listing of unused symbols.  Level indicates how many nested files
*     down unused symbols may be listed from.  A level of 0 prevents all
*     unused symbols from being listed.  Level 1 causes them only to be listed
*     from the top source file (not from any include files).  Level 2
*     lists unused symbols from the first level of include file, etc.
*     The special level ID "ALL" causes unused symbols to be listed without
*     regard to their source file nesting level.  The default is level set
*     to 1 (list only the unused symbols declared in the top source file).
*
*   -TIME
*
*     This options causes timing information to be written after translation
*     is complete.  The default is not to show any timing information.
*
*   -LOCAL_INS
*
*     Cause local copies of include files to be used, when available.
*     If a file exists in the current working directory that has the
*     same name as the leafname of an include file, the local file will
*     be used instead of the include file actually specified.  This can
*     be useful when include files are reserved with DSEE.  The default
*     is to use the include file name exactly as specified.
*
*   -INS <source file name>
*
*     Indicate that the purpose of running SST is to translate the indicated
*     include file.  All symbols, and only those symbols defined in this
*     file will be written to the output file.  This file named here may
*     be the same as the input file.
*
*   -WRITE_ALL
*
*     All symbols, implicit and otherwise, will be written to the output
*     file.  This is intended for translating top level include files.
*
*   -UNAME name
*
*     Set unique name string to use in implicitly created output variables.
*
*   -GUI
*
*     Indicates that this module is part of a program that interacts with
*     the user thru graphics, not just the command line.  Some systems
*     (like Microsoft Windows) have different interfaces to the main
*     program routine for a GUI application than for a command line
*     application.  This switch only effects the interface and some
*     initialization of the main program routine.  It is ignored for target
*     systems that make no distinction between command line and GUI
*     applications, and for all routines except the main program routine.
*     By default, SST assumes the final program is a command line, as
*     apposed to a GUI, application.
}
program sst;
%include 'sst2.ins.pas';

const
  max_msg_parms = 8;                   {max parameters we can pass to a message}

var
  fnam_in,                             {input file name from command line}
  fnam_out,                            {output file name}
  fnam_config,                         {target machine configuration file}
  gnam:                                {generic leaf name of input file}
    %include '(cog)lib/string_treename.ins.pas';
  ext:
    %include '(cog)lib/string80.ins.pas';
  stack_loc: util_stack_loc_handle_t;  {unused subroutine return argument}
  i: sys_int_machine_t;                {scratch integer and loop counter}
  univ_p: univ_ptr;                    {scratch arbitrary pointer}
  timing: boolean;                     {TRUE if supposed to print timing info}
  uname_set: boolean;                  {TRUE if -UNAME command line option used}
  timer_all: sys_timer_t;              {timer for whole program}
  timer_front: sys_timer_t;            {timer for front end}
  timer_back: sys_timer_t;             {timer for back end}
  sec_all, sec_front, sec_back, sec_base: real; {seconds for parts of the program}
  pc_all, pc_front, pc_back, pc_base: real; {percent time for parts of the program}

  opt:                                 {command line option name}
    %include '(cog)lib/string32.ins.pas';
  parm:                                {command line option parameter}
    %include '(cog)lib/string_treename.ins.pas';
  pick: sys_int_machine_t;             {number of token picked from list}
  msg_parm:                            {message parameter references}
    array[1..max_msg_parms] of sys_parm_msg_t;
  stat: sys_err_t;                     {completion status code}

  str_all: string_var4_t :=            {ALL command line option parameter}
    [max := sizeof(str_all.str), len := 3, str := 'ALL'];

label
  next_opt, done_opts, found_dot;

begin
  sys_timer_init (timer_all);          {init timer for whole program}
  sys_timer_start (timer_all);         {start timing the whole program}
  sys_timer_init (timer_front);        {init the other timers}
  sys_timer_init (timer_back);

  string_cmline_init;                  {init for command line processing}
  string_cmline_token (fnam_in, stat); {get input file name from command line}
  string_cmline_req_check (stat);      {input file name is required on command line}
{
*   Init to defaults before processing command line options.
}
  sst_level_debug := 0;
  sst_level_unused := 1;
  timing := false;
  sst_local_ins := false;
  sst_ins := false;
  sst_ins_tnam.max := sizeof(sst_ins_tnam.str);
  sst_ins_tnam.len := 0;
  sst_writeall := false;
  sst_oname_unique.max := sizeof(sst_oname_unique);
  sst_oname_unique.len := 0;
  uname_set := false;
  sys_cognivis_dir ('lib', fnam_config);
  string_appends (fnam_config, '/config_sst'(0));
  sst_gui := false;
{
*   Back here for each new command line option.
}
next_opt:
  string_cmline_token (opt, stat);     {get next command line option name}
  if string_eos(stat) then goto done_opts; {exhausted command line ?}
  sys_error_abort (stat, 'string', 'cmline_opt_err', nil, 0);
  string_upcase (opt);                 {make upper case for matching list}
  string_tkpick80 (                    {pick option name from list}
    opt,                               {option name}
    '-OUT -CONFIG -DEBUG -SHOW_UNUSED -TIME -LOCAL_INS -INS -WRITE_ALL -UNAME -GUI',
    pick);                             {number of picked option}
  case pick of                         {do routine for specific option}
{
*   -OUT <output file name>
}
1: begin
  string_cmline_token (fnam_out, stat);
  end;
{
*   -CONFIG <config file name>
}
2: begin
  string_cmline_token (fnam_config, stat);
  end;
{
*   -DEBUG level
}
3: begin
  string_cmline_token_int (sst_level_debug, stat);
  sst_level_debug := max(sst_level_debug, 0); {clip negative numbers to zero}
  end;
{
*   -SHOW_UNUSED level
}
4: begin
  string_cmline_token (parm, stat);
  string_cmline_parm_check (stat, opt);
  string_upcase (parm);
  if string_equal(parm, str_all)
    then begin                         {parm was "ALL"}
      sst_level_unused := sst_level_unused_all_k;
      end
    else begin                         {parm wasn't "ALL", it better be a number}
      string_t_int (parm, sst_level_unused, stat);
      sst_level_unused := max(sst_level_unused, 0); {prevent negative numbers}
      end
    ;
  end;
{
*   -TIME
}
5: begin
  timing := true;
  end;
{
*   -LOCAL_INS
}
6: begin
  sst_local_ins := true;
  end;
{
*   -INS fnam
}
7: begin
  string_cmline_token (parm, stat);
  string_cmline_parm_check (stat, opt);
  string_treename (parm, sst_ins_tnam);
  sst_ins := true;
  sst_writeall := false;
  end;
{
*   -WRITE_ALL
}
8: begin
  sst_writeall := true;
  sst_ins := false;
  end;
{
*   -UNAME
}
9: begin
  string_cmline_token (sst_oname_unique, stat);
  uname_set := true;
  end;
{
*   -GUI
}
10: begin
  sst_gui := true;
  end;
{
*   Illegal command line option.
}
otherwise
    string_cmline_opt_bad;             {complain about bad OPT and bomb}
    end;                               {end of command line option case statement}

  string_cmline_parm_check (stat, opt); {check for parameter error}
  goto next_opt;                       {back for next command line option}
done_opts:                             {done with all the command line options}

  if (not uname_set) and sst_ins then begin {need default unique name ?}
    string_generic_fnam (sst_ins_tnam, ''(0), parm); {make leafname of -INS file}
    for i := 1 to parm.len do begin    {once for each character in PARM}
      if parm.str[i] = '.'
        then begin                     {hit . character in file name}
          exit;
          end
        else begin                     {regular file name character}
          string_append1 (sst_oname_unique, parm.str[i]);
          end
        ;
      end;                             {back for next input token character}
    end;                               {done defaulting -UNAME}
{
*   Done reading command line.
*
*   Initialize the back end.
}
  sst_init (                           {init SST library}
    40,                                {max length of any symbol}
    util_top_mem_context);             {parent memory context}
  sst_config_out (fnam_config);        {get target machine configuration info}

  case sst_config.lang of              {chose back end driver and initialize it}
sst_lang_c_k: sst_w_c_init;            {output language is C}
otherwise
    sys_msg_parm_int (msg_parm[1], ord(sst_config.lang));
    sys_message_bomb ('sst', 'language_unsupported', msg_parm, 1);
    end;
{
*   Initialize the front end.
*   The choice of front end language comes from the input file name suffix.
}
  for i := fnam_in.len downto 1 do begin {scan input fnam backwards, look for "."}
    if fnam_in.str[i] = '.' then goto found_dot; {found "." suffix separator ?}
    end;
  sys_msg_parm_vstr (msg_parm[1], fnam_in);
  sys_message_bomb ('sst', 'fnam_input_no_suffix', msg_parm, 1);
found_dot:                             {found suffix separator character}
  string_substr (fnam_in, i+1, fnam_in.len, parm); {extract file name suffix}
  string_upcase (parm);                {make suffix case-insensitive}
  string_tkpick80 (parm,               {pick suffix from supported list}
    'PAS COG SYO SYN',
    pick);
  case pick of
1, 2: begin                            {PAS or COG}
      sst_r_pas_init;
      end;
3:  begin                              {SYO}
      sst_r_syo_init;
      end;
4:  begin                              {SYN}
      sst_r_syn_init;
      end;
otherwise
    sys_msg_parm_vstr (msg_parm[1], parm);
    sys_message_bomb ('sst', 'language_input_unrecognized', msg_parm, 1);
    end;                               {end of input file suffix cases}
{
*   Run the front end.
*   The input source code will be read and "compiled" into data structures
*   in memory.
}
  sys_timer_start (timer_front);       {start timer for front end}
  sst_r.doit^ (fnam_in, gnam, stat);
  sys_timer_stop (timer_front);        {stop timer for front end}

  if sys_stat_match (sst_subsys_k, sst_stat_err_handled_k, stat) then begin
    sys_exit_error;                    {exit quietly with error condition}
    end;
  sys_error_abort (stat, 'sst', 'readin', nil, 0);
{
*   Reset state between running the front and back ends.
}
  sst_scope_p := sst_scope_root_p;     {reset current scope to root scope}
  sst_names_p := sst_scope_p;
  sst_opc_p := sst_opc_first_p;        {reset current opcode to first in list}
  sst_opc_next_pp := nil;              {this should no longer be used}
  util_stack_loc_start (sst_stack, stack_loc, univ_p); {get address of stack start}
  util_stack_popto (sst_stack, univ_p); {reset stack to empty}
  sst_flag_used_opcodes (sst_opc_first_p); {propagate USED flags to all used symbols}
{
*   Run the back end.
*   The data structures in memory will be used to write the output source code.
}
  if fnam_out.len > 0 then begin       {output file name was explicitly specified ?}
    string_copy (fnam_out, gnam);      {explicitly set generic output file name}
    end;
  string_copy (sst_config.suffix_fnam, ext); {make output file suffix in simple str}
  string_fill (ext);
  string_fnam_extend (gnam, ext.str, fnam_config); {make extended file name}
  string_treename (fnam_config, fnam_out); {make full output file tree name}
  file_delete_name (fnam_out, stat);   {delete output file before trying to write it}
  discard( file_not_found(stat) );     {OK if output file not already present}
  sys_msg_parm_vstr (msg_parm[1], fnam_out);
  sys_error_abort (stat, 'file', 'delete', msg_parm, 1);

  sys_timer_start (timer_back);        {start timer for back end}
  sst_w.doit^ (fnam_out, stat);        {create the output source code}
  sys_timer_stop (timer_back);         {stop timer for back end}

  if sys_stat_match (sst_subsys_k, sst_stat_err_handled_k, stat) then begin
    sys_exit_error;                    {exit quietly with error condition}
    end;
  sys_error_abort (stat, 'sst', 'writeout', nil, 0);
{
*   Print timing info, if requested.
}
  sys_timer_stop (timer_all);          {stop timing the whole program}

  if timing then begin                 {user requested timing info be printed ?}
    sec_all := sys_timer_sec(timer_all); {seconds for whole program}
    sec_front := sys_timer_sec(timer_front); {seconds for front end}
    sec_back := sys_timer_sec(timer_back); {seconds for back end}
    sec_base := sec_all - sec_front - sec_back; {seconds for base translator}

    pc_all := 100.0;                   {make times in percent of total}
    pc_front := 100.0 * sec_front / sec_all;
    pc_back := 100.0 * sec_back / sec_all;
    pc_base := 100.0 * sec_base / sec_all;

    sys_msg_parm_real (msg_parm[1], sec_front);
    sys_msg_parm_real (msg_parm[2], pc_front);
    sys_msg_parm_real (msg_parm[3], sec_base);
    sys_msg_parm_real (msg_parm[4], pc_base);
    sys_msg_parm_real (msg_parm[5], sec_back);
    sys_msg_parm_real (msg_parm[6], pc_back);
    sys_msg_parm_real (msg_parm[7], sec_all);
    sys_msg_parm_real (msg_parm[8], pc_all);

    sys_message_parms ('sst', 'timing_info', msg_parm, 8); {print timing info}
    end;
  end.
