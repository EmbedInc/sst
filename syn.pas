{   SYN <input file name> [ options ]
*
*   Version of the SST program specific to SYN input files.
}
program syn;
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
  next_opt, done_opts;

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
    '-OUT -CONFIG -DEBUG -SHOW_UNUSED -TIME',
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
}
  sst_r_syn_init;                      {init the SYN interpretation front end}
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
