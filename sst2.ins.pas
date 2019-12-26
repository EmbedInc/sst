{   Private include file used by the base translator routines.  Routines that
*   are specific to reading or writing a particular language do not include
*   this file.
}
%include 'sys.ins.pas';
%include 'util.ins.pas';
%include 'string.ins.pas';
%include 'file.ins.pas';
%include 'syn.ins.pas';
%include 'sst.ins.pas';

const
  sst_mem_pool_size = 4096;            {size of memory pools allocated at a time}
  sst_mem_pool_chunk = 256;            {max size chunk allowed to use from pool}
{
*   Declare common block where current translator state is kept.
}
var (sst2)
  mem_p: util_mem_context_p_t;         {points to top level SST memory context}
  max_symbol_len: sys_int_machine_t;   {max allowed input symbol name}
{
*   Private entry point declarations.
}
procedure sst_out_allow_break;         {allow line break at current position}
  extern;

procedure sst_out_append (             {append characters for current statement}
  in      str: univ string_var_arg_t); {the characters to append}
  extern;

procedure sst_out_appendn (            {append N characters to current output line}
  in      chars: univ string;          {the characters to append}
  in      n_chars: string_index_t);    {the number of characters to append}
  extern;

procedure sst_out_appends (            {append string to current output line}
  in      s: string);                  {string to append, trailing blanks ignored}
  extern;

procedure sst_out_append_sym_name (    {append symbol output name to curr position}
  in      sym: sst_symbol_t);          {symbol descriptor to write name of}
  extern;

procedure sst_out_blank_line;          {make sure preceeding line is blank}
  extern;

procedure sst_out_break;               {break line at current position}
  extern;

procedure sst_out_comment_end;         {write end of comment string}
  extern;

procedure sst_out_comment_set (        {set comment to correspond to next char}
  in      s: univ string_var_arg_t);   {body of comment string}
  extern;

procedure sst_out_comment_start;       {write start of comment string}
  extern;

procedure sst_out_config;              {set target machine configuration data}
  extern;

procedure sst_out_delimit;             {write delim or break line before next output}
  extern;

procedure sst_out_indent;              {increase indentation by one level}
  extern;

procedure sst_out_line_close;          {close current line}
  extern;

procedure sst_out_line_insert;         {raw insert new line at current position}
  extern;

procedure sst_out_line_new;            {set up so next char goes on next line}
  extern;

procedure sst_out_line_new_cont;       {set up for next line is continuation line}
  extern;

procedure sst_out_name (               {make output symbol name from input name}
  in      name_in: univ string;        {input symbol name characters}
  in      name_in_len: string_index_t; {number of characters in NAME_IN}
  in      ext: univ string;            {output name suffix, if any}
  in      ext_len: string_index_t;     {number of characters in EXT}
  in      rename: sst_rename_k_t;      {what kind of re-naming is allowed}
  in out  name_out: univ string_var_arg_t; {resulting output source symbol name}
  out     pos: string_hash_pos_t);     {pos handle where name goes in symbol table}
  extern;

procedure sst_out_name_sym (           {set output name for an existing symbol}
  in out  sym: sst_symbol_t);          {NOP if symbol already has output name}
  extern;

procedure sst_out_notify_src_range (   {declare source chars range for new output}
  in      str_h: syn_string_t);        {new out chars related to these in chars}
  extern;

procedure sst_out_tab_indent;          {tab to current indentation level}
  extern;

procedure sst_out_undent;              {decrease indentation by one level}
  extern;

procedure sst_out_undent_all;          {reset to no indentation level}
  extern;

procedure sst_out_write (              {write output lines from memory to file}
  in out  conn: file_conn_t;           {connection handle to output file}
  out     stat: sys_err_t);            {completion status code}
  extern;
