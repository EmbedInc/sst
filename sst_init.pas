{   Subroutine SST_INIT (SYMBOL_LEN,PARENT_MEM)
*
*   Init the translator data structures.  This must be the first SST call in an
*   application.  SYMBOL_LEN is the maximum length of any symbol.  This is used
*   to configure the hash tables.  PARENT_MEM is the memory context under which
*   a new memory context will be created.  All translator dynamic memory will be
*   allocated subordinate to this new context.
}
module sst_INIT;
define sst_init;
%include 'sst2.ins.pas';

procedure sst_init (                   {init translator data stuctures}
  in      symbol_len: sys_int_machine_t; {max length allowed for a symbol}
  in out  parent_mem: util_mem_context_t); {parent memory context to use}

var
  m_p: util_mem_context_p_t;           {scratch memory context pointer}

begin
  max_symbol_len := symbol_len;        {save max allowed symbol length}
  sst_hash_buckets := 128;             {number of buckets for new hash tables}

  util_mem_context_get (parent_mem, mem_p); {create our own memory context}
  mem_p^.pool_size := sst_mem_pool_size; {set size we want for our memory pools}
  mem_p^.max_pool_chunk := sst_mem_pool_chunk; {max size chunk to alloc from pool}
{
*   Init top level scope.
}
  util_mem_context_get (mem_p^, m_p);  {make mem context for top scope}
  util_mem_grab (                      {allocate top scope block}
    sizeof(sst_scope_p^), m_p^, false, sst_scope_p);
  sst_scope_p^.mem_p := m_p;           {set mem context for this scope}
  sst_scope_p^.parent_p := nil;        {top scope has no parent}
  sst_scope_p^.symbol_p := nil;
  sst_scope_p^.flag_ref_used := false; {init to not flag references symbols as used}
  string_hash_create (                 {init input name hash table for top scope}
    sst_scope_p^.hash_h,               {handle to new hash table}
    512,                               {number of buckets in top hash table}
    max_symbol_len,                    {max length of any entry name in hash table}
    sizeof(sst_symbol_p_t),            {amount of user memory for hash entries}
    [ string_hashcre_memdir_k,         {use parent memory context directly}
      string_hashcre_nodel_k],         {won't need to individually deallocate entries}
    sst_scope_p^.mem_p^);              {parent memory context for hash table}
  string_hash_create (                 {init output name hash table for top scope}
    sst_scope_p^.hash_out_h,           {handle to new hash table}
    512,                               {number of buckets in top hash table}
    max_symbol_len,                    {max length of any entry name in hash table}
    sizeof(sst_symbol_p_t),            {amount of user memory for hash entries}
    [ string_hashcre_memdir_k,         {use parent memory context directly}
      string_hashcre_nodel_k],         {won't need to individually deallocate entries}
    sst_scope_p^.mem_p^);              {parent memory context for hash table}

  sst_names_p := sst_scope_p;          {init lowest name space to same as curr scope}
  sst_scope_root_p := sst_scope_p;     {set pointer to root scope}
  sst_align := sst_align_natural_k;    {init current alignment rule}

  sst_opc_first_p := nil;              {init to no opcodes in opcodes list}
  sst_opc_p := nil;
  sst_opc_next_pp := addr(sst_opc_first_p);
{
*   Init the front end call table to "default" routines.
}
  sst_r.doit := nil;
{
*   Init back end call table to "default" routines.
}
  sst_w.allow_break := addr(sst_out_allow_break);
  sst_w.append := addr(sst_out_append);
  sst_w.appendn := addr(sst_out_appendn);
  sst_w.appends := addr(sst_out_appends);
  sst_w.append_sym_name := addr(sst_out_append_sym_name);
  sst_w.blank_line := addr(sst_out_blank_line);
  sst_w.break := addr(sst_out_break);
  sst_w.comment_end := addr(sst_out_comment_end);
  sst_w.comment_set := addr(sst_out_comment_set);
  sst_w.comment_start := addr(sst_out_comment_start);
  sst_w.delimit := addr(sst_out_delimit);
  sst_w.doit := nil;
  sst_w.indent := addr(sst_out_indent);
  sst_w.line_close := addr(sst_out_line_close);
  sst_w.line_insert := addr(sst_out_line_insert);
  sst_w.line_new := addr(sst_out_line_new);
  sst_w.line_new_cont := addr(sst_out_line_new_cont);
  sst_w.name := addr(sst_out_name);
  sst_w.name_sym := addr(sst_out_name_sym);
  sst_w.notify_src_range := addr(sst_out_notify_src_range);
  sst_w.tab_indent := addr(sst_out_tab_indent);
  sst_w.undent := addr(sst_out_undent);
  sst_w.undent_all := addr(sst_out_undent_all);
  sst_w.write := addr(sst_out_write);
{
*   Init state controlling writing characters to internal description of
*   output file.
}
  util_mem_context_get (mem_p^, sst_out.mem_p); {make mem context for output lines}
  sst_out.first_str_p := nil;
  sst_out.wrap_len := 60;
  sst_out.indent_size := 2;
  sst_out.dyn_p := addr(sst_out_dyn);

  sst_out.dyn_p^.str_p := nil;
  sst_out.dyn_p^.indent_chars := 0;
  sst_out.dyn_p^.indent_level := 0;
  sst_out.dyn_p^.break_len := 0;
  sst_out.dyn_p^.break_start := 0;
  sst_out.dyn_p^.wpos := sst_wpos_before_k;
  sst_out.dyn_p^.comm.max := sizeof(sst_out.dyn_p^.comm.str);
  sst_out.dyn_p^.comm.len := 0;
  sst_out.dyn_p^.commented_pos := 0;

  sst_out.comm_start.max := sizeof(sst_out.comm_start.str);
  sst_out.comm_start.len := 0;
  sst_out.comm_end.max := sizeof(sst_out.comm_end.str);
  sst_out.comm_end.len := 0;
  sst_out.comm_pos := 40;
{
*   Init the front/back end stack.  We set up this stack, it is exclusively for
*   use by the front and back ends.
}
  util_stack_alloc (mem_p^, sst_stack); {create the stack}
{
*   Init other stuff in the common block.
}
  sst_dtype_none.symbol_p := nil;
  sst_dtype_none.dtype := sst_dtype_undef_k;
  sst_dtype_none.bits_min := 0;
  sst_dtype_none.align_nat := 1;
  sst_dtype_none.align := 1;
  sst_dtype_none.size_used := 0;
  sst_dtype_none.size_align := 0;
  end;
