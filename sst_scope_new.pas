{   Subroutine SST_SCOPE_NEW
*
*   Create and initialize a new current scope.  The previous current scope will
*   be the new parent scope.
}
module sst_SCOPE_NEW;
define sst_scope_new;
%include 'sst2.ins.pas';

procedure sst_scope_new;               {create new scope subordinate to curr scope}

var
  mem_p: util_mem_context_p_t;         {points to memory context for new scope}
  scope_p: sst_scope_p_t;              {points to newly created scope}

begin
  util_mem_context_get (sst_scope_p^.mem_p^, mem_p); {create new mem context}
  util_mem_grab (sizeof(scope_p^), mem_p^, false, scope_p); {alloc scope descriptor}
  scope_p^.mem_p := mem_p;             {save mem context handle for new scope}
  string_hash_create (                 {init hash table for input symbols this level}
    scope_p^.hash_h,                   {handle to new hash table}
    64,                                {number of buckets in this hash table}
    max_symbol_len,                    {max length of any entry name in hash table}
    sizeof(sst_symbol_p_t),            {amount of user memory for hash entries}
    [ string_hashcre_memdir_k,         {use parent memory context directly}
      string_hashcre_nodel_k],         {won't need to individually deallocate entries}
    scope_p^.mem_p^);                  {parent memory context for hash table}
  string_hash_create (                 {init hash table for output symbols this level}
    scope_p^.hash_out_h,               {handle to new hash table}
    64,                                {number of buckets in this hash table}
    max_symbol_len,                    {max length of any entry name in hash table}
    sizeof(sst_symbol_p_t),            {amount of user memory for hash entries}
    [ string_hashcre_memdir_k,         {use parent memory context directly}
      string_hashcre_nodel_k],         {won't need to individually deallocate entries}
    scope_p^.mem_p^);                  {parent memory context for hash table}
  scope_p^.parent_p := sst_scope_p;    {link new scope to its parent}
  scope_p^.symbol_p := nil;            {init to no symbol for this scope level}
  scope_p^.flag_ref_used := false;     {init to not flag references symbols as used}
  sst_scope_p := scope_p;              {make new scope the current scope}
  sst_names_p := scope_p;              {set namespace to same as scope}
  end;
