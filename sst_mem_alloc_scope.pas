{   Subroutine SST_MEM_ALLOC_SCOPE (SIZE,P)
*
*   Allocate memory that will be tied to the current most local scope.   SIZE
*   is the amount of memory to allocate, and P is returned pointing to the
*   start of the new memory area.
}
module sst_MEM_ALLOC_SCOPE;
define sst_mem_alloc_scope;
%include 'sst2.ins.pas';

procedure sst_mem_alloc_scope (        {allocate memory tied to current scope}
  in      size: sys_int_adr_t;         {amount of memory to allocate}
  out     p: univ_ptr);                {pointer to start of new memory area}

begin
  util_mem_grab (size, sst_scope_p^.mem_p^, false, p);
  end;
