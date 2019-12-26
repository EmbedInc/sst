{   Subroutine SST_MEM_ALLOC_NAMESP (SIZE,P)
*
*   Allocate memory that will be tied to the current most local name space.
*   SIZE is the amount of memory to allocate, and P is returned pointing to the
*   start of the new memory area.
}
module sst_MEM_ALLOC_NAMESP;
define sst_mem_alloc_namesp;
%include 'sst2.ins.pas';

procedure sst_mem_alloc_namesp (       {allocate memory tied to current name space}
  in      size: sys_int_adr_t;         {amount of memory to allocate}
  out     p: univ_ptr);                {pointer to start of new memory area}

begin
  util_mem_grab (size, sst_names_p^.mem_p^, false, p);
  end;
