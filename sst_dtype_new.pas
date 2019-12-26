{   Subroutine SST_DTYPE_NEW (DTYPE_P)
*
*   Allocate memory under the current scope for a new data type descriptor.  The
*   descriptor will be initialized, and DTYPE_P will be returned pointing to it.
}
module sst_DTYPE_NEW;
define sst_dtype_new;
%include 'sst2.ins.pas';

procedure sst_dtype_new (              {allocate new data type in current scope}
  out     dtype_p: sst_dtype_p_t);     {pnt to new created and initted block}

begin
  sst_mem_alloc_scope (sizeof(dtype_p^), dtype_p); {allocate mem for new data type}
  with dtype_p^:d do begin             {D is new data type descriptor block}
    d.symbol_p := nil;
    d.dtype := sst_dtype_undef_k;
    d.bits_min := 0;
    d.align_nat := 0;
    d.align := 0;
    d.size_used := 0;
    d.size_align := 0;
    end;                               {done with D abbreviation}
  end;
