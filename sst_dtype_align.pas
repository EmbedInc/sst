{   Subroutine SST_DTYPE_ALIGN (DTYPE,RULE_ALIGN)
*
*   Set the final alignment used for this data type.  The following state will be
*   be used as input:
*
*     RULE_ALIGN  -  The alignment rule to apply.  This may be SST_ALIGN to
*       get the current default alignment rule.
*
*     DTYPE.ALIGN_NAT  -  Alignment that this data type would have if naturally
*       aligned.
*
*     DTYPE.SIZE_USED  -  Size of whole data type from first used bit to last used
*       bit.  This value is the minimum number of machine address increments that
*       could possibly hold the data type.  This is the value returned by the
*       SIZEOF function.
*
*   The following state is set by this subroutine:
*
*     DTYPE.ALIGN  -  Actual alignment chosen for this data type.  This will be
*       a number >= 0, and therefore will not be a special flag value.
*
*     DTYPE.SIZE_ALIGN  -  Size of whole data type in machine address increments,
*       rounded up to nearest multiple of its own alignment.  This would be the
*       size to allocate for each array element if an array of this data type
*       was made.
}
module sst_DTYPE_ALIGN;
define sst_dtype_align;
%include 'sst2.ins.pas';

procedure sst_dtype_align (            {set data type alignment}
  in out  dtype: sst_dtype_t;          {data type to set alignment for}
  in      rule_align: sys_int_machine_t); {alignment rule to apply}

var
  dt_p: sst_dtype_p_t;                 {points to base data type}

begin
  if rule_align = sst_align_natural_k
    then begin                         {alignment rule is NATURAL ALIGNMENT}
      dtype.align := dtype.align_nat;
      end
    else begin                         {alignment rule is explicit alignment}
      dtype.align := rule_align;
      end
    ;

  dt_p := addr(dtype);                 {resolve pointer to base data type}
  while dt_p^.dtype = sst_dtype_copy_k do dt_p := dt_p^.copy_dtype_p;
  if dt_p^.dtype = sst_dtype_rec_k then begin {this data type is a record ?}
    if dtype.align_nat = 0
      then begin                       {this is a packed record}
        dtype.align :=
          max(dtype.align, sst_config.align_min_rec_pack);
        end
      else begin                       {this is a non-packed record}
        dtype.align :=
          max(dtype.align, sst_config.align_min_rec);
        end
      ;
    end;

  if dtype.align <= 1
    then begin                         {no unused addresses possible}
      dtype.size_align := dtype.size_used;
      end
    else begin                         {need to round to alignment multiple}
      dtype.size_align := dtype.align *
        ((dtype.size_used + dtype.align - 1) div dtype.align)
      end
    ;
  end;
