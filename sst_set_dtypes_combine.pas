{   Subroutine SST_SET_DTYPES_COMBINE (DT_IN1,DT_IN2,DT_OUT_P)
*
*   Make a composite data type, if possible, from two SET data types.
*   DT_IN1 and DT_IN2 are the data type descriptors for the two sets.
*   DT_OUT_P will be returned pointing to a composite data type that is a
*   superset of both SET data types.  DT_OUT_P will be returned NIL if no
*   superset exists.
*
*   It is assumed that DT_IN1 and DT_IN2 are SET data types.  Results are
*   undefined when they are not.
}
module sst_SET_DTYPES_COMBINE;
define sst_set_dtypes_combine;
%include 'sst2.ins.pas';

procedure sst_set_dtypes_combine (     {make composite data type from two set dtypes}
  in      dt_in1: sst_dtype_t;         {data type descriptor for first set}
  in      dt_in2: sst_dtype_t;         {data type descriptor for second set}
  out     dt_out_p: sst_dtype_p_t);    {pnt to combined dtype, NIL = incompatible}

var
  dt1s_p, dt2s_p: sst_dtype_p_t;       {point to set base data types}
  dt1_p, dt2_p: sst_dtype_p_t;         {point to set element base data types}
  dt1, dt2: sst_dtype_k_t;             {set elements base data type ID}
  ord_min, ord_max: sys_int_max_t;     {min/max ordinal value of all set elements}
  o1_min, o1_max: sys_int_max_t;       {ordinal value limits for set 1}
  o2_min, o2_max: sys_int_max_t;       {ordinal value limits for set 2}
{
************************************************************
*
*   Local subroutine FIND_ORD (D,OMIN,OMAX)
*
*   Find the ordinal value limits of the data type D.
}
procedure find_ord (
  in      d: sst_dtype_t;              {data type descriptor}
  out     omin, omax: sys_int_max_t);  {returned ordinal value limits}

begin
  case d.dtype of
sst_dtype_int_k: begin
      omax := rshft(~0, 1);
      omin := ~omax;
      end;
sst_dtype_enum_k: begin
      omin := 0;
      omax := d.enum_last_p^.enum_ordval;
      end;
sst_dtype_bool_k: begin
      omin := 0;
      omax := 1;
      end;
sst_dtype_char_k: begin
      omin := 0;
      omax := 255;
      end;
sst_dtype_range_k: begin
      omin := d.range_ord_first;
      omax := omin + d.range_n_vals - 1;
      end;
otherwise
    sys_message_bomb ('sst', 'dtype_not_ordinal', nil, 0);
    end;
  end;
{
************************************************************
*
*   Start of main routine.
}
begin
  dt_out_p := nil;                     {init to set data types are incompatible}
  sst_dtype_resolve (dt_in1, dt1s_p, dt1); {resolve base data types of set elements}
  sst_dtype_resolve (dt1s_p^.set_dtype_p^, dt1_p, dt1);
  sst_dtype_resolve (dt_in2, dt2s_p, dt2);
  sst_dtype_resolve (dt2s_p^.set_dtype_p^, dt2_p, dt2);

  if dt1_p = dt2_p then begin          {both sets have same base element data type ?}
    if dt2s_p^.set_dtype_final         {pass back hard dtype if there is a choice}
      then dt_out_p := addr(dt_in2)
      else dt_out_p := addr(dt_in1);
    return;
    end;
  if dt_in1.set_dtype_final and dt_in2.set_dtype_final {both set dtypes hard fixed ?}
    then return;
  if                                   {set 1 is NULL with unknown data type ?}
      (dt_in1.set_n_ent = 0) and (not dt_in1.set_dtype_final)
      then begin
    dt_out_p := addr(dt_in2);
    return;
    end;
  if                                   {set 2 is NULL with unknown data type ?}
      (dt_in2.set_n_ent = 0) and (not dt_in2.set_dtype_final)
      then begin
    dt_out_p := addr(dt_in2);
    return;
    end;
  if dt1 <> dt2 then return;           {base element data types incompatible ?}
{
*   At this point we know that both sets have different data types, but that
*   the base data type IDs of the set elements are the same.  Also, at least
*   one of the sets' data type is not hard-wired, so that there is still the
*   possibility of a combined data type.  Each set is either the complete
*   set of the base data type, or a subrange of it.
*
*   Now find the combined min and max ordinal values of all the possible elements
*   in both sets.
}
  find_ord (dt1_p^, o1_min, o1_max);   {get ordinal value limits of first set}
  find_ord (dt2_p^, o2_min, o2_max);   {get ordinal value limits of second set}
  ord_min := min(o1_min, o2_min);      {combine min/max ranges of both sets}
  ord_max := max(o1_max, o2_max);
{
*   The ordinal value min/max limits for both sets, and the combined min/max
*   limits have been found.  Now check for either set having a hard-wired
*   data type.  This is OK as long as the combined ordinal value limit matches
*   the ordinal value limit for that set.
}
  if dt_in1.set_dtype_final then begin {set 1 has hard wired data type ?}
    if (ord_min = o1_min) and (ord_max = o1_max)
      then begin                       {this set exactly matches combined limits}
        dt_out_p := addr(dt_in1);      {use data type for this set directly}
        end
      else begin                       {other set is not a subset of this one}
        return;
        end
      ;
    end;

  if dt_in2.set_dtype_final then begin {set 2 has hard wired data type ?}
    if (ord_min = o2_min) and (ord_max = o2_max)
      then begin                       {this set exactly matches combined limits}
        dt_out_p := addr(dt_in2);      {use data type for this set directly}
        end
      else begin                       {other set is not a subset of this one}
        return;
        end
      ;
    end;
{
*   No conflict exists.  We now know there definately exists a valid combined
*   data type.  First check that one of the sets is a superset of the other,
*   in which case its data type is the combined data type.
}
  if (o1_min = ord_min) and (o1_max = ord_max) then begin {set 1 is superset ?}
    dt_out_p := addr(dt_in1);
    return;
    end;

  if (o2_min = ord_min) and (o2_max = ord_max) then begin {set 2 is superset ?}
    dt_out_p := addr(dt_in2);
    return;
    end;
{
*   Neither set is a complete superset of the other.  This means we will need
*   to create our own data type.  If the ordinal max range exactly matches the
*   base data type, then we will use it directly.  Otherwise we will create
*   a subrange of it.
}
  sst_dtype_new_subrange (             {create new data type, if necessary}
    dt1_p^,                            {base data type}
    ord_min, ord_max,                  {ordinal range of desired data type}
    dt2_p);                            {returned data type}

  sst_dtype_new (dt_out_p);            {create and init new SET data type}
  dt_out_p^.dtype := sst_dtype_set_k;
  dt_out_p^.bits_min := ord_max - ord_min + 1;
  dt_out_p^.align_nat := sst_config.int_machine_p^.align_nat;
  dt_out_p^.align := dt_out_p^.align_nat;
  dt_out_p^.size_used :=
    (dt_out_p^.bits_min + sst_config.bits_adr - 1) div sst_config.bits_adr;
  dt_out_p^.size_align :=
    ((dt_out_p^.size_used + dt_out_p^.align - 1) div dt_out_p^.align)
    * dt_out_p^.align;
  dt_out_p^.set_dtype_p := dt2_p;
  dt_out_p^.set_n_ent :=
    (dt_out_p^.bits_min + sst_set_ele_per_word -1) div sst_set_ele_per_word;
  dt_out_p^.set_dtype_final := false;
  end;
