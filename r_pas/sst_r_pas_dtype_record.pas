{   Subroutine SST_R_PAS_DTYPE_RECORD (D)
*
*   Process the RECORD_DATA_TYPE syntax.  D is the data type definition block that
*   will be filled in.
}
module sst_r_pas_DTYPE_RECORD;
define sst_r_pas_dtype_record;
%include 'sst_r_pas.ins.pas';

procedure sst_r_pas_dtype_record (     {process RECORD_DATA_TYPE}
  in out  d: sst_dtype_t);             {returned pointer to newly created data type}

var
  tag: sys_int_machine_t;              {syntax tags from .syn file}
  str_h: syo_string_t;                 {handle to string for a tag}
  align_old: sys_int_adr_t;            {saved copy of old alignment rule}
  start_pp: sst_symbol_pp_t;           {adr of start of fields chain pointer}
  variant: sys_int_machine_t;          {sequential overlay variant number, 0 = base}
  val: sst_var_value_t;                {user variant ID value}
  val_dtype_p: sst_dtype_p_t;          {points to data type of user variant IDs}
  pack: boolean;                       {TRUE when fields are being packed into rec}
  variant_used: boolean;               {TRUE if fields created with curr VARIANT}
{
*************************************************************************
*
*   Local subroutine RECORD_FIELD (NEXT_PP, D, OFSA, OFSB)
*
*   Process RECORD_FIELD syntax.  NEXT_PP is the address of the pointer that
*   will point to the next field name symbol.  NEXT_PP will be updated to point
*   to the new end of chain NIL pointer.  D is the data type descriptor for the
*   whole record.  The size and alignment fields will be updated for the
*   fields read in.  OFSA and OFSB are the current offsets of where the next
*   field will start within the whole record.  OFSA is the offset in whole
*   machine addresses.  OFSB is any additional offset in bits.  OFSA and OFSB
*   will be updated to after the new field.
}
procedure record_field (
  in out  next_pp: sst_symbol_pp_t;    {pointer to end of chain pointer}
  in out  d: sst_dtype_t;              {data type block for whole record}
  in out  ofsa: sys_int_max_t;         {machine address offset from record start}
  in out  ofsb: sys_int_machine_t);    {bit offset within machine address}

var
  tag: sys_int_machine_t;              {syntax tags from .syn file}
  str_h: syo_string_t;                 {handle to string for a tag}
  sym_first_p: sst_symbol_p_t;         {pointer to first symbol in chain}
  sym_p: sst_symbol_p_t;               {pointer to current symbol}
  dt_p: sst_dtype_p_t;                 {pointer to data type for fields}
  dt2_p: sst_dtype_p_t;                {points to base data type for fields}
  sz: sys_int_max_t;                   {scratch size value}
  scope_save_p: sst_scope_p_t;         {saved pointer to scope for field names}
  stat: sys_err_t;                     {completion status code}

label
  loop;

begin
  syo_level_down;                      {down into RECORD_FIELD syntax}
  sym_first_p := nil;                  {indicate no symbol created yet}

loop:                                  {back here each new RECORD_FIELD tag}
  syo_get_tag_msg (tag, str_h, 'sst_pas_read', 'dtype_bad', nil, 0); {get next tag}
  case tag of
{
*   RECORD_FIELD is empty.  This should only occurr as the first tag.
}
syo_tag_end_k: begin
  syo_level_up;                        {up from RECORD_FIELD syntax}
  return;
  end;
{
*   Tag was the name of a field that will represent the data type to
*   be read in later.
}
1: begin
  sst_symbol_new                       {create new field name symbol}
    (str_h, syo_charcase_down_k, sym_p, stat);
  syo_error_abort (stat, str_h, '', '', nil, 0);

  sym_p^.symtype := sst_symtype_field_k; {new symbol is field of a record data type}
  sym_p^.flags := sym_p^.flags + [sst_symflag_def_k]; {symbol will be defined}
  sym_p^.field_parent_p := addr(d);    {point to data type for whole record}
  sym_p^.field_next_p := nil;          {init to this field is end of fields chain}
  sym_p^.field_variant := variant;     {set sequential variant ID}
  variant_used := true;                {flag that fields created in this variant}
  sym_p^.field_var_val := val;         {set user variant ID}

  if sym_first_p = nil then begin      {this is first new symbol ?}
    sym_first_p := sym_p;              {save pointer to first symbol for this field}
    end;
  next_pp^ := sym_p;                   {link new symbol to end of field names chain}
  next_pp := addr(sym_p^.field_next_p); {update pointer to new end of chain}
  end;
{
*   Tag was the data type that all the previous fields represent.
}
2: begin
  dt_p := nil;                         {indicate to create new data type descriptor}

  scope_save_p := sst_scope_p;         {save pointer to field names' scope}
  sst_scope_p := sst_scope_p^.parent_p; {pop scope one level back}
  sst_names_p := sst_scope_p;
  sst_r_pas_data_type (dt_p);          {create data type descriptor}
  sst_scope_p := scope_save_p;         {restore scope to that for field names}
  sst_names_p := sst_scope_p;
{
*   Make sure this is a legal field data type if the record is packed.
}
  if pack then begin                   {record fields are being packed together ?}
    dt2_p := dt_p;                     {resolve pointer to base field data type}
    while dt2_p^.dtype = sst_dtype_copy_k do dt2_p := dt2_p^.copy_dtype_p;
    case dt2_p^.dtype of               {what is base data type ?}
sst_dtype_int_k,
sst_dtype_enum_k,
sst_dtype_bool_k,
sst_dtype_set_k,
sst_dtype_range_k: ;                   {all the data types that may be packed}
otherwise
      syo_error (str_h, 'sst_pas_read', 'dtype_packed_bad', nil, 0);
      end;
    end;

  sym_p := sym_first_p;                {init to first symbol for this field}
  while sym_p <> nil do begin          {once for each name for this field}
    if dt_p^.align > 0 then begin      {this field more than just bit-aligned ?}
      if pack then begin               {packing fields, but field not bit-aligned ?}
        syo_error (str_h, 'sst_pas_read', 'dtype_packed_aligned', nil, 0);
        end;
      ofsa := ofsa +                   {go to end of any partially used address}
        ((ofsb + sst_config.bits_adr - 1) div sst_config.bits_adr);
      ofsb := 0;
      ofsa :=                          {add padding for alignment, if needed}
        ((ofsa + dt_p^.align - 1) div dt_p^.align) * dt_p^.align;
      d.align_nat :=                   {update alignment for whole record}
        max(d.align_nat, dt_p^.align);
      end;                             {OFSA, OFSB all set for this new field start}

    sym_p^.field_dtype_p := dt_p;      {point symbol to data type for this field}
    sym_p^.field_ofs_adr := ofsa;      {offset in machine addresses}
    sym_p^.field_ofs_bits := ofsb;     {additional offset in bits}
    sym_p := sym_p^.field_next_p;      {advance to next name symbol for this field}

    if pack
      then begin                       {record is PACKED, use BITS_MIN field size}
        ofsb := ofsb + dt_p^.bits_min; {skip over just bits needed for this field}
        sz := ofsb div sst_config.bits_adr; {number of whole addresses in OFSB}
        ofsa := ofsa + sz;             {move whole addresses into OFSA}
        ofsb := ofsb - (sst_config.bits_adr * sz);
        end
      else begin                       {record is NOT packed, use SIZE_USED size}
        ofsa := ofsa + dt_p^.size_used; {skip over whole addresses used by field}
        end
      ;                                {all done updating OFSA,OFSB to after field}
    sz := (ofsa * sst_config.bits_adr) + ofsb; {min new curr offset in bits}
    d.bits_min := max(d.bits_min, sz); {update record size to include new field}
    end;

  syo_level_up;                        {up from RECORD_FIELD syntax}
  return;
  end;

otherwise
    syo_error_tag_unexp (tag, str_h);
    end;                               {end of RECORD_FIELD tag cases}

  goto loop;                           {back for next RECORD_FIELD tag}
  end;
{
*************************************************************************
*
*   Local subroutine RECORD_FIELDS (NEXT_PP, D, OFS_A, OFS_B)
*
*   Read the RECORD_FIELDS syntax.  NEXT_PP is the address of the pointer that
*   will point to the next field name symbol.  NEXT_PP will be updated to point
*   to the new end of chain NIL pointer.  D is the data type descriptor for the
*   whole record.  The size and alignment fields will be updated for the
*   fields read in.  OFS_A and OFS_B are the current offsets of where the next
*   field will start within the whole record.  OFS_A is the offset in whole
*   machine addresses.  OFS_B is any additional offset in bits.
}
procedure record_fields (
  in out  next_pp: sst_symbol_pp_t;    {pointer to end of chain pointer}
  in out  d: sst_dtype_t;              {data type block for whole record}
  in      ofs_a: sys_int_max_t;        {machine address offset from record start}
  in      ofs_b: sys_int_machine_t);   {bit offset within machine address}

var
  tag: sys_int_machine_t;              {syntax tags from .syn file}
  str_h: syo_string_t;                 {handle to string for a tag}
  ofsa: sys_int_max_t;                 {current offsets to end of last invariant rec}
  ofsb: sys_int_machine_t;
  dt: sst_dtype_k_t;                   {scratch data type ID}
  err: boolean;                        {scratch error flag}

label
  loop;

begin
  syo_level_down;                      {down into RECORD_FIELDS syntax}
  ofsa := ofs_a;                       {init current offset to start given}
  ofsb := ofs_b;

loop:                                  {back here each new non-variant field}
  syo_get_tag_msg (tag, str_h, 'sst_pas_read', 'dtype_bad', nil, 0); {get next tag}
  case tag of

syo_tag_end_k: begin                   {no more tags in this RECORD_FIELDS syntax}
  syo_level_up;                        {up from RECORD_FIELDS syntax}
  return;
  end;

1: begin                               {tag is for a new non-variant field}
  record_field (next_pp, d, ofsa, ofsb); {process RECORD_FIELD syntax}
  end;

2: begin                               {tag is data type for later overlay IDs}
  if variant <> 0 then begin           {already within a variant part ?}
    syo_error (str_h, 'sst_pas_read', 'dtype_case_nested', nil, 0);
    end;
  val_dtype_p := nil;                  {indicate find old descriptor, if possible}
  sst_r_pas_data_type (val_dtype_p);   {get pointer to variant IDs data type}
  sst_dtype_resolve (                  {resolve base data type}
    val_dtype_p^, val_dtype_p, dt);
  case dt of                           {what kind of data type is it ?}
sst_dtype_int_k,                       {all the legal data types}
sst_dtype_enum_k,
sst_dtype_bool_k,
sst_dtype_char_k: ;
otherwise
    syo_error (str_h, 'sst', 'dtype_not_ordinal2', nil, 0);
    end;
  end;

3: begin                               {tag is for new overlay ID expression}
  if                                   {OK to increment to next sequential variant ?}
      (variant = 0) or                 {currently in non-variant part ?}
      variant_used                     {fields created with current variant ID ?}
      then begin
    variant := variant + 1;            {make sequential overlay ID for this variant}
    variant_used := false;             {init to no fields with this new variant yet}
    end;
  sst_r_pas_exp_eval (val);            {get user overlay ID expression value}
  case val.dtype of                    {what is data type of overlay ID value ?}
sst_dtype_int_k: begin
      err := not sst_dtype_convertable (
        sst_dtype_int_max_p^,          {value data type}
        val_dtype_p^);                 {data type must be convertable to}
      end;
sst_dtype_enum_k: begin
      err := not sst_dtype_convertable (
        val.enum_p^.enum_dtype_p^,     {value data type}
        val_dtype_p^);                 {data type must be convertable to}
      end;
sst_dtype_bool_k: begin
      err := not sst_dtype_convertable (
        sst_dtype_bool_p^,             {value data type}
        val_dtype_p^);                 {data type must be convertable to}
      end;
sst_dtype_char_k: begin
      err := not sst_dtype_convertable (
        sst_dtype_char_p^,             {value data type}
        val_dtype_p^);                 {data type must be convertable to}
      end;
otherwise
    err := true;                       {definately a data type mismatch error}
    end;
  if err then begin                    {overlay ID doesn't match data type ?}
    syo_error (str_h, 'sst', 'dtype_exp_mismatch', nil, 0);
    end;
  end;

4: begin                               {tag is for fields within current variant}
  record_fields (next_pp, d, ofsa, ofsb); {process this variant case}
  end;

otherwise
    syo_error_tag_unexp (tag, str_h);
    end;                               {end of RECORD_FIELDS TAG cases}
  goto loop;                           {back for next tag in RECORD_FIELDS}
  end;
{
*************************************************************************
*
*   Start of main routine.
}
begin
  syo_level_down;                      {down into RECORD_DATA_TYPE syntax}
  variant := 0;                        {init to next fields are base fields}
  variant_used := false;               {init to no fields of this variant found}
  val.dtype := sst_dtype_int_k;        {init user variant ID to harmless value}
  val.int_val := 0;
  syo_get_tag_msg (                    {get alignment tag}
    tag, str_h, 'sst_pas_read', 'dtype_bad', nil, 0);
{
*   The keywords preceeding RECORD effect the alignment of the whole record,
*   and the alignment of the fields within the record.  SST_ALIGN will be
*   set to the new alignment rules for fields in this record.  The old
*   alignment rule will be saved in ALIGN_OLD.
}
  align_old := sst_align;              {save old alignment rule}
  pack := false;                       {init to fields not PACKED}

  case tag of
1:  begin                              {ALIGNED}
      sst_align := sst_align_natural_k; {fields will be naturally aligned}
      end;
2:  begin                              {UNALIGNED}
      sst_align := sst_align_natural_k; {align fields naturally within record}
      end;
3:  begin                              {PACKED}
      sst_align := 0;                  {fields will be bit-aligned}
      pack := true;                    {indicate fields are being packed}
      end;
4:  begin                              {no explicit keyword, use current alignment}
      end;
otherwise
    syo_error_tag_unexp (tag, str_h);
    end;                               {end of alignment directive cases}

  sst_scope_new;                       {make new scope for all the field names}

  syo_get_tag_msg (                    {get RECORD_FIELDS tag}
    tag, str_h, 'sst_pas_read', 'dtype_bad', nil, 0);
  if tag <> 1 then begin
    syo_error_tag_unexp (tag, str_h);
    end;
  d.dtype := sst_dtype_rec_k;          {this data type is a record}
  d.bits_min := 0;                     {no size since no fields yet}
  d.align_nat := 0;
  d.size_used := 0;
  d.rec_scope_p := sst_scope_p;        {point to scope for field names}
  d.rec_first_p := nil;                {init field names chain to empty}
  start_pp := addr(d.rec_first_p);     {init adr of start of fields chain pointer}
  record_fields (start_pp, d, 0, 0);   {create the fields for the record}

  d.size_used :=                       {make min machine addresses actually used}
    (d.bits_min + sst_config.bits_adr - 1) div sst_config.bits_adr;

  sst_scope_old;                       {restore old current scope}
  sst_align := align_old;              {restore old alignment rule}
  syo_level_up;                        {up from RECORD_DATA_TYPE syntax}
  end;
