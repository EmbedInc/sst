{   Subroutine SST_R_PAS_DATA_TYPE (DTYPE_P)
*
*   Read in a DATA_TYPE syntax and return DTYPE_P pointing to the corresponding
*   data type descriptor.  If DTYPE_P is NIL on entry to this routine, then
*   a new data type descriptor is either created or an existing one found.  If
*   DTYPE_P is no NIL, then DTYPE_P will not be altered, and the resulting data
*   type will be overwritten into the block DTYPE_P^.
*
*   This is used when a previously referenced but undefined data type is finally
*   declared.
}
module sst_r_pas_DATA_TYPE;
define sst_r_pas_data_type;
%include 'sst_r_pas.ins.pas';

procedure sst_r_pas_data_type (        {process DATA_TYPE}
  in out  dtype_p: sst_dtype_p_t);     {returned pointer to new or reused data type}

const
  max_msg_parms = 3;                   {max parameters we can pass to a message}

var
  tag: sys_int_machine_t;              {syntax tags from .syn file}
  str_h, str2_h: syo_string_t;         {handle to string for a tag}
  dt_p: sst_dtype_p_t;                 {scratch data type pointer}
  dt_set_p: sst_dtype_p_t;             {points to original data type for a SET}
  dt_original_p: sst_dtype_p_t;        {save copy of DTYPE_P on entry}
  dt_id: sst_dtype_k_t;                {scratch data type ID}
  dt_align: sys_int_machine_t;         {alignment for this data type}
  dt_enum: sst_dtype_t;                {data type for enumerated type elements}
  sym_p: sst_symbol_p_t;               {scratch symbol pointer}
  prev_sym_p: sst_symbol_p_t;          {previous symbol pointer for chaining}
  exp_p: sst_exp_p_t;                  {scratch expression pointer}
  i, j: sys_int_machine_t;             {scratch integer and loop counter}
  bits: sys_int_machine_t;             {number of bits required}
  token: string_var32_t;               {scratch string for number conversion}
  dt_ar_p: sst_dtype_p_t;              {pointer to next data type of array remainder}
  range_start, range_end:              {subscript start/end ordinal values}
    sys_int_max_t;
  n_ele: sys_int_conv32_t;             {total number of elements in array}
  sz: sys_int_adr_t;                   {scratch for saving amount of memory}
  n_subscr: sys_int_machine_t;         {scratch number of subscripts}
  func: boolean;                       {TRUE if routine is a function}
  pointer: boolean;                    {TRUE if data type is a pointer}
  created: boolean;                    {TRUE if we created new dtype descriptor}
  reusing: boolean;                    {TRUE if re-using  dtype (DTYPE_P <> NIL)}
  enum_dtype_set: boolean;             {TRUE when enum dtype explicitly set}
  set_size_set: boolean;               {TRUE when bit size of set given explicitly}
  fnam: string_treename_t;             {file name passed to a message}
  lnum: sys_int_machine_t;             {line number passed to a message}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;
  stat: sys_err_t;                     {completion status code}

label
  loop_tag_start, do_enum_dtype_set, enum_loop, done_enum, set_dtype_bad,
  loop_ar_dtype, got_set_size;
{
***********************************************
*
*   Local function BITS_MIN (V1, V2)
*
*   Return the minimum number of bits that can be used to represent the
*   subrange of ordinal values from V1 to V2.  A negative value is returned to
*   indicate that no target language integer is capable of holding the subrange.
*   It is assumed that V1 <= v2.
}
function bits_min (
  in      v1, v2: sys_int_max_t)       {limits of the integer subrange}
  :sys_int_machine_t;

var
  imin, imax: sys_int_max_t;           {min/max values of current integer size}
  bmin: sys_int_machine_t;             {internal BITS_MIN value}
  unsig: boolean;                      {TRUE if could match unsigned data type}

label
  got_bmin;

begin
  unsig := (v1 >= 0) and (v2 >= 0);    {TRUE if range limits allow unsigned integer}
  bits_min := -1;                      {init to no suitable target size exists}

  if unsig
    then begin                         {test for fit as unsigned integer}
      imax := 1;                       {init value limit for 1 bit integer}
      for bmin := 1 to sst_dtype_int_max_p^.bits_min do begin {loop to max size}
        if imax >= v2 then goto got_bmin; {this size integer is big enough ?}
        imax := ~lshft(~imax, 1);      {update to max value for next size integer}
        end;                           {back and check this new size int for fit}
      end
    else begin                         {test for fit as signed integer}
      imin := -1;                      {init value limits for 1 bit integer}
      imax := 0;
      for bmin := 1 to sst_dtype_int_max_p^.bits_min do begin {loop to max size}
        if (imin <= v1) and (imax >= v2) then goto got_bmin; {this size fits ?}
        imin := lshft(imin, 1);        {update value limits for next bigger size}
        imax := ~lshft(~imax, 1);
        end;                           {back and check this new size int for fit}
      end
    ;
  return;                              {didn't fit into largest target integer}

got_bmin:                              {found minimum BMIN value that works}
  if                                   {used max int as unsigned with wrong lang ?}
      (bmin = sst_dtype_int_max_p^.bits_min) and {used max size integer ?}
      unsig and                        {assumed integer could be unsigned ?}
      (sst_config.lang <> sst_lang_c_k) {language doesn't support this ?}
    then return;                       {no suitable integer found}
  bits_min := bmin;                    {return minimum bits value}
  end;
{
***********************************************
*
*   Local subroutine DTYPE_CREATE (P)
*
*   Create a new data type descriptor.  P will be returned pointing to the
*   new descriptor.  CREATED will also be set to TRUE, to indicate that
*   a new data type descriptor was created instead of an old one referenced.
}
procedure dtype_create (
  out     p: sst_dtype_p_t);

begin
  if reusing then begin                {re-using caller's data type descriptor ?}
    if created then begin              {can't re-use same data type twice}
      sys_message_bomb ('sst_pas_read', 'dtype_reuse_twice', nil, 0);
      end;
    created := true;                   {a new data type descriptor was created}
    dtype_p := dt_original_p;          {make sure we are pointing to callers desc}
    return;
    end;

  created := true;                     {a new data type descriptor was created}
  util_mem_grab (sizeof(p^), sst_scope_p^.mem_p^, false, p); {allocate the memory}
  p^.symbol_p := nil;                  {init mandatory fields}
  p^.dtype := sst_dtype_undef_k;
  p^.bits_min := 0;
  p^.align_nat := 0;
  p^.align := dt_align;
  p^.size_used := 0;
  p^.size_align := 0;
  end;
{
***********************************************
*
*   Start of main routine.
}
begin
  token.max := sizeof(token.str);      {init var strings}
  fnam.max := sizeof(fnam.str);
  dt_original_p := dtype_p;            {save DTYPE_P as passed in from caller}

  reusing := false;                    {init to not reusing descriptor at DTYPE_P}
  if dtype_p <> nil then begin         {caller wants us to fill in DTYPE_P^ ?}
    if dtype_p^.dtype <> sst_dtype_undef_k then begin
      sys_message_bomb ('sst_pas_read', 'dtype_not_undefined', nil, 0);
      end;
    reusing := true;
    end;

  dt_align := sst_align;               {default our alignment rule to curr rule}
  created := false;                    {init to no new dtype descriptor created}
  syo_level_down;                      {down into DATA_TYPE syntax}

loop_tag_start:                        {back here for each new tag at syntax start}
  syo_get_tag_msg (                    {get starting tag in DATA_TYPE syntax}
    tag, str_h, 'sst_pas_read', 'dtype_bad', nil, 0);
  case tag of

1: begin                               {data type is not a pointer}
      pointer := false;
      end;

2: begin                               {data type is a pointer}
      pointer := true;
      end;

3: begin                               {tag is for DATA_TYPE_ATTRIBUTE syntax}
      syo_level_down;                  {down into DATA_TYPE_ATTRIBUTE syntax}
      syo_get_tag_msg (                {get tag for this attribute}
        tag, str_h, 'sst_pas_read', 'dtype_bad', nil, 0);
      case tag of
1:      dt_align := sst_align_natural_k; {NATURAL}
2:      ;                              {ATOMIC}
3:      dt_align := 1;                 {ALIGNED or ALIGNED(0)}
4:      dt_align := 2;                 {ALIGNED(1)}
5:      dt_align := 4;                 {ALIGNED(2)}
6:      dt_align := 8;                 {ALIGNED(3)}
syo_tag_end_k: ;                       {nothing here at all (this is legal)}
otherwise
        syo_error_tag_unexp (tag, str_h);
        end;
      syo_level_up;                    {back up from DATA_TYPE_ATTRIBUTE syntax}
      goto loop_tag_start;             {back for another tag at start of DATA_TYPE}
      end;

otherwise
    syo_error_tag_unexp (tag, str_h);
    end;                               {end of pointer yes/no tag cases}

  enum_dtype_set := false;             {init to enum data type not explicitly set}
  syo_get_tag_msg (                    {get tag for basic data type class}
    tag, str_h, 'sst_pas_read', 'dtype_bad', nil, 0);
  case tag of
{
*   The code for each of the cases is responsible for making sure DTYPE_P is
*   pointing to a valid data type descriptor.  CREATED must be set to TRUE
*   if DTYPE_P is pointing to a data type descriptor that was newly created
*   here.  This is used later to decide whether the data type descriptor may
*   be edited, or whether a copy must be created first.
*
*   When CREATED is TRUE, the ALIGN and SIZE_ALIGN fields of the data type
*   descriptor will be set later.
*
*   CREATED is automatically set to TRUE by the local subroutine DTYPE_CREATE.
*
*************************************************
*
*   SIMPLE_DATA_TYPE
}
1: begin
  syo_level_down;                      {down into SIMPLE_DATA_TYPE syntax}
  syo_get_tag_msg (tag, str_h, 'sst_pas_read', 'dtype_bad', nil, 0);
  str2_h := str_h;                     {save handle to original simple data type}
do_enum_dtype_set:                     {back here after ele data types determined}
  case tag of
{
*******************************
*
*   SIMPLE_DATA_TYPE > INTEGER16
}
1: begin
  dtype_p := dtype_i16_p;
  end;
{
*******************************
*
*   SIMPLE_DATA_TYPE > INTEGER32
}
2: begin
  dtype_p := dtype_i32_p;
  end;
{
*******************************
*
*   SIMPLE_DATA_TYPE > REAL
}
12: begin
  dtype_p := sst_config.float_machine_p;
  end;
{
*******************************
*
*   SIMPLE_DATA_TYPE > SINGLE
}
3: begin
  dtype_p := sst_config.float_single_p;
  end;
{
*******************************
*
*   SIMPLE_DATA_TYPE > DOUBLE
}
4: begin
  dtype_p := sst_config.float_double_p;
  end;
{
*******************************
*
*   SIMPLE_DATA_TYPE > BOOLEAN
}
5: begin
  dtype_p := sst_dtype_bool_p;
  end;
{
*******************************
*
*   SIMPLE_DATA_TYPE > CHAR
}
6: begin
  dtype_p := sst_dtype_char_p;
  end;
{
*******************************
*
*   SIMPLE_DATA_TYPE > UNIV_PTR
}
7: begin
  dtype_p := sst_dtype_uptr_p;
  end;
{
*******************************
*
*   SIMPLE_DATA_TYPE > STRING
}
8: begin
  dtype_p := dtype_str_p;
  end;
{
*******************************
*
*   SIMPLE_DATA_TYPE > enumerated type
}
9: begin
  i := 0;                              {init value of next enumerated name}

  if enum_dtype_set
{
*   DTYPE_P is pointing to the explicit data type to use for all
*   elements of this enumerated type.  We will make a copy for reference
*   in DT_ENUM.  DTYPE_P will then be set to a data type descriptor that
*   the final enumerated type can be written in.  All the alignment, etc.,
*   fields will already be set, so we just return when done.
}
    then begin
      dt_p := dtype_p;                 {resolve base elements data type}
      while dt_p^.dtype = sst_dtype_copy_k do dt_p := dt_p^.copy_dtype_p;
      dt_enum := dt_p^;                {save copy of data type for the elements}
      case dt_enum.dtype of            {what kind of data type will elements have}
sst_dtype_int_k,                       {all the legal element data types}
sst_dtype_bool_k,
sst_dtype_char_k: ;
sst_dtype_range_k: begin               {need to determine starting name value}
        i := dt_enum.range_ord_first;  {first name will have first value of range}
        end;
otherwise
        syo_error (str2_h, 'sst_pas_read', 'dtype_enum_ele_bad', nil, 0);
        end;
      if not created then begin        {can't re-use DTYPE_P^ ?}
        dtype_create (dtype_p);        {point to new data type descriptor}
        dtype_p^.bits_min := dt_enum.bits_min; {init final enum data type}
        dtype_p^.align_nat := dt_enum.align_nat;
        dtype_p^.size_used := dt_enum.size_used;
        end;
      end
{
*   This is a regular enumerated type without the explicit elements
*   data type field.
}
    else begin
      j := 1;                          {init index to integer size info for enum}
      while sst_config.size_int[j].size <> sst_config.size_enum
        do j := j + 1;
      dt_enum :=                       {data type name values must be convertable to}
        sst_config.size_int[j].dtype_p^;
      dtype_create (dtype_p);          {create new data type descriptor}
      dtype_p^.align_nat := sst_config.size_enum;
      dtype_p^.size_used := sst_config.size_enum;
      end
    ;

  syo_level_down;                      {down into ENUMERATED_DATA_TYPE syntax}

  dtype_p^.dtype := sst_dtype_enum_k;
  dtype_p^.enum_first_p := nil;
  prev_sym_p := nil;                   {init to no previous symbol exists}

enum_loop:                             {back here each new tag}
  syo_get_tag_msg (tag, str_h, 'sst_pas_read', 'dtype_enum_bad', nil, 0);
  if tag = syo_tag_end_k then goto done_enum; {no more enumerated names ?}
  sst_symbol_new (str_h, syo_charcase_down_k, sym_p, stat);
  syo_error_abort (stat, str_h, '', '', nil, 0);
  sym_p^.symtype := sst_symtype_enum_k; {symbol is enumerated name}
  sym_p^.flags := [sst_symflag_def_k];
  sym_p^.enum_next_p := nil;           {init to this symbol is last in chain}
  if prev_sym_p = nil
    then begin                         {this is first symbol in chain}
      dtype_p^.enum_first_p := sym_p;
      sym_p^.enum_prev_p := nil;
      end
    else begin                         {new symbol is not first of chain}
      prev_sym_p^.enum_next_p := sym_p;
      sym_p^.enum_prev_p := prev_sym_p;
      end
    ;
  sym_p^.enum_dtype_p := dtype_p;      {point symbol to its parent data type}
  prev_sym_p := sym_p;                 {new symbol becomes previous symbol}

  syo_get_tag_msg (                    {get optional value tag for this name}
    tag, str_h, 'sst_pas_read', 'dtype_enum_bad', nil, 0);
  case tag of
1:  begin                              {explicit value was given for this name}
      sst_r_pas_exp (str_h, true, exp_p); {get explicit name value expression}
      sst_exp_useage_check (exp_p^, [sst_rwflag_read_k], dt_enum); {check expression}
      sst_ordval (exp_p^.val, i, stat); {get expression ordinal value in I}
      syo_error_abort (stat, str_h, '', '', nil, 0);
      end;
2:  ;                                  {default value for this name, I already set}
otherwise
    syo_error_tag_unexp (tag, str_h);
    end;

  sym_p^.enum_ordval := i;             {set ordinal value of this enumerated name}
  i := i + 1;                          {make default value for next enumerated name}
  goto enum_loop;                      {back and process next enumerated name}

done_enum:                             {jump here when no more enumerated names}
  dtype_p^.enum_last_p := prev_sym_p;  {set pointer to end of symbols chain}
  syo_level_up;                        {back up from ENUMERATED_DATA_TYPE syntax}

  if not enum_dtype_set then begin     {explicit data type not already set ?}
    bits := 1;                         {init number of bits needed}
    j := 2;                            {number of states for current BITS value}
    while j < i do begin               {still need more bits ?}
      bits := bits + 1;
      j := j + j;
      end;
    dtype_p^.bits_min := bits;         {set minumum bits needed for this type}
    end;
  end;
{
*******************************
*
*   SIMPLE_DATA_TYPE > subrange type
}
10: begin
  dtype_create (dtype_p);              {make sure DTYPE_P points to dtype desc}
  syo_level_down;                      {down into SUBRANGE_DATA_TYPE syntax}
  dtype_p^.dtype := sst_dtype_range_k; {indicate this is a subrange data type}
{
*   Process range start expression.  This sets the final data type fields
*   RANGE_DTYPE_P and RANGE_FIRST_P.  The variable RANGE_START is also set to
*   the ordinal value of the range start expression.
}
  syo_get_tag_msg (tag, str_h, 'sst_pas_read', 'dtype_range_bad', nil, 0);
  if tag <> 1 then syo_error_tag_unexp (tag, str_h);
  sst_r_pas_exp (str_h, true, dtype_p^.range_first_p); {process range start value}
  sst_dtype_resolve (                  {get pointer to root data type descriptor}
    dtype_p^.range_first_p^.dtype_p^,  {raw dtype of range start expression}
    dtype_p^.range_dtype_p,            {will point to root data type descriptor}
    dt_id);                            {unused root data type ID}
  sst_ordval (                         {get ordinal value of subrange start}
    dtype_p^.range_first_p^.val,       {input constant value descriptor}
    range_start,                       {returned ordinal value}
    stat);
  syo_error_abort (stat, str_h, 'sst_pas_read', 'dtype_range_illegal', nil, 0);
{
*   Process the range end expression.  It is verified to conform to the
*   data type established by the start range expression.  This also sets
*   the final data type field RANGE_LAST_P.  The variable RANGE_END
*   is also set to the ordinal value of the range end expression.
}
  syo_get_tag_msg (tag, str_h, 'sst_pas_read', 'dtype_range_bad', nil, 0);
  if tag <> 1 then syo_error_tag_unexp (tag, str_h);
  sst_r_pas_exp (str_h, true, dtype_p^.range_last_p); {process range end value}
  if not sst_dtype_convertable (       {data type incompatible with start range ?}
      dtype_p^.range_last_p^.dtype_p^,
      dtype_p^.range_first_p^.dtype_p^)
      then begin
    syo_error (str_h, 'sst_pas_read', 'dtype_range_not_match', nil, 0);
    end;
  sst_ordval (                         {get ordinal value of subrange end}
    dtype_p^.range_last_p^.val,        {input constant value descriptor}
    range_end,                         {returned ordinal value}
    stat);
  syo_error_abort (stat, str_h, 'sst_pas_read', 'dtype_range_illegal', nil, 0);

  if range_end < range_start then begin {subrange is inverted ?}
    syo_error (str_h, 'sst_pas_read', 'dtype_range_inverted', nil, 0);
    end;

  syo_get_tag_msg (tag, str_h, 'sst_pas_read', 'dtype_range_bad', nil, 0);
  if tag <> syo_tag_end_k then syo_error_tag_unexp (tag, str_h);
  syo_level_up;                        {done with SUBRANGE_DATA_TYPE syntax level}
{
*   All done parsing the input stream for this subrange.  All the data has
*   been collected.  Now use that information to fill in the remaining fields.
}
  dtype_p^.range_ord_first := range_start;
  dtype_p^.range_n_vals :=             {set number of values in subrange}
    range_end - range_start + 1;
  dtype_p^.bits_min :=                 {set minimum number of bits required}
    bits_min(range_start, range_end);
  if dtype_p^.bits_min < 0 then begin  {right size integer not available ?}
    syo_error (dtype_p^.range_first_p^.str_h,
     'sst_pas_read', 'dtype_range_int_nosize', nil, 0);
    end;

  if dtype_p^.range_first_p^.val.dtype = sst_dtype_int_k then begin {range of int ?}
    for i := 1 to sst_config.n_size_int do begin {once for each target int size}
      dtype_p^.range_dtype_p := sst_config.size_int[i].dtype_p; {init this size fits}
      if dtype_p^.range_dtype_p^.bits_min >= dtype_p^.bits_min {really does fit ?}
        then exit;
      end;                             {back and try next target language integer}
    end;                               {done setting base subrange integer data type}

  dtype_p^.align_nat := dtype_p^.range_dtype_p^.align_nat;
  dtype_p^.size_used := dtype_p^.range_dtype_p^.size_used;
  end;
{
*******************************
*
*   SIMPLE_DATA_TYPE > symbol_name
}
11: begin
  sst_symbol_lookup (str_h, sym_p, stat); {look up data type name}
  if
      pointer and then
      sys_stat_match (sst_subsys_k, sst_stat_sym_not_found_k, stat)
    then begin                         {pointing to undefined data type}
      sst_symbol_new                   {create symbol that will be defined later}
        (str_h, syo_charcase_down_k, sym_p, stat);
      syo_error_abort (stat, str_h, '', '', nil, 0);
      sst_dtype_new (dt_p);            {create data type block for new symbol}
      sym_p^.symtype := sst_symtype_dtype_k; {new symbol is a data type}
      sym_p^.dtype_dtype_p := dt_p;    {point symbol to its data type block}
      dt_p^.symbol_p := sym_p;         {point data type to its symbol block}
      end                              {done handling pointer to undef data type}
    else begin                         {not pointing to undefined data type}
      syo_error_abort (stat, str_h, '', '', nil, 0);
      if sym_p^.symtype <> sst_symtype_dtype_k then begin
        if sym_p^.char_h.crange_p = nil
          then begin                   {no source char, internally created}
            syo_error (str_h, 'sst_pas_read', 'symbol_not_dtype_internal', nil, 0);
            end
          else begin                   {symbol has know source character}
            sst_charh_info (sym_p^.char_h, fnam, lnum);
            sys_msg_parm_vstr (msg_parm[1], sym_p^.name_in_p^);
            sys_msg_parm_int (msg_parm[2], lnum);
            sys_msg_parm_vstr (msg_parm[3], fnam);
            syo_error (str_h, 'sst_pas_read', 'symbol_not_dtype', msg_parm, 3);
            end
          ;
        end;
      end
    ;
  if pointer
    then begin                         {new data type is pointing to old data type}
      dtype_p := sym_p^.dtype_dtype_p; {pointed-to data type descriptor}
      end
    else begin                         {new data type is copy of old data type}
      dtype_create (dtype_p);          {create new data type descriptor}
      dtype_p^.dtype := sst_dtype_copy_k;
      dtype_p^.copy_dtype_p := sym_p^.dtype_dtype_p;
      while                            {loop until hit true copied data type}
          dtype_p^.copy_dtype_p^.dtype = sst_dtype_copy_k
          do begin
        dtype_p^.copy_dtype_p := dtype_p^.copy_dtype_p^.copy_dtype_p;
        end;
      if dtype_p^.copy_dtype_p^.dtype = sst_dtype_undef_k then begin
        sst_charh_info (sym_p^.char_h, fnam, lnum);
        sys_msg_parm_vstr (msg_parm[1], sym_p^.name_in_p^);
        sys_msg_parm_int (msg_parm[2], lnum);
        sys_msg_parm_vstr (msg_parm[3], fnam);
        syo_error (str_h, 'sst_pas_read', 'dtype_undefined', msg_parm, 3);
        end;
      dtype_p^.copy_symbol_p := sym_p;
      dtype_p^.bits_min := sym_p^.dtype_dtype_p^.bits_min;
      dtype_p^.align_nat := sym_p^.dtype_dtype_p^.align_nat;
      dtype_p^.size_used := sym_p^.dtype_dtype_p^.size_used;
      end                              {done with new data type is copy of old}
    ;                                  {done with new data type is pointer Y/N}
  end;                                 {end of SIMPLE_DATA_TYPE is symbol name}
{
*******************************
*
*   SIMPLE DATA TYPE > *** unexpected ***
}
otherwise
    syo_error_tag_unexp (tag, str_h);
    end;                               {end of SIMPLE_DATA_TYPE cases}
{
*******************************
*
*   Done processing this simple data type tag.  This data type may have
*   been just the explicit data type for enumerated elements.  If so,
*   the next tag is for the ENUMERATED_DATA_TYPE syntax.
}
  syo_get_tag_msg (                    {get optional enumerated data type tag}
    tag, str_h, 'sst_pas_read', 'dtype_bad', nil, 0);
  case tag of
9:  begin                              {now at enumerated data type}
      enum_dtype_set := true;          {indicate explicit enum data type exists}
      if created then begin            {need to set final alignment ?}
        if dt_align = sst_align_natural_k then begin {desired alignment is NATURAL ?}
          dt_align := dtype_p^.align_nat; {resolve what natural alignment means}
          end;                         {DT_ALIGN is now set to hard alignment rule}
        sst_dtype_align (dtype_p^, dt_align);
      end;
      goto do_enum_dtype_set;          {back to handle enum with explicit data type}
      end;
syo_tag_end_k: ;                       {the previous data type stands by itself}
otherwise
    syo_error_tag_unexp (tag, str_h);
    end;
  syo_level_up;                        {back up from SIMPLE_DATA_TYPE}
  end;                                 {end of SIMPLE_DATA_TYPE case}
{
*************************************************
*
*   SET DATA TYPE
}
2: begin
  if pointer then begin
    syo_error (str_h, 'sst_pas_read', 'dtype_pnt_not_allowed', nil, 0);
    end;
  dtype_create (dtype_p);              {create new data type descriptor}
  syo_level_down;                      {down into SET_DATA_TYPE syntax}

  syo_get_tag_msg (                    {get tag for optional BITSIZE keyword}
    tag, str2_h, 'sst_pas_read', 'dtype_set_syerr', nil, 0);
  case tag of
1:  begin                              {tag is for explicit set size expression}
      sst_r_pas_exp (str2_h, true, exp_p); {get bit size expression}
      sst_exp_useage_check (           {make sure proper kind of expression}
        exp_p^, [sst_rwflag_read_k], sst_dtype_int_max_p^);
      sst_ordval (exp_p^.val, j, stat); {get min required bits in J}
      syo_error_abort (stat, str2_h, '', '', nil, 0);
      set_size_set := true;            {indicate explicit set size was given}
      end;
2:  begin                              {no explicit bit size given}
      set_size_set := false;
      end;
otherwise
    syo_error_tag_unexp (tag, str2_h);
    end;

  syo_get_tag_msg (                    {get tag for elements data type}
    tag, str_h, 'sst_pas_read', 'dtype_set_syerr', nil, 0);
  if tag <> 1 then begin
    syo_error_tag_unexp (tag, str_h);
    end;
  dt_set_p := nil;                     {indicate to return pointer to new data type}
  sst_r_pas_data_type (dt_set_p);      {get pointer to base data type descriptor}
  dt_p := dt_set_p;                    {init pointer to base data type descriptor}
  while dt_p^.dtype = sst_dtype_copy_k do begin {loop until hit original data type}
    dt_p := dt_p^.copy_dtype_p;        {point to copied data type descriptor}
    end;
  case dt_p^.dtype of                  {what data type is the base type ?}

sst_dtype_enum_k: begin                {set of enumerated data type}
      dtype_p^.bits_min := 0;          {init min required bits}
      sym_p := dt_p^.enum_first_p;     {init curr enum name to first in list}
      while sym_p <> nil do begin      {loop thru all the enumerated names}
        i := sym_p^.enum_ordval;       {get ordinal value of this enumerated name}
        if i < 0 then begin            {can't make set of negative element values}
          sys_msg_parm_vstr (msg_parm[1], sym_p^.name_in_p^);
          sys_msg_parm_int (msg_parm[2], i);
          syo_error (str_h, 'sst_pas_read', 'dtype_set_enum_negative', msg_parm, 2);
          end;
        dtype_p^.bits_min := max(      {take this enumerated value into account}
          dtype_p^.bits_min, i + 1);
        sym_p := sym_p^.enum_next_p;   {advance to next enumerated name in list}
        end;                           {back to process new enumerated name}
      end;

sst_dtype_bool_k: begin                {set of BOOLEAN}
      dtype_p^.bits_min := 2;
      end;

sst_dtype_char_k: begin                {set of CHAR}
      dtype_p^.bits_min := 256;
      end;

sst_dtype_range_k: begin               {set of subrange data type}
      if dt_p^.range_ord_first < 0 then goto set_dtype_bad; {start val is in range ?}
      dtype_p^.bits_min := max(dt_p^.range_ord_first, dt_p^.range_n_vals) + 1;
      end;

otherwise
    goto set_dtype_bad;                {complain about illegal base data type}
    end;                               {end of base data type cases}

  if set_size_set then begin           {min number of bits explicitly set ?}
    if j < dtype_p^.bits_min then begin {specified less than min required bits ?}
      sys_msg_parm_int (msg_parm[1], dtype_p^.bits_min);
      sys_msg_parm_int (msg_parm[2], j);
      syo_error (str2_h, 'sst_pas_read', 'set_bitsize_too_small', msg_parm, 2);
      end;
    dtype_p^.bits_min := j;            {set min required bits to specified value}
    end;

  if dtype_p^.bits_min > 256 then begin {too many elements in set ?}
set_dtype_bad:                         {jump here if base data type not useable}
    sys_msg_parm_vstr (msg_parm[1], dt_p^.symbol_p^.name_in_p^);
    syo_error (str_h, 'sst_pas_read', 'dtype_set_bad', msg_parm, 1);
    end;

  dtype_p^.dtype := sst_dtype_set_k;   {output data type is definately SET}

  sz :=                                {minimum amount of memory needed}
    (dtype_p^.bits_min + sst_config.bits_adr - 1) div sst_config.bits_adr;
  if set_size_set
    then begin                         {user explicitly specified bits in set}
      for i := 1 to sst_config.n_size_int do begin {look for smallest int that fits}
        if sst_config.size_int[i].size >= sz then begin {found right int to use ?}
          dtype_p^.size_used := sst_config.size_int[i].dtype_p^.size_used;
          dtype_p^.align_nat := sst_config.size_int[i].dtype_p^.align_nat;
          goto got_set_size;
          end;
        end;                           {back to inspect next integer size}
      end
    else begin                         {no explicit set size was specified}
      for i := 1 to sst_config.n_size_set do begin {look thru all the SET sizes}
        if sst_config.size_set[i].size >= sz then begin {found a best fit size ?}
          dtype_p^.size_used := sst_config.size_set[i].dtype_p^.size_used;
          dtype_p^.align_nat := sst_config.size_set[i].dtype_p^.align_nat;
          goto got_set_size;
          end;
        end;                           {back and check out next possible SET size}
      end
    ;
{
*   The number of elements in the SET is greater than the number of bits in
*   the largest integer we are allowed to use as a whole for a set.  We must
*   now use multiples of one of the integer sizes.  This integer size is
*   specified by the config file command SIZE_SET_MULTIPLE.
}
  j := (sz + sst_config.size_set_multiple.size - 1) div {num multiple words needed}
    sst_config.size_set_multiple.size;
  dtype_p^.size_used := j * sst_config.size_set_multiple.size;
  dtype_p^.align_nat := sst_config.size_set_multiple.dtype_p^.align_nat;
got_set_size:                          {all done with SIZE_USED and ALIGN_NAT fields}

  dtype_p^.set_dtype_p := dt_p;
  dtype_p^.set_n_ent :=                {number of array elements for constant value}
    (dtype_p^.bits_min + sst_set_ele_per_word - 1)
    div sst_set_ele_per_word;
  dtype_p^.set_dtype_final := true;    {final data type is definately known}
  syo_level_up;                        {back up from SET_DATA_TYPE syntax}
  end;
{
*************************************************
*
*   RECORD_DATA_TYPE
}
3: begin
  if pointer then begin
    syo_error (str_h, 'sst_pas_read', 'dtype_pnt_not_allowed', nil, 0);
    end;
  dtype_create (dtype_p);              {create new data type descriptor}
  sst_r_pas_dtype_record (dtype_p^);   {separate routine does all the work}
  end;
{
*************************************************
*
*   ARRAY_DATA_TYPE
}
4: begin
  if pointer then begin
    syo_error (str_h, 'sst_pas_read', 'dtype_pnt_not_allowed', nil, 0);
    end;
  dtype_create (dtype_p);              {create new data type descriptor}
  syo_level_down;                      {down into ARRAY_DATA_TYPE syntax}
  syo_get_tag_msg (tag, str_h, 'sst_pas_read', 'dtype_bad', nil, 0); {get PACKED tag}

  dtype_p^.ar_n_subscr := 0;           {init number of subscripts}
  dtype_p^.ar_dtype_rem_p := nil;      {init pointer to array "remainder" data type}
  n_ele := 1;                          {init total number of elements accumulator}
  dt_ar_p := nil;                      {init to not curr dtype for this subscript}

loop_ar_dtype:                         {back here each new tag in ARRAY_DATA_TYPE}
  syo_get_tag_msg (tag, str2_h, 'sst_pas_read', 'dtype_bad', nil, 0); {get next tag}
  case tag of
{
*   Tag is for a new ARRAY_INDEX_RANGE syntax.  This declares the conditions
*   for one subscript.
}
1: begin
      if dt_ar_p = nil
        then begin                     {no curr subscr data type, init to first}
          dt_ar_p := dtype_p;
          end
        else begin                     {curr subscr dtype exists, make new one}
          sst_dtype_new (dt_ar_p^.ar_dtype_rem_p); {create new data type descriptor}
          dt_ar_p := dt_ar_p^.ar_dtype_rem_p; {make new descriptor current}
          dt_ar_p^.dtype := sst_dtype_array_k;
          dt_ar_p^.ar_dtype_rem_p := nil; {init to this is last subscript in array}
          end
        ;

      dtype_p^.ar_n_subscr := dtype_p^.ar_n_subscr + 1; {one more subscript in array}
      syo_level_down;                  {down into ARRAY_INDEX_RANGE syntax}

      syo_get_tag_msg (                {get tag for min subscript value expression}
        tag, str_h, 'sst_pas_read', 'subscript_range_bad', nil, 0);
      if tag <> 1 then syo_error_tag_unexp (tag, str_h);
      sst_r_pas_exp (str_h, true, dt_ar_p^.ar_ind_first_p); {get min value exp}
      sst_ordval                       {get ordinal value of subscript range min}
        (dt_ar_p^.ar_ind_first_p^.val, range_start, stat);
      syo_error_abort (stat, str_h, 'sst_pas_read', 'subscript_dtype_bad', nil, 0);

      syo_get_tag_msg (                {get tag for max subscript value expression}
        tag, str_h, 'sst_pas_read', 'subscript_range_bad', nil, 0);

      case tag of
1:      begin                          {tag is expression for max subscript value}
          sst_r_pas_exp (str_h, true, dt_ar_p^.ar_ind_last_p); {get max val exp}
          if                           {first/last exp data types incompatible ?}
              (not sst_dtype_convertable(
                dt_ar_p^.ar_ind_first_p^.dtype_p^,
                dt_ar_p^.ar_ind_last_p^.dtype_p^))
              or
              (not sst_dtype_convertable(
                dt_ar_p^.ar_ind_last_p^.dtype_p^,
                dt_ar_p^.ar_ind_first_p^.dtype_p^))
              then begin
            syo_error (str_h, 'sst_pas_read', 'subscript_dtype_different', nil, 0);
            end;
          sst_ordval                   {get ordinal value of subscript range max}
            (dt_ar_p^.ar_ind_last_p^.val, range_end, stat);
          syo_error_abort (
            stat, str_h, 'sst_pas_read', 'subscript_dtype_bad', nil, 0);
          if range_end < range_start then begin
            syo_error (str_h, 'sst_pas_read', 'subscript_range_order', nil, 0);
            end;
          dt_ar_p^.ar_ind_n := range_end - range_start + 1; {num of subscript vals}
          end;
2:     begin                           {tag indicates max subscript is unlimited}
          dt_ar_p^.ar_ind_last_p := nil; {indicate subscript max value is unlimited}
          dt_ar_p^.ar_ind_n := 1;      {otherwise treat as if just one value allowed}
          end;
otherwise
        syo_error_tag_unexp (tag, str_h);
        end;                           {end of subscript max value cases}

      n_ele := n_ele * dt_ar_p^.ar_ind_n; {accumulate total number of array elements}
      syo_get_tag_msg (                {this tag should be end of syntax}
        tag, str_h, 'sst_pas_read', 'subscript_range_bad', nil, 0);
      if tag <> syo_tag_end_k then syo_error_tag_unexp (tag, str_h);
      syo_level_up;                    {up from ARRAY_INDEX_RANGE to ARRAY_DATA_TYPE}
      goto loop_ar_dtype;
      end;                             {end of tag is ARRAY_INDEX_RANGE case}

2:    ;                                {tag is for data type of array elements}
{
*   Unexepected TAG value.
}
otherwise
    syo_error_tag_unexp (tag, str_h);
    end;                               {end of ARRAY_DATA_TYPE syntax tag cases}
{
*   The last tag read is for the data type of the array elements.
}
  dtype_p^.ar_dtype_ele_p := nil;      {indicate to create a new data type descr}
  sst_r_pas_data_type (dtype_p^.ar_dtype_ele_p); {get data type of the array eles}
  sz := dtype_p^.ar_dtype_ele_p^.size_align; {allocated size for each array element}
  dtype_p^.dtype := sst_dtype_array_k;
  dtype_p^.bits_min := sz * n_ele * sst_config.bits_adr;
  dtype_p^.align_nat := dtype_p^.ar_dtype_ele_p^.align_nat;
  dtype_p^.size_used := sz * n_ele;
{
*   Loop over each chained data type of this array.  There is a new data type
*   descriptor for the "rest" of the array after each subscript.
*
*   SZ is set to the array element size in machine addresses, and N_ELE is the
*   total number of elements in the whole array.
}
  dt_ar_p := dtype_p^.ar_dtype_rem_p;  {init to data type after first subscript}
  n_ele := n_ele div dtype_p^.ar_ind_n; {init number of elements at curr subscript}
  n_subscr := dtype_p^.ar_n_subscr;    {init number of remaining subscripts}

  while dt_ar_p <> nil do begin        {once for each subscript}
    n_subscr := n_subscr - 1;          {one less subscript left in array}
    dt_ar_p^.bits_min := sz * n_ele * sst_config.bits_adr;
    dt_ar_p^.align_nat := dtype_p^.align_nat;
    dt_ar_p^.align := dtype_p^.align;
    dt_ar_p^.size_used := sz * n_ele;
    dt_ar_p^.size_align := sz * n_ele;
    dt_ar_p^.ar_dtype_ele_p := dtype_p^.ar_dtype_ele_p;
    dt_ar_p^.ar_n_subscr := n_subscr;
    n_ele := n_ele div dt_ar_p^.ar_ind_n; {number of elements after this subscript}
    dt_ar_p := dt_ar_p^.ar_dtype_rem_p; {to array subset after this subscript}
    end;

  dt_ar_p := dtype_p^.ar_dtype_ele_p;
  while dt_ar_p^.dtype = sst_dtype_copy_k do begin {resolve base ele dtype desc}
    dt_ar_p := dt_ar_p^.copy_dtype_p;
    end;

  dtype_p^.ar_string :=                {TRUE if array is a string of characters}
    (dt_ar_p^.dtype = sst_dtype_char_k) and {element data type is CHAR ?}
    (dtype_p^.ar_n_subscr = 1);        {one-dimensional array ?}

  syo_level_up;                        {up from ARRAY_DATA_TYPE syntax}
  end;
{
*************************************************
*
*   ROUTINE_TYPE
}
5: begin
  if not pointer then begin
    syo_error (str_h, 'sst_pas_read', 'dtype_proc_not_pointer', nil, 0);
    end;
  dtype_create (dtype_p);              {create new data type descriptor}
  dtype_p^.dtype := sst_dtype_proc_k;  {data type is a procedure}
  dtype_p^.size_used := sst_config.int_adr_p^.size_align;
  dtype_p^.bits_min := dtype_p^.size_used * sst_config.bits_adr;
  dtype_p^.align_nat := dtype_p^.size_used;
  sst_mem_alloc_scope (                {alloc routine descriptor}
    sizeof(dtype_p^.proc_p^), dtype_p^.proc_p);
  dtype_p^.proc_p^.sym_p := nil;       {this template has no associated symbol}
  dtype_p^.proc_p^.dtype_func_p := nil; {init to routine is not a function}
  dtype_p^.proc_p^.n_args := 0;        {init number of routine arguments}
  dtype_p^.proc_p^.flags := [];        {init to no special flags apply}
  dtype_p^.proc_p^.first_arg_p := nil; {init args chain to empty}

  syo_level_down;                      {down into ROUTINE_TYPE syntax}

  syo_get_tag_msg (                    {get routine type tag}
    tag, str_h, 'sst_pas_read', 'dtype_bad', nil, 0);
  case tag of                          {what type of routine is this}
1:  begin                              {routine is a procedure}
      func := false;
      end;
2:  begin                              {routine is a function}
      func := true;
      end;
otherwise
    syo_error_tag_unexp (tag, str_h);
    end;                               {done with routine type keyword cases}

  syo_get_tag_msg (                    {get args definition tag}
    tag, str_h, 'sst_pas_read', 'dtype_bad', nil, 0);
  if tag <> 1 then begin
    syo_error_tag_unexp (tag, str_h);
    end;
  sst_r_pas_proc_args (dtype_p^.proc_p^); {read routine arg declarations, if any}

  syo_get_tag_msg (                    {get function value data type tag}
    tag, str_h, 'sst_pas_read', 'dtype_bad', nil, 0);
  case tag of
1:  begin                              {no function data type present}
      if func then begin
        syo_error (str_h, 'sst_pas_read', 'func_no_data_type', nil, 0);
        end;
      end;
2:  begin                              {function data type is present}
      if not func then begin
        syo_error (str_h, 'sst_pas_read', 'proc_data_type', nil, 0);
        end;
      sst_r_pas_data_type (dtype_p^.proc_p^.dtype_func_p); {get func return dtype}
      end;
otherwise
    syo_error_tag_unexp (tag, str_h);
    end;                               {end of function data type cases}

  syo_get_tag_msg (                    {get VAL_PARAM tag, if present}
    tag, str_h, 'sst_pas_read', 'dtype_bad', nil, 0);
  case tag of
syo_tag_end_k: ;                       {VAL_PARAM not present}
1:  begin                              {VAL_PARAM was specified}
      sst_r_pas_vparam (dtype_p^.proc_p^); {adjust argument list accordingly}
      end;
otherwise
    syo_error_tag_unexp (tag, str_h);
    end;

  syo_level_up;                        {up from ROUTINE_TYPE to DATA_TYPE syntax}
  end;
{
*************************************************
*
*   Unexpected tag value.
}
otherwise
    syo_error_tag_unexp (tag, str_h);
    end;                               {end of DATA_TYPE tag cases}
{
*************************************************
*
*   Done filling in the basic info in the data type block pointed to by
*   DTYPE_P.  Now set the final alignment, cleanup, and leave.
}
  syo_level_up;                        {pop back from DATA_TYPE syntax}

  if created and pointer then begin    {set alignment of pointed-to data type ?}
    sst_dtype_align (dtype_p^, sst_align); {use default alignment rule}
    end;

  if pointer then begin                {need to create and pass back pointer dtype ?}
    if created and reusing
      then begin                       {not allowed to alter DTYPE_P}
        sst_dtype_new (dt_p);          {create new data type descriptor}
        dt_p^ := dtype_p^;             {make copy of pointed-to data type}
        dt_p^.symbol_p := nil;         {new descriptor not linked to a symbol}
        end
      else begin                       {OK to call DTYPE_CREATE}
        dt_p := dtype_p;               {save pointer to pointed-to data type}
        dtype_create (dtype_p);        {create pointer data type descriptor}
        end
      ;                                {DTYPE_P^ is new block, DT_P^ is base dtype}
    sym_p := dtype_p^.symbol_p;        {save pointer to data type's symbol, if any}
    dtype_p^ := sst_dtype_uptr_p^;     {init from universal pointer template}
    dtype_p^.symbol_p := sym_p;        {restore symbol pointer}
    dtype_p^.pnt_dtype_p := dt_p;      {set pointer to pointed-to data type}
    end;

  if dt_align = sst_align_natural_k then begin {desired alignment is NATURAL ?}
    dt_align := dtype_p^.align_nat;    {resolve what natural alignment means}
    end;                               {DT_ALIGN is now set to hard alignment rule}

  if                                   {need to make COPY data type ?}
      (not created) and
      ((dtype_p^.align <> dt_align) or reusing)
      then begin
    dt_p := dtype_p;                   {save pointer to base data type}
    dtype_create (dtype_p);            {create new data type descriptor}
    dtype_p^.dtype := sst_dtype_copy_k;
    dtype_p^.bits_min := dt_p^.bits_min;
    dtype_p^.align_nat := dt_p^.align_nat;
    dtype_p^.size_used := dt_p^.size_used;
    dtype_p^.copy_symbol_p := dt_p^.symbol_p;
    dtype_p^.copy_dtype_p := dt_p;
    end;

  if created then begin                {need to fix alignment ?}
    sst_dtype_align (dtype_p^, dt_align);
    end;
  end;
