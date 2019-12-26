{   Subroutine SST_SET_VAL_CONVERT (VAL,DTYPE,SUCCESS)
*
*   Convert the SET constant value in VAL to the data type defined by the data
*   type descriptor DTYPE.  SUCCESS is set to TRUE if the conversion was
*   successful.  The values in VAL are changed directly.  It is an error if
*   VAL does not contain a SET value, or if DTYPE is not a SET data type.
*
*   If the data types already match, then this is considered success.
*   The only conversion allowed is if the set in VAL is a subrange that is a
*   subset of the possible set elements implied by DTYPE.  In any case,
*   if SUCCESS is TRUE, then the data type pointer in VAL for the set value
*   will be pointing to DTYPE.
}
module sst_SET_VAL_CONVERT;
define sst_set_val_convert;
%include 'sst2.ins.pas';

procedure sst_set_val_convert (        {convert set value expression data type}
  in out  val: sst_var_value_t;        {set value to convert}
  in      dtype: sst_dtype_t;          {desired target SET data type}
  out     success: boolean);           {TRUE if conversion was successful}

var
  dt_set_p: sst_dtype_p_t;             {pnt to base dtype descriptor for set dtype}
  dt_set: sst_dtype_k_t;               {base set data type ID}
  dt_out_p: sst_dtype_p_t;             {pnt to base desired dtype descriptor}
  dt_out: sst_dtype_k_t;               {base desired data type ID}
  ord1_out: sys_int_max_t;             {ordinal value of first ele in output set}
  ord1_val: sys_int_max_t;             {ordinal value of first ele in input set}
  word_val_p: sys_int_conv32_p_t;      {points to input set data word}
  word_out_p: sys_int_conv32_p_t;      {points to output set data word}
  mask_val, mask_out: sys_int_conv32_t; {mask words for input and output set word}
  val_old: sst_var_value_t;            {local copy of old VAL descriptor}
  i: sys_int_machine_t;                {loop counter}
  ele_val, ele_out: sys_int_machine_t; {input and output set element numbers}

label
  successful;

begin
  success := false;                    {init to conversion was NOT successful}
  if val.dtype <> sst_dtype_set_k then begin {value descriptor is not for a set ?}
    sys_message_bomb ('sst', 'value_not_a_set', nil, 0);
    end;

  if val.set_dtype_p = addr(dtype)     {already set to desired data type ?}
    then goto successful;

  sst_dtype_resolve (                  {resolve set's base data types}
    val.set_dtype_p^,                  {data type of set's elements}
    dt_set_p, dt_set);                 {returned base data types}
  sst_dtype_resolve (                  {resolve desired base data types}
    dtype.set_dtype_p^,                {data type of desired set's elements}
    dt_out_p, dt_out);                 {returned base desired data types}
  if dt_out_p^.dtype <> sst_dtype_set_k then begin {desired data type not SET type}
    sys_message_bomb ('sst', 'dtype_target_not_a_set', nil, 0);
    end;
  if dt_set_p = dt_out_p               {already sets of the same data type ?}
    then goto successful;
{
*   The set has a different data type descriptor than the target, and the
*   elements also use a different data type descriptor than the target.
}
  if dt_set_p^.dtype <> sst_dtype_range_k {VAL not a set of a subrange ?}
    then return;                       {unsuccessful}
  if dt_set <> dt_out                  {set elements not same base data type ID}
    then return;                       {unsuccessful}

  case dt_out_p^.dtype of              {what data type are the desired set elements ?}
sst_dtype_enum_k,                      {desired data type is set of ENUMERATED}
sst_dtype_bool_k,                      {desired data type is set of BOOLEAN}
sst_dtype_char_k: begin                {desired data type is set of CHAR}
      ord1_out := 0;                   {first element is at ordinal value 0}
      end;
sst_dtype_range_k: begin               {desired data type is set of SUBRANGE}
      ord1_out := dt_out_p^.range_ord_first; {get ordinal value of first element}
      end;
otherwise                              {illegal base data type for SET}
    return;                            {unsuccessful}
    end;

  ord1_val := dt_set_p^.set_dtype_p^.range_ord_first; {ord val of first VAL ele}
  if                                   {VAL not a proper subset of DTYPE ?}
      (ord1_val < ord1_out) or
      ((ord1_val + dt_set_p^.bits_min) > (ord1_out + dtype.bits_min))
    then return;
  if                                   {both sets use same memory layout ?}
      (ord1_val = ord1_out) and
      (dt_set_p^.set_n_ent = dtype.set_n_ent)
    then goto successful;
{
*   Conversion is possible.  The set in VAL is a set of a SUBRANGE data type,
*   and the desired set is either a set of the same base data type, or a
*   superset SUBRANGE of the same base data type.
}
  val_old := val;                      {save original copy of VAL}
  val_old.set_dtype_p := dt_set_p;     {point directly to base dtype descriptor}
  sst_mem_alloc_scope (                {allocate new memory for set storage}
    sizeof(val.set_val_p^) * dtype.set_n_ent, {amount of memory to allocate}
    val.set_val_p);                    {returned pointer to new memory}
  val.set_dtype_p := dt_out_p;         {point VAL to its new base dtype descriptor}
{
*   VAL is set up with the new data type and has a new storage area for the
*   set elements.  VAL_OLD is a local copy of the old VAL.  First, initialize
*   all the output bits to zero, then copy all the input set elements one
*   at a time.
}
  for i := 0 to dtype.set_n_ent-1 do begin {once for each output set word}
    val.set_val_p^[i] := 0;            {clear all the bits in each word}
    end;

  ele_out := ord1_val - ord1_out;      {out set ele number of first in set element}
  for ele_val := 0 to dt_set_p^.bits_min-1 do begin {once for each input set ele}
    sst_set_ele_find (                 {get handle to input set element}
      val_old,                         {whole value descriptor}
      ele_val,                         {set element number}
      word_val_p,                      {will point to word containing element}
      mask_val);                       {mask word to select particular element}
    sst_set_ele_find (                 {get handle to output set element}
      val,
      ele_out,
      word_out_p,
      mask_out);
    if (word_val_p^ & mask_val) <> 0   {copy element from input to output set}
      then word_out_p^ := word_out_p^ ! mask_out;
    ele_out := ele_out + 1;            {make number of next output set element}
    end;                               {back for next input set element}

successful:                            {common exit point if conversion successful}
  val.set_dtype_p := addr(dtype);      {point value descriptor to desired data type}
  success := true;                     {indicate conversion DID succeed}
  end;
