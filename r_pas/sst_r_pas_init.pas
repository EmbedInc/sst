{   Subroutine SST_R_PAS_INIT
*
*   Init the local state for routines used to read in PASCAL source code.
}
module sst_r_pas_INIT;
define sst_r_pas_init;
%include 'sst_r_pas.ins.pas';

procedure sst_r_pas_init;              {init state for reading in PASCAL}

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  i, j: sys_int_machine_t;             {loop counters}
  bits: sys_int_machine_t;             {number of bits in data type}
  sz: sys_int_adr_t;                   {number of machine addresses}
  dt_p: sst_dtype_p_t;                 {scratch data type descriptor pointer}
  token, token2: string_var80_t;       {scratch strings for making intrinsic names}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;
  stat: sys_err_t;                     {completion status code}
{
*************************************************************
*
*   Local subroutine INTRINSIC_DTYPE_REF (NAME, DTYPE)
*
*   Set the input name for the data type DTYPE to NAME.  The input name will be
*   entered in the input symbol table.  A symbol descriptor should already exist
*   for DTYPE.  It is permissable for DTYPE to already have an existing input
*   name.  The old name will still referr to this data type, but the data type
*   will only refer to NAME.
}
procedure intrinsic_dtype_ref (
  in      name: string;                {input name for intrinsic data type symbol}
  in out  dtype: sst_dtype_t);         {data type descriptor block}

var
  namev: string_var32_t;               {var string symbol name}
  pos: string_hash_pos_t;              {handle to position in hash table}
  found: boolean;                      {TRUE if symbol previously existed}
  sym_pp: sst_symbol_pp_t;             {points to hash entry user data area}

begin
  namev.max := sizeof(namev.str);      {init local var string}
  string_vstring (namev, name, sizeof(name)); {name var string symbol name}

  if dtype.symbol_p = nil then begin   {data type has no symbol descriptor ?}
    sys_msg_parm_vstr (msg_parm[1], namev);
    sys_message_bomb ('sst_pas_read', 'dtype_output_no_name', msg_parm, 1);
    end;

  string_hash_pos_lookup (             {get hash table position for new entry}
    sst_scope_p^.hash_h,               {handle to hash table}
    namev,                             {name to add to hash table}
    pos,                               {returned hash table position handle}
    found);                            {returned TRUE if name already present}
  if found then begin                  {NAME already in input symbol table ?}
    sys_msg_parm_vstr (msg_parm[1], namev);
    sys_message_bomb ('sst_pas_read', 'name_dtype_dup_internal', msg_parm, 1);
    end;
  string_hash_ent_add (                {add symbol name to hash table}
    pos,                               {position handle of where to add symbol}
    dtype.symbol_p^.name_in_p,         {returned pointer to stored symbol name}
    sym_pp);                           {returned pointing to hash ent user data area}
  sym_pp^ := dtype.symbol_p;           {point hash entry to symbol descriptor}
  dtype.symbol_p^.flags :=             {indicate this is an intrinsic symbol}
    dtype.symbol_p^.flags + [sst_symflag_intrinsic_in_k];
  end;
{
*************************************************************
*
*   Local subroutine INTRINSIC_DTYPE_COPY (NAME, DTYPE)
*
*   Create a new data type symbol of name NAME.  The data type will be a copy
*   of the data type descriptor DTYPE.  An additional data type with symbol
*   will be created that will be a pointer to the main data type.  The name
*   for the pointer data type will be the name of the main data type with
*   "_P" inserted before the last two characters.  It is assumed that NAME
*   ends in "_T".
}
procedure intrinsic_dtype_copy (
  in      name: string_var80_t;        {name of new data type symbol}
  in      dtype: sst_dtype_t);         {descriptor of data type to be copied}

var
  namep: string_var80_t;               {name of pointer data type}
  sym_p: sst_symbol_p_t;               {points to new symbol descriptor}
  dt_p: sst_dtype_p_t;                 {points to new data type descriptor}
  dtp_p: sst_dtype_p_t;                {points to new pointer data type descriptor}

begin
  namep.max := sizeof(namep.str);      {init local var string}

  sst_symbol_new_name (name, sym_p, stat); {create symbol descriptor for this name}
  sys_error_abort (stat, '', '', nil, 0);
  sst_dtype_new (dt_p);                {create new data type descriptor}

  sym_p^.symtype := sst_symtype_dtype_k; {set up symbol to be a data type}
  sym_p^.dtype_dtype_p := dt_p;
  sym_p^.flags := [sst_symflag_def_k, sst_symflag_intrinsic_in_k];

  dt_p^ := dtype;                      {init new data type as exact copy of DTYPE}
  dt_p^.symbol_p := sym_p;             {customize the new data type descriptor}
  dt_p^.dtype := sst_dtype_copy_k;
  dt_p^.copy_symbol_p := dtype.symbol_p;
  dt_p^.copy_dtype_p := addr(dtype);
{
*   Create pointer data type with symbol.
}
  string_copy (name, namep);           {make name of pointer data type}
  namep.len := namep.len - 2;
  string_appendn (namep, '_p_t', 4);
  sst_symbol_new_name (namep, sym_p, stat); {create symbol descriptor for this name}
  sys_error_abort (stat, '', '', nil, 0);
  sst_dtype_new (dtp_p);               {create new data type descriptor}

  sym_p^.symtype := sst_symtype_dtype_k; {set up symbol to be a data type}
  sym_p^.dtype_dtype_p := dtp_p;
  sym_p^.flags := [sst_symflag_def_k, sst_symflag_intrinsic_in_k];

  dtp_p^ := sst_config.int_adr_p^;     {init dtype descriptor from SYS_INT_ADR_T}
  dtp_p^.symbol_p := sym_p;            {customize the new data type descriptor}
  dtp_p^.dtype := sst_dtype_pnt_k;
  dtp_p^.pnt_dtype_p := dt_p;
  end;
{
*************************************************************
*
*   Local subroutine INTRINSIC_CONST_INT (NAME, VAL)
*
*   Create an intrinsic constant with an integer value.  NAME is the name
*   of the constant, and VAL is the constant's value.
}
procedure intrinsic_const_int (
  in      name: string;                {name of new constant}
  in      val: sys_int_machine_t);     {value for new constant}

var
  sym_p: sst_symbol_p_t;               {pointer to new symbol descriptor}
  exp_p: sst_exp_p_t;                  {pointer to constant value expression}
  namev: string_var80_t;               {var string name, upcased}

begin
  namev.max := sizeof(namev.str);      {init local var string}

  string_vstring (namev, name, sizeof(name)); {make var string constant name}
  string_downcase (namev);             {these symbols are stored in lower case}
  sst_symbol_new_name (namev, sym_p, stat); {create new symbol descriptor}
  sys_error_abort (stat, '', '', nil, 0);
  sst_exp_const_int (val, exp_p);      {create expression descriptor for const value}

  sym_p^.symtype := sst_symtype_const_k; {fill in symbol descriptor}
  sym_p^.flags := [sst_symflag_def_k, sst_symflag_intrinsic_in_k];
  sym_p^.const_exp_p := exp_p;
  end;
{
*************************************************************
*
*   Local subroutine INTRINSIC_FUNCTION (NAME, ID)
*
*   Add NAME to the symbol table as the front end intrinsic function with
*   id ID.  The symbol will be installed indicating it is private to the
*   front end.
}
procedure intrinsic_function (
  in      name: string;                {name of intrinsic function}
  in      id: ifunc_k_t);              {ID of intrinsic function}

var
  namev: string_var32_t;               {var string symbol name}
  sym_p: sst_symbol_p_t;               {points to intrinsic func symbol descriptor}
  stat: sys_err_t;

begin
  namev.max := sizeof(namev.str);      {init local var string}

  string_vstring (namev, name, sizeof(name)); {name var string symbol name}
  string_downcase (namev);             {these symbols are stored in lower case}
  sst_symbol_new_name (namev, sym_p, stat); {create and init new symbol}
  if sys_error(stat) then begin
    sys_msg_parm_vstr (msg_parm[1], namev);
    sys_error_abort (stat, 'sst_pas_read', 'name_ifunc_dup_internal', msg_parm, 1);
    end;
  sym_p^.symtype := sst_symtype_front_k; {symbol is private to front end}
  sym_p^.flags := sym_p^.flags +       {symbol is defined and front-end intrinsic}
    [sst_symflag_def_k, sst_symflag_intrinsic_in_k];
  sst_mem_alloc_scope (                {allocate memory for private front-end data}
    sizeof(sym_p^.front_p^), sym_p^.front_p);
  sym_p^.front_p^.ifunc := id;
  end;
{
*************************************************************
*
*   Start of main routine.
}
begin
  token.max := sizeof(token.str);      {init local var strings}
  token2.max := sizeof(token2.str);

  sst_r.doit := addr(sst_r_pas_doit);  {set up front end call table}

  intrinsic_dtype_ref ('univ_ptr', sst_dtype_uptr_p^);

  dtype_i16_p := nil;
  dtype_i32_p := nil;
  for i := 1 to sst_config.n_size_int do begin {loop thru available output integers}
    bits := sst_config.size_int[i].dtype_p^.bits_min; {number of bits this data type}
    if bits = 16 then begin            {16 bit integer exists ?}
      intrinsic_dtype_ref ('integer', sst_config.size_int[i].dtype_p^);
      intrinsic_dtype_ref ('integer16', sst_config.size_int[i].dtype_p^);
      dtype_i16_p := sst_config.size_int[i].dtype_p;
      end;                             {done with 16 bit integers}
    if bits = 32 then begin            {32 bit integer exists ?}
      intrinsic_dtype_ref ('integer32', sst_config.size_int[i].dtype_p^);
      dtype_i32_p := sst_config.size_int[i].dtype_p;
      end;                             {done with 32 bit integers}
    end;                               {back and check next available integer size}

  if dtype_i16_p = nil then begin      {16 bit integers not available directly ?}
    sst_intrinsic_dtype (
      'integer',
      sst_dtype_int_k,
      (16 + sst_config.bits_adr - 1) div sst_config.bits_adr,
      dtype_i16_p);
    dtype_i16_p^.bits_min := 16;
    sst_intrinsic_dtype (
      'integer16',
      sst_dtype_int_k,
      (16 + sst_config.bits_adr - 1) div sst_config.bits_adr,
      dtype_i16_p);
    dtype_i16_p^.bits_min := 16;
    end;

  if dtype_i32_p = nil then begin      {32 bit integer not available directly ?}
    sst_intrinsic_dtype (
      'integer32',
      sst_dtype_int_k,
      (32 + sst_config.bits_adr - 1) div sst_config.bits_adr,
      dtype_i32_p);
    dtype_i32_p^.bits_min := 32;
    end;

  intrinsic_dtype_ref ('single', sst_config.float_single_p^);
  intrinsic_dtype_ref ('double', sst_config.float_double_p^);
  intrinsic_dtype_ref ('real', sst_config.float_machine_p^);

  intrinsic_dtype_ref ('boolean', sst_dtype_bool_p^);

  intrinsic_dtype_ref ('char', sst_dtype_char_p^);

  sz :=                                {machine addresses needed for 80 chars}
    ((80 * sst_config.bits_char) + sst_config.bits_adr - 1)
    div sst_config.bits_adr;
  sst_intrinsic_dtype (
    'string',
    sst_dtype_array_k,
    sz,
    dtype_str_p);
  dtype_str_p^.align_nat := sst_dtype_char_p^.align_nat;
  dtype_str_p^.align := sst_dtype_char_p^.align;
  dtype_str_p^.ar_dtype_ele_p := sst_dtype_char_p;
  dtype_str_p^.ar_dtype_rem_p := nil;
  sst_exp_const_int (1, dtype_str_p^.ar_ind_first_p);
  sst_exp_const_int (80, dtype_str_p^.ar_ind_last_p);
  dtype_str_p^.ar_ind_n := 80;
  dtype_str_p^.ar_n_subscr := 1;
  dtype_str_p^.ar_string := true;
{
*   Create the implicit SYS_ constants.
}
  intrinsic_const_int ('sys_bits_adr_k', sst_config.bits_adr);
  intrinsic_const_int ('sys_bits_char_k', sst_config.bits_char);
{
*   Create the implicit integer type SYS_INT_ADR_T.  This must be an UNSIGNED
*   integer of the size specified by SST_CONFIG.INT_ADR_P^.  The only way we
*   have of faking unsigned integer data types is with subranges that start
*   at zero.
}
  sst_intrinsic_dtype (                {create data type and symbol descriptors}
    'sys_int_adr_t',                   {symbol name}
    sst_dtype_range_k,                 {basic data type ID}
    sst_config.int_adr_p^.size_used,   {number of machine addresses actually used}
    dt_p);                             {pointer to new data type descriptor}
  dt_p^.range_dtype_p := sst_config.int_adr_p; {point to subrange base data type}
  dt_p^.range_ord_first := 0;          {start of range ordinal value}
  dt_p^.range_n_vals := lshft(1, dt_p^.bits_min - 1); {number of values in range}
  sst_exp_const_int (                  {create range start value expression}
    dt_p^.range_ord_first,             {integer expression value}
    dt_p^.range_first_p);              {returned expression descriptor pointer}
  sst_exp_const_int (                  {create range end value expression}
    dt_p^.range_n_vals - 1,            {integer expression value}
    dt_p^.range_last_p);               {returned expression descriptor pointer}
  sst_config.int_adr_p := dt_p;        {update official SYS_INT_ADR_T pointer}

  sst_intrinsic_dtype (                {make dtype and symbol for SYS_INT_ADR_P_T}
    'sys_int_adr_p_t',                 {symbol name}
    sst_dtype_pnt_k,                   {basic data type ID}
    sst_config.int_adr_p^.size_used,   {number of machine addresses actually used}
    dt_p);                             {pointer to new data type descriptor}
  dt_p^.pnt_dtype_p := sst_config.int_adr_p; {pointer to SYS_INT_ADR_T data type}
{
*   Create the other implicit SYS_ data types.
}
  intrinsic_dtype_copy (string_v('sys_int_machine_t'),
    sst_config.int_machine_p^);
  intrinsic_dtype_copy (string_v('sys_int_max_t'),
    sst_config.size_int[sst_config.n_size_int].dtype_p^);
  intrinsic_dtype_copy (string_v('sys_fp1_t'),
    sst_config.float_single_p^);
  intrinsic_dtype_copy (string_v('sys_fp2_t'),
    sst_config.float_double_p^);
  intrinsic_dtype_copy (string_v('sys_fp_machine_t'),
    sst_config.float_machine_p^);
  intrinsic_dtype_copy (string_v('sys_fp_max_t'),
    sst_config.size_float[sst_config.n_size_float].dtype_p^);

  for i := 1 to sst_config.n_size_int do begin {look thru all the integer sizes}
    if sst_config.size_int[i].size = sst_config.float_single_p^.size_used then begin
      intrinsic_dtype_copy (string_v('sys_int_fp1_t'),
        sst_config.size_int[i].dtype_p^);
      end;
    if sst_config.size_int[i].size = sst_config.float_double_p^.size_used then begin
      intrinsic_dtype_copy (string_v('sys_int_fp2_t'),
        sst_config.size_int[i].dtype_p^);
      end;
    end;

  bits := 0;                           {init number of bits in curr dtype}
  j := 0;                              {init index into SIZE_INT array}
  for i := 1 to sst_config.size_int[sst_config.n_size_int].dtype_p^.bits_min do begin
    if i > bits then begin             {we need the next integer size up ?}
      j := j + 1;                      {make index to next integer size up}
      bits := sst_config.size_int[j].size * sst_config.bits_adr;
      end;
    string_f_int (token2, i);          {make number of bits name string}

    token.len := 0;                    {declare this MIN data type}
    string_appends (token, 'sys_int_min');
    string_append (token, token2);
    string_appendn (token, '_t', 2);
    intrinsic_dtype_copy (token, sst_config.size_int[j].dtype_p^);

    token.len := 0;                    {declare this CONV data type}
    string_appends (token, 'sys_int_conv');
    string_append (token, token2);
    string_appendn (token, '_t', 2);
    if i >= sst_config.int_machine_p^.size_used
      then begin                       {same size of larger than machine int}
        intrinsic_dtype_copy (token, sst_config.size_int[j].dtype_p^);
        end
      else begin                       {smaller than machine int}
        intrinsic_dtype_copy (token, sst_config.int_machine_p^);
        end
      ;
    end;                               {back for next higher number of bits}
{
*   Create intrinsic functions.
}
  intrinsic_function ('abs', ifunc_abs_k);
  intrinsic_function ('addr', ifunc_addr_k);
  intrinsic_function ('arctan', ifunc_arctan_k);
  intrinsic_function ('arshft', ifunc_arshft_k);
  intrinsic_function ('chr', ifunc_chr_k);
  intrinsic_function ('cos', ifunc_cos_k);
  intrinsic_function ('exp', ifunc_exp_k);
  intrinsic_function ('firstof', ifunc_firstof_k);
  intrinsic_function ('lastof', ifunc_lastof_k);
  intrinsic_function ('ln', ifunc_ln_k);
  intrinsic_function ('lshft', ifunc_lshft_k);
  intrinsic_function ('max', ifunc_max_k);
  intrinsic_function ('min', ifunc_min_k);
  intrinsic_function ('odd', ifunc_odd_k);
  intrinsic_function ('ord', ifunc_ord_k);
  intrinsic_function ('pred', ifunc_pred_k);
  intrinsic_function ('round', ifunc_round_k);
  intrinsic_function ('rshft', ifunc_rshft_k);
  intrinsic_function ('sin', ifunc_sin_k);
  intrinsic_function ('sizeof', ifunc_sizeof_k);
  intrinsic_function ('sqr', ifunc_sqr_k);
  intrinsic_function ('sqrt', ifunc_sqrt_k);
  intrinsic_function ('succ', ifunc_succ_k);
  intrinsic_function ('trunc', ifunc_trunc_k);
  intrinsic_function ('xor', ifunc_xor_k);

  intrinsic_function ('alignof', ifunc_alignof_k);
  intrinsic_function ('arctan2', ifunc_arctan2_k);
  intrinsic_function ('offset', ifunc_offset_k);
  intrinsic_function ('shift', ifunc_shift_k);
  intrinsic_function ('size_align', ifunc_sizeof_k);
  intrinsic_function ('size_char', ifunc_szchar_k);
  intrinsic_function ('size_min', ifunc_szmin_k);
  intrinsic_function ('setof', ifunc_setof_k);

  intrinsic_function ('val', ifunc_val_k);
{
*   Init other values in the common block.
}
  error_syn_found := false;            {init to no syntax error}
  addr_of := false;                    {init to not doing arg of ADDR function}
  top_block := top_block_none_k;       {not in top block yet}
  nest_level := 0;                     {init block nesting level}
  end;
