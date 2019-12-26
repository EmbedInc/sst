{   Subroutine SST_W_C_DTYPE_VREC (FIELD_P)
*
*   Write data type definition of a variant within a record.  On entry,
*   FIELD_P is pointing to the first field within each variant (overlay).
*   On return, it will be pointing to the next field after the last field
*   in this overlay, or NIL if no fields follow this overlay.  The C record
*   will have the structure below.  Square brackets are used because curly
*   brackets are meaningful in the Pascal source code here:
*
*   typdef union recname_t [
*     struct [                         /* non-variant part, always called BASE */
*       int a;
*       int b;
*       ] base;
*     struct [                         /* first overlay */
*       char unused[8];
*       float x;
*       float y;
*       ] i1;
*     struct [                         /* second overlay */
*       char unused_1[8];
*       float u;
*       float v;
*       ] i2;
*     ] recname_t;
*
*   This routine will be called the first time for the "A" field.  The
*   top UNION declartion line has already been written.  Each invocation
*   must handle all the fields within the same overlay.  If the variant IDs
*   are enumerated values, then "clever" naming will be used to elminate
*   as much redundancy in the struct and field names as possible.
}
module sst_w_c_dtype_vrec;
define sst_w_c_dtype_vrec;
%include 'sst_w_c.ins.pas';

var                                    {static variables preseved between calls}
  enum_trunc_beg: sys_int_machine_t;   {num chars to trunc from beg of enum name}
  enum_trunc_end: sys_int_machine_t;   {num chars to trunc from end of enum name}
  name_nv: string_var16_t :=           {name of STRUCT for non-variant fields}
    [str := 'base', len := 4, max := 16];

procedure sst_w_c_dtype_vrec (         {write data type definition of variant record}
  in out  field_p: sst_symbol_p_t;     {pnt to first field in variant, updated}
  in      pack: boolean);              {TRUE if variant part of packed record}
  val_param;

var
  str_h: syn_string_t;                 {string handle for error message}
  pos_hash: string_hash_pos_t;         {hash table position handle}
  ovlid: sys_int_machine_t;            {sequential ID of this overlay}
  trunc_beg: sys_int_machine_t;        {num char to trunc from field names}
  f_p: sst_symbol_p_t;                 {scratch field pointer}
  e_p, e2_p: sst_symbol_p_t;           {scratch enum value symbol pointers}
  i, j, k: sys_int_machine_t;          {scratch integers and loop counters}
  unused1_p, unused2_p: univ_ptr;      {unused arguments returned from subroutine}
  sname: string_var32_t;               {STRUCT name}
  str: string_var32_t;                 {scratch string}
  name: string_var80_t;                {scratch name string}

label
  got_enum_truncs, done_first, got_sname, loop_field;

begin
  sname.max := sizeof(sname.str);      {init local var strings}
  str.max := sizeof(str.str);
  name.max := sizeof(name.str);

  if field_p^.field_variant <> 1       {not first overlay in a new record ?}
    then goto done_first;
{
*******************************************************************
*
*   This section gets run only once per data type before any variants
*   are written.  Some information common to all the variants is saved
*   in static storage so that it doesn't need to be recomputed for other
*   variants within the same record.
*
*   If the variant ID data type is enumerated, then find the prefix and
*   suffix common to all the enumerated names.  This allows using just
*   the unique part later.  The prefix and suffix are identified by setting
*   ENUM_TRUNC_BEG and ENUM_TRUNC_END.  These are static variables, since
*   they will apply to all the variants in this record.
}
  if                                   {variant IDs are ENUMERATED ?}
      field_p^.field_var_val.dtype = sst_dtype_enum_k
      then begin
    e_p :=                             {get pointer to first enumerated name}
      field_p^.field_var_val.enum_p^.enum_dtype_p^.enum_first_p;
    e2_p := e_p^.enum_next_p;          {get pointer to next enumerated name}
    if e2_p = nil then begin           {only one enumerated name in data type ?}
      enum_trunc_beg := 0;             {don't truncate name at all}
      enum_trunc_end := 0;
      goto got_enum_truncs;            {done computing enum truncation lengths}
      end;
    enum_trunc_beg := e_p^.name_in_p^.len; {init truncation lengths to whole name}
    enum_trunc_end := enum_trunc_beg;
    while e2_p <> nil do begin         {once for each enum name to compare}
      j := e_p^.name_in_p^.len;        {get length of first name}
      k := e2_p^.name_in_p^.len;       {get length of second name}
      enum_trunc_beg := min(enum_trunc_beg, k); {clip to length of new name}
      enum_trunc_end := min(enum_trunc_end, k);
      for i := 1 to enum_trunc_beg do begin {check beginning truncation length}
        if                             {name characters don't match here ?}
            e2_p^.name_in_p^.str[i] <> e_p^.name_in_p^.str[i]
            then begin
          enum_trunc_beg := i - 1;     {names only same up to previous character}
          exit;                        {done checking beginning truncation length}
          end;
        end;                           {back to check out next character}
      for i := 1 to enum_trunc_end do begin {check end truncation length}
        if                             {name characters don't match here ?}
            e2_p^.name_in_p^.str[k+1-i] <> e_p^.name_in_p^.str[j+1-i]
            then begin
          enum_trunc_end := i - 1;     {names only same up to previous character}
          exit;                        {done checking ending truncation length}
          end;
        end;                           {back to check out next character}
      e_p := e2_p;                     {old second name becomes new first name}
      e2_p := e_p^.enum_next_p;        {make pointer to new second name}
      end;                             {back to process this new name}
{
*   ENUM_TRUNC_BEG and ENUM_TRUNC_END identify the prefix and suffix
*   common to all the enumerated names.  Now make sure the prefix and
*   suffix we actually use end/start on underscores.  This prevents
*   accidentally cutting into what is intended to be the unique part, which
*   may cause forward compatibility problems when new names are added that
*   don't start with the same characters in the unique part.
}
    k := 0;                            {set default beginning trunc value}
    for i := enum_trunc_beg downto 1 do begin {scan backwards thru prefix}
      if e_p^.name_in_p^.str[i] = '_' then begin {found last underscore in prefix ?}
        k := i;                        {underscore is last character in prefix}
        exit;
        end;
      end;                             {back for previous prefix character}
    enum_trunc_beg := k;               {set final beginnning truncation length}

    k := 0;                            {set default ending trunc value}
    j := e_p^.name_in_p^.len;          {get length of name to check suffix with}
    for i := enum_trunc_end downto 1 do begin {scan forwards thru suffix}
      if e_p^.name_in_p^.str[j+1-i] = '_' then begin {first underscore in suffix ?}
        k := i;                        {underscore is first character in suffix}
        exit;
        end;
      end;                             {back for next suffix character}
    enum_trunc_end := k;               {set final ending truncation length}
    end;                               {done with variant data type is ENUMERATED}
got_enum_truncs:                       {ENUM_TRUNC_BEG, ENUM_TRUNC_END all set}

done_first:                            {all done with first time for this data type}
{
*   Done with code that gets run only once for each new data type, before
*   any fields are written.
*
*******************************************************************
}
  ovlid := field_p^.field_variant;     {save sequential ID of this overlay}
  trunc_beg := 0;                      {init to not truncate start of field names}
{
*   Put the STRUCT name in SNAME.
}
  if ovlid = 0 then begin              {this STRUCT is for non-variant part ?}
    string_copy (name_nv, sname);      {set name explicitly}
    goto got_sname;                    {SNAME all set}
    end;

  case field_p^.field_var_val.dtype of {what type is the variant ID ?}

sst_dtype_int_k: begin                 {variant ID type is INTEGER}
      string_vstring (sname, 'i', 1);  {set struct name prefix}
      i := field_p^.field_var_val.int_val; {get raw user variant ID}
      if i < 0 then begin              {variant ID value is negative ?}
        string_append1 (sname, 'm');   {use "m" instead of minus sign}
        i := -i;                       {make absolute value}
        end;
      string_f_int (str, i);           {make variant number str}
      string_append (sname, str);      {make composite STRUCT name}
      end;

sst_dtype_enum_k: begin                {variant ID type is ENUMERATED}
      i := field_p^.field_var_val.enum_p^.name_in_p^.len; {get raw user var name len}
      string_substr (                  {extract unique portion from variant name}
        field_p^.field_var_val.enum_p^.name_in_p^, {input string to extract from}
        enum_trunc_beg + 1,            {index of first char to extract}
        i - enum_trunc_end,            {index of last char to extract}
        str);                          {extracted string}
      sst_w.name^ (                    {rename to avoid intrinsic symbols, etc}
        str.str, str.len,              {preferred name and length}
        '', 0,                         {mandatory suffix name and length}
        sst_rename_ncheck_k,           {just avoid reserved symbols, etc.}
        sname,                         {returned name}
        pos_hash);                     {hash table handle for new name, unused}
{
*   Set TRUNC_BEG to the number of characters to truncate from the beginning
*   of field names.  This is to avoid redundant naming with the STRUCT
*   name.  The raw STRUCT name used for comparison is in STR.  STR will
*   be trashed.
}
      f_p := field_p;                  {init pointer to first field in list}
      string_append1 (str, '_');       {may truncate STRUCT name plus underscore}
      trunc_beg := str.len;            {init to max possible trunc length}
      while f_p <> nil do begin        {once for each field in this variant}
        if f_p^.field_variant <> ovlid {this new field not within our variant ?}
          then exit;
        trunc_beg :=                   {clip to max allowed trunc for this field}
          min(trunc_beg, f_p^.name_out_p^.len - 1);
        for i := 1 to trunc_beg do begin {scan start of names}
          if f_p^.name_out_p^.str[i] <> str.str[i] then begin {first mismatch char ?}
            trunc_beg := i - 1;        {set index of last common character found}
            exit;                      {no point looking further}
            end;
          end;                         {check next character in this field name}
        f_p := f_p^.field_next_p;      {advance to next field in record}
        end;                           {back to process this new field name}
      k := 0;                          {init trunc length to last underscore}
      for i := trunc_beg downto 1 do begin {scan backwards thru truncated string}
        if str.str[i] = '_' then begin {found last underscore in truncated string}
          k := i;                      {set truncation length to here}
          exit;
          end;
        end;                           {back to check previous character for "_"}
      trunc_beg := k;                  {set final field name truncation length}
{
*   Punt the whole truncation attempt if any of the field names now don't
*   start with a letter.
}
      f_p := field_p;                  {init pointer to first field in list}
      while f_p <> nil do begin        {once for each field in this variant}
        if f_p^.field_variant <> ovlid {this new field not within our variant ?}
          then exit;
        if                             {first non-truncated char not a letter ?}
            (f_p^.name_out_p^.str[trunc_beg + 1] < 'a') or
            (f_p^.name_out_p^.str[trunc_beg + 1] > 'z')
            then begin
          trunc_beg := 0;              {disable all truncation}
          exit;
          end;
        f_p := f_p^.field_next_p;      {advance to next field in record}
        end;                           {back to process this new field name}
      end;                             {end of variant ID type is ENUMERATED}

sst_dtype_bool_k: begin                {variant ID type is BOOLEAN}
      if field_p^.field_var_val.bool_val
        then begin                     {variant ID is TRUE}
          string_vstring (sname, 't', 1);
          end
        else begin                     {variant ID is FALSE}
          string_vstring (sname, 'f', 1);
          end;
        ;
      end;

sst_dtype_char_k: begin                {variant ID type is CHARACTER}
      string_vstring (sname, 'c', 1);  {set struct name prefix}
      string_f_int (str, ord(field_p^.field_var_val.char_val));
      string_append (sname, str);      {make composite STRUCT name}
      end;

otherwise                              {unexpected variant ID type}
    str_h.first_char := field_p^.char_h;
    str_h.last_char := field_p^.char_h;
    syn_error (str_h, 'sst_c_write', 'variant_id_dtype_unexpected', nil, 0);
    end;

got_sname:                             {got STRUCT name in SNAME}
{
*   The final STRUCT name for this variant is in SNAME.  TRUNC_BEG
*   is all set if the user variant data type is enumerated.
}
  sst_w.tab_indent^;                   {write STRUCT header}
  sst_w.appendn^ ('struct {', 8);
  sst_w.line_close^;
  sst_w.indent^;                       {add indentation level for STRUCT}
{
*   Make sure the first field starts at the correct record offset.
*   Padding is added as an array of CHAR to reach the correct offset,
*   if not already there.
}
  if field_p^.field_ofs_adr > 0 then begin {need to add padding ?}
    sst_w.name^ (                      {make unique name for this UNUSED array}
      'unused', 6,                     {desired name}
      '', 0,                           {name suffix}
      sst_rename_all_k,                {this name will definately be unique}
      name,                            {resulting name for UNUSED array}
      pos_hash);                       {hash table position handle for new name}
    string_hash_ent_add (              {make sure this UNUSED name never used again}
      pos_hash,                        {hash table handle where entry will go}
      unused1_p, unused2_p);           {unused arguments returned by subroutine}
    sst_w.tab_indent^;
    sst_w.appendn^ ('char', 4);
    sst_w.delimit^;
    sst_w.append^ (name);
    sst_w.appendn^ ('[', 1);
    string_f_int (str, field_p^.field_ofs_adr);
    sst_w.append^ (str);
    sst_w.appendn^ ('];', 2);
    sst_w.line_close^;
    end;                               {done adding padding at start of this variant}
{
*   Loop thru all the fields of this variant (overlay).  FIELD_P will
*   be left pointing to the first field not part of this variant, or
*   NIL, if there are no fields after this variant.
}
loop_field:                            {back here for each new field}
  string_substr (                      {make field name with lead chars truncated}
    field_p^.name_out_p^,              {string to extract substring from}
    trunc_beg + 1,                     {first char index of substring}
    field_p^.name_out_p^.len,          {last char index of substring}
    str);                              {resulting substring}
  if string_equal (str, name_nv) then begin {same name as non-variant STRUCT ?}
    string_appendn (str, '_2', 2);     {at least some attempt to make unique name}
    end;
  sst_w.tab_indent^;                   {go to proper indentation level}
  sst_w.indent^;                       {indent wrapped lines}
  sst_w_c_dtype_simple (               {write data type declaration}
    field_p^.field_dtype_p^,           {descriptor for data type to write}
    str,                               {name of field to declare}
    pack);                             {TRUE if this is field in packed record}
  sst_w.undent^;                       {undo indent for wrapped lines}
  sst_w.appendn^ (';', 1);
  sst_w.line_close^;
{
*   Done writing the C code declaring this field.  The field name actually
*   declared is in STR.  Now update the field output name so that it will
*   be correctly referenced in later code.
}
  string_copy (sname, name);           {start with STRUCT name}
  string_append1 (name, '.');
  string_append (name, str);           {add field name}

  if field_p^.name_out_p^.max < name.len then begin {old string too small ?}
    string_alloc (                     {allocate memory for new string}
      name.len,                        {min required length of new string}
      sst_scope_p^.mem_p^,             {handle to memory context}
      false,                           {don't need to individually deallocate this}
      field_p^.name_out_p);            {returned pointer to new memory}
    end;                               {NAME_OUT_P now all set for copy}
  string_copy (name, field_p^.name_out_p^); {set new name for this field}
{
*   Advance to next field.
}
  field_p := field_p^.field_next_p;    {point to next field in record}
  if                                   {back and process this new field ?}
      (field_p <> nil) and then        {new field exists ?}
      (field_p^.field_variant = ovlid) {still within same variant ?}
    then goto loop_field;
{
*   Done declaring all the fields in this variant.
*   Now close the STRUCT for this variant.
}
  sst_w.tab_indent^;                   {go to proper indentation level}
  sst_w.appendn^ ('} ', 2);
  sst_w.append^ (sname);
  sst_w.appendn^ (';', 1);
  sst_w.line_close^;
  sst_w.undent^;                       {undo indentation for STRUCT}
  end;
