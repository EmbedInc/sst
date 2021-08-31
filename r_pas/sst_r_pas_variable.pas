{   Subroutine SST_R_PAS_VARIABLE (VAR_P)
*
*   Process VARIABLE syntax.  The result is compiled into a VAR descriptor, and
*   VAR_P is returned pointing to it.
}
module sst_r_pas_VARIABLE;
define sst_r_pas_variable;
%include 'sst_r_pas.ins.pas';

procedure sst_r_pas_variable (         {process VARIABLE syntax}
  out     var_p: sst_var_p_t);         {returned pointer to VAR descriptor}

const
  max_msg_parms = 2;                   {max parameters we can pass to a message}

var
  tag: sys_int_machine_t;              {syntax tag ID}
  str_h: syo_string_t;                 {handle to string associated with TAG}
  str2_h: syo_string_t;                {saved copy of STR_H}
  mod_p: sst_var_mod_p_t;              {points to current modifier descriptor}
  name: string_var80_t;                {scratch field name}
  dt_p: sst_dtype_p_t;                 {scratch pointer to data type descriptor}
  dt_base_p: sst_dtype_p_t;            {points to base accumulated dtype descriptor}
  mod_allowed: boolean;                {TRUE if modifiers may be allowed}
  first: boolean;                      {TRUE if working on first array subscript}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;
  stat: sys_err_t;                     {completion status code}

label
  next_mod, loop_subscr;

begin
  name.max := sizeof(name.str);        {init local var string}

  sst_mem_alloc_namesp (               {allocate memory for root variable descriptor}
    sizeof(var_p^), var_p);
  syo_level_down;                      {down into VARIABLE syntax level}
  syo_get_tag_msg (                    {get tag for top level variable name}
    tag, str_h, 'sst_pas_read', 'symbol_syntax_bad', nil, 0);
  if tag <> 1 then syo_error_tag_unexp (tag, str_h);
  sst_symbol_lookup (str_h, var_p^.mod1.top_sym_p, stat); {get pointer to symbol descr}
  syo_error_abort (stat, str_h, '', '', nil, 0);
  var_p^.mod1.next_p := nil;           {init to no following modifiers}
  var_p^.mod1.modtyp := sst_var_modtyp_top_k; {indicate this is top level name}
  var_p^.mod1.top_str_h := str_h;      {save string handle to top level symbol name}
  var_p^.dtype_p := nil;               {init to data type does not apply here}
  var_p^.rwflag := [];                 {init to not allowed to read or write var}
  mod_allowed := false;                {init to no modifiers allowed to top name}
  with var_p^.mod1.top_sym_p^: sym do begin {SYM is symbol for top level name}
    case sym.symtype of                {what kind of symbol is top level name ?}

sst_symtype_const_k: begin             {top symbol is a named constant}
        var_p^.dtype_p := sym.const_exp_p^.dtype_p;
        var_p^.rwflag := [sst_rwflag_read_k];
        var_p^.vtype := sst_vtype_const_k;
        var_p^.const_val_p := addr(sym.const_exp_p^.val);
        end;

sst_symtype_enum_k: begin              {top symbol is enumerated constant name}
        var_p^.dtype_p := sym.enum_dtype_p;
        var_p^.rwflag := [sst_rwflag_read_k];
        var_p^.vtype := sst_vtype_const_k;
        sst_mem_alloc_scope (          {allocate mem for constant value descriptor}
          sizeof(var_p^.const_val_p^), var_p^.const_val_p);
        var_p^.const_val_p^.dtype := sst_dtype_enum_k;
        var_p^.const_val_p^.enum_p := addr(sym);
        end;

sst_symtype_dtype_k: begin             {top symbol is name of a data type}
        var_p^.dtype_p := sym.dtype_dtype_p;
        var_p^.vtype := sst_vtype_dtype_k;
        mod_allowed := true;
        end;

sst_symtype_var_k: begin               {top symbol is simple variable name}
        var_p^.dtype_p := sym.var_dtype_p;
        if sym.var_proc_p = nil
          then begin                   {symbol is not dummy arg or func return val}
            var_p^.rwflag := [sst_rwflag_read_k, sst_rwflag_write_k];
            end
          else begin                   {symbol is related to a routine}
            if sym.var_arg_p = nil
              then begin               {symbol is "variable" for stuffing func val}
                var_p^.rwflag := [sst_rwflag_write_k]; {access is write-only}
                end
              else begin               {symbol is a dummy argument}
                var_p^.rwflag := sym.var_arg_p^.rwflag_ext;
                if sst_rwflag_write_k in var_p^.rwflag then begin {declared at least OUT}
                  var_p^.rwflag :=     {allow writing to OUT dummy arguments}
                    var_p^.rwflag + [sst_rwflag_read_k];
                  end;
                end
              ;
            end
          ;
        var_p^.vtype := sst_vtype_var_k;
        mod_allowed := true;
        end;

sst_symtype_abbrev_k: begin            {top symbol is an abbreviation}
        var_p^.dtype_p := sym.abbrev_var_p^.dtype_p;
        var_p^.rwflag := sym.abbrev_var_p^.rwflag;
        var_p^.vtype := sym.abbrev_var_p^.vtype;
        case sym.abbrev_var_p^.vtype of {what type of "var" is being abbreviated ?}
sst_vtype_var_k: begin
            mod_allowed := true;
            end;
sst_vtype_dtype_k: begin
            mod_allowed := true;
            end;
sst_vtype_rout_k: begin
            var_p^.rout_proc_p := sym.abbrev_var_p^.rout_proc_p;
            end;
sst_vtype_const_k: begin
            var_p^.const_val_p := sym.abbrev_var_p^.const_val_p;
            end;
otherwise
          sys_msg_parm_int (msg_parm[1], ord(sym.abbrev_var_p^.vtype));
          sys_message_bomb ('sst', 'vtype_unexpected', msg_parm, 1);
          end;
        end;

sst_symtype_proc_k: begin              {top symbol is routine name}
        if (sym.proc.dtype_func_p = nil) or addr_of
          then begin                   {var refers to routine directly}
            var_p^.dtype_p := sym.proc_dtype_p;
            var_p^.rwflag := [];
            end
          else begin                   {var refers to value returned by function}
            var_p^.dtype_p := sym.proc.dtype_func_p;
            var_p^.rwflag := [sst_rwflag_read_k];
            end
          ;
        var_p^.vtype := sst_vtype_rout_k;
        var_p^.rout_proc_p := addr(sym.proc);
        end;

sst_symtype_com_k: begin               {top symbol is common block name}
        var_p^.dtype_p := nil;
        var_p^.vtype := sst_vtype_com_k;
        end;

otherwise                              {illegal symbol type for this usage}
      sys_msg_parm_vstr (msg_parm[1], sym.name_in_p^);
      syo_error (str_h, 'sst_pas_read', 'symbol_bad_type', msg_parm, 1);
      end;                             {end of symbol type cases}
    end;                               {done with SYM abbreviation}
  mod_p := addr(var_p^.mod1);          {init pointer to current modifier block}
{
*   Jump back here for each new modifier to the exising variable descriptor.
*   VAR_P points to the top variable descriptor, and MOD_P points to the last
*   modifier descriptor.
}
next_mod:
  dt_base_p := var_p^.dtype_p;         {init pointer to base data type descriptor}
  if dt_base_p <> nil then begin       {pointing to a data type block ?}
    while dt_base_p^.dtype = sst_dtype_copy_k do begin {pointing to a copy ?}
      dt_base_p := dt_base_p^.copy_dtype_p; {point to copied data type}
      end;                             {back and check for for copy again}
    end;                               {VAR_P^.DTYPE_P all set}
  if                                   {variable has become procedure ?}
      (var_p^.vtype = sst_vtype_var_k) and {is a variable ?}
      (dt_base_p^.dtype = sst_dtype_proc_k) {data type is procedure ?}
      then begin
    var_p^.vtype := sst_vtype_rout_k;  {var descriptor now stands for routine}
    var_p^.rout_proc_p := dt_base_p^.proc_p; {get pointer to routine descriptor}
    if var_p^.rout_proc_p^.dtype_func_p = nil
      then begin                       {routine is a function}
        var_p^.rwflag := [sst_rwflag_read_k];
        end
      else begin                       {routine is not a function}
        var_p^.rwflag := [];
        end
      ;
    mod_allowed := false;              {no further modifiers allowed now}
    end;
  syo_get_tag_msg (tag, str_h, 'sst_pas_read', 'symbol_syntax_bad', nil, 0);
  if tag = syo_tag_end_k then begin    {done processing VARIABLE syntax ?}
    syo_level_up;                      {back up from VARIABLE syntax}
    return;
    end;
  sst_mem_alloc_namesp (sizeof(mod_p^.next_p^), mod_p^.next_p); {get mem for new mod}
  mod_p := mod_p^.next_p;              {make new modifier the current modifier}
  with mod_p^: modf do begin           {MODF is abbreviation for this modifier}
    modf.next_p := nil;                {this modifier is now new end of chain}
    case tag of
{
*   Tag is for field name of record.
}
1: begin
  if                                   {parent not a record ?}
      (not mod_allowed) or
      (dt_base_p^.dtype <> sst_dtype_rec_k)
      then begin
    syo_error (str_h, 'sst_pas_read', 'parent_not_record', nil, 0);
    end;
  syo_get_tag_string (str_h, name);    {get field name}
  string_downcase (name);              {make lower case for name matching}
  modf.modtyp := sst_var_modtyp_field_k; {set type for this modifier}
  modf.field_str_h := str_h;           {save handle to source file characters}
  modf.field_sym_p := dt_base_p^.rec_first_p; {init to first field in chain}
  while not string_equal(modf.field_sym_p^.name_in_p^, name) do begin {not this sym ?}
    modf.field_sym_p := modf.field_sym_p^.field_next_p; {to next field in chain}
    if modf.field_sym_p = nil then begin {hit end of field names chain ?}
      sys_msg_parm_vstr (msg_parm[1], name);
      sys_msg_parm_vstr (msg_parm[2], dt_base_p^.symbol_p^.name_in_p^);
      syo_error (str_h, 'sst_pas_read', 'field_name_not_found', msg_parm, 2);
      end;
    end;                               {back and check for found named field}
  var_p^.dtype_p := modf.field_sym_p^.field_dtype_p; {update new data type so far}
  end;
{
*   Tag indicates pointer dereference.
}
2: begin
  if                                   {parent not a pointer ?}
      (not mod_allowed) or
      (dt_base_p^.dtype <> sst_dtype_pnt_k)
      then begin
    syo_error (str_h, 'sst_pas_read', 'symbol_not_pointer', nil, 0);
    end;
  if dt_base_p^.pnt_dtype_p = nil then begin
    syo_error (str_h, 'sst_pas_read', 'pointer_dereference_bad', nil, 0);
    end;
  modf.modtyp := sst_var_modtyp_unpnt_k; {this modifier is pointer dereference}
  case var_p^.vtype of                 {what kind of "variable" do we have ?}
sst_vtype_var_k: begin                 {regular variable}
      if sst_rwflag_read_k in var_p^.rwflag
        then begin                     {pointer is readable}
          var_p^.rwflag := [sst_rwflag_read_k, sst_rwflag_write_k];
          end
        else begin
          var_p^.rwflag := [];         {can't touch data thru non-readable pointer}
          end
        ;
      end;
sst_vtype_dtype_k: ;
otherwise
    sys_msg_parm_int (msg_parm[1], ord(var_p^.vtype));
    syo_error (str_h, 'sst', 'vtype_unexpected', msg_parm, 1);
    end;
  var_p^.dtype_p := dt_base_p^.pnt_dtype_p; {data type of dereferenced pointer}
  end;
{
*   Tag indicates a new array subscript.
}
3: begin
  if                                   {parent not an array ?}
      (not mod_allowed) or
      (dt_base_p^.dtype <> sst_dtype_array_k)
      then begin
    syo_error (str_h, 'sst_pas_read', 'symbol_not_array', nil, 0);
    end;
  first := true;                       {init to next subscript if first in array}
  dt_p := dt_base_p;                   {init to "rest" of array is whole array}
  syo_level_down;                      {down into ARRAY_SUBSCRIPTS syntax}
  str2_h := str_h;                     {save string handle to all the subscripts}

loop_subscr:                           {back here for each new ARRAY_SUBSCRIPTS tag}
  syo_get_tag_msg (                    {get tag for next subscript expression}
    tag, str_h, 'sst_pas_read', 'symbol_syntax_bad', nil, 0);
  case tag of
1: begin                               {tag is for another subscript}
      if dt_p = nil then begin         {too many subscripts ?}
        sys_msg_parm_int (msg_parm[1], dt_base_p^.ar_n_subscr);
        syo_error (str2_h, 'sst_pas_read', 'subscripts_too_many', msg_parm, 1);
        end;
      if not first then begin          {not first subscript in this array ?}
        sst_mem_alloc_scope (          {grab memory for this new var modifier}
          sizeof(mod_p^.next_p^), mod_p^.next_p);
        mod_p := mod_p^.next_p;        {make new modifier the current modifier}
        mod_p^.next_p := nil;          {init to this is last modifier in chain}
        end;
      mod_p^.modtyp := sst_var_modtyp_subscr_k; {this modifier is for a subscript}
      mod_p^.subscr_first := first;    {set first/subsequent subscript in array flag}
      first := false;                  {next subscript will no longer be the first}
      sst_r_pas_exp (                  {get subscript expression value}
        str_h, false, mod_p^.subscr_exp_p);
      sst_exp_useage_check (           {check expression attributes for this useage}
        mod_p^.subscr_exp_p^,          {expression to check}
        [sst_rwflag_read_k],           {read/write access needed to expression value}
        dt_base_p^.ar_ind_first_p^.dtype_p^); {data type value must be compatible with}
      dt_p := dt_p^.ar_dtype_rem_p;    {point to data type for "rest" of array}
      mod_p^.subscr_last := dt_p = nil; {TRUE if this is last subscript in array}
      goto loop_subscr;                {back for next ARRAY_SUBSCRIPTS syntax tag}
      end;
syo_tag_end_k: begin                   {tag is end of ARRAY_SUBSCRIPTS syntax}
      syo_level_up;                    {back up from ARRAY_SUBSCRIPTS syntax}
      if dt_p <> nil then begin        {no more subscripts, but not end of array ?}
        sys_msg_parm_int (msg_parm[1], dt_base_p^.ar_n_subscr);
        syo_error (str2_h, 'sst_pas_read', 'subscripts_too_few', msg_parm, 1);
        end;
      var_p^.dtype_p := dt_base_p^.ar_dtype_ele_p; {dtype is now array elements}
      end;
otherwise                              {unexpected syntax tag value}
    syo_error_tag_unexp (tag, str_h);
    end;                               {end of ARRAY_SUBSCRIPTS tag cases}
  end;
{
*   Unexpected tag value.
}
otherwise
      syo_error_tag_unexp (tag, str_h);
      end;                             {end of tag cases}
    goto next_mod;                     {back for new tag with new data type block}
    end;                               {done with MODF abbreviation}
  end;
