{   Public include file for the base source to source translator routines.
*   These routines are independent of any particular input or output language.
}
const
  sst_size_list_max = 8;               {max available sizes for a number type}
  sst_set_ele_per_word = 32;           {max set elements stored in one word}

  sst_subsys_k = -11;                  {our subsystem ID}

  sst_stat_sym_not_found_k = 1;        {symbol name was not found}
  sst_stat_sym_prev_def_k = 2;         {sym prev exists, name/fnam/lnum err message}
  sst_stat_err_handled_k = 3;          {error occurred, already handled}
  sst_stat_eod_k = 4;                  {end of input data encountered}
  sst_stat_rout_dtypef_nmatch_k = 5;   {function return value data types not same}
  sst_stat_rout_nargs_nmatch_k = 6;    {number of routine arguments don't match}
  sst_stat_rout_nret_nmatch_k = 7;     {NO RETURN status not match between routines}
  sst_stat_rout_glbl_nmatch_k = 8;     {GLOBAL attribute not match between routines}
  sst_stat_rout_ext_nmatch_k = 9;      {EXTERNAL attribute not match between routines}
  sst_stat_rout_dtypea_nmatch_k = 10;  {argument data types are not compatible}
  sst_stat_rout_dir_nmatch_k = 11;     {argument not passed in legal direction}
  sst_stat_rout_pass_nmatch_k = 12;    {arg passing conventions don't match}
  sst_stat_write_bad_k = 13;           {write access attempted but not allowed}
  sst_stat_read_bad_k = 14;            {read access attempted but not allowed}
  sst_stat_no_ordval_k = 15;           {no ordinal value exists for expression}
  sst_stat_sym_found_k = 16;           {sym found in sym table, name in err message}
{
*   Mnemonics for data structure alignment special flag values.  Normal alignment
*   values are machine address increments.  0 = indicates arbitrary bit alignment,
*   1 = arbitrary address alignment, 4 = 32 bit alignment (assuming 8 bits/per adr).
}
  sst_align_natural_k = -1;            {alignment flag value for natural align}

  sst_level_unused_all_k = -1;         {show unused symbols from all levels}

type
  sst_symtype_k_t = (                  {all the different symbol types}
    sst_symtype_const_k,               {constant of value known at compile time}
    sst_symtype_enum_k,                {name of an enumerated type, constant int}
    sst_symtype_dtype_k,               {data type definition}
    sst_symtype_field_k,               {field name of record structured data type}
    sst_symtype_var_k,                 {variable definition}
    sst_symtype_abbrev_k,              {abbreviation for other symbol reference}
    sst_symtype_proc_k,                {procedure}
    sst_symtype_prog_k,                {program}
    sst_symtype_com_k,                 {common block}
    sst_symtype_module_k,              {PROGRAM or MODULE name}
    sst_symtype_label_k,               {symbol is label (GOTO target)}
    sst_symtype_front_k,               {symbol type reserved for front end use}
    sst_symtype_back_k,                {symbol type reserved for back end use}
    sst_symtype_illegal_k);            {illegal symbol type, not set properly}

  sst_dtype_k_t = (                    {all the different data types}
    sst_dtype_int_k,                   {integer}
    sst_dtype_enum_k,                  {enumerated (names for each value)}
    sst_dtype_float_k,                 {floating point}
    sst_dtype_bool_k,                  {TRUE/FALSE (Boolean)}
    sst_dtype_char_k,                  {character}
    sst_dtype_rec_k,                   {record}
    sst_dtype_array_k,                 {array}
    sst_dtype_set_k,                   {set of an enumerated type}
    sst_dtype_range_k,                 {subrange of a simple data type}
    sst_dtype_proc_k,                  {pointer to a procedure}
    sst_dtype_pnt_k,                   {pointer}
    sst_dtype_copy_k,                  {exact copy of another data type}
    sst_dtype_undef_k);                {data type is undefined}

  sst_symflag_k_t = (                  {independent one bit flags for each symbol}
    sst_symflag_def_k,                 {symbol is defined, not just referenced}
    sst_symflag_used_k,                {this symbol is actually used}
    sst_symflag_following_k,           {currently following symbol references}
    sst_symflag_following_dt_k,        {currently following data type references}
    sst_symflag_followed_k,            {completed following symbol references}
    sst_symflag_writing_k,             {symbol is being written to output file}
    sst_symflag_writing_dt_k,          {currently writing data type definition}
    sst_symflag_written_k,             {symbol has been written to output file}
    sst_symflag_created_k,             {symbol was created by translator}
    sst_symflag_intrinsic_in_k,        {symbol is intrinsic to input language}
    sst_symflag_intrinsic_out_k,       {symbol is intrinsic to output language}
    sst_symflag_global_k,              {symbol will be globally known to linker}
    sst_symflag_extern_k,              {symbol lives externally to this module}
    sst_symflag_defnow_k,              {will be defined now, used by back end}
    sst_symflag_ok_sname_k,            {OK if other out symbol is given same name}
    sst_symflag_static_k);             {symbol represents storage that is static}

  sst_ifunc_k_t = (                    {list of functions intrinsic to translator}
    sst_ifunc_abs_k,                   {absolute value}
    sst_ifunc_addr_k,                  {address of}
    sst_ifunc_align_k,                 {min alignment needed by data type of arg}
    sst_ifunc_atan_k,                  {arctangent given slope as ratio of 2 numbers}
    sst_ifunc_cos_k,                   {cosine, argument in radians}
    sst_ifunc_dec_k,                   {next smaller value of}
    sst_ifunc_exp_k,                   {E to power of argument}
    sst_ifunc_first_k,                 {first possible value of}
    sst_ifunc_inc_k,                   {next greater value of}
    sst_ifunc_char_k,                  {convert to char as used on target machine}
    sst_ifunc_int_near_k,              {convert to integer, round to nearest}
    sst_ifunc_int_zero_k,              {convert to integer, round towards zero}
    sst_ifunc_last_k,                  {last possible value of}
    sst_ifunc_ln_k,                    {logarithm base E}
    sst_ifunc_max_k,                   {maximum value of all arguments}
    sst_ifunc_min_k,                   {minimum value of all arguments}
    sst_ifunc_offset_k,                {machine address offset of field in record}
    sst_ifunc_ord_val_k,               {ordinal value of}
    sst_ifunc_shift_lo_k,              {logical shift arg1 by arg2 bits right}
    sst_ifunc_shiftl_lo_k,             {logical shift left arg1 by arg2 bits}
    sst_ifunc_shiftr_ar_k,             {arithmetic shift right arg1 by arg2 bits}
    sst_ifunc_shiftr_lo_k,             {logical shift right arg1 by arg2 bits}
    sst_ifunc_sin_k,                   {sine, arguments in radians}
    sst_ifunc_size_align_k,            {align-padded size of in machine addresses}
    sst_ifunc_size_char_k,             {number of characters that can fit}
    sst_ifunc_size_min_k,              {minimum size of in machine addresses}
    sst_ifunc_sqr_k,                   {square of}
    sst_ifunc_sqrt_k,                  {square root of}
    sst_ifunc_xor_k,                   {exclusive OR of all arguments}
    sst_ifunc_setinv_k);               {set inversion}

  sst_procflag_k_t = (                 {independent one bit flags for routines}
    sst_procflag_noreturn_k);          {routine will never return to caller}

  sst_pass_k_t = (                     {different ways to pass procedure args}
    sst_pass_none_k,                   {pass method unknown or not yet set}
    sst_pass_val_k,                    {pass by value}
    sst_pass_ref_k,                    {pass by reference}
    sst_pass_refcpy_k);                {pass by reference, routine makes local copy}

  sst_rwflag_k_t = (                   {flags allowed to read/write entity}
    sst_rwflag_read_k,                 {entity may be read from}
    sst_rwflag_write_k);               {entity may be written to}

  sst_manuf_k_t = (                    {machine manufacturer ID}
    sst_manuf_none_k,                  {unspecified}
    sst_manuf_apollo_k,                {Apollo division of Hewlett Packard}
    sst_manuf_hp_k,                    {Hewlett Packard, other than Apollo}
    sst_manuf_ibm_k,                   {International Business Machines}
    sst_manuf_sgi_k,                   {Silicon Graphics}
    sst_manuf_sun_k,                   {Sun Microsystems}
    sst_manuf_dec_k);                  {Digital Equipment Corporation}

  sst_lang_k_t = (                     {source language ID}
    sst_lang_none_k,                   {unspecified}
    sst_lang_pas_k,                    {Pascal}
    sst_lang_c_k,                      {C}
    sst_lang_ftn_k);                   {FORTRAN}

  sst_vtype_k_t = (                    {all the different types of "variable" references}
    sst_vtype_var_k,                   {a variable}
    sst_vtype_dtype_k,                 {a data type reference}
    sst_vtype_rout_k,                  {a routine reference}
    sst_vtype_const_k,                 {a named constant reference}
    sst_vtype_com_k);                  {a common block reference}

  sst_rename_k_t = (                   {what kind of re-naming is allowed for sym}
    sst_rename_ncheck_k,               {use rename rules, no check for uniquness}
    sst_rename_none_k,                 {use rename rules, must be unique in curr scope}
    sst_rename_scope_k,                {re-name to make unique in current scope}
    sst_rename_all_k);                 {re-name to make unique in all visible scopes}

  sst_term_k_t = (                     {all the different term types in an expression}
    sst_term_const_k,                  {term is a constant}
    sst_term_var_k,                    {term is contents of a variable}
    sst_term_func_k,                   {term is value of function return}
    sst_term_ifunc_k,                  {term is value of an intrinsic function}
    sst_term_type_k,                   {term is hard type-casting of expression}
    sst_term_set_k,                    {term is a set value}
    sst_term_exp_k,                    {term is nested expression}
    sst_term_field_k,                  {term is whole expression for field in record}
    sst_term_arele_k);                 {term is for a range of array elements}

  sst_op1_k_t = (                      {all the different unadic operators}
    sst_op1_none_k,                    {no operation, use item value directly}
    sst_op1_plus_k,                    {leading plus, converted to NONE if legal}
    sst_op1_minus_k,                   {arithmetic negation}
    sst_op1_not_k,                     {logical negation}
    sst_op1_1comp_k);                  {ones complement (flip all the bits)}

  sst_op2_k_t = (                      {all the different diadic operators}
    sst_op2_none_k,                    {no operation, value is first term}
    sst_op2_add_k,                     {term1 + term2}
    sst_op2_sub_k,                     {term1 - term2}
    sst_op2_mult_k,                    {term1 * term2}
    sst_op2_div_k,                     {term1 / term2}
    sst_op2_divi_k,                    {term1 / term2, term2 and result are integer}
    sst_op2_rem_k,                     {remainder of term1 / term2}
    sst_op2_pwr_k,                     {term1 to-power-of term2}
    sst_op2_btand_k,                   {bitwise and}
    sst_op2_btor_k,                    {bitwise or}
    sst_op2_eq_k,                      {TRUE if term1 equal to term2}
    sst_op2_ne_k,                      {TRUE if term1 not equal to term2}
    sst_op2_ge_k,                      {TRUE if term1 greater than or equal to term2}
    sst_op2_gt_k,                      {TRUE if term1 greater than term2}
    sst_op2_le_k,                      {TRUE if term1 less than or equal to term2}
    sst_op2_lt_k,                      {TRUE if term1 less than term2}
    sst_op2_and_k,                     {logical AND}
    sst_op2_or_k,                      {logical OR}
    sst_op2_andthen_k,                 {logical AND, first op evaluated first}
    sst_op2_orelse_k,                  {logical OR, first op evaluated first}
    sst_op2_in_k,                      {TRUE if term1 is member of term2}
    sst_op2_union_k,                   {term1 UNION term2}
    sst_op2_isect_k,                   {term1 INTERSECTION term2}
    sst_op2_remov_k,                   {REMOVE all members of term2 from term1}
    sst_op2_subset_k,                  {TRUE if term1 is proper subset of term2}
    sst_op2_subset_eq_k,               {TRUE if term1 is subset or equal to term2}
    sst_op2_superset_k,                {TRUE if term1 is proper superset of term2}
    sst_op2_superset_eq_k);            {TRUE if term1 is superset or equal to term2}

  sst_var_modtyp_k_t = (               {all the different variable modifier types}
    sst_var_modtyp_top_k,              {modifier indicates top variable name}
    sst_var_modtyp_unpnt_k,            {pointer dereference}
    sst_var_modtyp_subscr_k,           {expression for next less sig subscript}
    sst_var_modtyp_field_k);           {field name in current record}

  sst_incdir_k_t = (                   {loop increment direction flag}
    sst_incdir_up_k,                   {increment is positive}
    sst_incdir_down_k,                 {increment is negative}
    sst_incdir_unk_k);                 {increment direction is unknown}

  sst_opc_k_t = (                      {all the different internal opcodes}
    sst_opc_module_k,                  {start of a grouping of routines}
    sst_opc_prog_k,                    {start of top level program}
    sst_opc_rout_k,                    {start of a routine}
    sst_opc_exec_k,                    {points to chain of executable code}
    {
    *   Opcodes only used within executable code.
    }
    sst_opc_label_k,                   {indicate handle for a label}
    sst_opc_call_k,                    {subroutine call}
    sst_opc_assign_k,                  {assignment statement}
    sst_opc_goto_k,                    {unconditional transfer of control}
    sst_opc_case_k,                    {execute one of N blocks of code}
    sst_opc_if_k,                      {IF ... THEN ... ELSE ... statement}
    sst_opc_loop_cnt_k,                {counted loop (Pascal FOR, Fortran DO, etc)}
    sst_opc_loop_ttop_k,               {loop with test at start of loop}
    sst_opc_loop_tbot_k,               {loop with test at end of loop}
    sst_opc_loop_next_k,               {go to start of next time around loop}
    sst_opc_loop_exit_k,               {unconditionally exit loop}
    sst_opc_return_k,                  {return from subroutine}
    sst_opc_abbrev_k,                  {abbreviations in effect for block of code}
    sst_opc_discard_k,                 {call function, but discard its return value}
    sst_opc_write_k,                   {write expression value to standard output}
    sst_opc_write_eol_k);              {write end of line to standard output}

  sst_wpos_k_t = (                     {write position with respect to current line}
    sst_wpos_before_k,                 {before start of current line}
    sst_wpos_end_k,                    {at end of current line}
    sst_wpos_after_k);                 {after end of current line}

  sst_dtype_set_t =                    {any combination of data types}
    set of sst_dtype_k_t;

  sst_symflag_t = set of sst_symflag_k_t; {all the flags together in one word}

  sst_procflag_t =                     {all routine flags together as one set}
    set of sst_procflag_k_t;

  sst_rwflag_t =                       {all possible read/write permission combos}
    set of sst_rwflag_k_t;

  sst_scope_p_t =                      {pointer to a scope (context) block}
    ^sst_scope_t;

  sst_symbol_p_t =                     {pointer to a symbol definition}
    ^sst_symbol_t;

  sst_symbol_pp_t =
    ^sst_symbol_p_t;

  sst_dtype_p_t =                      {pointer to a data type definition}
    ^sst_dtype_t;

  sst_dtype_pp_t =
    ^sst_dtype_p_t;

  sst_proc_p_t =                       {pointer to procedure definition}
    ^sst_proc_t;

  sst_proc_arg_p_t =                   {pointer to procedure arg definition}
    ^sst_proc_arg_t;

  sst_proc_arg_pp_t =
    ^sst_proc_arg_p_t;

  sst_var_value_p_t =                  {pointer to descriptor for variable value}
    ^sst_var_value_t;

  sst_var_val_rec_p_t =                {pointer to variable value if is a record}
    ^sst_var_val_rec_t;

  sst_var_val_ar_p_t =                 {pointer to value of array entries}
    ^sst_var_val_ar_t;

  sst_set_val_p_t =                    {pointer to a constant set value}
    ^sst_set_val_t;

  sst_opc_p_t =                        {pointer to start of an opcode}
    ^sst_opc_t;

  sst_opc_pp_t =                       {pointer to and opcode chain pointer}
    ^sst_opc_p_t;

  sst_exp_term_p_t =                   {pointer to descriptor for one term in exp}
    ^sst_exp_term_t;

  sst_exp_p_t =                        {pointer to arbitrary expression definition}
    ^sst_exp_t;

  sst_exp_pp_t =                       {pointer to an expression descriptor pointer}
    ^sst_exp_p_t;

  sst_var_mod_p_t =                    {pointer to one variable desciptor modifier}
    ^sst_var_mod_t;

  sst_var_p_t =                        {pointer to full variable name descriptor}
    ^sst_var_t;

  sst_sym_front_p_t =                  {points to symbol data specific to front end}
    ^sst_sym_front_t;

  sst_sym_back_p_t =                   {points to symbol data specific to back end}
    ^sst_sym_back_t;

  sst_exp_chain_p_t =                  {points to chain of expression descriptors}
    ^sst_exp_chain_t;

  sst_exp_chain_pp_t =                 {points to expression chain pointer}
    ^sst_exp_chain_p_t;

  sst_exp_chain_t = record             {chain of expression descriptors}
    next_p: sst_exp_chain_p_t;         {points to next link in chain}
    exp_p: sst_exp_p_t;                {points to expression for this link in chain}
    end;

  sst_proc_t = record                  {interface data for one procedure}
    sym_p: sst_symbol_p_t;             {points to routine name symbol, if any}
    dtype_func_p: sst_dtype_p_t;       {points to function val data type, if any}
    n_args: sys_int_machine_t;         {total number of call arguments}
    flags: sst_procflag_t;             {set of one-bit flags}
    first_arg_p: sst_proc_arg_p_t;     {points to list of call arguments}
    end;

  sst_proc_arg_t = record              {data about one procedure call argument}
    next_p: sst_proc_arg_p_t;          {points to data about next call arg}
    sym_p: sst_symbol_p_t;             {point to arg symbol in actual routine}
    name_p: string_var_p_t;            {pnt to arg name if routine template}
    exp_p: sst_exp_p_t;                {pnt to arg value expression if called}
    dtype_p: sst_dtype_p_t;            {points to data type for this call arg}
    pass: sst_pass_k_t;                {how is this argument passed}
    rwflag_int: sst_rwflag_t;          {arg read/write permission from inside proc}
    rwflag_ext: sst_rwflag_t;          {internal read/write flag as seen by caller}
    univ: boolean;                     {TRUE if allowed to match any data type}
    end;

  sst_dtype_t = record                 {definition of a data type}
    symbol_p: sst_symbol_p_t;          {points to symbol representing this data type}
    dtype: sst_dtype_k_t;              {data type, use SST_DTYPE_xxx_K}
    bits_min: sys_int_machine_t;       {minimum bits could use for whole data type}
    align_nat: sys_int_machine_t;      {natural alignment, = 0 for packed record}
    align: sys_int_machine_t;          {chosen alignment of this data type}
    size_used: sys_int_adr_t;          {size of used area in machine addresses}
    size_align: sys_int_adr_t;         {align padded size, size if array element}
    case sst_dtype_k_t of              {different data for each data type}
sst_dtype_int_k: (                     {data type is an integer}
      );
sst_dtype_enum_k: (                    {data type is enumerated}
      enum_first_p: sst_symbol_p_t;    {points to first enumerated name}
      enum_last_p: sst_symbol_p_t;     {points to last enumerated name}
      );
sst_dtype_float_k: (                   {data type is floating point}
      );
sst_dtype_bool_k: (                    {data type is boolean}
      );
sst_dtype_char_k: (                    {data type is character}
      );
sst_dtype_rec_k: (                     {data type is record}
      rec_scope_p: sst_scope_p_t;      {points to scope for field names}
      rec_first_p: sst_symbol_p_t;     {points to symbol def for first field}
      );
sst_dtype_array_k: (                   {data type is an array}
      ar_dtype_ele_p: sst_dtype_p_t;   {data type of final array elements}
      ar_dtype_rem_p: sst_dtype_p_t;   {dtype of array "remainder" after 1st subscr}
      ar_ind_first_p: sst_exp_p_t;     {pnt to exp for first legal subscript value}
      ar_ind_last_p: sst_exp_p_t;      {pnt to exp for last val, NIL = unlimited}
      ar_ind_n: sys_int_machine_t;     {number of indicies in first subscript}
      ar_n_subscr: sys_int_machine_t;  {number of subscripts}
      ar_string: boolean;              {TRUE if one-dimensional array of characters}
      );
sst_dtype_set_k: (                     {data type is a set}
      set_dtype_p: sst_dtype_p_t;      {points to data type of set elements}
      set_n_ent: sys_int_machine_t;    {internal array entries needed for set value}
      set_dtype_final: boolean;        {TRUE if final data type definately known}
      );
sst_dtype_range_k: (                   {data type is a subrange of another data type}
      range_dtype_p: sst_dtype_p_t;    {points to base data type of subrange}
      range_first_p: sst_exp_p_t;      {expression for start of range value}
      range_last_p: sst_exp_p_t;       {expression of end of range value}
      range_ord_first: sys_int_max_t;  {ordinal value of first possible value}
      range_n_vals: sys_int_max_t;     {number of values}
      );
sst_dtype_proc_k: (                    {data type is a procedure}
      proc_p: sst_proc_p_t;            {root data about a procedure interface}
      );
sst_dtype_pnt_k: (                     {data type is a pointer}
      pnt_dtype_p: sst_dtype_p_t;      {pointed to data type, NIL = UNIV_PTR}
      );
sst_dtype_copy_k: (                    {data type is a copy of another}
      copy_symbol_p: sst_symbol_p_t;   {points to copied data type symbol}
      copy_dtype_p: sst_dtype_p_t;     {points to ultimate data type definition}
      );
sst_dtype_undef_k: (                   {data type is undefined, declared later}
      );
    end;

  sst_var_value_t = record             {data describing a known constant value}
    dtype: sst_dtype_k_t;              {data type of the constant}
    case sst_dtype_k_t of              {different data for each data type}
sst_dtype_int_k: (                     {data type is an integer}
      int_val: sys_int_max_t;
      );
sst_dtype_enum_k: (                    {data type is enumerated}
      enum_p: sst_symbol_p_t;
      );
sst_dtype_float_k: (                   {data type is floating point}
      float_val: double;
      );
sst_dtype_bool_k: (                    {data type is boolean}
      bool_val: boolean;
      );
sst_dtype_char_k: (                    {data type is character}
      char_val: char;
      );
sst_dtype_array_k: (                   {data type is array}
      ar_str_p: string_var_p_t;        {points to string if ar is string data type}
      );
sst_dtype_set_k: (                     {data type is a SET}
      set_dtype_p: sst_dtype_p_t;      {points to data type descriptor for SET}
      set_val_p: sst_set_val_p_t;      {set value, one bit for each possible element}
      );
sst_dtype_pnt_k: (                     {data type is a pointer}
      pnt_dtype_p: sst_dtype_p_t;      {points to data type desc of pointer}
      pnt_exp_p: sst_exp_p_t;          {pnt to variable being referenced, may be NIL}
      );
    end;

  sst_set_val_t =                      {one bit for each possible element in a set}
    array[0..0] of sys_int_conv32_t;   {32 bits stored in each array element}

  sst_symbol_t = record                {all the data about one symbol}
    name_in_p: string_var_p_t;         {points to name as appeared in source code}
    name_out_p: string_var_p_t;        {points to name as used in output}
    next_p: sst_symbol_p_t;            {chain pointer for application use}
    char_h: syo_char_t;                {handle to source character}
    scope_p: sst_scope_p_t;            {points to scope this symbol defined in}
    symtype: sst_symtype_k_t;          {symbol type, use SST_SYMTYPE_xxx_K}
    flags: sst_symflag_t;              {set of one-bit status flags}
    case sst_symtype_k_t of            {different data for each symbol type}
sst_symtype_const_k: (                 {symbol is a constant}
      const_exp_p: sst_exp_p_t;        {points to expression defining constant value}
      );
sst_symtype_enum_k: (                  {symbol is value of an enumerated type}
      enum_next_p: sst_symbol_p_t;     {points to name for next higher value}
      enum_prev_p: sst_symbol_p_t;     {points to name for next lower value}
      enum_dtype_p: sst_dtype_p_t;     {points to enumerated data type}
      enum_ordval: sys_int_machine_t;  {ordinal value of this name}
      );
sst_symtype_dtype_k: (                 {symbol is a data type}
      dtype_dtype_p: sst_dtype_p_t;    {points to data type descriptor}
      );
sst_symtype_field_k: (                 {symbol is a field name of record data type}
      field_parent_p: sst_dtype_p_t;   {points to parent data type of field}
      field_next_p: sst_symbol_p_t;    {points to name for next field in record}
      field_dtype_p: sst_dtype_p_t;    {points to data type for this field}
      field_ofs_adr: sys_int_adr_t;    {machine adr offset from record start}
      field_ofs_bits: sys_int_machine_t; {additional bits offset}
      field_variant: sys_int_machine_t; {sequential overlay number, 0 = base}
      field_var_val: sst_var_value_t;  {user ID for this overlay}
      );
sst_symtype_var_k: (                   {symbol is a variable}
      var_dtype_p: sst_dtype_p_t;      {pointer to data type definition}
      var_val_p: sst_exp_p_t;          {points to initial value expression, if any}
      var_arg_p: sst_proc_arg_p_t;     {points to arg descriptor if dummy argument}
      var_proc_p: sst_proc_p_t;        {points to routine descriptor if dummy arg}
      var_com_p: sst_symbol_p_t;       {points to common block symbol if in common}
      var_next_p: sst_symbol_p_t;      {points to next var in common block}
      );
sst_symtype_abbrev_k: (                {symbol is an abbreviation}
      abbrev_var_p: sst_var_p_t;       {points to "variable" being abbreviated}
      );
sst_symtype_proc_k: (                  {symbol is a procedure}
      proc: sst_proc_t;
      proc_scope_p: sst_scope_p_t;     {points to scope for rest of procedure}
      proc_dtype_p: sst_dtype_p_t;     {points to PROCEDURE data type descriptor}
      proc_funcvar_p: sst_symbol_p_t;  {points to function return "variable" symbol}
      );
sst_symtype_prog_k: (                  {symbol is a program name}
      prog_scope_p: sst_scope_p_t;     {points to scope for rest of program}
      );
sst_symtype_com_k: (                   {symbol is a common block name}
      com_first_p: sst_symbol_p_t;     {points to first variable in common block}
      com_size: sys_int_max_t;         {common block size in machine addresses}
      );
sst_symtype_module_k: (                {symbol is a module name}
      module_scope_p: sst_scope_p_t;   {points to scope for rest of module}
      );
sst_symtype_label_k: (                 {symbol is a statement label}
      label_opc_p: sst_opc_p_t;        {points to opcode for label}
      );
sst_symtype_front_k: (                 {reserved symbol type for front end use}
      front_p: sst_sym_front_p_t;      {points to private data used by front end}
      );
sst_symtype_back_k: (                  {reserved symbol type for back end use}
      back_p: sst_sym_back_p_t;        {points to private data used by back end}
      );
sst_symtype_illegal_k: (               {illegal, internal error if reference this}
      );
    end;

  sst_scope_t = record                 {data about a scope or namespace}
    mem_p: util_mem_context_p_t;       {points to mem context for this scope}
    hash_h: string_hash_handle_t;      {hash table for input names at this scope}
    hash_out_h: string_hash_handle_t;  {hash table for output names at this scope}
    parent_p: sst_scope_p_t;           {points to parent scope block}
    symbol_p: sst_symbol_p_t;          {points to top symbol for this scope}
    flag_ref_used: boolean;            {flag referenced symbols as used when TRUE}
    end;
{
*   Data types related to internal opcodes.  The input source code is "compiled"
*   into these opcodes.  The output source code is then derived from the opcode list.
}
  sst_op1_t =                          {set of all the unadic operators}
    set of sst_op1_k_t;

  sst_op2_t =                          {set of all the diadic operators}
    set of sst_op2_k_t;

  sst_ele_exp_p_t =                    {pointer to set elements range descriptor}
    ^sst_ele_exp_t;

  sst_ele_exp_t = record               {descriptor for a range of set elements}
    next_p: sst_ele_exp_p_t;           {points to descriptor for next elements range}
    first_p: sst_exp_p_t;              {points to exp for ele val or range start}
    last_p: sst_exp_p_t;               {ponts to exp for ele range end or NIL}
    end;

  sst_exp_term_t = record              {descriptor for additional term in expression}
    next_p: sst_exp_term_p_t;          {points to next term in parent expression}
    op2: sst_op2_k_t;                  {operator between previous and this term}
    op1: sst_op1_k_t;                  {unadic operator applied to this term}
    ttype: sst_term_k_t;               {what kind of term this is}
    str_h: syo_string_t;               {handle to source file character range}
    dtype_p: sst_dtype_p_t;            {points to root data type for this item}
    dtype_hard: boolean;               {TRUE if data type is hard}
    val_eval: boolean;                 {TRUE if attempted to resolve value}
    val_fnd: boolean;                  {TRUE if value known at compile time}
    val: sst_var_value_t;              {expression value if VAL_FND is TRUE}
    rwflag: sst_rwflag_t;              {read/write permission for this term}
    case sst_term_k_t of
sst_term_const_k: (                    {term is a constant, value in VAL field}
      );
sst_term_var_k: (                      {term is a variable value}
      var_var_p: sst_var_p_t;          {pointer to full variable descriptor}
      );
sst_term_func_k: (                     {term is returned value of a function}
      func_var_p: sst_var_p_t;         {points to descriptor for this func ref}
      func_proc_p: sst_proc_p_t;       {points to function call descriptor}
      func_proct_p: sst_proc_p_t;      {points to function template descriptor}
      );
sst_term_ifunc_k: (                    {term is value of intrinsic function}
      ifunc_id: sst_ifunc_k_t;         {ID for this intrinsic function}
      ifunc_args_p: sst_exp_chain_p_t; {points to chain of function arguments}
      );
sst_term_type_k: (                     {term is hard type-casting of expression}
      type_dtype_p: sst_dtype_p_t;     {points to dtype acting as casting function}
      type_exp_p: sst_exp_p_t;         {points to expression being casted}
      );
sst_term_set_k: (                      {term is a set value}
      set_first_p: sst_ele_exp_p_t;    {points to descriptor for first element(s)}
      );
sst_term_exp_k: (                      {term is a nested expression}
      exp_exp_p: sst_exp_p_t;          {pointer to nested expression}
      );
sst_term_field_k: (                    {term is for whole field in a record}
      field_sym_p: sst_symbol_p_t;     {points to symbol descriptor for this field}
      field_exp_p: sst_exp_p_t;        {points to expression for field value}
      );
sst_term_arele_k: (                    {term is for set of array elements}
      arele_start: sys_int_machine_t;  {start index ordinal with this val, first = 0}
      arele_n: sys_int_machine_t;      {number of indicies with this value}
      arele_exp_p: sst_exp_p_t;        {points to expression for elements value}
      );
    end;

  sst_exp_t = record                   {definition of an arbitrary expression}
    str_h: syo_string_t;               {handle to source file character range}
    dtype_p: sst_dtype_p_t;            {points to root data type for this expression}
    dtype_hard: boolean;               {TRUE if data type is hard}
    val_eval: boolean;                 {TRUE if attempted to resolve value}
    val_fnd: boolean;                  {TRUE if value known at compile time}
    val: sst_var_value_t;              {expression value if VAL_FND is TRUE}
    rwflag: sst_rwflag_t;              {read/write permission for this expression}
    term1: sst_exp_term_t;             {descriptor for first term in expression}
    end;

  sst_var_mod_t = record               {data for one variable modifier}
    next_p: sst_var_mod_p_t;           {points to next modifier in chain}
    modtyp: sst_var_modtyp_k_t;        {what kind of modifier this is}
    case sst_var_modtyp_k_t of
sst_var_modtyp_top_k: (                {modifier is top variable name}
      top_str_h: syo_string_t;         {handle to source file character range}
      top_sym_p: sst_symbol_p_t;       {points to symbol for top name}
      );
sst_var_modtyp_unpnt_k: (              {modifier is to dereference pointer}
      );
sst_var_modtyp_subscr_k: (             {next less significant subscript of array}
      subscr_exp_p: sst_exp_p_t;       {points to expression for this subscript}
      subscr_first: boolean;           {TRUE if first subscript of set}
      subscr_last: boolean;            {TRUE if last subscript of set}
      );
sst_var_modtyp_field_k: (              {modifier is field name of record}
      field_str_h: syo_string_t;       {handle to source file character range}
      field_sym_p: sst_symbol_p_t;     {points to symbol for this field name}
      );
    end;

  sst_var_t = record                   {full descriptor of a variable or symbol ref}
    mod1: sst_var_mod_t;               {first (mandatory) modifier for this variable}
    dtype_p: sst_dtype_p_t;            {points to data type descriptor when applicable}
    rwflag: sst_rwflag_t;              {read/write permission for this "variable"}
    vtype: sst_vtype_k_t;              {what type of variable descriptor is this ?}
    case sst_vtype_k_t of
sst_vtype_var_k: (                     {a variable}
      );
sst_vtype_dtype_k: (                   {a data type reference}
      );
sst_vtype_rout_k: (                    {a routine reference}
      rout_proc_p: sst_proc_p_t;       {points to template of called routine}
      );
sst_vtype_const_k: (                   {a named constant reference}
      const_val_p: sst_var_value_p_t;  {points to constant's value descriptor}
      );
sst_vtype_com_k: (                     {a common block reference}
      );
    end;

  sst_case_opc_p_t =                   {pnts to exec block desc for one CASE choice}
    ^sst_case_opc_t;

  sst_case_val_p_t =                   {points to desc for one CASE expression value}
    ^sst_case_val_t;

  sst_case_val_pp_t =                  {points to a choice values chain pointer}
    ^sst_case_val_p_t;

  sst_case_val_t = record              {describes one CASE expression value}
    exp_p: sst_exp_p_t;                {points to expression for this choice value}
    val: sys_int_max_t;                {ordinal value of choice expression}
    opc_p: sst_case_opc_p_t;           {pointer to descriptor for executable block}
    next_val_p: sst_case_val_p_t;      {points to next value in CASE}
    next_opc_p: sst_case_val_p_t;      {points to next value for same choice}
    end;

  sst_case_opc_t = record              {describes code for one choice in CASE sment}
    val_first_p: sst_case_val_p_t;     {points to first value that selects this case}
    code_p: sst_opc_p_t;               {points to opcodes for this executable block}
    next_p: sst_case_opc_p_t;          {points to next executable block descriptor}
    end;

  sst_opc_t = record                   {mandatory header for all opcodes}
    next_p: sst_opc_p_t;               {points to next successive opcode, NIL = end}
    opcode: sst_opc_k_t;               {what type of opcode this is}
    str_h: syo_string_t;               {handle to source file character range}
    case sst_opc_k_t of
sst_opc_module_k: (                    {opcode is grouping of routines}
      module_sym_p: sst_symbol_p_t;    {points to module name symbol}
      module_p: sst_opc_p_t;           {points to opcodes in this module}
      );
sst_opc_prog_k: (                      {opcode is start of new program}
      prog_sym_p: sst_symbol_p_t;      {points to program name symbol}
      prog_p: sst_opc_p_t;             {points to opcodes in this program}
      );
sst_opc_rout_k: (                      {opcode is start of a routine}
      rout_sym_p: sst_symbol_p_t;      {points to routine name symbol}
      rout_p: sst_opc_p_t;             {points to opcodes in this routine}
      );
sst_opc_exec_k: (                      {opcode points to chain of executable code}
      exec_p: sst_opc_p_t;             {points to first opcode in executable chain}
      );
sst_opc_label_k: (                     {opcode indicates a label exists here}
      label_sym_p: sst_symbol_p_t;     {points to label symbol}
      );
sst_opc_call_k: (                      {opcode is subroutine call}
      call_var_p: sst_var_p_t;         {points to subroutine name reference}
      call_proc_p: sst_proc_p_t;       {points to called routine descriptor}
      call_proct_p: sst_proc_p_t;      {points to template for called routine}
      );
sst_opc_assign_k: (                    {opcode is assignment statement}
      assign_var_p: sst_var_p_t;       {points to full target variable descriptor}
      assign_exp_p: sst_exp_p_t;       {points to full expression descriptor}
      );
sst_opc_goto_k: (                      {opcode indicates unconditional GOTO}
      goto_sym_p: sst_symbol_p_t;      {points to label symbol}
      );
sst_opc_case_k: (                      {execute only one of a list of blocks}
      case_exp_p: sst_exp_p_t;         {expression that selects which block}
      case_val_p: sst_case_val_p_t;    {points to first expression value in chain}
      case_opc_p: sst_case_opc_p_t;    {points to first exec block desc in chain}
      case_none_p: sst_opc_p_t;        {code when no match, may be NIL}
      );
sst_opc_if_k: (                        {opcode is IF ... THEN ... ELSE ... statement}
      if_exp_p: sst_exp_p_t;           {pointer to boolean expression}
      if_true_p: sst_opc_p_t;          {points to opcodes chain for TRUE case}
      if_false_p: sst_opc_p_t;         {points to opcodes chain for FALSE case}
      );
sst_opc_loop_cnt_k: (                  {counted loop (Pascal FOR, Fortran DO, etc)}
      lpcn_var_p: sst_var_p_t;         {points to counting variable}
      lpcn_exp_start_p: sst_exp_p_t;   {points to starting value expression}
      lpcn_exp_end_p: sst_exp_p_t;     {points to ending value expression}
      lpcn_exp_inc_p: sst_exp_p_t;     {points to increment value expression}
      lpcn_code_p: sst_opc_p_t;        {executable statements of the loop}
      lpcn_inc_dir: sst_incdir_k_t;    {up/down/unknown increment direction}
      );
sst_opc_loop_ttop_k: (                 {loop with test at start of loop}
      lptp_exp_p: sst_exp_p_t;         {pnt to bool condition, TRUE continues loop}
      lptp_code_p: sst_opc_p_t;        {executable statements of the loop}
      );
sst_opc_loop_tbot_k: (                 {loop with test at end of loop}
      lpbt_exp_p: sst_exp_p_t;         {pnt to bool condition, FALSE continues loop}
      lpbt_code_p: sst_opc_p_t;        {executable statements of the loop}
      );
sst_opc_loop_next_k: (                 {go to start of next time around loop}
      );
sst_opc_loop_exit_k: (                 {unconditionally exit loop}
      );
sst_opc_return_k: (                    {return from subroutine}
      );
sst_opc_abbrev_k: (                    {abbreviations in effect over a block of code}
      abbrev_scope_p: sst_scope_p_t;   {points to scope of abbreviation symbols}
      abbrev_code_p: sst_opc_p_t;      {points to code that can use abbrevs}
      abbrev_sym_first_p: sst_symbol_p_t; {points to first abbrev sym in linked list}
      );
sst_opc_discard_k: (                   {call function but discard its return value}
      discard_exp_p: sst_exp_p_t;      {expression which is one function reference}
      );
sst_opc_write_k: (                     {write expression to standard output}
      write_exp_p: sst_exp_p_t;        {expression to write value of}
      write_width_exp_p: sst_exp_p_t;  {total field width exp, NIL = free form}
      write_width2_exp_p: sst_exp_p_t; {extra field wid, # char right of "."}
      );
sst_opc_write_eol_k: (                 {write end of line to standard output}
      );
    end;

  sst_frame_opc_pos_t = record         {stack frame for opcode position}
    opc_p: sst_opc_p_t;                {pointer to current opcode}
    opc_pp: sst_opc_pp_t;              {pointer to where next opcode pointer goes}
    end;

  sst_frame_opc_pos_p_t =              {pointer to opcode position stack frame}
    ^sst_frame_opc_pos_t;

  sst_config_size_t = record           {descriptor for target data type size}
    size: sys_int_machine_t;           {size in machine addresses}
    name: string_var32_t;              {data type name in target language}
    dtype_p: sst_dtype_p_t;            {points to data type descriptor}
    end;

  sst_size_list_t =                    {list of available sizes for a number type}
    array[1..sst_size_list_max] of sst_config_size_t;

  sst_size_set_t = record              {data about one available SET size}
    size: sys_int_machine_t;           {size in machine addresses}
    dtype_p: sst_dtype_p_t;            {points to INTEGER dtype when not native SET}
    end;

  sst_size_set_list_t =                {list of available sizes for SET data type}
    array[1..sst_size_list_max] of sst_size_set_t;

  sst_config_t = record                {configuration info about target machine}
    bits_adr: sys_int_machine_t;       {number of bits in one machine address}

    n_size_int: sys_int_machine_t;     {number of different integer sizes available}
    size_int: sst_size_list_t;         {size of all the different integers}
    int_machine_p: sst_dtype_p_t;      {pnt to data type for "machine" integer}
    int_adr_p: sst_dtype_p_t;          {pnt to data type for int to hold machine adr}
    name_univ_ptr: string_var32_t;     {name of universal pointer data type}

    n_size_float: sys_int_machine_t;   {number of floating point sizes available}
    size_float: sst_size_list_t;       {size of different floating point formats}
    float_machine_p: sst_dtype_p_t;    {pnt to dtype for "machine" float}
    float_single_p: sst_dtype_p_t;     {pnt to dtype for "single precision" float}
    float_double_p: sst_dtype_p_t;     {pnt to dtype for "double precision" float}

    n_size_set: sys_int_machine_t;     {number of explicit SET sizes available}
    size_set: sst_size_set_list_t;     {data about each explicit SET size}
    size_set_multiple: sst_size_set_t; {data about multiple SET size for large sets}

    align_min_rec: sys_int_adr_t;      {min alignment for non-packed records}
    align_min_rec_pack: sys_int_adr_t; {min alignment for packed records}
    pass_val_size_max: sys_int_adr_t;  {max size arg allowed to pass by value}
    size_enum_native: sys_int_adr_t;   {size target compiler native enumerated var}
    size_enum: sys_int_adr_t;          {size of enumerated data type}
    size_bool: sys_int_adr_t;          {size of default boolean type}
    name_bool: string_var32_t;         {name of boolean data type}
    bits_char: sys_int_machine_t;      {number of bits in standard character}
    size_char: sys_int_adr_t;          {number of machine addresses for one char}
    name_char: string_var32_t;         {name of character data type}
    manuf: sst_manuf_k_t;              {target manufacturer selection}
    os: sys_os_k_t;                    {target operating system selection}
    lang: sst_lang_k_t;                {output language selection}
    suffix_fnam: string_var32_t;       {mandatory output file name suffix}
    suffix_dtype: string_var32_t;      {suffix for data type names}
    suffix_const: string_var32_t;      {suffix for constant names}
    sym_len_max: sys_int_machine_t;    {max characters in output symbol names}
    charcase: syo_charcase_k_t;        {make names upper, lower, or case sensitive}
    reserve_p: string_chain_ent_p_t;   {points to start of reserved names chain}
    config: string_var8192_t;          {config info specific to particular back end}
    end;
{
*   Call table for use by built-in front end routines.
}
  sst_r_t = record

doit: ^procedure (                     {do the whole front end phase}
  in      fnam: univ string_var_arg_t; {raw input file name}
  in out  gnam: univ string_var_arg_t; {returned as generic name of input file}
  out     stat: sys_err_t);            {completion status code}

  end;
{
*   Call table for use by built-in back end routines.
}
  sst_w_t = record

allow_break: ^procedure;               {allow line break at current position}

append: ^procedure (                   {append var string to current output line}
  in      str: univ string_var_arg_t); {the string to append}

appendn: ^procedure (                  {raw append N chars to current output line}
  in      chars: univ string;          {the characters to append}
  in      n_chars: string_index_t);    {the number of characters to append}

appends: ^procedure (                  {append string to current output line}
  in      s: string);                  {string to append, trailing blanks ignored}

append_sym_name: ^procedure (          {append symbol output name to curr position}
  in      sym: sst_symbol_t);          {symbol descriptor to write name of}

blank_line: ^procedure;                {make sure preceeding line is blank}

break: ^procedure;                     {break line at curr break point, if any}

comment_end: ^procedure;               {write end of comment string}

comment_set: ^procedure (              {set comment to correspond to next char}
  in      s: univ string_var_arg_t);   {body of comment string}

comment_start: ^procedure;             {write start of comment string}

delimit: ^procedure;                   {write delim or break line before next output}

doit: ^procedure (                     {do the whole back end phase}
  in      gnam: univ string_var_arg_t; {raw output file name}
  out     stat: sys_err_t);            {completion status code}

indent: ^procedure;                    {increase indentation by one level}

line_close: ^procedure;                {close current line}

line_insert: ^procedure;               {raw insert new line at current position}

line_new: ^procedure;                  {set up so next char goes on next line}

line_new_cont: ^procedure;             {set up for next line is continuation line}

name: ^procedure (                     {make output symbol name from input name}
  in      name_in: univ string;        {input symbol name characters}
  in      name_in_len: string_index_t; {number of characters in NAME_IN}
  in      ext: univ string;            {output name suffix, if any}
  in      ext_len: string_index_t;     {number of characters in EXT}
  in      rename: sst_rename_k_t;      {what kind of re-naming is allowed}
  in out  name_out: univ string_var_arg_t; {resulting output source symbol name}
  out     pos: string_hash_pos_t);     {pos handle where name goes in symbol table}

name_sym: ^procedure (                 {set output name for an existing symbol}
  in out  sym: sst_symbol_t);          {NOP if symbol already has output name}

notify_src_range: ^procedure (         {declare source chars range for new output}
  in      str_h: syo_string_t);        {new out chars related to these in chars}

tab_indent: ^procedure;                {tab to current indentation level}

undent: ^procedure;                    {decrease indentation by one level}

undent_all: ^procedure;                {reset to no indentation level}

write: ^procedure (                    {write output lines from memory to file}
  in out  conn: file_conn_t;           {connection handle to output file}
  out     stat: sys_err_t);            {completion status code}

  end;                                 {end of back end call table}
{
*   Data types related to back end output stream handling.
}
  sst_out_dyn_t = record               {"dynamic" output state}
    str_p: string_chain_ent_p_t;       {string chain entry for current line}
    indent_chars: sys_int_machine_t;   {number of characters to indent}
    indent_level: sys_int_machine_t;   {logical indentation level}
    break_len: string_index_t;         {line length if break done}
    break_start: string_index_t;       {first char that goes on next line if break}
    str_h: syo_string_t;               {source stream handle for curr out chars}
    wpos: sst_wpos_k_t;                {write position relative to curr line}
    comm: string_var80_t;              {comment for this line}
    commented_pos: string_index_t;     {identifies char tagged with comment}
    end;

  sst_out_dyn_p_t =                    {pointer to dynamic output chars state}
    ^sst_out_dyn_t;

  sst_out_t = record                   {current state for writing to out lines list}
    mem_p: util_mem_context_p_t;       {points to memory context for out lines}
    first_str_p: string_chain_ent_p_t; {points to first output line block}
    wrap_len: sys_int_machine_t;       {try wrap line before exceed this many chars}
    indent_size: sys_int_machine_t;    {number of characters per indentation level}
    dyn_p: sst_out_dyn_p_t;            {points to current dynamic state}
    comm_start: string_var4_t;         {comment start, used by default routine}
    comm_end: string_var4_t;           {comment end, used by default routine}
    comm_pos: sys_int_machine_t;       {default column for start of comment}
    end;
{
*   Public common block.
}
var (sst)
  sst_scope_root_p: sst_scope_p_t;     {points to root scope (has no parent)}
  sst_scope_p: sst_scope_p_t;          {points to current scope hierarchy}
  sst_names_p: sst_scope_p_t;          {points to current name space hierarchy}
  sst_align: sys_int_machine_t;        {current data alignment rule}
  sst_hash_buckets: sys_int_machine_t; {number of buckets for any new hash tables}
  sst_opc_first_p: sst_opc_p_t;        {points to first opcode in chain}
  sst_opc_p: sst_opc_p_t;              {points to current opcode in chain}
  sst_opc_next_pp: sst_opc_pp_t;       {pnt to where pnt to next new opcode put}
  sst_stack: util_stack_handle_t;      {stack for use by front/back ends}
  sst_level_debug: sys_int_machine_t;  {debug level from command line, 0 = none}
  sst_level_unused: sys_int_machine_t; {from -SHOW_UNUSED command line option}
  sst_local_ins: boolean;              {true on -LOCAL_INS command line option}
  sst_ins: boolean;                    {true on -INS command line option}
  sst_writeall: boolean;               {true on -WRITE_ALL command line option}
  sst_gui: boolean;                    {true on -GUI command line option}
  sst_ins_tnam: string_treename_t;     {name of INS file to translate if SST_INS true}
  sst_oname_unique: string_var32_t;    {unique string for making output names}
  sst_r: sst_r_t;                      {call table for front end functions}
  sst_w: sst_w_t;                      {call table for back end functions}
{
*   Pointers to basic data types.
}
  sst_dtype_int_max_p: sst_dtype_p_t;  {maximum size machine integer}
  sst_dtype_float_max_p: sst_dtype_p_t; {maximum size machine floating point}
  sst_dtype_uptr_p: sst_dtype_p_t;     {universal pointer (machine address)}
  sst_dtype_bool_p: sst_dtype_p_t;     {normal boolean}
  sst_dtype_char_p: sst_dtype_p_t;     {normal character}
  sst_dtype_enum_p: sst_dtype_p_t;     {points to INT used to emulate ENUM, if any}
  sst_dtype_none: sst_dtype_t;         {used to indicate no checking internally}

  sst_config: sst_config_t;            {config info about target machine}
  sst_out: sst_out_t;                  {current state of output stream}
  sst_out_dyn: sst_out_dyn_t;          {dynamic part of output stream state}
{
*   Entry point declarations.
}
procedure sst_call (                   {create subroutine call opcode}
  in      sym: sst_symbol_t);          {symbol for name of subroutine to call}
  val_param; extern;

procedure sst_call_arg_enum (          {add constant enum call arg to call}
  in      opc: sst_opc_t;              {CALL opcode to add argument to}
  in      sym: sst_symbol_t);          {symbol descriptor for enumerated value}
  val_param; extern;

procedure sst_call_arg_int (           {add constant integer call arg to call}
  in      opc: sst_opc_t;              {CALL opcode to add argument to}
  in      ival: sys_int_max_t);        {integer value for argument}
  val_param; extern;

procedure sst_call_arg_str (           {add constant string call arg to call}
  in      opc: sst_opc_t;              {CALL opcode to add argument to}
  in      str: univ string;            {string for argument value}
  in      len: sys_int_machine_t);     {string length}
  val_param; extern;

procedure sst_call_arg_var (           {add variable call arg to call}
  in      opc: sst_opc_t;              {CALL opcode to add argument to}
  in      sym: sst_symbol_t);          {variable to add as call argument}
  val_param; extern;

function sst_char_from_ins (           {check for char is from special include file}
  in      char_h: syo_char_t)          {handle to source character to check}
  :boolean;                            {TRUE is character is from the include file}
  val_param; extern;

procedure sst_charh_info (             {get info from handle to source character}
  in      char_h: syo_char_t;          {handle to source character}
  in out  fnam: univ string_var_arg_t; {name of source file for first char}
  out     lnum: sys_int_machine_t);    {source file line number of first char}
  extern;

procedure sst_config_out (             {configure and init back end}
  in      fnam: univ string_var_arg_t); {configuration file name}
  extern;

procedure sst_debug_dtype (            {print data about a data type}
  in      d: sst_dtype_t;              {data type to print}
  in      indent: sys_int_machine_t;   {how many columns to indent}
  in      detail: boolean);            {TRUE if print detail, not just name}
  extern;

procedure sst_dtype_align (            {set data type alignment}
  in out  dtype: sst_dtype_t;          {data type to set alignment for}
  in      rule_align: sys_int_machine_t); {alignment rule to apply}
  extern;

function sst_dtype_convertable (       {check if data type convertable to another}
  in      dtype_in: sst_dtype_t;       {data type to be converted}
  in      dtype_res: sst_dtype_t)      {target data type to convert to}
  :boolean; extern;                    {TRUE unambigous conversion possible}

procedure sst_dtype_new (              {allocate new data type in current scope}
  out     dtype_p: sst_dtype_p_t);     {pnt to new created and initted block}
  extern;

procedure sst_dtype_new_string (       {create new dtype descriptor for a string}
  in      str_len: string_index_t;     {length of string}
  out     dtype_p: sst_dtype_p_t);     {pointer to new data type descriptor}
  extern;

procedure sst_dtype_new_subrange (     {create new subrange data type, if necessary}
  in      dtype_base: sst_dtype_t;     {base data type to create subrange of}
  in      ord_min: sys_int_max_t;      {minimum ordinal value of subrange}
  in      ord_max: sys_int_max_t;      {maximum ordinal value of subrange}
  out     dtype_p: sst_dtype_p_t);     {subrange dtype, or base dtype if whole range}
  extern;

procedure sst_dtype_resolve (          {resolve arbitrary data type to base dtype}
  in      dtype: sst_dtype_t;          {descriptor for data type to resolve}
  out     dtype_base_p: sst_dtype_p_t; {points to base data type descriptor}
  out     dtype_base: sst_dtype_k_t);  {resolved base data type}
  extern;

procedure sst_dtype_size (             {set all the size fields given basic info}
  in out  dtype: sst_dtype_t);         {data type to set sizes for}
  extern;

procedure sst_exp_eval (               {evaluate compiled expression}
  in out  exp: sst_exp_t;              {expression, fills in value and data type}
  in      nval_err: boolean);          {unknown value at compile time is err if TRUE}
  extern;

procedure sst_exp_const_enum (         {create const expression with ENUM value}
  in      sym: sst_symbol_t;           {symbol descriptor for enumerated value}
  out     exp_p: sst_exp_p_t);         {pointer to new expression descriptor}
  extern;

procedure sst_exp_const_float (        {create const expression with FLOAT value}
  in      f: double;                   {floating point value}
  out     exp_p: sst_exp_p_t);         {pointer to new expression descriptor}
  val_param; extern;

procedure sst_exp_const_int (          {create const expression with INTEGER value}
  in      i: sys_int_max_t;            {integer value}
  out     exp_p: sst_exp_p_t);         {pointer to new expression descriptor}
  val_param; extern;

procedure sst_exp_const_bool (         {create const expression with BOOLEAN value}
  in      b: boolean;                  {boolean value}
  out     exp_p: sst_exp_p_t);         {pointer to new expression descriptor}
  val_param; extern;

procedure sst_exp_const_str (          {create const expression with STRING value}
  in      str: univ string;            {string value}
  in      len: sys_int_machine_t;      {number of characters in the string}
  in      dtlen: sys_int_machine_t;    {max length of string data type}
  out     exp_p: sst_exp_p_t);         {pointer to new expression descriptor}
  val_param; extern;

procedure sst_exp_const_vstr (         {create const expression with STRING value}
  in      s: univ string_var_arg_t;    {string value, described using VAR STRING}
  out     exp_p: sst_exp_p_t);         {pointer to new expression descriptor}
  extern;

function sst_exp_make_var (            {make expression that references a variable}
  in      sym: sst_symbol_t)           {symbol of variable to make expression from}
  :sst_exp_p_t;                        {returned pointer to new expression}
  extern;

function sst_exp_simple (              {check for expression is simple/complicated}
  in      exp: sst_exp_t)              {expression descriptor to check}
  :boolean;                            {TRUE if no computes needed to evaluate exp}
  extern;

procedure sst_exp_useage_check (       {check expression attributes for given useage}
  in      exp: sst_exp_t;              {expression to check}
  in      rw: sst_rwflag_t;            {read/write expression useage}
  in      dtype: sst_dtype_t);         {required data type of expression}
  extern;

procedure sst_flag_used_dtype (        {flag symbols eventually used from a dtype}
  in      dtype: sst_dtype_t);         {dtype descriptor that may reference symbols}
  extern;

procedure sst_flag_used_exp (          {flag symbols eventually used from expression}
  in      exp: sst_exp_t);             {expression that may reference symbols}
  extern;

procedure sst_flag_used_opcodes (      {flag symbols eventually used from opcodes}
  in      first_p: sst_opc_p_t);       {points to first opcode in chain, may be NIL}
  extern;

procedure sst_flag_used_rout (         {flag symbols eventually used from rout call}
  in      proc: sst_proc_t);           {routine descriptor}
  extern;

procedure sst_flag_used_symbol (       {flag symbols eventually used from this sym}
  in out  sym: sst_symbol_t);          {descriptor for top symbol to follow}
  extern;

procedure sst_flag_used_var (          {flag symbols eventually used from var desc}
  in      v: sst_var_t);               {var descriptor that may reference symbols}
  extern;

procedure sst_func_arg (               {add argument to function value expression}
  in out  exp: sst_exp_t;              {function reference exp to add argument to}
  in var  arg: sst_exp_t);             {argument expression}
  val_param; extern;

function sst_func_exp (                {make function value expression, no args}
  in var  func: sst_symbol_t)          {function symbol}
  :sst_exp_p_t;                        {pointer to new expression}
  val_param; extern;

procedure sst_init (                   {init translator data stuctures}
  in      symbol_len: sys_int_machine_t; {max length allowed for a symbol}
  in out  parent_mem: util_mem_context_t); {parent memory context to use}
  extern;

procedure sst_intrinsic_dtype (        {create intrinsic data type in curr scope}
  in      name: string;                {data type name}
  in      dtype: sst_dtype_k_t;        {which base data type is this}
  in      size: sys_int_adr_t;         {size in machine addresses}
  out     dt_p: sst_dtype_p_t);        {pointer to created and initialized data type}
  extern;

procedure sst_mem_alloc_namesp (       {allocate memory tied to current name space}
  in      size: sys_int_adr_t;         {amount of memory to allocate}
  out     p: univ_ptr);                {pointer to start of new memory area}
  extern;

procedure sst_mem_alloc_scope (        {allocate memory tied to current scope}
  in      size: sys_int_adr_t;         {amount of memory to allocate}
  out     p: univ_ptr);                {pointer to start of new memory area}
  extern;

procedure sst_name_new_out (           {add symbol to output table at curr scope}
  in      name: univ string_var_arg_t; {name of new symbol}
  out     name_p: univ string_var_p_t; {will point to name stored in hash table}
  out     sym_pp: sst_symbol_pp_t;     {points to hash table entry user data area}
  out     stat: sys_err_t);            {completion status code}
  extern;

procedure sst_opcode_new;              {create new empty opcode, make current}
  extern;

procedure sst_opcode_pos_pop;          {pop curr opcode position from stack}
  extern;

procedure sst_opcode_pos_push (        {push curr opc position, start new chain}
  in out  opc_p: sst_opc_p_t);         {pointer to new chain, will be set to NIL}
  extern;

procedure sst_ordval (                 {find ordinal value from value descriptor}
  in      val: sst_var_value_t;        {input constant value descriptor}
  out     ordval: sys_int_max_t;       {returned ordinal value}
  out     stat: sys_err_t);            {set to error if ordinal value not exist}
  extern;

function sst_rec_variant (             {determine if record has any overlays}
  in      dtype: sst_dtype_t)          {descriptor for record's data type}
  :boolean;                            {TRUE if record has variants}
  val_param; extern;

procedure sst_routines_match (         {check that two routine descriptions match}
  in      proc1: sst_proc_t;           {descriptor for first routine}
  in      proc2: sst_proc_t;           {descriptor for second routine}
  out     stat: sys_err_t);            {no error if routines match}
  extern;

procedure sst_rwcheck (                {check for proper read/write access used}
  in      rw_used: sst_rwflag_t;       {read/write access actually used}
  in      rw_allowed: sst_rwflag_t;    {read/write access allowed}
  out     stat: sys_err_t);            {no error if legal access}
  extern;

procedure sst_scope_new;               {create new scope subordinate to curr scope}
  extern;

procedure sst_scope_old;               {pop back to parent scope}
  extern;

procedure sst_scope_unused_show (      {write names of unused symbols in a scope}
  in      scope: sst_scope_t);         {scope to list unused symbols in}
  extern;

procedure sst_set_dtypes_combine (     {make composite data type from two set dtypes}
  in      dt_in1: sst_dtype_t;         {data type descriptor for first set}
  in      dt_in2: sst_dtype_t;         {data type descriptor for second set}
  out     dt_out_p: sst_dtype_p_t);    {pnt to combined dtype, NIL = incompatible}
  extern;

procedure sst_set_ele_find (           {get handle to bit for particular set element}
  in      val: sst_var_value_t;        {constant set expression value descriptor}
  in      ele: sys_int_machine_t;      {set element number, 0 - N_ELE-1}
  out     p: sys_int_conv32_p_t;       {points to word containing set element}
  out     mask: sys_int_conv32_t);     {mask word for this set element bit}
  extern;

procedure sst_set_val_convert (        {convert set value expression data type}
  in out  val: sst_var_value_t;        {set value to convert}
  in      dtype: sst_dtype_t;          {desired target SET data type}
  out     success: boolean);           {TRUE if conversion was successful}
  extern;

procedure sst_strh_info (              {get info from handle to source string}
  in      str_h: syo_string_t;         {handle to source characters}
  in out  fnam: univ string_var_arg_t; {name of source file for first char}
  out     lnum: sys_int_machine_t);    {source file line number of first char}
  extern;

procedure sst_sym_dtype_new_out (      {create output symbol that is a data type}
  in      name: univ string_var_arg_t; {name of symbol to create}
  in      rename: sst_rename_k_t;      {re-name strategy flag}
  in      dtype: sst_dtype_k_t;        {which base data type it is}
  in      size: sys_int_adr_t;         {data type size in machine addresses}
  out     dt_p: sst_dtype_p_t);        {points to new data type descriptor}
  extern;

procedure sst_sym_var (                {make variable descriptor from simple var symbol}
  in      var sym: sst_symbol_t;       {symbol of a simple variable}
  out     var_p: sst_var_p_t);         {returned pointer to new variable descriptor}
  val_param; extern;

procedure sst_sym_var_new_out (        {create output symbol that is a variable}
  in      dtype: sst_dtype_t;          {descriptor for variable's data type}
  out     sym_p: sst_symbol_p_t);      {points to newly created symbol descriptor}
  extern;

procedure sst_symbol_lookup (          {get data about an existing symbol}
  in      name_h: syo_string_t;        {handle to name string from tag}
  out     sym_p: sst_symbol_p_t;       {returned pointer to symbol descriptor}
  out     stat: sys_err_t);            {completion status code}
  extern;

procedure sst_symbol_lookup_name (     {look up symbol in visible name spaces}
  in      name: univ string_var_arg_t; {name of symbol to look up}
  out     sym_p: sst_symbol_p_t;       {returned pointer to symbol descriptor}
  out     stat: sys_err_t);            {completion status code}
  extern;

procedure sst_symbol_new (             {add symbol to current scope given syn handle}
  in      name_h: syo_string_t;        {handle to name string from tag}
  in      charcase: syo_charcase_k_t;  {SYO_CHARCASE_xxx_K with DOWN, UP or ASIS}
  out     symbol_p: sst_symbol_p_t;    {points to new symbol descriptor}
  out     stat: sys_err_t);            {completion status code}
  val_param; extern;

procedure sst_symbol_new_name (        {add symbol to curr scope given symbol name}
  in      name: univ string_var_arg_t; {name of new symbol}
  out     symbol_p: sst_symbol_p_t;    {points to new symbol descriptor}
  out     stat: sys_err_t);            {completion status code}
  extern;

procedure sst_term_eval (              {evaluate compiled term in expression}
  in out  term: sst_exp_term_t;        {term, fills in value and data type}
  in      nval_err: boolean);          {unknown value at compile time is err if TRUE}
  extern;

function sst_term_simple (             {check for term is simple/complicated}
  in      term: sst_exp_term_t)        {term descriptor to check}
  : boolean;                           {TRUE if no computes needed to evaluate term}
  extern;

procedure sst_var_funcname (           {change var from func return val to func name}
  in out  v: sst_var_t);               {variable descriptor to convert}
  extern;
{
*   Init routines to install various front and back ends.
}
procedure sst_r_pas_init;              {init front end state for reading PASCAL}
  extern;

procedure sst_r_syn_init;              {init front end state for reading .syn files}
  extern;

procedure sst_w_c_init;                {init back end state for writing C}
  extern;
