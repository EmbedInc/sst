msg arg_pass_method_bad
  lan english -def
    INTERNAL ERROR (C back end):  Illegal or unimplemented routine
    argument passing method.  Passing method ID number is %p1i.
msg arg_pass_method_unknown
  lan english -def
    INTERNAL ERROR:  Argument passing method is unknown in C back
    end.
msg array_mode_unexpected
  lan english -def
    INTERNAL ERROR (C back end):  Illegal, unimplemented, or unexpected
    value for array identifier interpretation mode.
    Mode ID value is %p1i.
msg const_implicit_bad
  lan english -def
    INTERNAL ERROR (C back end):  Illegal or unimplemented implicit
    constant identifier encountered.  Value of identifier was %p1i.
msg decl_statement_type_bad
  lan english -def
    INTERNAL ERROR:  Bad declaration statement type.  Statement type
    ID is %p1i.
msg dtype_exp_unexpected
  lan english -def
    C BACK END ERROR:  The data type of this expression was not
    expected here.  Internal data type ID was %p1i.
msg dtype_no_name
  lan english -def
    INTERNAL ERROR (C back end):  Data type has no name, but
    type-casting required.
msg exp_array_ind_order
  lan english -def
    UNIMPLEMENTED:  Initial values for array variables must contain
    all array elements in order in the C back end.
msg exp_array_ind_range
  lan english -def
    UNIMPLEMENTED:  A value for a range of array indicies is not
    implemented in the C back end.
msg exp_array_not_in_var
  lan english -def
    UNIMPLEMENTED:  Array value expressions other than just symbol
    references are not fully implement in the C back end.  They
    are allowed only for declaring an array variable's initial value.
msg exp_record_not_in_var
  lan english -def
    UNIMPLEMENTED:  Record value expressions other than just symbol
    references are not fully implement in the C back end.  They
    are allowed only for declaring a record variable's initial value.
msg extern_in_function
  lan english -def
    The symbol "%p1s" is local to a routine, but is declared as globally
    known.  This is not possible in C.  To fix, move the symbol declaration
    above all routines (module level in Pascal).
msg extern_not_global
  lan english -def
    INTERNAL ERROR:  Symbol "%p1s" is flagged as externally defined, but not
    globally known.
msg goto_nonlocal_unimp
  lan english -def
    UNIMPLEMENTED (C back end):  GOTOs to outside of the current routine
    are not implemented in the C back end.
msg install_reserved_name
  lan english -def
    Error occurred on attempt to install reserved name "%p1s" into
    the base output symbol table.
msg intrinsic_symbol_id_bad
  lan english -def
    INTERNAL ERROR (C back end):  Illegal or unimplemented intrinsic
    symbol ID encountered.  ID number was %p1i.
msg loop_inc_dir_bad
  lan english -def
    INTERNAL ERROR (C back end):  Loop increment direction is illegal
    or unimplemented.  The loop increment direction ID number is %p1i.
msg name_dup_dtype
  lan english -def
    INTERNAL ERROR:  Symbol name "%p1s" is already in the output symbol
    table.  Error occurred in C back end on attempt to add intrinsic
    data type to symbol table.
msg name_out_already_exists
  lan english -def
    INTERNAL ERROR (C back end):  An output name for symbol "%p1s"
    already exists when it was not expected.
msg op_compare_dtype_bad
  lan english -def
    UNIMPLEMENTED (C back end):  Equality comparisons on this data
    type are not implemented.
msg owner_exec_bad
  lan english -def
    INTERNAL ERROR:  Unexpected owner type for block of executable code.
    Type ID of owner is %p1i.
msg scope_old_not_parent
  lan english -def
    INTERNAL ERROR:  Old scope is not the parent of new scope in
    subroutine SST_W_PAS_SCOPE_SET.  Old scope name is "%p1s", new
    scope name is "%p2s".
msg scope_type_bad
  lan english -def
    INTERNAL ERROR:  Illegal, or unimplemented scope type.
    Scope type ID is %p1i.
msg stack_sment_frame_err
  lan english -def
    INTERNAL ERROR:  SST_W_C_SMENT_END was called, but the last frame
    on the stack was not the current statment stack frame.  This probably
    means that SST_W_C_SMENT_START / SST_W_C_SMENT_END, or
    SST_W_C_POS_PUSH / SST_W_C_POS_POP, or
    SST_W_C_ARMODE_PUSH / SST_W_C_ARMODE_POP are not being called correctly
    as matched pairs.
msg statement_type_bad
  lan english -def
    INTERNAL ERROR:  Statement type ID encountered that is not
    legal for the context.  Statement type ID number is %p1i.
msg statement_type_curr_bad
  lan english -def
    INTERNAL ERROR:  Illegal current statement type encountered.
    Current statement type ID is %p1i.
msg var_mod_too_many
  lan english -def
    INTERNAL ERROR (C Back end):  Attempted insert too many modifiers
    after top symbol in variable descriptor chain.  Error occurred in
    routine SST_W_C_VAR.
msg variant_id_dtype_unexpected
  lan english -def
    INTERNAL ERROR (C back end):  Unexpected data type encountered for
    variant IDs in variant record.
