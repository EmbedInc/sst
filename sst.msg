/*   Messages associated with status codes defined in SST.INS.PAS.
/*
msg err1
  lan english -def
    Symbol "%p1s" is undefined.
msg err2
  lan english -def
    Symbol "%p1s" was previously defined on line %p2i of file "%p3s".
msg err3                               /* error already handled, print nothing
  lan english -def
msg err4
  lan english -def
    End of input data encountered.
msg err5
  lan english -def
    The function return value data types don't match, or routines
    are either not both function or not both non-function subroutines.
msg err6
  lan english -def
    The number of subroutine arguments do not match.
msg err7
  lan english -def
    The NO RETURN status does not match between routine definitions.
msg err8
  lan english -def
    The GLOBAL attribute does not match between routine definitions.
msg err9
  lan english -def
    The EXTERNAL attribute does not match between routine definitions.
msg err10
  lan english -def
    Data types for argument %p1i do not match.
msg err11
  lan english -def
    Argument passing directions for argument %p1i do not match.
msg err12
  lan english -def
    Passing conventions for argument %p1i do not match.
msg err13
  lan english -def
    Attempt to write to variable/expression although it is not writeable.
msg err14
  lan english -def
    Attempt to read from variable/expression although it is not readable.
msg err15
  lan english -def
    Expression does not have an ordinal value.
msg err16
  lan english -def
    Symbol "%p1s" is already in output symbol table.
/*
/*   Messages not related to status codes.
/*
msg argt_exp_missing
  lan english -def
    Expression missing in routine template for argument that was not
    passed.
msg argt_exp_not_var
  lan english -def
    Illegal expression type in call argument template descriptor for
    an argument that was not passed.  Expression is either compound,
    or term is not of type VAR.
msg arg_addr_bad
  lan english -def
    Bad argument to ADDR function.  Only references to routines or
    variables are allowed here.  Compound expressions are not allowed.
msg arg_ifunc_offset_bad
  lan english -def
    Bad argument to OFFSET intrinsic function.  The argument must be a
    field in a record variable or record data type.
msg arg_ifunc_offset_unaligned
  lan english -def
    A field specified in the argument to this OFFSET intrinsic function
    is not aligned at an address boundary.
msg args_too_many
  lan english -def
    Too many call arguments discovered in subroutine call descriptor.
msg args_too_many_add
  lan english -def
    Error on attempt to add another call argument to a subroutine call,
    but subroutine can't take any more arguments.
msg common_block_useage_bad
  lan english -def
    This expression is a common block, which is not allowed in this
    useage.
msg dtype_element_bad
  lan english -def
    The data type of this term is not suitable for an element in a set.
    Set elements may only be integer, enumerated values, boolean,
    character, or subranges of any of these.
msg dtype_element_mismatch
  lan english -def
    The data type of this set element is not compatible with the data
    type of previous elements in this set.
msg dtype_exp_mismatch
  lan english -def
    Data type mismatch.  The data type of this expression is not
    compatible with its useage here.
msg dtype_float_unavailable
  lan english -def
    Data type "%p1s" is %p2i bit floating point.  This size is not
    available on the target machine.
msg dtype_int_unavailable
  lan english -def
    Data type "%p1s" is a %p2i bit integer.  This size is not
    available on the target machine.
msg dtype_not_ordinal
  lan english -def
    INTERNAL ERROR:  The elements of this data type do not have
    ordinal values.
msg dtype_not_ordinal2
  lan english -def
    Not an ordinal data type.  Only an ordinal data type is allowed here.
msg dtype_not_record
  lan english -def
    INTERNAL ERROR:  Data type is not a record.  Data type ID is %p1i.
msg dtype_subscript_mismatch
  lan english -def
    The data type of this subscript value expression is not compatible with
    the array declaration.
msg dtype_target_not_a_set
  lan english -def
    INTERNAL ERROR:  Attempt to convert a SET value to a non-SET
    data type.
msg dtype_term_mismatch
  lan english -def
    The data type of this term is incompatible with the data type of
    the previous expression.
msg dtype_unexpected
  lan english -def
    INTERNAL ERROR:  Invalid, unexpected, or unimplemented
    data type encountered.  Date type ID number is %p1i.
msg dtype_unexpected_exp
  lan english -def
    INTERNAL ERROR:  Unexpected data type encountered for
    previous partial expression on attempt to add new term to
    expression.  Internal data type ID number is %p1i.
    The pointer will be showing the start of the new term.
msg dtype_useage_bad
  lan english -def
    This expression is a data type, which is not allowed in this
    useage.
msg dtype_not_array
  lan english -def
    Data type is not an array.  An array data type is required here.
msg exp_not_const_val
  lan english -def
    A constant is required here, but a value could not be determined for
    this expression at compile time.
msg exp_not_readable
  lan english -def
    Expression is not readable, but is used in a context where it must
    have a computable value.
msg fnam_input_no_suffix
  lan english -def
    Input file name "%p1s" has no file type suffix.
    A suffix is required to determine the input language.
msg func_intrinsic_arg_dtype_bad
  lan english -def
    The data type of this intrinsic function argument is not allowed
    by the intrinsic function.
msg func_intrinsic_args_n_bad
  lan english -def
    Wrong number of arguments to this intrinsic function reference.
    The number of arguments expected was %p1i.
msg func_intrinsic_args_too_few
  lan english -def
    Too few arguments to intrinsic function.
    %p1i expected, %p2i found.
msg func_intrinsic_args_too_many
  lan english -def
    Too many arguments to intrinsic function.  This argument is the
    first to exceed the %p1i allowed.
msg func_intrinsic_unknown
  lan english -def
    INTERNAL ERROR:  Illegal or unimplemented
    base translator intrinsic function.  Intrinsic function ID is %p1i.
msg language_input_unrecognized
  lan english -def
    The input file name suffix of %p1s does not match the suffix for
    any supported input language.
msg ifunc_dec_min
  lan english -def
    The next lower value of this ordinal expression does not exist,
    since it is already the minimum value for its data type.
msg ifunc_inc_max
  lan english -def
    The next higher value of this ordinal expression does not exist,
    since it is already the maximum value for its data type.
msg opcode_unexpected
  lan english -def
    INTERNAL ERROR:  Invalid, unexpected, or unimplemented
    opcode encountered.  Opcode ID number is %p1i.
msg operator_mismatch
  lan english -def
    The operator preceeding this term is incompatible with the data types
    of the expression preceeding it and the term following it.
msg operator_mismatch_unadic
  lan english -def
    The unary operator preceeding this term is incompatible with the
    data type of the term.
msg operator_unknown_diadic
  lan english -def
    INTERNAL ERROR: Invalid, unexpected, or unimplemented
    diadic operator encountered.  Operator type ID number is %p1i.
msg operator_unknown_unadic
  lan english -def
    INTERNAL ERROR: Invalid, unexpected, or unimplemented
    unadic operator encountered.  Operator type ID number is %p1i.
msg out_line_too_long
  lan english -def
    INTERNAL ERROR:  Length of output line exceeds length of string to
    hold it.
msg out_name_illegal
  lan english -def
    BACK END ERROR:  The symbol "%p1s" may not be renamed, but is not
    a legal symbol name in the output language.
msg out_name_none
  lan english -def
    INTERNAL BACK END ERROR: Attempt to write symbol name although
    symbol output name was not previously set.  Symbol's input name
    is "%p1s".
msg out_name_too_long
  lan english -def
    Output source code symbol name is too long, and it can not be shortened
    any more.  Unmodified name is "%p1s" with suffix "%p2s".  Current
    output name attempt is "%p3s".
msg out_name_used
  lan english -def
    Name "%p1s" already exists in output symbol table in current scope,
    and renaming was disabled.  Declaration of original symbol with this
    name is shown:
msg pointer_not_readable
  lan english -def
    Pointer may not be dereferenced since the pointer itself is not
    readable.
msg readin
  lan english -def
    Error occurred on reading input source code.
msg rename_unknown
  lan english -def
    INTERNAL ERROR:  Invalid, unexpected, or unimplemented RENAME
    flag value encountered.  RENAME flag value is %p1i.
msg set_ele_out_of_range
  lan english -def
    Out of range set element number of %p1i encountered.  Legal element
    numbers for this set are from 0 to %p2i.
msg set_invert_dtype_ambiguous
  lan english -def
    Set inversion attempted on set with ambiguous data type.  Set
    inversion can only be performed on sets with absolutely certain
    data types.
msg symbol_not_subr_in_call
  lan english -def
    Symbol "%p1s" is not a subroutine entry point, although it was
    passed to SST_CALL.
msg symbol_type_unknown
  lan english -def
    INTERNAL ERROR: Invalid, unexpected, or unimplemented symbol
    type encountered.  Symbol type ID number is %p1i.
msg symbol_arg_unused
  lan english -def
    Argument "%p1s" to routine "%p2s" is not used inside the routine.
msg symbol_unused
  lan english -def
    .nfill
    *** Symbol "%p1s" unused.  (Line %p2i of %p3s)
msg term_not_readable
  lan english -def
    Term was used in a compound expression, but is not readable.
msg term_type_unknown
  lan english -def
    INTERNAL ERROR:  Unrecognized term type.  Term type
    ID number is %p1i.
msg type_cast_mismatch_size
  lan english -def
    Data type sizes don't match for input and output of type
    casting function.
msg value_not_a_set
  lan english -def
    The data represented by the constant value descriptor is not of
    type SET.
msg var_init_not_static
  lan english -def
    Variable not in static storage although an initial value was supplied.
    Only static variables are allowed to have initial values.
msg var_modifier_unknown
  lan english -def
    INTERNAL ERROR: Invalid, unexpected, or unimplemented
    variable descriptor modifier type encountered.
    Modifier type ID number is %p1i.
msg var_not_writeable
  lan english -def
    This variable is read-only, but was used in a context where its
    value may change.
msg vtype_unexpected
  lan english -def
    INTERNAL ERROR: Invalid, unexpected, or unimplemented variable
    descriptor type encountered.  Variable type ID number is %p1i.
msg writeout
  lan english -def
    Error occurred on writing output source code.
/*
/*   Messages used by top level SST module.
/*
msg language_unsupported
  lan english -def
    The target language chosen in the config file is a legal choice,
    but is currently not supported.  Language ID number is %p1i.
msg timing_info
  lan english -def
    .nfill
    SST Elapsed Time:

               Seconds  Percent
      Front end %p1F6.1 %p2F8.1
      Base      %p3F6.1 %p4F8.1
      Back end  %p5F6.1 %p6F8.1
      Total     %p7F6.1 %p8F8.1
/*
/*   Message related to reading config file.
/*
msg config_cmd_bad
  lan english -def
    Unrecognized command "%p1s" encounterd in config file.
msg config_cmd_missing
  lan english -def
    A mandatory value was not set in the config file.
    The config file command for setting this value is %p1s.
msg config_enum_size_bad
  lan english -def
    The enumerated data type size of %p1i specified with a SIZE_ENUMERATED_FORCE
    command in the config file does not match any available integer size
    specified with with a SIZE_INT command.
msg config_float_le_previous
  lan english -def
    The SIZE_FLOAT value specified here was not greater than the previous
    SIZE_FLOAT value.
msg config_fnam_lnum
  lan english -def
    Error occurred on line %p1i of configuration file "%p2s".
msg config_int_le_previous
  lan english -def
    The SIZE_INT value specified here was not greater than the previous
    SIZE_INT value.
msg config_nosuch_int_size
  lan english -def
    There is no previously declared integer size that matches the
    size specified in this command.
msg config_open
  lan english -def
    Error on attempt to open target machine configuration file "%p1s".
msg config_parm_err
  lan english -def
    Error with parameter to command "%p1s" in configuration file.
msg config_parm_range
  lan english -def
    Parameter to command "%p1s" in config file is unrecognized or
    out of range.
msg config_set_le_previous
  lan english -def
    The SIZE_SET value specified here was not greater than the previous
    SIZE_SET value.
msg config_size_bad
  lan english -def
    The size value specified here has not been previously declared.
msg config_token_extraneous
  lan english -def
    Too many tokens found after "%p1s" command in configuration file.
    First extraneous token was "%p2s".
msg config_too_many_float
  lan english -def
    Too many floating point sizes specified in configruration file.
msg config_too_many_int
  lan english -def
    Too many integer sizes specified in configruration file.
msg config_too_many_set
  lan english -def
    Too many SET sizes specified in configruration file.
