/*   Error messages used when reading PASCAL source code.
/*
msg args_syntax_err
  lan english -def
    Syntax error in procedure/function call arguments.
msg comment_eof
  lan english -def
    The end of file %p1s was encountered within a comment.
msg comment_start_start
  lan english -def
    The start of a new comment was found within a previous comment of
    the same type.  Error occurred on line %p1i of file %p2s:
msg common_and_extern
  lan english -def
    The variables here may not be declared EXTERN, since they also reside
    in a common block.
msg cond_exp_op_bad
  lan english -def
    Unrecognized condition compilation expression operator "%p1s".
msg cond_exp_paren_mismatch
  lan english -def
    Parenthesis mismatch in conditional compiler directive expression.
msg cond_then_not_found
  lan english -def
    %%THEN not found after expression in conditional compiler directive.
msg cond_var_not_found
  lan english -def
    Conditional compiler variable "%p1s" does not exist.
msg cond_var_exists
  lan english -def
    The conditional compiler directive variable "%p1s" already exists.
    Error occurred on line %p2i of file "%p3s".
msg const_bad
  lan english -def
    Syntax error in CONST statment.
msg constant_bad
  lan english -def
    Syntax error in constant expression.
msg case_duplicate_choice_vals
  lan english -def
    Duplicate CASE choice values.  A CASE statement choice with an
    ordinal value of %p1i was previously used on line %p2i of file "%p3s".
msg define_statement_bad
  lan english -def
    Syntax error in DEFINE statement.
msg directive_include_open
  lan english -def
    Error occurred on attempt to open file "%p1s" referenced by %%INCLUDE
    directive on line %p2i of file "%p3s".
msg directive_include_open_local
  lan english -def
    Error occurred on attempt to open file "%p1s" referenced by %%INCLUDE
    directive on line %p2i of file "%p3s".  Due to -LOCAL_INS command
    line option, attempted include file name was "%p4s".
msg directive_parm_bad
  lan english -def
    Bad parameter "%p1s" for compiler directive "%p2s"
    on line %p3i of file "%p4s".
msg directive_semicolon_missing
  lan english -def
    Missing ";" after compiler directive on line %p1i of file "%p2s".
msg directive_unrecognized
  lan english -def
    Unrecognized compiler directive "%%%p1s".
    Error occurred on line %p2i of file "%p3s".
msg discard_arg_bad
  lan english -def
    Illegal argument to DISCARD intrinsic procedure.  The argument to
    DISCARD may only be a function reference.
msg dtype_case_nested
  lan english -def
    Nested variant record encountered.  Only one CASE clause is allowed
    per record data type.
msg dtype_enum_ele_bad
  lan english -def
    Illegal data type for explicitly typed enumerated names.  Only
    integer, boolean, character, or subrange data types are allowed here.
msg dtype_enum_bad
  lan english -def
    Syntax error in enumerated type list.
msg dtype_bad
  lan english -def
    Syntax error in data type definition.
msg dtype_not_undefined
  lan english -def
    INTERNAL ERROR: Attempt to re-define a data type that was not
    of type UNDEFINED.
msg dtype_output_no_name
  lan english -def
    INTERNAL ERROR:  A suitable output data type exists for %p1s,
    but no symbol has been created for it.
msg dtype_packed_aligned
  lan english -def
    This data type for field in packed record is not, or
    can not be bit-aligned.
msg dtype_packed_bad
  lan english -def
    Illegal data type encountered for a field in a packed record.
    This data type can not be packed.
msg dtype_pnt_not_allowed
  lan english -def
    Declaring pointer to this data type directly is not allowed.
    Declare data type first, then declare separate data type as a
    pointer to it.
msg dtype_proc_not_pointer
  lan english -def
    Data types can not be declared to be procedures or functions.
    Data types, however, may be POINTERS to procedures or functions.
msg dtype_range_bad
  lan english -def
    Syntax error in subrange data type definition.
msg dtype_range_illegal
  lan english -def
    Illegal base data type for forming a subrange data type.
msg dtype_range_int_nosize
  lan english -def
    No target language integer is large enough to hold the subrange
    indicated here.
msg dtype_range_inverted
  lan english -def
    Ending value of subrange is less than starting value.
msg dtype_range_not_match
  lan english -def
    End of subrange data type does not match start of subrange data type.
msg dtype_reuse_twice
  lan english -def
    INTERNAL ERROR: Attempt to re-use same data type descriptor twice.
msg dtype_set_bad
  lan english -def
    A set can not be formed of data type "%p1s".  Sets can only be formed from
    enumerated, BOOLEAN, CHAR, and subrange data types.  In addition, the
    total possible values a variable of that data type can take on may not
    exceed 256.
msg dtype_set_enum_negative
  lan english -def
    Negative element value encountered on attempt to form set from an
    enumerated type.  All enumerated name must have an ordinalt value of
    zero or greater.  Enumerated name "%p1s" has a value of %p2i.
msg dtype_set_syerr
  lan english -def
    Syntax error in SET OF data type specification.
msg dtype_undefined
  lan english -def
    Data type "%p1s" is undefined.
    Its existance was first implied on line %p2i of file "%p3s".
msg exp_bad
  lan english -def
    Syntax error in expression.
msg exp_bad_case
  lan english -def
    The data type of this expression is illegal in a CASE statement.
msg exp_dtype_mismatch_case
  lan english -def
    The data type of this CASE statement choice is incompatible with
    the choice select expression.
msg exp_dtype_mismatch
  lan english -def
    Data type of this expression term is incompatible with the data type
    of the previous terms.
msg exp_dtype_not_bool
  lan english -def
    Expression data type is not BOOLEAN.  A boolean expression is
    required here.
msg exp_dtype_var_mismatch
  lan english -def
    The data type of this expression is incompatible with the data type
    of the assignment variable.
msg exp_not_int
  lan english -def
    Expression does not represent an integer value.  An integer value
    is required here.
msg exp_not_symbol
  lan english -def
    Expression is not a symbol.  Only a symbol name is allowed here.
msg exp_operand_mismatch
  lan english -def
    Operand is incompatible with the data type of the terms it is operating
    on.
msg exp_rout_not_func
  lan english -def
    Routine is not a function.
    Only symbols representing variables, constants, or functions
    are permitted here.
msg exp_symbol_not_func
  lan english -def
    Symbol does not represent a function, although an argument list
    was given.
    Only symbols representing variables, constants, or functions
    are permitted here.
msg exp_symbol_type_bad
  lan english -def
    Bad symbol type for usage in an expression.
    Only symbols representing variables, constants, or functions
    are permitted here.
msg field_name_not_found
  lan english -def
    "%p1s" is not a field of record data type "%p2s".
msg func_too_many_args
  lan english -def
    Too many arguments to function "%p1s".
msg func_no_data_type
  lan english -def
    Missing data type declaration for function return value.
msg ifunc_unexpected
  lan english -def
    INTERNAL ERROR (Pascal front end):  Unimplemented, illegal, or
    unexpected Pascal intrinsic function ID.  ID number is %p1i.
msg initial_value_already
  lan english -def
    An initial value already exists for this variable.  The initial
    value was previously declared on line %p1i of file "%p2s".
msg initial_value_no_dtype
  lan english -def
    An intial value was supplied before the variable's data type
    was declared.
msg initial_value_not_var
  lan english -def
    Initial value was supplied for a symbol that is not a variable.
msg item_dtype_nmatch_unadic
  lan english -def
    Data type of this item is incompatible with the preceeding unadic
    operator.
msg module_not_allowed_here
  lan english -def
    A MODULE statement is not allowed here.
msg name_dtype_dup_internal
  lan english -def
    INTERNAL ERROR:  Symbol %p1s was already in the symbol table on
    attempt to declare it as an intrinsic data type.  Error occurred
    during initialization of Pascal front end.
msg name_ifunc_dup_internal
  lan english -def
    INTERNAL ERROR:  Symbol %p1s was already in the symbol table on
    attempt to declare it as an intrinsic function.  Error occurred
    during initialization of Pascal front end.
msg not_a_procedure
  lan english -def
    Not a procedure.  A procedure was expected here.
msg not_a_variable
  lan english -def
    "%p1s" does not refer to a variable.  Only a variable is allowed here.
msg parent_not_record
  lan english -def
    A field name is not allowed here, since the parent data type is not
    a record.
msg pointer_dereference_bad
  lan english -def
    Illegal pointer dereference.  The pointer is either of type
    UNIV_PTR, or it is pointing to an undeclared data type.
msg proc_arg_decl_bad
  lan english -def
    Syntax error in call argument declaration.
msg proc_data_type
  lan english -def
    Function return data type declaration exists, but routine was declared
    a PROCEDURE, not a FUNCTION.
msg proc_global
  lan english -def
    Illegal INTERNAL routine attribute.  Routine was already global.
msg proc_mismatch
  lan english -def
    This declaration of routine %p1s is incompatible with a previous
    declaration of the same routine.
    The first declaration was on line %p2i of file "%p3s".
msg proc_redeclare
  lan english -def
    Redundant EXTERN routine declaration.
    Routine %p1s was already declared FORWARD or EXTERN
    on line %p2i of file "%p3s".
msg prog_not_allowed_here
  lan english -def
    A PROGRAM statement is not allowed here.
msg routine_is_function
  lan english -def
    This routine is a function that returns a value.  Only a regular
    subroutine may be used in this context.  Use DISCARD to call
    a function like a subroutine.
msg set_bitsize_too_small
  lan english -def
    BITSIZE value in set data type is too small.  At least %p1i bits
    are required for this set, although only %p2i were specified.
msg setof_arg1_missing
  lan english -def
    Missing argument to SETOF intrinsic function.  The first argument that
    declares the set data type is mandatory.
msg setof_dtype_ambiguous
  lan english -def
    The data type of the first argument to this SETOF intrinsic function
    is ambiguous.  This argument should be either a data type or a variable.
msg setof_dtype_bad
  lan english -def
    The data type of the first argument to this SETOF intrinsic function
    is not a set.
msg statement_bad
  lan english -def
    Unrecognized statement.
msg statement_case_bad
  lan english -def
    Syntax error in CASE statement.
msg statement_exec_bad
  lan english -def
    Syntax error in executable statement.
msg statement_label_bad
  lan english -def
    Syntax error in LABEL statement.
msg statement_module_bad
  lan english -def
    Syntax error in MODULE statement.
msg statement_proc_bad
  lan english -def
    Syntax error in PROCEDURE/FUNCTION header statement.
msg statement_prog_bad
  lan english -def
    Syntax error in PROGRAM statement.
msg statement_with_bad
  lan english -def
    Syntax error in WITH statement.
msg subscript_dtype_bad
  lan english -def
    Illegal data type for use as an array subscript.
msg subscript_dtype_different
  lan english -def
    Data types of starting and ending subscript range specifiers do not match.
msg subscript_range_bad
  lan english -def
    Syntax error in subscript range specifier.
msg subscript_range_order
  lan english -def
    Subscript range is specified out of order.  The end of the range has
    a lower ordinal value than the start of the range.
msg subscript_unlimited_bad
  lan english -def
    An unlimited subscript maximum value is not allowed for this subscript.
    Unlimited subscript maximum values may only be specified for the
    first subscript in an array.
msg subscripts_too_few
  lan english -def
    Too few subscripts supplied for this array.  The number expected was %p1i.
msg subscripts_too_many
  lan english -def
    Too many subscripts supplied for this array.  The number expected was %p1i.
msg symbol_already_def
  lan english -def
    Symbol "%p1s" was previously defined on line %p2i of file "%p3s".
msg symbol_bad_type
  lan english -def
    Symbol "%p1s" is not of the right type for the useage here.
msg symbol_not_array
  lan english -def
    The preceeding variable or symbol is not an array, although
    subscripts are supplied.
msg symbol_not_dtype
  lan english -def
    Symbol "%p1s" is not a data type.
    Only symbols representing data types are allowed here.
    "%p1s" was defined on line %p2i of file "%p3s".
msg symbol_not_dtype_internal
  lan english -def
    INTERNAL ERROR IN PASCAL FRONT END: Symbol is not a data type.
msg symbol_not_enum
  lan english -def
    Symbol "%p1s" is not a value of an enumerated data type.
    "%p1s" was defined on line %p2i of file "%p3s".
msg symbol_not_func
  lan english -def
    Symbol "%p1s" is not a function, although an argument list was
    supplied.
msg symbol_not_label
  lan english -def
    Symbol "%p1s" is not a label.  It was originally declared on line
    %p2i of file "%p3s".
msg symbol_not_pointer
  lan english -def
    Attempt to dereference a symbol that is not a pointer.
msg symbol_not_value
  lan english -def
    Symbol "%p1s" can not be resolved to a constant value at compile
    time.
msg symbol_syntax_bad
  lan english -def
    Syntax error in symbol reference.
msg type_sment_bad
  lan english -def
    Syntax error in TYPE statement.
msg unadic_not_match
  lan english -def
    Unadic operator is invalid for the data type of the item it preceeds.
msg var_not_simple
  lan english -def
    Only a simple variable name is allowed here.  Records, arrays, and
    pointer dereferences are not allowed.
msg var_init_index_too_few
  lan english -def
    Not enough initial array values found for the declared size of the
    array.  The last initial value at this subscript level is shown.
    %p1i values were expected, although only %p2i were found.
msg var_init_index_too_many
  lan english -def
    Too many initial values for the declared size of the array.  There should
    only be %p1i values for this subscript.
msg var_init_bad
  lan english -def
    Syntax error in variable initial value declaration.
msg var_init_set_unimp
  lan english -def
    Initial values for SET variables is not implemented.
msg var_sment_bad
  lan english -def
    Syntax error in VAR statement.
msg write_fwidth_not_integer
  lan english -def
    Field width specifier in WRITE or WRITELN statement does not
    represent an integer value.  Only integer values are allowed here.
msg write_fwidths_too_many
  lan english -def
    Too many field width specifiers in WRITE or WRITELN statement.
msg write_sment_bad
  lan english -def
    Syntax error in WRITE or WRITELN statment.
