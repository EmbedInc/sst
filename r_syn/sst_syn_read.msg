/*   Error and other messages used by SYN front end to SST.
/*
msg module_symbol_create
  lan english -def
    Error occurred on attempt to create name symbol for top module
msg occurs_limit_high_below
  lan english -def
    The upper limit in OCCURS operator is set less than the lower
    limit.  The upper limit must be greater than or equal to the
    lower limit.
msg occurs_limit_low_bad
  lan english -def
    Bad lower limit in OCCURS operator.
msg seq_subr_err
  lan english -def
    Error on attempt to make seqence number string for default subroutine
    name for symbol "%p1s".  Error at:
msg syerr_command
  lan english -def
    Unrecognized command.
msg syerr_declare
  lan english -def
    Syntax error in .SYMBOL command.
msg syerr_expression
  lan english -def
    Syntax error in syntax expression.
msg syerr_define
  lan english -def
    Syntax error in .DEFINE command.
msg syerr_string
  lan english -def
    Syntax error in string constant.
msg symbol_already_defined
  lan english -def
    Symbol %p1s was previously defined.
msg symbol_already_used
  lan english -def
    Symbol %p1s was previously declared.
msg symbol_external
  lan english -def
    Attempt to define symbol %p1s, although it was previously declared
    to be external.
msg symbol_label_create
  lan english -def
    Error occurred on attempt to create new label symbol "%p1s".
msg symbol_not_declared
  lan english -def
    Symbol %p1s was not declared before attempt to reference it.
msg symbol_predef_not_found
  lan english -def
    A required pre-defined symbol was not found in SYN front end to SST.
msg symbols_undefined_abort
  lan english -def
    Program aborted due to undefined symbols.
msg symbols_undefined_show
  lan english -def
    .nfill
    Undefined symbols in syntax definition file:
msg symbols_unused_show
  lan english -def
    .nfill
    Unused symbols in syntax definition file:
