{   Subroutine SST_R_PAS_EXP_SYM (SYM_P)
*
*   Process EXPRESSION syntax and return the pointer to the symbol that formed
*   the expression.  It is an error if the expression was more complex than just
*   a symbol.  The symbol will automatically be flagged as used.
}
module sst_r_pas_EXP_SYM;
define sst_r_pas_exp_sym;
%include 'sst_r_pas.ins.pas';

procedure sst_r_pas_exp_sym (          {point to symbol that is EXPRESSION}
  out     sym_p: sst_symbol_p_t);      {error if EXPRESSION is not just one symbol}

var
  tag: sys_int_machine_t;              {syntax tag ID}
  str_h: syo_string_t;                 {handle to string associated with TAG}
  stat: sys_err_t;                     {completion status code}

label
  not_symbol;

begin
  syo_level_down;                      {down into EXPRESSION syntax}
  syo_get_tag_msg (tag, str_h, 'sst_pas_read', 'constant_bad', nil, 0);
  syo_level_down;                      {down into EXPRESSION2 syntax}
  syo_get_tag_msg (tag, str_h, 'sst_pas_read', 'constant_bad', nil, 0);
  syo_level_down;                      {down into EXPRESSION3 syntax}
  syo_get_tag_msg (tag, str_h, 'sst_pas_read', 'constant_bad', nil, 0);
  syo_level_down;                      {down into EXPRESSION4 syntax}
  syo_get_tag_msg (tag, str_h, 'sst_pas_read', 'constant_bad', nil, 0);
  syo_level_down;                      {down into ITEM syntax}

  syo_get_tag_msg (tag, str_h, 'sst_pas_read', 'constant_bad', nil, 0); {unadic op tag}
  if tag <> 1 then goto not_symbol;
  syo_get_tag_msg (tag, str_h, 'sst_pas_read', 'constant_bad', nil, 0); {operand type tag}
  if tag <> 6 then goto not_symbol;
  syo_level_down;                      {down into VARIABLE syntax}
  syo_get_tag_msg (tag, str_h, 'sst_pas_read', 'constant_bad', nil, 0); {variable name tag}
  sst_symbol_lookup (str_h, sym_p, stat);
  syo_error_abort (stat, str_h, '', '', nil, 0);

  syo_get_tag_msg (tag, str_h, 'sst_pas_read', 'constant_bad', nil, 0); {get next tag if there}
  if tag <> syo_tag_end_k then goto not_symbol;
  syo_level_up;                        {back up to ITEM syntax}
  syo_get_tag_msg (tag, str_h, 'sst_pas_read', 'constant_bad', nil, 0); {get next tag in ITEM}
  if tag <> 1 then goto not_symbol;
  syo_level_up;                        {back up to EXPRESSION4 syntax}
  syo_get_tag_msg (tag, str_h, 'sst_pas_read', 'constant_bad', nil, 0); {get next tag if there}
  if tag <> syo_tag_end_k then goto not_symbol;
  syo_level_up;                        {back up to EXPRESSION3 syntax}
  syo_get_tag_msg (tag, str_h, 'sst_pas_read', 'constant_bad', nil, 0); {get next tag if there}
  if tag <> syo_tag_end_k then goto not_symbol;
  syo_level_up;                        {back up to EXPRESSION2 syntax}
  syo_get_tag_msg (tag, str_h, 'sst_pas_read', 'constant_bad', nil, 0); {get next tag if there}
  if tag <> syo_tag_end_k then goto not_symbol;
  syo_level_up;                        {back up to EXPRESSION1 syntax}
  syo_get_tag_msg (tag, str_h, 'sst_pas_read', 'constant_bad', nil, 0); {get next tag if there}
  if tag <> syo_tag_end_k then goto not_symbol;
  syo_level_up;                        {back up to EXPRESSION syntax}
  syo_get_tag_msg (tag, str_h, 'sst_pas_read', 'constant_bad', nil, 0); {get next tag if there}
  if tag <> syo_tag_end_k then goto not_symbol;
  syo_level_up;                        {back up to caller's syntax}

  sym_p^.flags := sym_p^.flags + [sst_symflag_used_k];
  return;

not_symbol:                            {jump here if expression not just a symbol}
  syo_error (str_h, 'sst_pas_read', 'exp_not_symbol', nil, 0);
  end;
