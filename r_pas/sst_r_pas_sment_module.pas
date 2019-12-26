{   Subroutine SST_R_PAS_SMENT_MODULE (STR_MOD_H)
*
*   Process MODULE_STATEMENT syntax.  We assume we stay in this module until
*   a new MODULE statement.  STR_MOD_H is the string handle to the MODULE_STATEMENT
*   syntax.
}
module sst_r_pas_SMENT_MODULE;
define sst_r_pas_sment_module;
%include 'sst_r_pas.ins.pas';

procedure sst_r_pas_sment_module (     {proces MODULE_STATEMENT syntax}
  in      str_mod_h: syn_string_t);    {string handle to MODULE_STATEMENT syntax}

var
  tag: sys_int_machine_t;              {syntax tag from .syn file}
  str_h: syn_string_t;                 {handle to string for a tag}
  sym_p: sst_symbol_p_t;               {pointer to module name symbol descriptor}
  stat: sys_err_t;

begin
  case nest_level of                   {how deep are we nested in blocks}
0: ;                                   {not in any block, no problem}
1: begin                               {already in top MODULE or PROGRAM block}
      sst_opcode_pos_pop;              {this finishes block we are currently in}
      sst_scope_old;                   {pop back to previous scope}
      nest_level := nest_level - 1;    {should now be above any top block}
      end;
otherwise
    syn_error (str_mod_h, 'sst_pas_read', 'module_not_allowed_here', nil, 0);
    end;
  nest_level := nest_level + 1;        {down into this MODULE block}
  top_block := top_block_module_k;     {indicate top block is a MODULE}

  syn_level_down;                      {down into MODULE_STATEMENT syntax level}
  syn_get_tag_msg (                    {get tag for module name}
    tag, str_h, 'sst_pas_read', 'statement_module_bad', nil, 0);
  if tag <> 1 then begin               {unexpected TAG value}
    syn_error_tag_unexp (tag, str_h);
    end;

  sst_symbol_new (                     {add module name to symbol table}
    str_h, syn_charcase_down_k, sym_p, stat);
  sst_scope_new;                       {make new scope level for this module}
  sym_p^.module_scope_p := sst_scope_p; {point symbol to module's scope}
  syn_error_abort (stat, str_h, '', '', nil, 0);
  sym_p^.symtype := sst_symtype_module_k; {this symbol is a module name}
  sym_p^.flags := sym_p^.flags +
    [sst_symflag_def_k, sst_symflag_used_k];
  sst_scope_p^.symbol_p := sym_p;      {point scope to its defining symbol}

  sst_opcode_new;                      {make opcode for this module}
  sst_opc_p^.opcode := sst_opc_module_k; {this opcode is a module}
  sst_opc_p^.str_h := str_h;           {save source file string handle}
  sst_opc_p^.module_sym_p := sym_p;    {point opcode to module symbol}
  sst_opcode_pos_push (sst_opc_p^.module_p); {new opcodes are for this module}

  syn_get_tag_msg (                    {get next tag in MODULE_STATEMENT}
    tag, str_h, 'sst_pas_read', 'statement_module_bad', nil, 0);
  if tag <> syn_tag_end_k then begin   {there should be no tag here}
    syn_error_tag_unexp (tag, str_h);
    end;
  syn_level_up;                        {back up from MODULE_STATEMENT syntax}
  end;
