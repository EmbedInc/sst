{   Subroutine SST_R_PAS_SMENT_LABEL
*
*   Process LABEL_STATEMENT syntax.
}
module sst_r_pas_SMENT_LABEL;
define sst_r_pas_sment_label;
%include 'sst_r_pas.ins.pas';

procedure sst_r_pas_sment_label;       {process LABEL_STATEMENT syntax}

var
  tag: sys_int_machine_t;              {syntax tag from .syn file}
  str_h: syn_string_t;                 {handle to string for a tag}
  sym_p: sst_symbol_p_t;               {scratch pointer to symbol table entry}
  stat: sys_err_t;

label
  loop;

begin
  syn_level_down;                      {down into LABEL_STATEMENT syntax}

loop:                                  {back here each new label in list}
  syn_get_tag_msg (tag, str_h, 'sst_pas_read', 'statement_label_bad', nil, 0);
  case tag of

1: begin                               {tag is for name of new label}
      sst_symbol_new (                 {add new symbol to current scope}
        str_h, syn_charcase_down_k, sym_p, stat);
      sym_p^.symtype := sst_symtype_label_k; {this symbol is a label}
      sym_p^.label_opc_p := nil;       {label has not appeared yet in the code}
      end;

syn_tag_end_k: begin                   {normal end of label statement}
      syn_level_up;                    {back up to parent syntax level}
      return;
      end;

otherwise                              {illegal TAG value}
    syn_error_tag_unexp (tag, str_h);
    end;                               {end of TAG cases}
  goto loop;                           {back for next tag in this syntax}
  end;
