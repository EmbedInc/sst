{   Subroutine SST_R_PAS_SMENT_CONST
*
*   Process the CONST_STATEMENT syntax.
}
module sst_r_pas_SMENT_CONST;
define sst_r_pas_sment_const;
%include 'sst_r_pas.ins.pas';

procedure sst_r_pas_sment_const;       {process CONST_STATEMENT syntax}

var
  tag: sys_int_machine_t;              {tag number from .syn file}
  str_h: syn_string_t;                 {handle to string for current tag}
  sym_p: sst_symbol_p_t;               {scratch pointer to symbol table entry}
  stat: sys_err_t;

begin
  syn_level_down;                      {down into CONST_STATEMENT syntax}
  syn_get_tag_msg (tag, str_h, 'sst_pas_read', 'const_bad', nil, 0);

  while tag <> syn_tag_end_k do begin  {once for each constant declared}
    if tag <> 1 then begin             {complain if unexpected TAG value}
      syn_error_tag_unexp (tag, str_h);
      end;
    sst_symbol_new                     {create new symbol for constant name}
      (str_h, syn_charcase_down_k, sym_p, stat);
    syn_error_abort (stat, str_h, '', '', nil, 0);

    syn_get_tag_msg (tag, str_h, 'sst_pas_read', 'const_bad', nil, 0); {expression tag}
    if tag <> 1 then begin             {complain if unexpected TAG value}
      syn_error_tag_unexp (tag, str_h);
      end;

    sst_r_pas_exp (str_h, true, sym_p^.const_exp_p); {build expression descriptor}
    sym_p^.flags := sym_p^.flags +     {symbol is defined and has a value}
      [sst_symflag_def_k];
    sym_p^.symtype := sst_symtype_const_k; {this symbol is a constant}

    syn_get_tag_msg (tag, str_h, 'sst_pas_read', 'const_bad', nil, 0); {next const name}
    end;                               {back and process new constant}

  syn_level_up;                        {up from CONST_STATMENT syntax}
  end;
