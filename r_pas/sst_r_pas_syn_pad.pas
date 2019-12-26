{   Subroutine SST_R_PAS_SYN_PAD (MFLAG)
*
*   This subroutine implements the PAD syntax referenced in PAS.SYN.  It is
*   intended to skip over "irrelevant" characters, such as space, end of line,
*   etc.  Comments have already been removed from the input stream by the
*   pre-processor.
}
module sst_r_pas_SYN_PAD;
define sst_r_pas_syn_pad;
%include 'sst_r_pas.ins.pas';

procedure sst_r_pas_syn_pad (          {implements PAD syntax}
  out     mflag: syn_mflag_k_t);       {syntax matched yes/no, use SYN_MFLAG_xxx_K}

var
  i: sys_int_machine_t;                {scratch character value}

label
  loop;

begin
  mflag := syn_mflag_yes_k;            {this syntax always matches}

loop:                                  {back here to try each new character}
  syn_p_cpos_push;                     {save state before getting character}
  syn_p_get_ichar (i);                 {get next input stream character}
  if                                   {is this a pad character ?}
      (i = ord(' ')) or                {space ?}
      (i = syn_ichar_eol_k) or         {end of line ?}
      (i = syn_ichar_eof_k)            {end of file ?}
      then begin
    syn_p_cpos_pop (syn_mflag_yes_k);  {remove pushed state from stack}
    goto loop;                         {back to test next character}
    end;

  syn_p_cpos_pop (syn_mflag_no_k);     {restore state to before non-pad character}
  end;
