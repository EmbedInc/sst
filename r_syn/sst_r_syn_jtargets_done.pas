{   Subroutine SST_R_SYN_JTARGETS_DONE (TARG)
*
*   Close use of the jump targets TARG.  This will cause any implicitly created
*   symbols for the "fall thru" case to be tagged with the current position.
}
module sst_r_syn_jtargets_done;
define sst_r_syn_jtargets_done;
%include 'sst_r_syn.ins.pas';

procedure sst_r_syn_jtargets_done (    {write implicit labels created by jump targs}
  in      targ: jump_targets_t);       {jump targets descriptor now done with}

var
  t: jtarg_k_t;                        {ID for current jump target}
{
********************************************
*
*   Local subroutine DO_TARGET (JT)
*
*   Process individual jump target JT.
}
procedure do_target (
  in      jt: jump_target_t);          {descriptor for jump target to process}

begin
  if jflag_indir_k in jt.flags then return; {this is not a base jump target ?}
  if not (jflag_fall_k in jt.flags) then return; {not fall thru case ?}
  if jt.lab_p = nil then return;       {no label used for this jump target ?}

  sst_opcode_new;                      {create label target opcode}
  sst_opc_p^.opcode := sst_opc_label_k;
  sst_opc_p^.label_sym_p := jt.lab_p;
  jt.lab_p^.label_opc_p := sst_opc_p;  {link label symbol to target opcode}
  end;
{
********************************************
*
*   Start of main routine.
}
begin
  for t := firstof(t) to lastof(t) do begin {once for each individual jump target}
    do_target (targ.ar[t]);            {process this individual jump target}
    end;                               {back to do next jump target}
  end;
