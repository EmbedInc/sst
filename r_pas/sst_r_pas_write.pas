{   Subroutine SST_R_PAS_WRITE
*
*   Process a WRITE or WRITELN statement.  The tag indicating a WRITE or
*   WRITELN statement was just read in the RAW_STATEMENT syntax.
}
module sst_r_pas_WRITE;
define sst_r_pas_write;
%include 'sst_r_pas.ins.pas';

procedure sst_r_pas_write;             {process WRITE and WRITELN statements}

var
  tag: sys_int_machine_t;              {syntax tag ID}
  str_h: syo_string_t;                 {handle to string associated with TAG}
  strh_top: syo_string_t;              {string handle for top WRITE/WRITELN tag}
  fw_max: sys_int_machine_t;           {max field width specifiers allowed}
  fw_n: sys_int_machine_t;             {field width specifier number}
  exp_p: sst_exp_p_t;                  {scratch pointer to expression descriptor}
  eol: boolean;                        {TRUE if need EOL after all writes}

label
  loop_arg, loop_width;

begin
  syo_get_tag_msg (                    {get WRITE/WRITELN tag}
    tag, strh_top, 'sst_pas_read', 'write_sment_bad', nil, 0);
  case tag of
1:  eol := false;                      {WRITE statement}
2:  eol := true;                       {WRITELN statement}
otherwise
    syo_error_tag_unexp (tag, strh_top);
    end;
{
******************************
*
*   Loop back here each new WRITELN_ARG syntax.
}
loop_arg:
  syo_get_tag_msg (                    {get tag for next WRITE/WRITELN argument}
    tag, str_h, 'sst_pas_read', 'write_sment_bad', nil, 0);
  case tag of
{
*   This tag in RAW_STATEMENT was for new WRITELN_ARG syntax.
}
1: begin                               {tag was for next argument in list}
  sst_opcode_new;                      {make new opcode for this argument}
  sst_opc_p^.str_h := str_h;           {save handle to source stream characters}
  sst_opc_p^.opcode := sst_opc_write_k; {indicate what kind of opcode this is}
  syo_level_down;                      {down into WRITELN_ARG syntax}
  syo_get_tag_msg (                    {tag for expression of value to write}
    tag, str_h, 'sst_pas_read', 'write_sment_bad', nil, 0);
  sst_r_pas_exp (str_h, false, sst_opc_p^.write_exp_p); {get write value expression}
  sst_opc_p^.write_width_exp_p := nil; {init to use free format to write value}
  sst_opc_p^.write_width2_exp_p := nil;
  if sst_opc_p^.write_exp_p^.val.dtype = sst_dtype_float_k
    then fw_max := 2                   {2 field widths allowed for floating point}
    else fw_max := 1;                  {1 field width allowed for everything else}
  fw_n := 1;                           {init number of next field width specifier}

loop_width:                            {back here for each new field width specifier}
  syo_get_tag_msg (                    {tag for next field width expression}
    tag, str_h, 'sst_pas_read', 'write_sment_bad', nil, 0);
  case tag of
1:  begin                              {found another field width specifier}
      if fw_n > fw_max then begin
        syo_error (str_h, 'sst_pas_read', 'write_fwidths_too_many', nil, 0);
        end;
      sst_r_pas_exp (str_h, false, exp_p); {create field width expression descriptor}
      if exp_p^.val.dtype <> sst_dtype_int_k then begin {field width not integer ?}
        syo_error (str_h, 'sst_pas_read', 'write_fwidth_not_integer', nil, 0);
        end;
      case fw_n of                     {set the right field width exp pointer}
1:      sst_opc_p^.write_width_exp_p := exp_p;
2:      sst_opc_p^.write_width2_exp_p := exp_p;
        end;
      fw_n := fw_n + 1;                {make number of next field width specifier}
      goto loop_width;                 {back for next field width specifier}
      end;                             {end of tag is field width specifier case}
syo_tag_end_k: ;                       {exit loop on no more field width specifiers}
otherwise
    syo_error_tag_unexp (tag, str_h);
    end;

  syo_level_up;                        {back up from WRITELN_ARG syntax}
  goto loop_arg;                       {back for next argument in WRITE/WRITELN}
  end;                                 {done with tag for this WRITELN_ARG syntax}
{
*   Tag indicates end of RAW_STATEMENT syntax.
}
syo_tag_end_k: ;                       {exit args loop}
{
*   Unexpected tag in RAW_STATEMENT.
}
otherwise
    syo_error_tag_unexp (tag, str_h);
    end;
{
******************************
*
*   We have hit the end of the RAW_STATEMENT syntax.  This definately ends
*   the WRITE/WRITELN statement.  We still need to indicate writing the EOL
*   if the original statement was a WRITELN (instead of WRITE).
}
  if eol then begin                    {need to write end of line ?}
    sst_opcode_new;                    {create the "write end of line" opcode}
    sst_opc_p^.str_h := strh_top;      {save handle to source stream characters}
    sst_opc_p^.opcode := sst_opc_write_eol_k; {set the type of opcode this is}
    end;
  end;
