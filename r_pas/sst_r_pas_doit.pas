{   Subroutine SST_R_PAS_DOIT (FNAM, GNAM, STAT)
*
*   Read the PASCAL input file FNAM into the in-memory data structures.  GNAM is
*   returned as the generic leaf name of the input file.
}
module sst_r_pas_doit;
define sst_r_pas_doit;
%include 'sst_r_pas.ins.pas';

procedure sst_r_pas_doit (             {read input source code into in-memory data}
  in      fnam: univ string_var_arg_t; {raw input file name}
  in out  gnam: univ string_var_arg_t; {returned as generic name of input file}
  out     stat: sys_err_t);            {completion status code}

type
  suffix_k_t = (                       {identifies which file name suffix was found}
    suffix_none_k,                     {no suffix found that we recognize}
    suffix_pas_k,                      {file name suffix was .pas}
    suffix_cog_k);                     {file name suffix_was .cog}

var
  mflag: syn_mflag_k_t;                {syntaxed matched yes/no flag}
  suffix_id: suffix_k_t;               {identifies which file name suffix found}

label
  loop, leave;

begin
  suffix_id := suffix_none_k;          {init to no file name suffix found}
  string_generic_fnam (fnam, '.cog .pas', gnam); {make generic input file name}
  if                                   {input file name could have a suffix ?}
      (fnam.len > 4) and
      (fnam.str[fnam.len-3] = '.')
      then begin
    if
        (fnam.str[fnam.len-2] = 'p') and
        (fnam.str[fnam.len-1] = 'a') and
        (fnam.str[fnam.len] = 's')
      then begin                       {file name suffix is ".pas"}
        suffix_id := suffix_pas_k;
        end
      else begin                       {file name suffix is not .pas}
        if
            (fnam.str[fnam.len-2] = 'c') and
            (fnam.str[fnam.len-1] = 'o') and
            (fnam.str[fnam.len] = 'g')
            then begin                 {file name suffix is ".cog"}
          suffix_id := suffix_cog_k;
          end;
        end
      ;
    end;                               {done handling input file name suffix}

  syn_init;                            {init syntaxer}
  sst_r_pas_preproc_init;              {init our pre-processor}
  syn_preproc_set (addr(sst_r_pas_preproc)); {install our pre-processor}
  if suffix_id = suffix_none_k         {set name of top level input file}
    then begin                         {FNAM doesn't have explicit suffix}
      syn_infile_top_set (fnam, '.cog .pas');
      end
    else begin                         {FNAM already contains explicit suffix}
      syn_infile_top_set (fnam, '');
      end
    ;
{
*   Keep looping until either an error or end of input.
}
loop:                                  {back here each new top level syntax}
  syn_tree_clear;                      {set up for parsing}
  toplev (mflag);                      {try to parse one top level syntax}
  if mflag = syn_mflag_yes_k
    then begin                         {syntax matched}
      error_syn_found := false;        {indicate no syntax error}
      end
    else begin                         {syntax did not match}
      syn_p_test_eod (mflag);          {check for end of input data}
      if mflag = syn_mflag_yes_k then begin {no error, encountered end of data}
        sys_error_none (stat);         {indicate no error condition}
        goto leave;
        end;
      syn_tree_err;                    {set up for error re-parse}
      toplev (mflag);                  {do error re-parse}
      error_syn_found := true;         {indicate we will hit error syntax tree end}
      end
    ;
  syn_tree_setup;                      {set up syntax tree for getting tags}
  sst_r_pas_statement (stat);          {process top level contruction}
  if (not error_syn_found) and (not sys_error(stat)) {everything went well ?}
    then goto loop;                    {back and do next top level construction}
{
*   Something unusual happened.
}
  if not sys_error(stat) then begin    {syntax error not caught as contents error ?}
    syn_error_print ('', '', nil, 0);  {print message about syntax error}
    end;

  if sys_stat_match (sst_subsys_k, sst_stat_eod_k, stat)
    then begin                         {no error, just hit end of input data}
      sys_error_none (stat);           {indicate normal condition}
      end
    else begin                         {error condition}
      sys_stat_set (sst_subsys_k, sst_stat_err_handled_k, stat); {indicate err handled}
      end
    ;                                  {STAT is all set for return}

leave:                                 {common exit point, STAT already set}
  end;
