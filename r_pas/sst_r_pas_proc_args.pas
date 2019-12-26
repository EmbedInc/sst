{   Subroutine SST_R_PAS_PROC_ARGS (PROC)
*
*   Process the PARAMETER_DECLARATION syntax.  PROC is the top level descriptor
*   for a routine (PROCEDURE or FUNCTION).  The routine arguments chain will be
*   created and PROC.FIRST_ARG_P will point to the start of the arguments chain.
*   PROC.FIRST_ARG_P will be set to NIL if there are no arguments.  PROC.N_ARGS
*   will be set to the number of arguments.
}
module sst_r_pas_PROC_ARGS;
define sst_r_pas_proc_args;
%include 'sst_r_pas.ins.pas';

procedure sst_r_pas_proc_args (        {process PARAMETER_DECLARATION syntax}
  in out  proc: sst_proc_t);           {top level procedure descriptor}

var
  next_pp: ^sst_proc_arg_p_t;          {pointer to current end of args chain pointer}
  tag: sys_int_machine_t;              {syntax tag from .syn file}
  str_h: syn_string_t;                 {handle to string for a tag}
  arg_p: sst_proc_arg_p_t;             {pointer to current arg descriptor}
  first_arg_p: sst_proc_arg_p_t;       {points to first arg of same data type}
  arg: sst_proc_arg_t;                 {template for current argument descriptor}
  name: string_var80_t;                {current call argument name}
  sz: sys_int_adr_t;                   {for calculating memory size}

label
  group_loop, tag_loop;

begin
  name.max := sizeof(name.str);        {init var string}

  proc.n_args := 0;                    {init number of arguments found}
  proc.first_arg_p := nil;             {init arguments chain to empty}
  next_pp := addr(proc.first_arg_p);   {set pointer to current end of chain pointer}
  arg.next_p := nil;                   {init static fields in arg template}
  arg.sym_p := nil;
  arg.name_p := nil;
  arg.exp_p := nil;
  arg.dtype_p := nil;
  syn_level_down;                      {down into PARAMETER_DECLARATION syntax}

group_loop:                            {back here for each new args group tag}
  syn_get_tag_msg (tag, str_h, 'sst_pas_read', 'proc_arg_decl_bad', nil, 0);
  if tag = syn_tag_end_k then begin    {all done, no more argument declarations ?}
    syn_level_up;                      {up from PARAMETER_DECLARATION syntax}
    return;
    end;
  if tag <> 1 then begin
    syn_error_tag_unexp (tag, str_h);
    end;
  arg.pass := sst_pass_none_k;         {default to passing method not yet known}
  arg.rwflag_int := [];                {default to pass direction not specified}
  arg.rwflag_ext := [];
  arg.univ := false;                   {init to type checking not disabled}
  first_arg_p := nil;                  {init to no new arg created yet}
  syn_level_down;                      {down into PARAM_DECL_GROUP syntax}

tag_loop:                              {back here each new tag in PARAM_DECL_GROUP}
  syn_get_tag_msg (tag, str_h, 'sst_pas_read', 'proc_arg_decl_bad', nil, 0);
  case tag of                          {what pass direction is specified ?}
1:  begin                              {IN}
      arg.pass := sst_pass_ref_k;
      arg.rwflag_int := arg.rwflag_int + [sst_rwflag_read_k];
      arg.rwflag_ext := arg.rwflag_int; {rout sees same permission as caller}
      end;
2:  begin                              {OUT}
      arg.pass := sst_pass_ref_k;
      arg.rwflag_int := arg.rwflag_int + [sst_rwflag_write_k];
      arg.rwflag_ext := arg.rwflag_int; {rout sees same permission as caller}
      end;
3:  begin                              {VAR}
      arg.pass := sst_pass_ref_k;
      arg.rwflag_int := arg.rwflag_int + [sst_rwflag_read_k, sst_rwflag_write_k];
      arg.rwflag_ext := arg.rwflag_int; {rout sees same permission as caller}
      end;
4:  begin                              {tag is argument name}
      if arg.pass = sst_pass_none_k then begin {no passing mode keyword given ?}
        arg.pass := sst_pass_refcpy_k; {passed by reference, but rout makes copy}
        arg.rwflag_int :=              {all access is allowed to local copy}
          [sst_rwflag_read_k, sst_rwflag_write_k];
        arg.rwflag_ext := [sst_rwflag_read_k]; {caller's arg will not be altered}
        end;
      sst_mem_alloc_scope (sizeof(arg_p^), arg_p); {allocate argument descriptor}
      if first_arg_p = nil then begin  {this is first arg for this data type ?}
        first_arg_p := arg_p;          {save pointer to first new arg in chain}
        end;
      next_pp^ := arg_p;               {link new arg to end of old chain}
      next_pp := addr(arg_p^.next_p);  {update pointer to new end of chain pointer}
      arg_p^ := arg;                   {init new arg descriptor from template}
      syn_get_tag_string (str_h, name); {get name of this call argument}
      string_downcase (name);
      sz := sizeof(string_var4_t) - 4 + name.len; {size of string to allocate}
      sst_mem_alloc_scope (sz, arg_p^.name_p); {allocate argument name string}
      arg_p^.name_p^.max := name.len;  {copy name into newly allocated string}
      string_copy (name, arg_p^.name_p^);
      end;
5:  begin                              {UNIV}
      arg.univ := true;
      end;
6:  begin                              {tag is data type for previous arguments}
      arg.dtype_p := nil;              {indicate to create new data type descriptor}
      sst_r_pas_data_type (arg.dtype_p); {make data type for this group of args}
      end;
7:  begin                              {VAL}
      arg.pass := sst_pass_val_k;      {caller will pass arg by value}
      arg.rwflag_int :=                {rout will make local copy, any access OK}
        [sst_rwflag_read_k, sst_rwflag_write_k];
      arg.rwflag_ext := [sst_rwflag_read_k]; {caller's arg will not be altered}
      end;
syn_tag_end_k: begin                   {no more tags in PARAM_DECL_GROUP}
      arg_p := first_arg_p;            {init to first arg with this data type}
      while arg_p <> nil do begin      {once for each arg with this data type}
        proc.n_args := proc.n_args + 1; {count one more call argument}
        arg_p^.dtype_p := arg.dtype_p; {point arg to its data type descriptor}
        arg_p^.univ := arg.univ;       {update type checking disable flag}
        arg_p := arg_p^.next_p;        {advance to next arg with this data type}
        end;
      syn_level_up;                    {up from PARAM_DECL_GROUP syntax}
      goto group_loop;                 {do next group of args with same data type}
      end;
otherwise
    syn_error_tag_unexp (tag, str_h);
    end;                               {end of PARAM_DECL_GROUP tag cases}
  goto tag_loop;                       {back for next PARAM_DECL_GROUP tag}
  end;
