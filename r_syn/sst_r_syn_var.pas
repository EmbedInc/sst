{   Routines to create SST expressions for use in syntax parsing functions.
}
module sst_r_syn_var;
define sst_r_syn_var_proc;
%include 'sst_r_syn.ins.pas';
{
********************************************************************************
*
*   Function SST_R_SYN_VAR_PROC (SYM)
*
*   Create a "variable" reference to the procedure that SYM is the symbol for.
}
function sst_r_syn_var_proc (          {make "variable" reference to a procedure}
  in var  sym: sst_symbol_t)           {the procedure symbol}
  :sst_var_p_t;                        {pointer to the new variable reference}
  val_param;

var
  var_p: sst_var_p_t;                  {pointer to the new variable reference}

begin
  sst_mem_alloc_scope (                {allocate memory for the variable reference}
    sizeof(var_p^), var_p);

  var_p^.mod1.next_p := nil;           {no second modifier}
  var_p^.mod1.modtyp := sst_var_modtyp_top_k; {this is first (and only) modifier}
  var_p^.mod1.top_str_h.first_char.crange_p := nil;
  var_p^.mod1.top_sym_p := addr(sym);  {pointer to symbol being referenced}
  var_p^.dtype_p := sym_ichar_p^.proc_dtype_p; {procedure data type}
  var_p^.rwflag := [sst_rwflag_read_k]; {read-only}
  var_p^.vtype := sst_vtype_rout_k;    {this var reference is to procedure}
  var_p^.rout_proc_p := addr(sym.proc); {points to procedure definition}

  sst_r_syn_var_proc := var_p;         {return pointer to the new variable reference}
  end;
