{   Subroutine SST_R_PAS_SMENT_CASE (STR_ALL_H)
*
*   Process a CASE statement.  The last syntax tag read was for the whole
*   CASE statement from inside the RAW_STATEMENT syntax.  STR_ALL_H is the
*   string handle for that tag.
}
module sst_r_pas_SMENT_CASE;
define sst_r_pas_sment_case;
%include 'sst_r_pas.ins.pas';

procedure sst_r_pas_sment_case (       {CASE statement, inside RAW_STATEMENT syntax}
  in      str_all_h: syo_string_t);    {string handle for whole CASE statement}

const
  max_msg_parms = 3;                   {max parameters we can pass to a message}

type
  chain_k_t = (                        {identifies which chain of choice values}
    chain_all_k,                       {chain of all choice value in CASE statement}
    chain_case_k);                     {chain of choice values for current case}

var
  tag: sys_int_machine_t;              {syntax tag ID}
  str_h: syo_string_t;                 {handle to string associated with TAG}
  dt_p: sst_dtype_p_t;                 {points to resolved base data type descriptor}
  dt: sst_dtype_k_t;                   {resolved base data type ID}
  case_opc_p: sst_case_opc_p_t;        {pnt to current case exec block descriptor}
  case_val_p: sst_case_val_p_t;        {pnt to current case choice value descriptor}
  fnam: string_treename_t;             {file name passed to a message}
  lnum: sys_int_machine_t;             {line number passed to a message}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;
  stat: sys_err_t;                     {completion status code}

label
  loop_case, loop_choice;
{
***********************************************************************
*
*   Local subroutine INSERT_CHAIN (CHAIN_ID)
*
*   Insert the current choice value descriptor, pointed to by CASE_VAL_P,
*   into the appropriate place in one of two chains.  CHAIN_ID selects
*   which chain to add the value descriptor to.  Valid values of CHAIN_ID are:
*
*     CHAIN_ALL_K  -  Add the choice value descriptor to the chain of all
*       the choice value descriptors for this whole CASE statement.
*
*     CHAIN_CASE_K  -  Add the choice value descriptor to the chain of
*       choices for the current case.  The descriptor for the current case
*       is pointed to by CASE_OPC_P.
*
*   In either case, the value descriptor is inserted to maintain the chain
*   sorted in ascending order.
}
procedure insert_chain (
  in      chain_id: chain_k_t);        {selects which chain to add to}

var
  prev_pp: sst_case_val_pp_t;          {points to chain pointer in previous entry}
  ent_p: sst_case_val_p_t;             {points to current chain entry}
  ofs: sys_int_adr_t;                  {offset of chain pointer from entry start}

label
  loop_ent;

begin
  case chain_id of                     {which chain are we adding descriptor to ?}
chain_all_k: begin                     {add to chain of all choice values}
      prev_pp := addr(sst_opc_p^.case_val_p);
      ofs :=                           {make adr offset for next entry pointer}
        sys_int_adr_t(addr(case_val_p^.next_val_p)) -
        sys_int_adr_t(case_val_p);
      end;
chain_case_k: begin                    {add to chain of choice value for curr case}
      prev_pp := addr(case_opc_p^.val_first_p);
      ofs :=                           {make adr offset for next entry pointer}
        sys_int_adr_t(addr(case_val_p^.next_opc_p)) -
        sys_int_adr_t(case_val_p);
      end;
otherwise
    sys_bomb;
    end;
  ent_p := prev_pp^;                   {init pointer to first entry in chain}

loop_ent:                              {back here to compare to next entry in chain}
  if ent_p = nil then begin            {hit end of chain ?}
    prev_pp^ := case_val_p;            {add new entry to end of chain}
    return;
    end;
  if ent_p^.val > case_val_p^.val then begin {insert right before this entry ?}
    prev_pp^ := case_val_p;            {point previous entry to new entry}
    prev_pp := univ_ptr(               {make address of "next" chain entry pointer}
      sys_int_adr_t(case_val_p) + ofs);
    prev_pp^ := ent_p;                 {point new entry to next in chain}
    return;
    end;
  if ent_p^.val = case_val_p^.val then begin {found duplicate choice value ?}
    sst_strh_info (ent_p^.exp_p^.str_h, fnam, lnum);
    sys_msg_parm_int (msg_parm[1], case_val_p^.val);
    sys_msg_parm_int (msg_parm[2], lnum);
    sys_msg_parm_vstr (msg_parm[3], fnam);
    syo_error (
      case_val_p^.exp_p^.str_h,
     'sst_pas_read', 'case_duplicate_choice_vals', msg_parm, 3);
    end;
  prev_pp := univ_ptr(                 {advance one entry in chain}
    sys_int_adr_t(ent_p) + ofs);
  ent_p := prev_pp^;
  goto loop_ent;                       {back and process this next chain entry}
  end;
{
***********************************************************************
*
*   Start of main routine.
}
begin
  fnam.max := sizeof(fnam.str);

  sst_opcode_new;                      {create new current opcode descriptor}
  with sst_opc_p^: opc do begin        {OPC is opcode descriptor for CASE statement}

  opc.opcode := sst_opc_case_k;        {opcode is for CASE switch}
  opc.str_h := str_all_h;              {save handle to source characters}
  opc.case_val_p := nil;               {init global choice values chain to empty}
  opc.case_opc_p := nil;               {init cases chain to empty}
  opc.case_none_p := nil;              {init to no OTHERWISE clause present}
{
*   Process expression that selects which case.
}
  syo_get_tag_msg (tag, str_h, 'sst_pas_read', 'statement_exec_bad', nil, 0);
  if tag <> 1 then syo_error_tag_unexp (tag, str_h);
  sst_r_pas_exp (str_h, false, opc.case_exp_p); {process choice selector expression}
  sst_dtype_resolve (                  {get base data types of this expression}
    opc.case_exp_p^.dtype_p^,          {input data type descriptor}
    dt_p, dt);                         {returned base data types}
  case dt of                           {what base data type ID does it have ?}
sst_dtype_int_k,
sst_dtype_enum_k,
sst_dtype_bool_k,
sst_dtype_char_k: ;                    {these are all the legal data types}
otherwise
    syo_error (str_h, 'sst_pas_read', 'exp_bad_case', nil, 0);
    end;

  case_opc_p := nil;                   {init to no current exec block descriptor}

loop_case:                             {back here each new complete case}
  syo_get_tag_msg (tag, str_h, 'sst_pas_read', 'statement_case_bad', nil, 0);
  case tag of
{
*******************************************
*
*   Tag indicates a whole new case with associated choice values and executable
*   block.
}
1: begin
  if case_opc_p = nil
    then begin                         {this is first exec case in chain}
      sst_mem_alloc_scope (            {allocate descriptor for this code block}
        sizeof(case_opc_p^), case_opc_p);
      opc.case_opc_p := case_opc_p;    {init pointer to first exec block in chain}
      end
    else begin                         {add new block to end of existing chain}
      sst_mem_alloc_scope (            {allocate descriptor for this code block}
        sizeof(case_opc_p^.next_p^), case_opc_p^.next_p);
      case_opc_p := case_opc_p^.next_p; {make new block the current block}
      end
    ;
  case_opc_p^.val_first_p := nil;      {init to no choice values chain here yet}
  case_opc_p^.code_p := nil;           {init to no executable statements this case}
  case_opc_p^.next_p := nil;           {indicate this block is end of chain}

loop_choice:                           {back here for each new tag this case}
  syo_get_tag_msg (tag, str_h, 'sst_pas_read', 'statement_case_bad', nil, 0);
  case tag of
{
*   Tag is for EXPRESSION syntax that identifies the value of another choice
*   for the current case.
}
1: begin
      sst_mem_alloc_scope (            {allocate descriptor for this choice value}
        sizeof(case_val_p^), case_val_p);
      case_val_p^.opc_p := case_opc_p; {point to descriptor for associated case}
      case_val_p^.next_val_p := nil;   {init chain pointers}
      case_val_p^.next_opc_p := nil;
      sst_r_pas_exp (str_h, true, case_val_p^.exp_p); {create expression descriptor}
      sst_exp_useage_check (           {check expression attributes for this useage}
        case_val_p^.exp_p^,            {expression to check}
        [sst_rwflag_read_k],           {read/write access needed to expression value}
        opc.case_exp_p^.dtype_p^);     {data type value must be compatible with}
      sst_ordval (                     {get ordinal value of this choice expression}
        case_val_p^.exp_p^.val,        {constant value descriptor}
        case_val_p^.val,               {returned ordinal value}
        stat);                         {error status return code}
      syo_error_abort (stat, str_h, '', '', nil, 0);
      insert_chain (chain_all_k);      {add to choices chain for whole CASE sment}
      insert_chain (chain_case_k);     {add to choices chain for this case}
      goto loop_choice;                {back to get next choice val for this case}
      end;
{
*   Tag is for RAW_STATEMENT syntax for the executable code for the current case.
}
2: begin
      sst_opcode_pos_push (case_opc_p^.code_p); {new opcodes get chained here}
      sst_r_pas_raw_sment;             {build opcodes chain for this case}
      sst_opcode_pos_pop;              {back to regular opcode chain}
      end;
{
*   Unexpected TAG value within current case.
}
otherwise
    syo_error_tag_unexp (tag, str_h);
    end;                               {end of tag cases within current case}
  goto loop_case;                      {back for next case in CASE statement}
  end;                                 {end of tag was another case in CASE sment}
{
*******************************************
*
*   TAG is for STATEMENTS syntax for the statements in the OTHERWISE clause.
}
2: begin
  sst_opcode_pos_push (opc.case_none_p); {new opcodes will get chained here}
  sst_r_pas_statements;                {build opcode chain for OTHERWISE case}
  sst_opcode_pos_pop;                  {back to regular opcode chain}
  end;
{
*******************************************
*
*   TAG indicates that this CASE statement has no OTHERWISE clause.
}
3: ;
{
*******************************************
*
*   Unexpected TAG value in CASE statement syntax.
}
otherwise
    syo_error_tag_unexp (tag, str_h);
    end;                               {end of CASE statement top level syntax tags}
  end;                                 {done with OPC abbreviation}
  end;
