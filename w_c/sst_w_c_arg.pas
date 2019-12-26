{   Subroutine SST_W_C_ARG (ARG_P, ARGT_P, DONE)
*
*   Write a call argument in a subroutine or function call.  ARG_P is pointing
*   to the calling argument descriptor, and ARGT_P is pointing to the argument
*   template descriptor.  Both will be advanced to the next argument upon return.
*   There are no more arguments when both ARG_P and ARGT_P are returned NIL.
}
module sst_w_c_ARG;
define sst_w_c_arg;
%include 'sst_w_c.ins.pas';

var
  star: string_var4_t :=               {"*" pointer data type operator}
    [str := '*', len := 1, max := sizeof(star.str)];

procedure sst_w_c_arg (                {write argument in function/subroutine call}
  in out  arg_p: sst_proc_arg_p_t;     {points to arg descriptor, will be advanced}
  in out  argt_p: sst_proc_arg_p_t);   {points to arg template, will be advanced}

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  at_p: sst_proc_arg_p_t;              {points to descriptor to use as the template}
  ac_p: sst_proc_arg_p_t;              {points to descriptor to for actual call}
  dt_force_p: sst_dtype_p_t;           {points to dtype to force arg into}
  dt_temp_p: sst_dtype_p_t;            {base data type of argument template}
  dt_arg_p: sst_dtype_p_t;             {base data type of argument}
  dt_p: sst_dtype_p_t;                 {scratch data type pointer}
  exp_p: sst_exp_p_t;                  {pointer to argument value expression}
  adr_cnt: sys_int_machine_t;          {number of times to take "address of" arg}
  enc: enclose_k_t;                    {enclose in parentheses yes/no flag}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

label
  done_univ;

begin
  enc := enclose_no_k;                 {init to don't need to enclose arg in ()}

  if argt_p = nil                      {arg template descriptor exists ?}
    then at_p := arg_p                 {no it doesn't, use arg as template}
    else at_p := argt_p;               {yes it does, use the template as template}

  if arg_p = nil                       {call descriptor exists ?}
    then ac_p := argt_p                {no it doesn't, use template}
    else ac_p := arg_p;                {yes it does, use ARG_P directly}

  exp_p := ac_p^.exp_p;                {get pointer to argument value expression}

  dt_temp_p := at_p^.dtype_p;          {init template data type}
  while dt_temp_p^.dtype = sst_dtype_copy_k {resolve base template data type}
    do dt_temp_p := dt_temp_p^.copy_dtype_p;

  dt_arg_p := ac_p^.dtype_p;           {init argument data type}
  while dt_arg_p^.dtype = sst_dtype_copy_k {resolve base argument data type}
    do dt_arg_p := dt_arg_p^.copy_dtype_p;

  case at_p^.pass of                   {what is argument passing method ?}
sst_pass_ref_k: begin                  {argument is passed by reference}
      adr_cnt := 1;
      end;
sst_pass_val_k: begin                  {argument is passed by value}
      adr_cnt := 0;
      end;
otherwise
    sys_msg_parm_int (msg_parm[1], ord(ac_p^.pass));
    sys_message_bomb ('sst_c_write', 'ac_pass_method_bad', msg_parm, 1);
    end;

  dt_force_p := dt_temp_p;             {init to force arg to be template data type}
  if
      (at_p^.univ or (                 {allowed to match any data type ?}
        (dt_temp_p^.dtype = sst_dtype_pnt_k) and {allowed to match any pointer ?}
        (dt_temp_p^.pnt_dtype_p = nil))) and
      (exp_p^.dtype_hard or            {argument data type is absolutely certain ?}
        not sst_dtype_convertable(dt_arg_p^, dt_temp_p^)) {arg can't be converted ?}
      then begin
    dt_force_p := nil;                 {don't try to alter argument data type}
    if                                 {passing CHAR to a STRING ?}
        (dt_temp_p^.dtype = sst_dtype_array_k) and
        dt_temp_p^.ar_string and
        (dt_arg_p^.dtype = sst_dtype_char_k)
        then begin
      goto done_univ;                  {no need for type casting function}
      end;
    if dt_arg_p <> dt_temp_p then begin {argument doesn't match template data type ?}
      sst_w.appendn^ ('(', 1);         {start of type casting operator}
      if dt_temp_p^.dtype = sst_dtype_array_k
        then begin                     {we are passing a UNIV array by reference}
          if dt_temp_p^.ar_dtype_rem_p = nil {resolve data type of array elements}
            then dt_p := dt_temp_p^.ar_dtype_ele_p
            else dt_p := dt_temp_p^.ar_dtype_rem_p;
          sst_w_c_dtype_simple (dt_p^, star, false); {write array elements data type}
          end
        else begin                     {we are passing a UNIV non-array by reference}
          sst_w_c_dtype_simple (at_p^.dtype_p^, star, false); {write target dtype}
          end
        ;
      sst_w.appendn^ (')', 1);         {done writing type casting operator}
      enc := enclose_yes_k;            {arg must now be enclosed in () if not simple}
      end;
    end;                               {done handling data type issues}
done_univ:                             {done handling UNIV argument template}

  if                                   {template data type is UNIVERSAL POINTER ?}
      (dt_temp_p^.dtype = sst_dtype_pnt_k) and
      (dt_temp_p^.pnt_dtype_p = nil)
      then begin
    dt_force_p := nil;                 {don't force argument's data type to match}
    end;

  if sst_rwflag_write_k in at_p^.rwflag_ext then begin {value being passed back ?}
    dt_force_p := nil;                 {don't force expression data type to match}
    end;

  sst_w_c_armode_push                  {array names will act like pointers to array}
    (array_pnt_whole_k);
  sst_w_c_exp (                        {write this argument value}
    exp_p^,                            {argument value expression}
    adr_cnt,                           {number of times to take "address of"}
    dt_force_p,                        {pointer to mandatory expression data type}
    enc);                              {enclose in parentheses yes/no flag}
  sst_w_c_armode_pop;

  if arg_p <> nil                      {advance to next call argument descriptor}
    then arg_p := arg_p^.next_p;
  if argt_p <> nil                     {advance to next argument template}
    then argt_p := argt_p^.next_p;
  end;
