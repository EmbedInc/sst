{   Subroutine SST_SET_ELE_FIND (VAL,ELE,P,MASK)
*
*   Find the bit representing a particular set element in the constant descriptor
*   for a set value.  The calling program can then easily read or write the bit.
*
*   VAL  -  Constant value descriptor.  It is an error if its data type
*     is not SET.
*
*   ELE  -  The element number within the set.  Elements are numbered from 0 to
*     (number of elements - 1).
*
*   P  -  Returned pointing to the integer word containing the particular
*     requested set bit.
*
*   MASK  -  Returned mask word that selects only the particular set element
*     bit.  All bits are zero, except for the bit corresponding to the set
*     element in P^.
}
module sst_SET_ELE_FIND;
define sst_set_ele_find;
%include 'sst2.ins.pas';

procedure sst_set_ele_find (           {get handle to bit for particular set element}
  in      val: sst_var_value_t;        {constant set expression value descriptor}
  in      ele: sys_int_machine_t;      {set element number, 0 - N_ELE-1}
  out     p: sys_int_conv32_p_t;       {points to word containing set element}
  out     mask: sys_int_conv32_t);     {mask word for this set element bit}

const
  max_msg_parms = 2;                   {max parameters we can pass to a message}

var
  dt_p: sst_dtype_p_t;                 {points to base data type descriptor of set}
  ind: sys_int_machine_t;              {set elements array index for word with ele}
  bit: sys_int_machine_t;              {number of element bit within its word}
  msg_parm:                            {references parameters for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

begin
  if val.dtype <> sst_dtype_set_k then begin {value descriptor is not for a set ?}
    sys_message_bomb ('sst', 'value_not_a_set', nil, 0);
    end;

  dt_p := val.set_dtype_p;             {init pointer to base data type descriptor}
  while dt_p^.dtype = sst_dtype_copy_k do begin {resolve copied data type}
    dt_p := dt_p^.copy_dtype_p;
    end;

  if (ele < 0) or (ele >= dt_p^.bits_min) then begin {ELE out of range ?}
    sys_msg_parm_int (msg_parm[1], ele);
    sys_msg_parm_int (msg_parm[2], dt_p^.bits_min-1);
    sys_message_bomb ('sst', 'set_ele_out_of_range', msg_parm, 2);
    end;

  ind := ele div sst_set_ele_per_word; {find which word the element bit is in}
  bit := ele - (sst_set_ele_per_word * ind); {number of bit within word}
  ind := dt_p^.set_n_ent - 1 - ind;    {make array index for this word}
  p := addr(val.set_val_p^[ind]);      {make pointer to word containing ele bit}
  mask := lshft(1, bit);               {make mask for this element bit}
  end;
