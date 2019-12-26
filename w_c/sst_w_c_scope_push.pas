{   Subroutine SST_W_C_SCOPE_PUSH (SCOPE,SCOPE_TYPE)
*
*   Set the current scope to SCOPE.  The state for the old scope will be pushed
*   onto the stack.  The old scope state can be recovered with SST_W_C_SCOPE_POP.
*   SCOPE_TYPE is the type of the new scope.  This must be one of the constants
*   SCOPE_TYPE_xxx_K.
}
module sst_w_c_SCOPE_PUSH;
define sst_w_c_scope_push;
%include 'sst_w_c.ins.pas';

procedure sst_w_c_scope_push (         {set a new scope as current}
  in      scope: sst_scope_t;          {scope to set current}
  in      scope_type: scope_type_k_t); {the type of the new scope}

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  fold_p: frame_scope_p_t;             {pointer to previous scope stack frame}
  global_only: boolean;                {declare only global symbols first if true}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

begin
%debug; write (sst_stack^.last_p^.curr_adr, ' ');
%debug; writeln ('SCOPE PUSH');

  frame_scope_p^.scope_p := sst_scope_p; {save pnt to old scope on old stack frame}
  fold_p := frame_scope_p;             {save pointer to old scope stack frame}

  util_stack_push (                    {create new stack frame}
    sst_stack, sizeof(frame_scope_p^), frame_scope_p);

  frame_scope_p^.prev_p := fold_p;
  frame_scope_p^.pos_decll := sst_out.dyn_p^;
  frame_scope_p^.pos_exec := sst_out.dyn_p^;
  frame_scope_p^.scope_p := addr(scope);
  frame_scope_p^.funcval_sym_p := nil; {init to no function return val var exists}
  frame_scope_p^.const_p := nil;       {init to no implicit const var created yet}
  frame_scope_p^.scope_type := scope_type; {set what kind of scope this is}

  case scope_type of
scope_type_global_k,
scope_type_module_k: begin             {scope is always global}
      frame_scope_p^.sment_type := sment_type_declg_k;
      sst_out.dyn_p := addr(pos_declg);
      global_only := false;
      end;
scope_type_prog_k: begin               {scope will start out as global}
      frame_scope_p^.sment_type := sment_type_declg_k;
      sst_out.dyn_p := addr(pos_declg);
      global_only := true;
      end;
scope_type_rout_k: begin               {scope is always local}
      global_only := false;            {declare all symbols for this scope}
      if                               {already within executable scope ?}
          (fold_p^.scope_type = scope_type_prog_k) or
          (fold_p^.scope_type = scope_type_rout_k)
        then begin                     {this is a nested executable scope}
          frame_scope_p^.funcval_sym_p := fold_p^.funcval_sym_p; {propagate state}
          frame_scope_p^.const_p := fold_p^.const_p;
          frame_scope_p^.sment_type := sment_type_exec_k; {curr in executable sments}
          sst_out.dyn_p := addr(frame_scope_p^.pos_exec); {use pos for exec sments}
          end
        else begin                     {this is executable scope of new routine}
          frame_scope_p^.sment_type := sment_type_decll_k; {assume local scope}
          sst_out.dyn_p := addr(frame_scope_p^.pos_decll);
          if                           {scope belongs to function with return val ?}
              (scope.symbol_p <> nil) and then
              scope.symbol_p^.symtype = sst_symtype_proc_k
              then begin
            frame_scope_p^.funcval_sym_p := {save pointer to function value variable}
              scope.symbol_p^.proc_funcvar_p;
            end;
          end
        ;
      end;
otherwise
    sys_msg_parm_int (msg_parm[1], ord(scope_type));
    sys_message_bomb ('sst_c_write', 'scope_type_bad', msg_parm, 1);
    end;

  sst_scope_p := addr(scope);          {set the new scope as current}
  sst_names_p := sst_scope_p;
  sst_w_c_symbols (global_only);       {declare symbols in this new scope}
  end;
