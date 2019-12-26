{   Subroutine SST_W_C_DECLARE (DECL)
*
*   Write the implicit declaration indicated by DECL.  DECL must be one of the
*   constants of name DECL_xxx_K.
}
module sst_w_c_DECLARE;
define sst_w_c_declare;
%include 'sst_w_c.ins.pas';

procedure sst_w_c_declare (            {write out one of the implicit declarations}
  in      decl: decl_k_t);             {selects which implicit declaration to write}

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

begin
  if decl in decl_done then return;    {this declaration already written before ?}

  case decl of                         {which declaration is being requested ?}
decl_nil_k: begin
      sst_w_c_pos_push (sment_type_declg_k);
      sst_w.appends^ ('#define nil 0'(0));
      end;
decl_true_k: begin
      sst_w_c_pos_push (sment_type_declg_k);
      if sst_config.os = sys_os_domain_k
        then begin                     {OS is Aegis, we need to talk to Pascal}
          sst_w.appends^ ('#define true 255'(0));
          end
        else begin                     {OS is not Aegis, use native C TRUE value}
          sst_w.appends^ ('#define true 1'(0));
          end
        ;
      end;
decl_false_k: begin
      sst_w_c_pos_push (sment_type_declg_k);
      sst_w.appends^ ('#define false 0'(0));
      end;
decl_nullset_k: begin
      sst_w_c_pos_push (sment_type_declg_k);
      sst_w.appends^ ('#define nullset 0'(0));
      end;
decl_unspec_int_k: begin
      sst_w_c_pos_push (sment_type_declg_k);
      sst_w.appends^ ('#define unspec_int 0'(0));
      end;
decl_unspec_enum_k: begin
      sst_w_c_pos_push (sment_type_declg_k);
      sst_w.appends^ ('#define unspec_enum 0'(0));
      end;
decl_unspec_float_k: begin
      sst_w_c_pos_push (sment_type_declg_k);
      sst_w.appends^ ('#define unspec_float 0.0'(0));
      end;
decl_unspec_bool_k: begin
      sst_w_c_declare (decl_false_k);
      sst_w_c_pos_push (sment_type_declg_k);
      sst_w.appends^ ('#define unspec_bool false'(0));
      end;
decl_unspec_char_k: begin
      sst_w_c_pos_push (sment_type_declg_k);
      sst_w.appends^ ('#define unspec_char 0'(0));
      end;
decl_unspec_set_k: begin
      sst_w_c_declare (decl_nullset_k);
      sst_w_c_pos_push (sment_type_declg_k);
      sst_w.appends^ ('#define unspec_set nullset'(0));
      end;
decl_unspec_pnt_k: begin
      sst_w_c_declare (decl_nil_k);
      sst_w_c_pos_push (sment_type_declg_k);
      sst_w.appends^ ('#define unspec_pnt nil'(0));
      end;
decl_stdlib_k: begin
      sst_w_c_pos_push (sment_type_declg_k);
      sst_w.appends^ ('#include <stdlib.h>'(0));
      end;
decl_stddef_k: begin
      sst_w_c_pos_push (sment_type_declg_k);
      sst_w.appends^ ('#include <stddef.h>'(0));
      end;
decl_stdio_k: begin
      sst_w_c_pos_push (sment_type_declg_k);
      sst_w.appends^ ('#include <stdio.h>'(0));
      end;
decl_string_k: begin
      sst_w_c_declare (decl_stddef_k); {STTDEF must come before STRING}
      sst_w_c_pos_push (sment_type_declg_k);
      sst_w.appends^ ('#include <string.h>'(0));
      end;
decl_math_k: begin
      sst_w_c_pos_push (sment_type_declg_k);
      sst_w.appends^ ('#include <math.h>'(0));
      end;
otherwise
    sys_msg_parm_int (msg_parm[1], ord(decl));
    sys_message_bomb ('sst_c_write', 'const_implicit_bad', msg_parm, 1);
    end;

  sst_w.line_close^;
  sst_w_c_pos_pop;
  decl_done := decl_done + [decl];     {flag this particular constant as written}
  end;
