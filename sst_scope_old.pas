{   Subroutine SST_SCOPE_OLD
*
*   Set the parent of the current scope as the new current scope.
}
module sst_SCOPE_OLD;
define sst_scope_old;
%include 'sst2.ins.pas';

procedure sst_scope_old;               {pop back to parent scope}

begin
  sst_scope_p := sst_scope_p^.parent_p;
  sst_names_p := sst_scope_p;
  end;
