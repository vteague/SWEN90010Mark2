with Ada.Text_IO;

procedure Hello_World_Renaming is
   package IO renames Ada.Text_IO;
begin
     IO.Put_Line("Hello, world!");
     IO.New_Line;
     IO.Put_Line("...using package renaming!");
end Hello_World_Renaming;
