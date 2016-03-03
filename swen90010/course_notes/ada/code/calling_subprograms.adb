with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Calling_Subprograms is
   An_Int : Integer := 50;
   Another_Int : Integer := 60;
begin
   Put(An_Int, 20);  -- positional parameter
   New_Line;         -- no parameters
   Put(Width => 20, Item => An_Int);  -- named associations
end Calling_Subprograms;
