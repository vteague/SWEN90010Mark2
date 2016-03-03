with Ada.Text_IO; use Ada.Text_IO;

procedure While_Loop is
   X : Integer := 0;
begin
   while X < 10 loop
      Put_Line("Hello again, world!");
      X := X + 1;
   end loop;
end While_Loop;
