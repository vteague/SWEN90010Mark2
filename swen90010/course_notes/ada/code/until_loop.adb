with Ada.Text_IO; use Ada.Text_IO;

procedure Until_Loop is
   X : Integer := 0;
begin
   loop
      Put_Line("Hello again, world!");
      X := X + 1;
      exit when X >= 10;
   end loop;
end Until_Loop;
