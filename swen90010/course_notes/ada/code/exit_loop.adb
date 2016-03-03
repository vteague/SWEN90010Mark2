with Ada.Text_IO; use Ada.Text_IO;

procedure Exit_Loop is
   X : Integer := 0;
   Y : Integer := 0;
begin
   loop
      Put_Line("Hello again, world!");
      X := X + 1;
      exit when X >= 10;
      Y := Y + 2;
   end loop;
end Exit_Loop;
