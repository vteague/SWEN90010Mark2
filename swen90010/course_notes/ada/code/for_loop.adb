with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure For_Loop is
begin
   for I in Integer range 1 .. 10 loop
      Put("Hello again, world!:");
      Put(I);
      New_Line;
   end loop;
end For_Loop;
