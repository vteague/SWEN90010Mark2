with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Array_Loop is
   X_Array : array(Integer range 1..10) of Integer :=
     (29, 28, 27, 26, 25, 24, 23, 22, 21, 20);
begin
   for I in X_Array'Range loop
      Put("Hello again, world!:");
      Put(X_Array(I));
      New_Line;
   end loop;
end Array_Loop;
