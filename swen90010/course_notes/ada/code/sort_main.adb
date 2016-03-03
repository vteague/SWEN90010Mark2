with Sort; use Sort;
with Ada.Text_IO; use  Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Sort_Main is
   Int_Array : Data_Array := (5, 3, 9, 1, 5, 4);
begin
   -- sort the array
   Insertion_Sort(Int_Array);

   -- iterate over the array and print the elements
   for I in Int_Array'First .. Int_Array'Last loop
      Put(Int_Array(I), 2);
      Put(", ");
   end loop;
end Sort_Main;
