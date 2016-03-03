with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Array_Examples is
   Int_Index : array(Integer range 5..10) of Character :=
     ('a', 'b', 'c', 'd', 'e', 'f');
   Ch_Index : array(Character) of Character;
begin
   Put(Int_Index(5));  -- accessing using an integer index
   New_Line;
   
   Ch_Index('a') := 'z'; -- setting using a char index
   Put(Ch_Index('a')); -- accessing using a char index
   New_Line;
   
   -- setting and getting an array 'slice'
   Int_Index(6 .. 8) := ('X', 'Y', 'Z');
   
   -- array attributes "'First" and "'Last"
   Put(Ch_Index'First);
   New_Line;
   Put(Int_Index'Last);
   New_Line;
end Array_Examples;
