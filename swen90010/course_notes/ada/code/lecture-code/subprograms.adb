with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body Subprograms is
   
   procedure Println(Int : in Integer; Width : in Integer) is
   begin
      Put(Int, Width);
   end Println;
   
   procedure Println(Int : in Integer) is
   begin
      Put(Int);
      Println;
   end Println;
   
   procedure Println is
   begin
      New_Line;
   end Println;
end Subprograms;
