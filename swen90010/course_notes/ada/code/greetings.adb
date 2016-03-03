with Ada.Text_IO; use Ada.Text_IO;

package body Greetings is
   procedure Hello is
   begin
      Put_Line("Hello, world!");
   end Hello;

   procedure Goodbye is
   begin
      Put_Line("Goodbye, world!");
   end Goodbye;
   
   function Talk(Text : in String) return Integer is
   begin
      Put_Line(Text);
      return 1;
   end Talk;
end Greetings;
