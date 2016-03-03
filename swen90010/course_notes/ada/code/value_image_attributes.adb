with Ada.Text_IO; use Ada.Text_IO;

procedure Value_Image_Attributes is
   An_Integer : Integer;
   A_String : String := "5";
begin
   An_Integer := Integer'Value(A_String);
   Put_Line("Hello, world!" & Integer'Image(An_Integer));
end Value_Image_Attributes;
