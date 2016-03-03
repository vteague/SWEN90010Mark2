procedure Subtyping is
   
   subtype One is Integer range 0..100;
   subtype Two is Integer range 20..300;
   
   X : One := 0;
   Y : Two := 25;
begin
   X := X + Y;
end Subtyping;
