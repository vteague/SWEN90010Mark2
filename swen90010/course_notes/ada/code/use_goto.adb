function Use_Goto return Integer is
   X : Integer := 5;
   Y : Integer := 10;
   Z : Integer;
begin
   Z := X + Y;

   goto Label;

   Z := 0;

   <<Label>>

   return Z;
end Use_Goto;
