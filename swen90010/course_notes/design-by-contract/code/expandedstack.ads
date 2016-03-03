package ExpandedStack
  --# own Top : TopRange; Elements : DataArray;
  --# initializes Top, Elements;
is
   -- the maximum size of the stack
   MAX_SIZE : constant Positive := 100;

   subtype Index is Integer range 1 .. MAX_SIZE;
   subtype TopRange is Integer range 0 .. MAX_SIZE;
   type DataArray is array(Index) of Integer;

   procedure Init;
   --# global out Elements, Top;
   --# derives Elements, Top from ;
   --# pre 0 >= Integer'First and 0 <= Integer'Last;
   --# post Top = 0 and (for all I in Index => (Elements(I) = 0));

   procedure Push(I : in Integer);
   --# global in out Elements, Top;
   --# derives Elements from Elements, Top, I &
   --#         Top from Top;
   --# pre Top < MAX_SIZE;
   --# post Top = Top~ + 1 and Elements(Top) = I and
   --#  (for all J in Index => (J /= Top -> Elements(J) = Elements~(J)));

   procedure Pop(I : out Integer);
   --# global in Elements; in out Top;
   --# derives I from Elements, Top & Top from Top;
   --# pre Top > 0;
   --# post Top = Top~ - 1 and I = Elements(Top~);

   procedure IsFull(Result : out Boolean);
   --# global in Top;
   --# derives Result from Top;
   --# post (Top = MAX_SIZE <-> Result);

   procedure IsEmpty(Result : out Boolean);
   --# global in Top;
   --# derives Result from Top;
   --# post (Top = 0 <-> Result); 

   function Size return TopRange;
   --# global in Top;
   --# return Top;
   
private

   Elements : DataArray := DataArray'(Index => 0);
   Top  : TopRange := 0;

end ExpandedStack;
