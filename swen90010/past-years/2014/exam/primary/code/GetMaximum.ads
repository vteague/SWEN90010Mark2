package GetMaximum is
   
   subtype Index is Integer range 1 .. 10;
   type PosIntArray is array(Index) of Integer;
   
   procedure GetMax(A : in PosIntArray; Max : out Integer);
   --# derives Max from A;
   --# post (A'Length = 0 -> Max = -1) and
   --#      (A'Length /= 0 -> (for all J in Index range 1..A'Length => (Max >= A(J))));

end GetMaximum;
