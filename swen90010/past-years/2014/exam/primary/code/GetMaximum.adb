package body GetMaximum is
   
   procedure GetMax(A : in PosIntArray; Max : out Integer) is
      I : Integer;
   begin
      Max := -1;
      I := 1;
      while I < A'Length loop
	 --# assert (for all J in Index range 1..I => (Max >= A(J)));
	 if A(I) > Max then
	    Max := A(I);
	 end if;
	 I := I + 1;
      end loop;      
   end GetMax;
   
end GetMaximum;
