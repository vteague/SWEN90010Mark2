package body Task5Solution is
   
   procedure Task5Procedure(A : in Boolean; 
			    B : in Boolean; 
			    C : out Boolean) is
   begin
      if not A then
	 C := True;
      else
	 if B then
	    C := True;
	 else
	    C := False;
	 end if;
      end if;
   end Task5Procedure;
end Task5Solution;

