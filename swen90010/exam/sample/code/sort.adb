package body Sort is

   procedure SelectionSort(List : in out IntArray)
   is
      I, J, MinIndex : Index;
      T : Integer;
   begin
      I := 1;
      while I < List'Length - 1 loop
         --# assert for all M in Index range 1 .. I - 1 =>
         --#  (for all N in Index range 1 .. I - 1 => (M <= N -> List(M) <= List(N)));

         -- find the minimum element in the remaining part of the list
         J := I + 1;
         MinIndex := I;
         while J < List'Length loop
            --# assert for all M in Index range I + 1 .. J =>
            --#     (List(M) >= List(MinIndex));
            if List(J) < List(MinIndex) then
               MinIndex := J;
            end if;

            J := J + 1;
         end loop;

         T := List(I);
         List(I) := List(MinIndex);
         List(MinIndex) := T;

         I := I + 1;
      end loop;

   end SelectionSort;
   
   procedure BinarySearch(A : in IntArray; Target : in Integer; Result : out Index) is
      Low : Index := Index'First;
      Mid : Index;
   begin
      Result := Index'Last;
      while (Low < Result) loop
	 --# assert (for all I in Index range 1 .. A'Length - 1 => (A(I) <= A(I+1))) and
	 --# (for some I in Index range Low .. Result => (A(I) = Target)) and
	 --# A(Low) <= Target and Target <= A(Result);
	 Mid := (Low + Result) / 2;
	 if A(Mid) < Target then
	    Low := Mid + 1;
	 else
	    Result := Mid;
	 end if;
      end loop;
   end BinarySearch;
   
   procedure PowerOfTwo(N : in Integer; Result : out Integer) is
      K : Integer;
   begin
      K := 0;
      Result := 1;
      while K /= N loop
	 --# assert Result = 2**K;
	 K := K + 1;
	 Result := 2 * Result;
      end loop;
   end PowerOfTwo;
   
   procedure FindMinIndex(A : in IntArray; MinIndex : out Index)
   is
      J : Index;
   begin
      -- find the minimum element in the list
      J := 1;
      MinIndex := 1;
      while J /= A'Length loop
         ----# assert for all I in Index range 1 .. J => (A(I) >= A(MinIndex));
         --# assert Min(A, 1, J, MinIndex);
        if A(J) < A(MinIndex) then
          MinIndex := J;
        end if;

        J := J + 1;
      end loop;

   end FindMinIndex;
end Sort;
