package Sort is

   subtype Index is Integer range 1 .. 10;
   type IntArray is array(Index) of Integer;

   --# function Sorted(List : IntArray; M : Index; N : Index) return Boolean;

   procedure SelectionSort(List : in out IntArray);
   --# derives List from List;
   --# post for all I in Index range 1 .. List'Length =>
   --#   (for all J in Index range 1 .. List'Length => (I <= J -> List(I) <= List(J)));
   
   procedure BinarySearch(A : in IntArray; Target : in Integer; Result : out Index);
   --# derives Result from A, Target;
   --# pre (for all I in Index range 1 .. A'Length - 1 => (A(I) <= A(I+1))) and
   --#     (for some I in Index range 1 .. A'Length => (A(I) = Target));
   --# post A(Result) = Target;

   --# function Min(A : IntArray; M : Index; N : Index; MinIndex : Index) return Boolean;
   
   procedure PowerOfTwo(N : in Integer; Result : out Integer);
   --# derives Result from N;
   --# post 2**N = Result;
   

   procedure FindMinIndex(A : in IntArray; MinIndex : out Index);
   --# derives MinIndex from A;
   --# post Min(A, 1, A'Length, MinIndex);
   -- for all I in Index range 1 .. A'Length => (A(I) >= A(MinIndex));

end Sort;
