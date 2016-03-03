package Sort is
   type Data_Array is array(Natural range <>) of Integer;
   procedure Insertion_Sort(Item : in out Data_Array);
end Sort;
