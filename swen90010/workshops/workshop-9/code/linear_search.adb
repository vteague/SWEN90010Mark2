package body Linear_Search
  with SPARK_Mode
is

   function Search (A : Ar; I : Integer) return Opt_Index is
   begin

      -- If we didn't find it, return No_Index (0)
      return No_Index;
   end Search;

end Linear_Search;
