with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Entry_Call is

   task type Encapsulated_Variable_Task_Type is
      entry Store (An_Integer : in  Integer);
      entry Fetch (An_Integer : out Integer);
   end Encapsulated_Variable_Task_Type;

   task body Encapsulated_Variable_Task_Type is
      Datum : Integer := 0;
   begin
      loop
         select
	    accept Store (An_Integer : in Integer) do
	       Datum := An_Integer;
	       Put ("Storing ");
	       Put (An_Integer);
	       New_Line;
	    end Store;
         or
            accept Fetch (An_Integer : out Integer) do
               An_Integer := Datum;
               Put ("Fetching ");
               Put (An_Integer);
               New_Line;
            end Fetch;
         end select;
      end loop;
   end Encapsulated_Variable_Task_Type;

   X, Y : Encapsulated_Variable_Task_Type;
   It : Integer;

begin
   It := 5;
   X.Store(It);
   X.Fetch(It);
   Y.Store(It);
end Entry_Call;
