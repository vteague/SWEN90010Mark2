package body Task3Solution with
     SPARK_Mode => On
is

   procedure Task3Procedure(Input : in Integer; Result : out Integer) is
   begin
      if Input <= Integer'Last / 10 and
        Input >= Integer'First / 10 then
         Result := Input * 10;
      else
         Result := Input;
      end if;
   end Task3Procedure;
end Task3Solution;

