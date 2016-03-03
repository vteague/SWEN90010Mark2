package body Stack is

   procedure Init is
   begin
      Top := 0;
      Elements := DataArray'(Index => 0);
   end Init;

   procedure Push(I : in Integer) is
   begin
      Top := Top + 1;
      Elements(Top) := I;
   end Push;

   procedure Pop(I : out Integer) is
   begin
      I := Elements(Top);
      Top := Top - 1;
   end Pop;

   procedure IsFull(Result : out Boolean) is
   begin
      if Top = MAX_SIZE then
         Result := True;
      else
         Result := False;
      end if;
   end IsFull;

   procedure IsEmpty(Result : out Boolean) is
   begin
      if Top = 0 then
         Result := True;
      else
         Result := False;
      end if;
   end IsEmpty;

   function Size return TopRange is
   begin
      return Top;
   end Size;

end Stack;


