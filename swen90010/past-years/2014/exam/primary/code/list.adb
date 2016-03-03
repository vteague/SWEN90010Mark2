package body List is
   
   procedure ListAverage(AList : in out FloatArray; Result : out Float)
   is
    Sum : Float;
    I : Integer;
  begin
    Sum := 0.0;
    I := 0;

    while I /= AList'Length loop
      --# assert Sum = Summation(I, AList);
      I := I + 1;
      Sum := Sum + AList(I);
    end loop;

    if AList'Length = 0 then
      Result := 0.0;
    else
      Result := Sum / Float(AList'Length);
    end if;

    AList(1) := Result;
  end ListAverage;
end List;
