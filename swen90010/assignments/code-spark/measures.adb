package body Measures is
   
   function Limit(Input : in Float; Fst : in Float; Lst : in Float) return Float
   is
      Output : Float;
   begin
      if Input < Fst then
	 Output := Fst;
      elsif Input > Lst then
	 Output := Lst;
      else
	 Output := Input;
      end if;
      
      return Output;
   end Limit;
   
   function LimitMmHg(Input : in Float) return MmHg 
   is begin
      return Limit(Input, MmHg'First, MmHg'Last);
   end LimitMmHg;
   
   function LimitDosage(Input : in Float) return Dosage 
   is begin
      return Limit(Input, Dosage'First, Dosage'Last);
   end LimitDosage;

end Measures;

