with Measures; 
with Heart;

-- This package simulates a simple impulse generator for an ICD. It is
--  provided with an amount to discharge, and provides this amount to
--  a heart in the next 'tick'.
package ImpulseGenerator
   with SPARK_Mode
is
   
   -- The generator type
   type GeneratorType is
      record
	 -- The current impulse; to be administered to the heart at
	 --  the next clock tick
	 Impulse : Measures.Joules;  
   
	 -- Indicates whether the generator has been turned on.
	 IsOn : Boolean;
      end record;
   
   -- Create and initialise a new generator.
   procedure Init(Generator : out GeneratorType)
     with Post => (Generator.IsOn = False and Generator.Impulse = 0);
   
   -- Turn on the generator, but do not administer any impulse yet.
   procedure On(Generator : in out GeneratorType)
     with Post => (Generator.IsOn);
   
   -- Turn off the generator;
   procedure Off(Generator : in out GeneratorType)
     with Post => (not Generator.IsOn);
   
   -- Query the status of the generator (on/off)
   function IsOn(Generator : in GeneratorType) return Boolean
     with Post => (IsOn'Result = Generator.IsOn);
   
   -- Set the impulse to be administered
   procedure SetImpulse(Generator : in out GeneratorType; 
                        J : in Measures.Joules)
     with Post => ((if Generator.IsOn then Generator.Impulse = J)
                   and
                  (if not Generator.IsOn then Generator = Generator'Old));
   
   -- Tick the clock, providing an impulse to the heart.
   procedure Tick(Generator : in GeneratorType; 
                  Hrt : in out Heart.HeartType)
     with Post => (if not Generator.IsOn then 
                     Hrt.Rate = Hrt'Old.Rate and Hrt.Impulse = Hrt'Old.Impulse);
   
end ImpulseGenerator;

