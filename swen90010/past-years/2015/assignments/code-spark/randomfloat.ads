package RandomFloat is
   -- Generate a random floating point number from a normal
   --  distribution of mean Mu and standard deviation Sigma
   function NormalDistribution(Mu : in Float;
                               Sigma : in Float) return Float;
   
   -- Generate a random floating point number from a uniform
   --  distribution between Lower and Upper (inclusive).
   function UniformDistribution(Lower : in Float; Upper : in Float) return Float;
   
   -- Generate a random floating point number within an error margin
   --  of a given floating point value
   function UniformError(Value : in Float; Error : in Float) return Float;
end RandomFloat;
