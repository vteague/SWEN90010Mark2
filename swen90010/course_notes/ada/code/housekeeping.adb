with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Housekeeping is

   task Check_CPU;
   task Backup_Disk;

   task body Check_CPU is
   begin
      for count in 1..50 loop
         Put (Item => "Check CPU status ");
         Put (Item => count, Width => 1);
         New_Line;
      end loop;
   end Check_CPU;

   task body Backup_Disk is
   begin
      for count in 1..50 loop
         Put (Item => "Backup disk status ");
         Put (Item => count, Width => 1);
         New_Line;
      end loop;
   end Backup_Disk;

   -- the two tasks are automatically created and begin execution

begin -- Housekeeping
   null;
   -- Housekeeping waits here for them to terminate
end Housekeeping;
