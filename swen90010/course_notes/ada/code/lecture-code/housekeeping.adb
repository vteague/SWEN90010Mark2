with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Housekeeping is

   task Check_CPU;
   task Backup_Disk;

   task body Check_CPU is
   begin
      null;
   end Check_CPU;

   task body Backup_Disk is
   begin
      null;
   end Backup_Disk;

begin -- Housekeeping
   null;
end Housekeeping;
