with Ada.Text_IO;          use Ada.Text_IO;
with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;
with SGF; use SGF;
procedure test_sgf is
   Sgf : T_SGF;
   procedure test_ex (Sgf : out T_SGF) is
      
   begin
      Initialize(Sgf);
      Create_File_Current_Directory(Sgf,"Test",12);
      pragma Assert(not Is_Empty(Sgf));
   end test_ex;
begin
   test_ex(Sgf);
end test_sgf;
