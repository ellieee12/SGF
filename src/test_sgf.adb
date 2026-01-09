with Ada.Text_IO;          use Ada.Text_IO;
with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;
with SGF; use SGF;
procedure test_sgf is
    Sgf : T_SGF;
    procedure test_ex (Sgf : out T_SGF) is
      
    begin
        Initialize(Sgf);
        Create_File_Current_Directory(Sgf,"Test1",12);
        Create_File_Current_Directory(Sgf,"Test2",12);
        Create_File_Current_Directory(Sgf,"Test3",12);
        Create_Directory_Current_Directory(Sgf,"Dir1");
        
        pragma Assert(not Is_Empty(Sgf));
        put_line(Get_Current_Directory(Sgf));
    end test_ex;
begin
    test_ex(Sgf);
end test_sgf;
