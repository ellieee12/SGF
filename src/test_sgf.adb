with Ada.Text_IO;          use Ada.Text_IO;
with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;
with SGF; use SGF;
procedure test_sgf is
    Sgf : T_SGF;
    procedure Construct_SGF_Example (Sgf : out T_SGF) is
        
    begin
        Initialize(Sgf);
        Create_Directory_Current_Directory(Sgf,"home");
        Current_Directory(Sgf,"home");
        Create_Directory_Current_Directory(Sgf,"user1");
        Current_Directory(Sgf,"user1");
        Create_Directory_Current_Directory(Sgf,"projet");
        Current_Directory(Sgf,"projet");
        Create_File(Sgf,"/home/user1/projet/exemple.adb",10);
        Create_File(Sgf,"/home/user1/projet/exemple.adb",10);
        put_line(Get_Current_Directory(Sgf));
        List_Files(sgf,"./");
        
        
    end Construct_SGF_Example;
    procedure test_ex (Sgf : out T_SGF) is
      
    begin
        Initialize(Sgf);
        Create_File(Sgf,"Test1",12);
        Create_File(Sgf,"Test2",12);
        Create_File(Sgf,"Test3",12);
        
        Create_Directory_Current_Directory(Sgf,"Dir1");
        List_Files(sgf,"./");
        Current_Directory(Sgf,"/Dir1");
        Create_Directory_Current_Directory(Sgf,"Dir2");
        Current_Directory(Sgf,"Dir2");
        put_line(Get_Current_Directory(sgf));
        pragma Assert(not Is_Empty(Sgf));
        List_Files_Recursive(Sgf);
        put_line(Get_Current_Directory(Sgf));
    end test_ex;
begin
    Construct_SGF_Example(Sgf);
end test_sgf;

