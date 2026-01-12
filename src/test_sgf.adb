with Ada.Text_IO;          use Ada.Text_IO;
with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;
with SGF; use SGF;
procedure test_sgf is
    Sgf : T_SGF;
    procedure Construct_SGF_Example (Sgf : out T_SGF) is
        
    begin
        Initialize(Sgf);
        Create_Directory(Sgf,"home");
        Create_Directory(Sgf,"/home/user1");

        Create_Directory(Sgf,"/home/user1/pim");
        Create_Directory(Sgf,"/home/user1/pim/projet");
        Create_File(Sgf,"/home/user1/pim/projet/exemple.adb",10);

        Create_Directory(Sgf,"/home/user1/pim/tp");
        Create_Directory(Sgf,"/home/user1/pim/tp/tp1");
        Create_File(Sgf,"/home/user1/pim/tp/tp1/min_max_serie.adb",10);
        Create_File(Sgf,"/home/user1/pim/tp/tp1/min_max_serie.py",10);
        Create_File(Sgf,"/home/user1/pim/tp/tp1/newton.adb",10);
        Create_File(Sgf,"/home/user1/pim/tp/tp1/puissance.adb",10);
       
        Current_Directory(Sgf,"/home/user1/pim/tp/tp1");
        List_Files_Recursive(sgf,"./");
        
        
    end Construct_SGF_Example;
    
begin
    Construct_SGF_Example(Sgf);
end test_sgf;

