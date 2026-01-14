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
        
        Create_Directory(Sgf,"/usr");
        Create_Directory(Sgf,"/usr/local");
        Create_Directory(Sgf,"/usr/local/share");
        put(List_Files_Recursive(Sgf,"./"));
        
        Move(Sgf, "/home/user1/pim/tp/tp1/min_max_serie.adb", "/usr/local/share");
        Copy(Sgf, "/home/user1/pim/tp/tp1/min_max_serie.py", "/usr/local/share");
        put(List_Files_Recursive(Sgf,"./"));
        
        
    end Construct_SGF_Example;
    
    procedure Get_Current_Working_Directory_Test (Sgf : out T_SGF) is
        
    begin
        Construct_SGF_Example(Sgf);
        pragma Assert (Get_Current_Directory(Sgf)="/");
        Current_Directory(Sgf,"/home");
        pragma Assert (Get_Current_Directory(Sgf)="/home/");
        Current_Directory(Sgf,"./user1");
        pragma Assert (Get_Current_Directory(Sgf)="/home/user1/");
        Current_Directory(Sgf,"./pim");
        pragma Assert (Get_Current_Directory(Sgf)="/home/user1/pim/");
        Current_Directory(Sgf,"./projet");
        pragma Assert (Get_Current_Directory(Sgf)="/home/user1/pim/projet/");
    end Get_Current_Working_Directory_Test;
    
    procedure Create_Directory_Test (Sgf : out T_SGF) is
        
    begin
        Initialize(Sgf);
        -- create directory in current directory 
        Create_Directory(Sgf,"test1");
        Create_Directory(Sgf,"./test2");
        Create_Directory(Sgf,"/test3");
        pragma Assert (List_Files(Sgf) = 
                         "test1" & ASCII.LF
                       & "test2" & ASCII.LF
                       & "test3" & ASCII.LF);
        
        -- create directory using relative path (./)
        Create_Directory(Sgf,"relative-path");
        Create_Directory(Sgf,"./relative-path/one-dot");
        Create_Directory(Sgf,"./relative-path/one-dot/1-test1");
        Create_Directory(Sgf,"./relative-path/one-dot/1-test2/");
        pragma Assert(List_Files(Sgf,"/relative-path/one-dot/") =
                        "1-test1" & ASCII.LF
                      & "1-test2" & ASCII.LF);
        
        -- create directory using relative path (../)
        Create_Directory(Sgf,"./relative-path/two-dot");
        Current_Directory(Sgf,"/relative-path/two-dot");
        Create_Directory(Sgf,"2-test1");
        Current_Directory(Sgf,"/relative-path/two-dot/2-test1");
        Create_Directory(Sgf,"../2-test2");
        Create_Directory(Sgf,"../2-test3/");
        pragma Assert(List_Files(Sgf,"/relative-path/two-dot/") =
                        "2-test1" & ASCII.LF
                      & "2-test2" & ASCII.LF
                      & "2-test3" & ASCII.LF);
        
        Create_Directory(Sgf,"../2-test2/2-test4");
        pragma Assert(List_Files(Sgf,"/relative-path/two-dot/2-test2/") =
                        "2-test4" & ASCII.LF);
        
        Create_Directory(Sgf,"../../2-test5");
        pragma Assert(List_Files(Sgf,"/relative-path/") =
                        "one-dot" & ASCII.LF
                      & "two-dot" & ASCII.LF
                      & "2-test5" & ASCII.LF);
        
        --TODO : abs path
        
        Current_Directory(Sgf,"/");
        put(List_Files_Recursive(Sgf,"./"));

        
    end Create_Directory_Test;
    
    procedure Create_Directory_Exception_Test(Sgf : out T_SGF) is
        Directory_Name_Conflict : Boolean := false;
        Name_Is_A_Dot : Boolean := false;
        Name_Is_Two_Dot : Boolean := false;
    begin
        -- Create a new directory with a name that already exists in the target 
        begin
            Create_Directory(Sgf,"relative-path");
        exception
            when Directory_Exists_Error => Directory_Name_Conflict := True;
        end;
        -- Create directory with a "." as name
        begin
            Create_Directory(Sgf,".");
        exception
            when Dot_Name_Error => Name_Is_A_Dot := True;
        end;
        -- Create directory with a ".." as name  
        begin
            Create_Directory(Sgf,"..");
        exception
            when Dot_Name_Error => Name_Is_Two_Dot := True;
        end;
            
        pragma Assert(Directory_Name_Conflict);
        pragma Assert(Name_Is_A_Dot);
        pragma Assert(Name_Is_Two_Dot);
        
    end Create_Directory_Exception_Test;
    
    procedure Archive_Directory_Test (Sgf :out T_SGF) is
    
    begin 
        Construct_SGF_Example (Sgf);
        -- archive target save path name given
        Archive_Directory(Sgf, "/home/user1/pim/projet/home.tar","/home");
        pragma Assert(Get_Name(Sgf,"/home/user1/pim/projet/home.tar")="home.tar");
        pragma Assert(Get_Size(Sgf,"/home/user1/pim/projet/home.tar")=50);
        Remove(Sgf,"/home/user1/pim/projet/home.tar");
        -- archive target save path name not given
        Archive_Directory(Sgf, "home.tar","/home");
        pragma Assert(Get_Name(Sgf,"home.tar")="home.tar");
        pragma Assert(Get_Size(Sgf,"home.tar")=50);
    end Archive_Directory_Test;

    
begin
    -- Get_Current_Working_Directory_Test(Sgf);
    Archive_Directory_Test(Sgf);
    --  Create_Directory_Test(Sgf);
    --  Create_Directory_Exception_Test(Sgf);
    --  Extract_Archive_Directory_Test(Sgf);
end test_sgf;

