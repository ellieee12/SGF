with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
package sgf is

    type T_SGF is private;
    type T_Pointer_Node is private;
    
    -- Initialise SGF and create root as directory
    procedure Initialize (Sgf : out T_SGF);
    
    -- Verify if the current working directory is empty
    function Is_Empty (Sgf : in out T_SGF; Path : in String) return boolean;
    
    -- Obtain the current working directory
    function Get_Current_Directory(Sgf : in T_SGF) return String;
   
    -- Create a new file based on the path name given (relative and absolute path included) 
    procedure Create_File(Sgf : in  out T_SGF;
                          Path : in String;
                          Size : in Long_Long_Integer);
   
    -- Create a new directory based on the path name given (relative and absolute path included)
    procedure Create_Directory(Sgf : in  out T_SGF;
                               Path : in String);
    
    -- Change the current working directory based on a new given (relative and absolute path included)
    procedure Current_Directory(SGF : in out T_SGF; path : in String := "/");
    
    -- List the files and directories of a directory as indicated by a given path name
    -- If no path name given, list the files and directories of the current working directory
    function List_Files(SGF : in out T_SGF; path : in String := ".";
                       listSize : in boolean := False) return String;
    
    -- List recursively all the files and directory of the current working directory 
    function List_Files_Recursive(SGF : in out T_SGF; path : in String := ".";
                                 listSize : in Boolean := False) return String;
    
    procedure Remove(SGF : in out T_SGF; path : in String);
    
    procedure Remove_Recursive(SGF : in out T_SGF; path : in String);
    
    procedure Change_File_Size(SGF : in out T_SGF; path : in String; size : in Long_Long_Integer);

    procedure Move(SGF : in out T_SGF; path : in String; new_path : in String);
    
    procedure Copy(SGF : in out T_SGF; path : in String; new_path : in String);

    procedure Archive_Directory (Sgf : in out T_SGF;
                                 Archive_Path_Name : in String;
                                 Dir_To_Be_Archived : in String) ;
    
    function Get_Size (Sgf : in out T_SGF ; Path : in String; IsDirectory : in Boolean) return Long_Long_Integer;
    function Get_Name (Sgf : in out T_SGF ;  Path : in String; IsDirectory : in Boolean) return String;
    

    
    procedure Copy_Recursive(SGF : in out T_SGF; path : in String; new_path : in String);
    
    Directory_Exists_Error : exception;
    Control_Character_Error : exception;
    Forbidden_Character_Error : exception;
    Dot_Name_Error : exception;
    Invalid_Archive_Path : exception;
    File_Exists_Error : exception;
    File_Name_Is_Directory_Error : exception;
    Negative_Size_Error: Exception;
    Empty_Name_Error : exception;
    Empty_Path : exception;
    Dir_Not_Found : exception;
    Not_A_Dir : exception;
    Not_A_File : exception;
private
    SIZE_LIMIT : Constant Long_Long_Integer := 1000000000000;
    type T_Node;
    type T_Pointer_Node is access T_Node;
    type T_Node is
       record
           Name : Unbounded_String;
           Size: Long_Long_Integer;
           IsDirectory : Boolean;
           Child : T_Pointer_Node;
           Parent : T_Pointer_Node;
           Next : T_Pointer_Node;
           Before : T_Pointer_Node;
       end record;
    
    
    type T_SGF is
       record
           Root : T_Pointer_Node;
           Current : T_Pointer_Node;
       end record;
   
   
    --  Control_Character_Error : exception;
    --  Forbidden_Character_Error : exception;
    --  Dot_Name_Error : exception;
    Remove_Root : exception;
    Size_Limit_Reach : exception;
    
    procedure Validate_Name (Name : in String);
    function Get_Node_From_Path(SGF : in out T_SGF; path : in String; onlyDirectory : in Boolean) return T_Pointer_Node;
    
    procedure Verify_File_Name_Existence (Current_Node : in T_Pointer_Node; 
                                          Name : in String);
    procedure Verify_Directory_Name_Existence (Current_Node : in T_Pointer_Node; 
                                               Name : in String);
    
    function Archive_Directory_Recursive (Sgf : in out T_SGF;
                                          node : in T_Pointer_Node;
                                          res : in Long_Long_Integer) return Long_Long_Integer ;
    function List_Files_Recursive(SGF : in out T_SGF; 
                                  node : in T_Pointer_Node; 
                                  res : in Unbounded_String; 
                                  level : in Natural;
                                  listSize : in boolean := False) return Unbounded_String;
    procedure Remove_Recursive(SGF : in out T_SGF; node : in T_Pointer_Node);
    
    procedure Copy_Recursive(SGF : in out T_SGF; node : in T_Pointer_Node; new_path : in String);
    
    procedure Extract_Archive_Info (Arg : String;
                                    Target_Path : out Unbounded_String;
                                    Zip_Name : out Unbounded_String);
    
    function Get_Total_Size(Sgf : in T_SGF) return Long_Long_Integer;
    
    
end sgf;
