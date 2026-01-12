with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
package sgf is

    type T_SGF is private;
    type T_Pointer_Node is private;
    
    function Is_Empty (Sgf : in T_SGF) return boolean;
    
    procedure Initialize (Sgf : out T_SGF);
   
    function Get_Current_Directory(Sgf : in T_SGF) return String;
   
    procedure Create_File(Sgf : in  out T_SGF;
                          Path : in String;
                          Size : in Integer);
   
    procedure Create_Directory(Sgf : in  out T_SGF;
                                             Path : in String);
    

    procedure Current_Directory(SGF : in out T_SGF; path : in String);
    
    procedure List_Files(SGF : in out T_SGF; path : in String := ".");
    
    procedure List_Files_Recursive(SGF : in out T_SGF; path : in String := ".");
    procedure List_Files_Recursive(SGF : in out T_SGF; node : in T_Pointer_Node);
    
    procedure Remove(SGF : in out T_SGF; path : in String);
    
    procedure Remove_Recursive(SGF : in out T_SGF; path : in String);
    procedure Remove_Recursive(SGF : in out T_SGF; node : in T_Pointer_Node);
    
    procedure Move(SGF : in out T_SGF; path : in String; new_path : in String);
    
    procedure Copy(SGF : in out T_SGF; path : in String; new_path : in String);

private
    type T_Node;
    type T_Pointer_Node is access T_Node;
    type T_Node is
        record
            Name : Unbounded_String;
            Size: Integer;
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
   
   
    Control_Character_Error : exception;
    Forbidden_Character_Error : exception;
    Dot_Name_Error : exception;
    Empty_Name_Error : exception;
    Empty_Path : exception;
    Dir_Not_Found : exception;
    Not_A_Dir : exception;
    Not_A_File : exception;
    File_Exists_Error : exception;
    File_Name_Is_Directory_Error : exception;
    Directory_Exists_Error : exception;
    
    procedure Validate_Name (Name : in String);
    function Glob_To_Regex (Pattern : String) return String;
    function Get_Node_From_Path(SGF : in out T_SGF; path : in String) return T_Pointer_Node;
    
    procedure Verify_File_Name_Existence (Current_Node : in T_Pointer_Node; 
                                          Name : in String);
    procedure Verify_Directory_Name_Existence (Current_Node : in T_Pointer_Node; 
                                          Name : in String);
  
end sgf;
