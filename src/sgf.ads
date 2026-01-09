package sgf is

    type T_SGF is private;
    function Is_Empty (Sgf : in T_SGF) return boolean;
    procedure Move(SGF : in out T_SGF; path : in String);
    procedure Initialize (Sgf : out T_SGF);
   
    function Get_Current_Directory(Sgf : in T_SGF) return String;
   
    procedure Create_File_Current_Directory (Sgf : in  out T_SGF;
                                             Name : in String;
                                             Size : in Integer);
   
    procedure Create_Directory (Sgf : in out T_SGF; Name : in String);

    procedure Current_Directory(SGF : in out T_SGF; path : in String);

private
    type T_Node;
    type T_Pointer_Node is access T_Node;
    type T_Node is
        record
            Name : String (1 .. 255);
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
    Incorect_Path_Format : exception;
    Dir_Not_Found : exception;
    Not_A_Dir : exception;
   
    procedure Validate_Name (Name : in String);
  
end sgf;
