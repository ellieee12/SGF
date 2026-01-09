package sgf is

    type T_SGF is limited private;
    
    procedure Current_Directory(SGF : in out T_SGF; path : in String);

    
private
    
    type T_Node is
        record
            Name : String;
            Size: Integer;
            IsDirectory : Boolean;
            Child : T_Pointer_Node;
            Parent : T_Pointer_Node;
            Next : T_Pointer_Node;
            Before : T_Pointer_Node;
        end record;
    type T_Pointer_Node is access T_Node;
    
    type T_SGF is
        record
            Root : T_Pointer_Node;
            Current : T_Pointer_Node;
        end record;

end sgf;
