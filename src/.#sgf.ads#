package sgf is

    type T_SGF is limited private;
    
private
    
    type T_Noeud is
        record
            Name : String;
            Size: Integer;
            isDirectory : Boolean;
            Child : T_Pointer_Children;
            Parent : T_Pointer_Noeud;
        end record;
    type T_Pointer_Noeud is access T_Noeud;
    
    type T_Children is 
        record
            next : T_Pointer_Noeud;
            before : T_Pointer_Noeud;
        end record
    type T_Pointer_Children is access T_Children;
    
    type T_SGF is
        record
            Root : T_Pointer_Noeud;
            Current : T_Pointer_Noeud;
        end record;

end sgf;
