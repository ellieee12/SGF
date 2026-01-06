package sgf is

    type T_SGF is limited private;
    
    procedure Move(SGF : in out T_SGF; path : in String);

    
private
    
    type T_Noeud is
        record
            Name : String;
            Size: Integer;
            isDirectory : Boolean;
            Child : T_Pointer_Noeud;
            Parent : T_Pointer_Noeud;
            next : T_Pointer_Noeud;
            before : T_Pointer_Noeud;
        end record;
    type T_Pointer_Noeud is access T_Noeud;
    
    type T_SGF is
        record
            Root : T_Pointer_Noeud;
            Current : T_Pointer_Noeud;
        end record;

end sgf;
