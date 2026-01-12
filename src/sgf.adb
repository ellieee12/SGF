with GNAT.RegExp; use GNAT.RegExp;
with GNAT.RegPat; use GNAT.RegPat;
with GNAT.Spitbol; use GNAT.Spitbol;
with Ada.Text_IO;          use Ada.Text_IO;
with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;

package body sgf is
    package SU renames Ada.Strings.Unbounded;

    procedure Free is
            new Ada.Unchecked_Deallocation (Object => T_Node, Name => T_Pointer_Node);
    
    function Get_Node_From_Path(SGF : in out T_SGF; path : in String) return T_Pointer_Node is
        temp_node : T_Pointer_Node;
        start : Positive;
    begin
        if path = "" then
            raise Empty_Path with "The path provided is empty !";
        end if;
        if path(path'First) = '/' then
            temp_node := SGF.Root;
            start := path'First + 1;
        else
            temp_node := SGF.Current;
            start := path'First;
        end if;
        for I in start .. path'Last loop
            if (I = path'Last and path(I) /= '/') or else (I /= path'Last and then path(I+1) = '/') then
                declare
                    part : constant String := path(Start .. I);
                begin
                    if part = ".." then
                        if temp_node.all.Parent /= Null then
                            temp_node := temp_node.all.Parent;
                        end if;
                    elsif part /= "." then
                        temp_node := temp_node.all.Child;
                        while temp_node /= Null and then temp_node.all.Name /= part loop
                            temp_node := temp_node.all.Next;
                        end loop;
                        if temp_node = Null then
                            raise Dir_Not_Found with "The path contain an unknown directory !";
                        end if;
                    end if;
                    start := I + 2;
                end;
            end if;
        end loop;
        return temp_node;
    end Get_Node_From_Path;
    
    procedure Current_Directory(SGF : in out T_SGF; path : in String) is
        temp_node : T_Pointer_Node;
    begin
        temp_node := Get_Node_From_Path(SGF, path);
        if not temp_node.IsDirectory then
            raise Not_A_Dir with "File is not a directory !";
        end if;
        SGF.Current := temp_node;
    end Current_Directory;
    
    procedure List_Files(SGF : in out T_SGF; path : in String := ".") is
        temp_node : T_Pointer_Node;
    begin
        temp_node := Get_Node_From_Path(SGF, path);
        if not temp_node.IsDirectory then
            raise Not_A_Dir with "File is not a directory !";
        end if;
        temp_node := temp_node.all.Child;
        while temp_node /= null loop
            Put_Line(temp_node.all.Name);
            temp_node := temp_node.all.Next;
        end loop;
    end List_Files;
    
    procedure List_Files_Recursive(SGF : in out T_SGF; path : in String := ".") is
        temp_node : T_Pointer_Node;
        new_path : Unbounded_String;
    begin
        temp_node := Get_Node_From_Path(SGF, path);
        if not temp_node.IsDirectory then
            raise Not_A_Dir with "File is not a directory !";
        end if;
        temp_node := temp_node.all.Child;
        while temp_node /= null loop
            Put_Line(temp_node.all.Name);
            if temp_node.all.Child /= Null then
                new_path := SU.To_Unbounded_String(path & "/") & temp_node.all.Name;
                List_Files_Recursive(SGF, SU.To_String(new_path));
            end if;
            temp_node := temp_node.all.Next;
        end loop;
    end List_Files_Recursive;
    
    procedure Remove(SGF : in out T_SGF; path : in String) is
        temp_node : T_Pointer_Node;
    begin
        temp_node := Get_Node_From_Path(SGF, path);
        if temp_node.all.Before /= Null then
            temp_node.all.Before.Next := temp_node.Next;
        else
            temp_node.all.Parent.Child := temp_node.Next;
        end if;
        Free(temp_node);
    end Remove;
    
    procedure Remove_Recursive(SGF : in out T_SGF; path : in String) is
        temp_node : T_Pointer_Node;
    begin
        temp_node := Get_Node_From_Path(SGF, path);
        if temp_node.all.Before /= Null then
            temp_node.all.Before.Next := temp_node.Next;
        else
            temp_node.all.Parent.Child := temp_node.Next;
        end if;
        Free(temp_node);
    end Remove;


    function Is_Empty (Sgf : in T_SGF) return boolean is
      
    begin
        return Sgf.Current/=null;
    end Is_Empty;
   
    procedure Initialize (Sgf : out T_SGF) is 
      
    begin
        Sgf.Root := new T_Node'(SU.To_Unbounded_String("Root"),0,True,null,null,null,null);
        Sgf.Current := Sgf.Root;
    end Initialize;
   
    function Get_Current_Directory(Sgf : in T_SGF) return String is
        tmp_node : T_Pointer_Node;
        path_name, current_name :Unbounded_String;
    begin
        tmp_node := sgf.Current;
        path_name := SU.To_Unbounded_String("");
        current_name := SU.To_Unbounded_String("");
        while tmp_node /= null loop
            current_name := tmp_node.all.Name;
            path_name := "/" & current_name & "/" & path_name;
            tmp_node := tmp_node.all.Parent;
        end loop;
        return SU.To_String(path_name);
    end Get_Current_Directory; 
   
    procedure Create_File_Current_Directory (Sgf : in  out T_SGF;
                                             Name : in String;
                                             Size : in Integer) is
        Negative_Size_Error, Empty_Name_Error: Exception;
        current_child, new_node : T_Pointer_Node;
        head,tail : T_Pointer_Node := null;
    begin
        if Size < 0 then
            raise Negative_Size_Error;
        end if;
        Validate_Name(Name);
        new_node := new T_Node'(SU.To_Unbounded_String(Name),Size,False,null,Sgf.Current,null,null);
        head:=Sgf.Current.all.Child;
        tail := head;
        if tail = null then 
            sgf.Current.all.Child := new_node;
        else
            while tail.all.Next /= null loop
                tail := tail.all.Next;
            end loop;
            tail.all.next := new_node;
            new_node.all.Before := Tail;
        end if;
        --TODO : exception handling
    end Create_File_Current_Directory;
    
    procedure Create_Directory_Current_Directory (Sgf : in  out T_SGF;
                                             Name : in String) is
        Negative_Size_Error, Empty_Name_Error: Exception;
        current_child, new_node : T_Pointer_Node;
        head,tail : T_Pointer_Node := null;
    begin
        Validate_Name(Name);
        new_node := new T_Node'(SU.To_Unbounded_String(Name),0,True,null,Sgf.Current,null,null);
        head:=Sgf.Current.all.Child;
        tail := head;
        if tail = null then 
            sgf.Current.all.Child := new_node;
        else
            while tail.all.Next /= null loop
                tail := tail.all.Next;
            end loop;
            tail.all.next := new_node;
            new_node.all.Before := Tail;
        end if;
        --TODO : exception handling
    end Create_Directory_Current_Directory ;
   
    procedure Validate_Name (Name : in String) is
      
    begin
        -- Rule 1 : cannot be "." or ".."
        if Name = "." or else Name = ".." then
            raise Dot_Name_Error;
        end if;
        -- Rule 2 : cannot be empty
        if Name'Length = 0 then
            raise Empty_Name_Error;
        end if;
        for c of Name loop
            -- Rule 3 : cannot contain control characters
            if Character'Pos(c)<32 or else Character'Pos(c)=127 then
                raise Control_Character_Error;
            end if;
            -- Rule 4 : Cannot contain symboles / \ : * ? < > |
            case c is 
            when '\' | '/' | ':' | '*' | '?' | '"' | '<' | '>' | '|' =>
                raise Forbidden_Character_Error;
            when others =>
                null;
            end case;
        end loop;
    end Validate_Name;
end sgf;
