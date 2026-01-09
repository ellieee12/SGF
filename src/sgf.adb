with GNAT.RegExp; use GNAT.RegExp;
with GNAT.RegPat; use GNAT.RegPat;
with GNAT.Spitbol; use GNAT.Spitbol;
with Ada.Text_IO;          use Ada.Text_IO;
with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

package body sgf is
    package SU renames Ada.Strings.Unbounded;

    procedure Current_Directory(SGF : in out T_SGF; path : in String) is
        temp_node : T_Pointer_Node;
        start : Positive;
        part: String(1..255);
    begin
        if path(path'First) = '/' then
            temp_node := SGF.Root.all.Child;
            start := path'First + 1;
        elsif path'Length >= 2 and then path(path'First .. path'First + 1) = "./" then
            temp_node := SGF.Current.all.Child;
            start := path'First + 2;
        elsif path'Length >= 3 and then path(path'First .. path'First + 2) = "../" then
            temp_node := SGF.Current;
            start := path'First + 3;
        else
            raise Incorect_Path_Format with "The path given is incorrect !";
        end if;
        if temp_node = Null then
            raise Dir_Not_Found with "The path contain an unknown directory !";
        end if;
        for I in start .. path'Last + 1 loop
            if path (I) = '/' or else I = path'Last + 1 then
                part := path(Start .. I - 1);
                if part = ".." then
                    if temp_node.all.Parent /= Null then
                        temp_node := temp_node.all.Parent;
                    end if;
                else
                    while temp_node /= Null and then Temp_Node.Name /= part loop
                        temp_node := temp_node.all.Next;
                    end loop;
                    if temp_node = Null then
                        raise Dir_Not_Found with "The path contain an unknown directory !";
                    end if;
                    if not temp_node.IsDirectory then
                        raise Not_A_Dir with "File is not a directory !";
                    end if;
                    temp_node := temp_node.all.Child;
                end if;
                Start := I + 1;
            end if;
        end loop;
        SGF.Current.all := temp_node.all;
    end Current_Directory;

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
            tmp_node := tmp_node.Parent;
        end loop;
        return SU.To_String(path_name);
    end Get_Current_Directory; 
   
    procedure Create_File_Current_Directory (Sgf : in  out T_SGF;
                                             Name : in String;
                                             Size : in Integer) is
        Negative_Size_Error, Empty_Name_Error: Exception;
        current_child : T_Pointer_Node;
      
    begin
        if Size < 0 then
            raise Negative_Size_Error;
        end if;
        Validate_Name(Name);
        current_child := Sgf.Current.all.Child;
        while current_child /= null loop
            current_child := current_child.Next;
        end loop;
        Sgf.Current.all.Child := new T_Node'(SU.To_Unbounded_String(Name),Size,False,null,Sgf.Current,null,null);
        current_child := Sgf.Current.all.Child;
        --TODO : exception handling
    exception
        when Negative_Size_Error =>
            null;
        when Control_Character_Error =>
            null;
        when Empty_Name_Error =>
            null;
        when Dot_Name_Error =>
            null;
        when Forbidden_Character_Error =>
            null;
        when others =>
            null;
    end Create_File_Current_Directory;
   
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
