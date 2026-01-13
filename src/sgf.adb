with GNAT.RegExp; use GNAT.RegExp;
with Ada.Text_IO;          use Ada.Text_IO;
with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body sgf is
    package SU renames Ada.Strings.Unbounded;

    procedure Free is
      new Ada.Unchecked_Deallocation (Object => T_Node, Name => T_Pointer_Node);
    
    function Glob_To_Regex (Pattern : String) return String is
        Result : Unbounded_String := SU.To_Unbounded_String("^");
    begin
        for C of Pattern loop
            case C is
            when '*' =>
                Result := Result & ".*";
            when '?' =>
                Result := Result & ".";
            when '.' | '+' | '(' | ')' | '[' | ']' | '^' | '$' | '\' =>
                Result := Result & '\' & C;
            when others =>
                Result := Result & C;
            end case;
        end loop;

        return SU.To_String(Result) & "$";
    end Glob_To_Regex;
    
    function Get_Node_From_Path(SGF : in out T_SGF; path : in String) return T_Pointer_Node is
        temp_node : T_Pointer_Node;
        start : Positive;
        regex : Regexp;
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
                        if (for some C of part => C = '*' or else C = '?') then
                            regex := Compile(Glob_To_Regex(part));
                            while temp_node /= Null and then Match(SU.To_String(temp_node.all.Name), regex) loop
                                temp_node := temp_node.all.Next;
                            end loop;
                        else
                            --  put_line("part:"&part);
                            --  put_line("node name :"&temp_node.all.Name);
                            while temp_node /= Null and then SU.To_String(temp_node.all.Name) /= part loop
                                temp_node := temp_node.all.Next;
                            end loop;
                        end if;
                        if temp_node = Null then
                            put("here");
                            raise Dir_Not_Found with "The path contains an unknown directory!";
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
        if not temp_node.all.IsDirectory then
            raise Not_A_Dir with "File is not a directory !";
        end if;
        SGF.Current := temp_node;
    end Current_Directory;
    
    procedure List_Files(SGF : in out T_SGF; path : in String := ".") is
        temp_node : T_Pointer_Node;
    begin
        temp_node := Get_Node_From_Path(SGF, path);
        if not temp_node.all.IsDirectory then
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
    begin
        temp_node := Get_Node_From_Path(SGF, path);
        if not temp_node.all.IsDirectory then
            raise Not_A_Dir with "File is not a directory !";
        end if;
        temp_node := temp_node.all.Child;
        while temp_node /= null loop
            Put_Line(temp_node.all.Name);
            if temp_node.all.Child /= Null then
                List_Files_Recursive(SGF, temp_node);
            end if;
            temp_node := temp_node.all.Next;
        end loop;
    end List_Files_Recursive;
    
    procedure List_Files_Recursive(SGF : in out T_SGF; node : in T_Pointer_Node) is
        temp_node : T_Pointer_Node;
    begin
        temp_node := node.all.Child;
        while temp_node /= null loop
            Put_Line(temp_node.all.Name);
            if temp_node.all.Child /= Null then
                List_Files_Recursive(SGF, temp_node);
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
        if not temp_node.all.IsDirectory then
            raise Not_A_Dir with "File is not a directory !";
        end if;
        if temp_node.all.Before /= Null then
            temp_node.all.Before.Next := temp_node.Next;
        else
            temp_node.all.Parent.Child := temp_node.Next;
        end if;
        if temp_node.all.Child /= Null then
            temp_node := temp_node.all.Child;
            while temp_node /= null loop
                Remove_Recursive(SGF, temp_node);
                temp_node := temp_node.all.Next;
            end loop;
        end if;
        Free(temp_node);
    end Remove_Recursive;
    
    procedure Remove_Recursive(SGF : in out T_SGF; node : in T_Pointer_Node) is
        temp_node : T_Pointer_Node;
    begin
        temp_node := node;
        if temp_node.all.Child /= Null then
            temp_node := temp_node.all.Child;
            while temp_node /= null loop
                Remove_Recursive(SGF, temp_node);
                temp_node := temp_node.all.Next;
            end loop;
        end if;
        Free(temp_node);
    end Remove_Recursive;
    
    procedure Move(SGF : in out T_SGF; path : in String; new_path : in String) is
        temp_node, new_node : T_Pointer_Node;
        P : constant Natural := Index (new_path, "/", Going => Ada.Strings.Backward);
    begin
        temp_node := Get_Node_From_Path(SGF, path);
        if temp_node.all.IsDirectory then
            raise Not_A_File with "Directory is not a file !";
        end if;
        if temp_node.all.Before /= Null then
            temp_node.all.Before.Next := temp_node.Next;
        else
            temp_node.all.Parent.Child := temp_node.Next;
        end if;
        if P = 0 then
            temp_node.all.Name := SU.To_Unbounded_String(new_path);
        elsif P /= new_path'Last then
            temp_node.all.Name := SU.To_Unbounded_String(new_path(P + 1 .. new_path'Last));
            new_node := Get_Node_From_Path(SGF, new_path(new_path'First .. P - 1));
            if not new_node.all.IsDirectory then
                raise Not_A_Dir with "File is not a directory !";
            end if;
            new_node := new_node.all.Child;
            while new_node.all.Next /= Null loop
                new_node := new_node.all.Next;
            end loop;
            new_node.Next := temp_node;
        else
            new_node := Get_Node_From_Path(SGF, new_path);
            if not new_node.all. IsDirectory then
                raise Not_A_Dir with "File is not a directory !";
            end if;
            new_node := new_node.all.Child;
            while new_node.all.Next /= Null loop
                new_node := new_node.all.Next;
            end loop;
            new_node.Next := temp_node;
        end if;
    end Move;
    
    procedure Copy(SGF : in out T_SGF; path : in String; new_path : in String) is
        temp_node, new_node : T_Pointer_Node;
    begin
        temp_node := Get_Node_From_Path(SGF, path);
        if temp_node.all. IsDirectory then
            raise Not_A_File with "Directory is not a file !";
        end if;
        new_node := Get_Node_From_Path(SGF, new_path);
        if not new_node.all. IsDirectory then
            raise Not_A_Dir with "File is not a directory !";
        end if;
        new_node := new_node.all.Child;
        while new_node.all.Next /= Null loop
            new_node := new_node.all.Next;
        end loop;
        new_node.next := new T_Node'(temp_node.all.Name, temp_node.all.Size, temp_node.all.IsDirectory, Null, Null, Null, new_node);
    end Copy;


    function Is_Empty (Sgf : in T_SGF) return boolean is
      
    begin
        return Sgf.Current/=null;
    end Is_Empty;
   
    procedure Initialize (Sgf : out T_SGF) is 
      
    begin
        Sgf.Root := new T_Node'(SU.To_Unbounded_String(""),0,True,null,null,null,null);
        Sgf.Current := Sgf.Root;
    end Initialize;
   
    function Get_Current_Directory(Sgf : in T_SGF) return String is
        tmp_node : T_Pointer_Node;
        path_name, current_name :Unbounded_String;
    begin
        tmp_node := sgf.Current;
        path_name := SU.To_Unbounded_String("");
        current_name := SU.To_Unbounded_String("/");
        while tmp_node /= null loop
            current_name := tmp_node.all.Name;
            path_name := Current_name & "/" & path_name;
            tmp_node := tmp_node.all.Parent;
        end loop;
        return SU.To_String(path_name);
    end Get_Current_Directory; 
   
    procedure Create_File(Sgf : in  out T_SGF;
                          Path : in String;
                          Size : in Integer) is
        Negative_Size_Error, Empty_Name_Error: Exception;
        current_child, new_node : T_Pointer_Node;
        head,tail : T_Pointer_Node := null;
        Name,Target_Path,Path_Unbounded : Unbounded_String;
        L, K : Integer;
    begin
        Path_Unbounded := SU.To_Unbounded_String(Path);
        if Size < 0 then
            raise Negative_Size_Error;
        end if;
        -- extract file name
        -- if path given ends with / a
        L := Path'Length;
        -- TODO : manage empty path
        K := SU.Index (Source => Path_Unbounded,
                       Pattern => "/",
                       From => L,
                       Going => Ada.Strings.Backward);
        -- if path name does not contain "/" then we create the a file in the current directory
        if K = 0 then
            head:=Sgf.Current;
            Target_Path := SU.To_Unbounded_String("");
            Name := Path_Unbounded;
        else
            if SU.Element(Path_Unbounded,L)  = '/' then
                K := Index (Source =>Path_Unbounded,
                            Pattern => "/",
                            From => L-1,
                            Going => Ada.Strings.Backward);
            else
                K := Index (Source => Path_Unbounded,
                            Pattern => "/",
                            From => L,
                            Going => Ada.Strings.Backward);
            end if;
            Target_Path := SU.To_Unbounded_String(SU.Slice(Path_Unbounded,1,K-1));
            Name := SU.To_Unbounded_String(SU.Slice(Path_Unbounded ,K+1,L));
            Head := Get_Node_From_Path(Sgf,SU.To_String(Target_Path));
        end if;
        -- get node of the target directory
        
        -- verify that the file name is does not exists in the directory
        -- if file name exists, verify that it is not a directory 
        Verify_File_Name_Existence(head,SU.To_String(Name));
        
        -- validate file name
        Validate_Name(SU.To_String(Name));
        
        -- create file
        new_node := new T_Node'(Name,Size,False,null,head,null,null);
        tail := Head.all.Child;

        if tail = null then
            Head.all.Child := new_node;
        else
            while tail.all.Next /= null loop
                tail := tail.all.Next;
            end loop;

            tail.all.Next := new_node;
            new_node.all.Before := tail;
        end if;
    end Create_File;
    
    procedure Verify_File_Name_Existence (Current_Node : in T_Pointer_Node; 
                                          Name : in String) is
        temp_node : T_Pointer_Node;
    begin
        
        if Current_Node /= null then
            temp_node := Current_Node;
            while temp_node /= null loop
                if temp_node.all.Name = Name then
                    if temp_node.all.IsDirectory then
                        raise File_Name_Is_Directory_Error;
                    else
                        raise File_Exists_Error;
                    end if;
                else
                    temp_node := temp_node.all.Next;
                end if;
            end loop;
        end if;
    end Verify_File_Name_Existence;
      
    procedure Create_Directory (Sgf : in  out T_SGF;
                                Path: in String) is
        Negative_Size_Error, Empty_Name_Error: Exception;
        current_child, new_node : T_Pointer_Node;
        head,tail : T_Pointer_Node := null;
        Name,Target_Path,Path_Unbounded : Unbounded_String;
        L, K : Integer;
    begin
        Path_Unbounded := SU.To_Unbounded_String(Path);
        -- extract directory name
        L := Path'Length;
        -- TODO : manage empty path
        K := SU.Index (Source => Path_Unbounded,
                       Pattern => "/",
                       From => L,
                       Going => Ada.Strings.Backward);
        -- if path name does not contain "/" then we create the a directory in the current directory
        if K = 0 then
            head:=Sgf.Current;
            Target_Path := SU.To_Unbounded_String("");
            Name := Path_Unbounded;
        else
            if SU.Element(Path_Unbounded,L)  = '/' then
                K := Index (Source =>Path_Unbounded,
                            Pattern => "/",
                            From => L-1,
                            Going => Ada.Strings.Backward);
            else
                K := Index (Source => Path_Unbounded,
                            Pattern => "/",
                            From => L,
                            Going => Ada.Strings.Backward);
            end if;
            Target_Path := SU.To_Unbounded_String(SU.Slice(Path_Unbounded,1,K-1));
            Name := SU.To_Unbounded_String(SU.Slice(Path_Unbounded ,K+1,L));
            Head := Get_Node_From_Path(Sgf,SU.To_String(Target_Path));
        end if;
        
        
        if Head = null then
            raise Program_Error; -- parent directory does not exist
        end if;

        new_node := new T_Node'(
                                Name,
                                0,
                                True,
                                null,
                                Head,
                                null,
                                null
                               );

        tail := Head.all.Child;

        if tail = null then
            Head.all.Child := new_node;
        else
            while tail.all.Next /= null loop
                tail := tail.all.Next;
            end loop;

            tail.all.Next := new_node;
            new_node.all.Before := tail;
        end if;

        -- verify that the directory name is does not exists in the directory
        --  Verify_Directory_Name_Existence(head,SU.To_String(Name));
        --  Validate_Name(SU.To_String(Name));
        --  new_node := new T_Node'(Name,0,True,null,head,null,null);
        --  
        --  tail := head;
        --  if tail = null then
        --      --  sgf.Current.all.Child := new_node;
        --      head := new T_Node'(Name,0,True,null,head,null,null);
        --  else
        --      while tail.all.Next /= null loop
        --          tail := tail.all.Next;
        --      end loop;
        --      tail.all.next := new_node;
        --      new_node.all.Before := Tail;
        --  end if;
        --TODO : exception handling
    end Create_Directory;
    
    procedure Verify_Directory_Name_Existence (Current_Node : in T_Pointer_Node; 
                                               Name : in String) is
        temp_node : T_Pointer_Node;
    begin
        
        if Current_Node /= null then
            temp_node := Current_Node;
            while temp_node /= null loop
                if temp_node.all.Name = Name and temp_node.all.IsDirectory then
                    raise Directory_Exists_Error;
                else
                    temp_node := temp_node.all.Next;
                end if;
            end loop;
        end if;
    end Verify_Directory_Name_Existence;
   
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
