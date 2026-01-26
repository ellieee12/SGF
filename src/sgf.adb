with GNAT.RegExp; use GNAT.RegExp;
with GNAT.Regpat; use GNAT.Regpat;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with Text_IO; use Text_IO;

package body sgf is
    package SU renames Ada.Strings.Unbounded;
    
    procedure Free is
      new Ada.Unchecked_Deallocation (Object => T_Node, Name => T_Pointer_Node);
    
    function Get_Node_From_Path(SGF : in out T_SGF; path : in String; onlyDirectory : in Boolean) return T_Pointer_Node is
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
            if I = path'Last or else path(I) = '/' then
                declare
                    part : constant String := (if path(I) = '/' then path(Start .. I-1) else path(Start .. I));
                    has_glob : Constant Boolean := (for some C of part => C = '*' or else C = '?');
                begin
                    if part = ".." then
                        if temp_node.all.Parent /= Null then
                            temp_node := temp_node.all.Parent;
                        end if;
                        
                    elsif part /= "." then
                        temp_node := temp_node.all.Child;
                        while temp_node /= Null and then ((not temp_node.all.IsDirectory and then (I /= path'Last or else onlyDirectory or else (I = path'Last and then path(path'Last) = '/'))) -- skip all file, until on the last of the path wich may be kept if not looking for a directory
                                                          or else (temp_node.all.IsDirectory and then (I = path'Last and then not onlyDirectory)) -- skip directories at the end if not looking for a directory
                                                          or else (if has_glob then not Match(SU.To_String(temp_node.all.Name), Compile(part, True)) -- if the path contain regex, use it to found the correct node
                                                                   else temp_node.all.Name /= Part)) loop -- else, use a simple comparaison
                            temp_node := temp_node.all.Next;
                        end loop;
                        if temp_node = Null then
                            raise Dir_Not_Found with "The path contains an unknown directory!";
                        end if;
                    end if;
                    start := I + 1;
                end;
            end if;
        end loop;
        if onlyDirectory and not temp_node.all.IsDirectory then
            raise Not_A_Dir with "File is not a directory !";
        elsif not onlyDirectory and temp_node.all.IsDirectory then
            raise Not_A_File with "Directory is not a file !";
        end if;
        return temp_node;
    end Get_Node_From_Path;
    
    procedure Current_Directory(SGF : in out T_SGF; 
                                path : in String := "/") is
        temp_node : T_Pointer_Node;
    begin
        temp_node := Get_Node_From_Path(SGF, path, True);
        SGF.Current := temp_node;
    end Current_Directory;
    
    function List_Files(SGF : in out T_SGF; 
                        path : in String := ".";
                        listSize : in boolean := False) return String is
        temp_node : T_Pointer_Node;
        res : Unbounded_String;
    begin
        res := SU.To_Unbounded_String("");
        temp_node := Get_Node_From_Path(SGF, path, True);
        temp_node := temp_node.all.Child;
        while temp_node /= null loop
            res := res & temp_node.all.Name ;
            if listSize then
                res := res & ASCII.HT
                  & SU.To_Unbounded_String(Long_Long_Integer'Image(temp_node.all.Size));
                append(res,ASCII.LF);
                temp_node := temp_node.all.Next;
            end if;
        end loop;
        return SU.To_String(res);
    end List_Files;
    
    function List_Files_Recursive(SGF : in out T_SGF; 
                                  path : in String := ".";
                                  listSize : in boolean := False
                                 ) return String is
        temp_node : T_Pointer_Node;
        res : Unbounded_String;
    begin
        temp_node := Get_Node_From_Path(SGF, path, True);
        if temp_node.all.Parent=null then
            res := SU.To_Unbounded_String("/"&ASCII.LF);
        else
            res := SU.To_Unbounded_String("");
        end if;
        temp_node := temp_node.all.Child;
        while temp_node /= null loop
            if temp_node.all.IsDirectory then
                res :=  res & "\-- " & temp_node.all.Name&ASCII.LF;
            else
                res :=  res & "|-- " & temp_node.all.Name & ASCII.LF;
            end if;
            if temp_node.all.Child /= Null then
                res := res & List_Files_Recursive(SGF, temp_node, res, 1, listSize);
            end if;
            temp_node := temp_node.all.Next;
        end loop;
        return SU.To_String(res);
    end List_Files_Recursive;
    
    function List_Files_Recursive(SGF : in out T_SGF; 
                                  node : in T_Pointer_Node; 
                                  res : in Unbounded_String;
                                  level : in Natural;
                                  listSize : in boolean := False) return Unbounded_String is
        temp_node : T_Pointer_Node;
        temp_res : Unbounded_String;
        Indent : constant String := (1 .. Level * 4 => ' ');
    begin
        temp_node := node.all.Child;
        while temp_node /= null loop
            if temp_node.all.IsDirectory then
                temp_res := temp_res 
                  & SU.To_Unbounded_String (Indent & "\-- ") 
                  & temp_node.all.Name 
                  & ASCII.LF;
            else 
                if listSize then
                    temp_res := temp_res 
                      & SU.To_Unbounded_String (Indent & "|-- ") 
                      & temp_node.all.Name 
                      & ASCII.HT
                      & SU.To_Unbounded_String(Long_Long_Integer'Image(temp_node.all.Size))
                      & ASCII.LF;
                else
                    temp_res := temp_res 
                      & SU.To_Unbounded_String (Indent & "|-- ") 
                      & temp_node.all.Name
                      & ASCII.LF;
                end if;
            end if;
            if temp_node.all.Child /= Null then
                temp_res := temp_res & List_Files_Recursive(SGF, temp_node,temp_res,level + 1,listSize);
            end if;
            temp_node := temp_node.all.Next;
        end loop;
        return temp_res;
    end List_Files_Recursive;
    
    procedure Remove(SGF : in out T_SGF; path : in String) is
        temp_node : T_Pointer_Node;
    begin
        temp_node := Get_Node_From_Path(SGF, path, False);
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
        if path = "/" then
            raise Remove_Root with "Cannot remove the root !";
        end if;
        temp_node := Get_Node_From_Path(SGF, path, True);
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
    begin
        temp_node := Get_Node_From_Path(SGF, path, False);
        
        begin
            new_node := Get_Node_From_Path(SGF, new_path, True);
            if temp_node.all.Before /= Null then
                temp_node.all.Before.Next := temp_node.Next;
            else
                temp_node.all.Parent.Child := temp_node.Next;
            end if;
            temp_node.Next := Null;
            if new_node.all.Child /= Null then
                new_node := new_node.all.Child;
                while new_node.all.Next /= Null loop
                    new_node := new_node.all.Next;
                end loop;
                new_node.Next := temp_node;
            else
                new_node.Child := temp_node;
            end if;
        exception
            when Dir_Not_Found => 
                declare
                    P : constant Natural := Index (new_path, "/", Going => Ada.Strings.Backward);
                begin
                    if P = 0 then
                        temp_node.all.Name := SU.To_Unbounded_String(new_path);
                    else
                        temp_node.all.Name := SU.To_Unbounded_String(new_path(P + 1 .. new_path'Last));
                        new_node := Get_Node_From_Path(SGF, new_path(new_path'First .. P - 1), True);
                        if temp_node.all.Before /= Null then
                            temp_node.all.Before.Next := temp_node.Next;
                        else
                            temp_node.all.Parent.Child := temp_node.Next;
                        end if;
                        temp_node.Next := Null;
                        if new_node.all.Child /= Null then
                            new_node := new_node.all.Child;
                            while new_node.all.Next /= Null loop
                                new_node := new_node.all.Next;
                            end loop;
                            new_node.Next := temp_node;
                        else
                            new_node.Child := temp_node;
                        end if;
                    end if;
                end;
        end;
    end Move;
    
    procedure Copy(SGF : in out T_SGF; path : in String; new_path : in String) is
        temp_node : T_Pointer_Node;
    begin
        temp_node := Get_Node_From_Path(SGF, path, False);
        declare
            correct_new_path : constant String := (if new_path(New_Path'Last) = '/' then new_path else new_path & "/") & SU.To_String(temp_node.all.Name);
        begin
            Create_File(SGF, correct_new_path, temp_node.all.Size);
        end;
    end Copy;
    
    procedure Copy_Recursive(SGF : in out T_SGF; path : in String; new_path : in String) is
        temp_node : T_Pointer_Node;
    begin
        temp_node := Get_Node_From_Path(SGF, path, True);
        declare
            correct_new_path : constant String := (if new_path(New_Path'Last) = '/' then new_path else new_path & "/") & SU.To_String(temp_node.all.Name);
        begin
            Create_Directory(SGF, correct_new_path);
            temp_node := temp_node.all.Child;
            while temp_node /= Null loop
                if temp_node.IsDirectory then
                    Copy_Recursive(SGF, temp_node, correct_new_path);
                else
                    Create_File(SGF, correct_new_path & "/" & SU.To_String(temp_node.all.Name), temp_node.all.Size);
                end if;
                temp_node := temp_node.all.Next;
            end loop;
        end;
    end Copy_Recursive;
    
    procedure Copy_Recursive(SGF : in out T_SGF; node : in T_Pointer_Node; new_path : in String) is
        temp_node : T_Pointer_Node;
    begin
        temp_node := node;
        declare
            correct_new_path : constant String := (if new_path (New_Path'Last) = '/' then new_path else new_path & "/") & SU.To_String(temp_node.all.Name);
        begin
            Create_Directory(SGF, correct_new_path);
            temp_node := temp_node.all.Child;
            while temp_node /= Null loop
                if temp_node.IsDirectory then
                    Copy_Recursive(SGF, temp_node, correct_new_path);
                else
                    Create_File(SGF, correct_new_path & "/" & SU.To_String(temp_node.all.Name), temp_node.all.Size);
                end if;
                temp_node := temp_node.all.Next;
            end loop;
        end;
    end Copy_Recursive;

    function Is_Empty (Sgf : in out T_SGF; Path : in String) return boolean is
        temp_node : T_Pointer_Node;
    begin
        temp_node := Get_Node_From_Path(Sgf,Path, True);
        return temp_node.all.Child/=null;
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
                          Size : in Long_Long_Integer) is
        Negative_Size_Error, Empty_Name_Error: Exception;
        current_child, new_node : T_Pointer_Node;
        head,tail : T_Pointer_Node := null;
        Name,Target_Path,Path_Unbounded : Unbounded_String;
        L, K : Integer;
    begin
        if Get_Total_Size(Sgf) + size > SIZE_LIMIT then
            raise Size_Limit_Reach with "Stockage insufisant !";
        end if;
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
                Name := SU.To_Unbounded_String(SU.Slice(Path_Unbounded ,K+1,L-1));
            else
                K := Index (Source => Path_Unbounded,
                            Pattern => "/",
                            From => L,
                            Going => Ada.Strings.Backward);
                Name := SU.To_Unbounded_String(SU.Slice(Path_Unbounded ,K+1,L));
            end if;
            Target_Path := SU.To_Unbounded_String(SU.Slice(Path_Unbounded,1,K-1));
            
            if Target_Path = "" then
                head:=Sgf.Current;
            else
                Head := Get_Node_From_Path(Sgf,SU.To_String(Target_Path), True);
            end if;
        end if;
        -- get node of the target directory
        
        -- verify that the file name is does not exists in the directory
        -- if file name exists, verify that it is not a directory 
        Verify_File_Name_Existence(head.all.Child,SU.To_String(Name));
        
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
                    if not temp_node.all.IsDirectory then
                        raise File_Exists_Error;
                    end if;
                end if;
                temp_node := temp_node.all.Next;
            end loop;
        end if;
    end Verify_File_Name_Existence;
      
    procedure Create_Directory (Sgf : in  out T_SGF;
                                Path: in String) is
        current_child, new_node : T_Pointer_Node;
        head,tail : T_Pointer_Node := null;
        Name,Target_Path,Path_Unbounded : Unbounded_String;
        L, K : Integer;
    begin
        Path_Unbounded := SU.To_Unbounded_String(Path);
        -- extract directory name
        L := Path'Last;
        -- TODO : manage empty path
        K := SU.Index (Source => Path_Unbounded,
                       Pattern => "/",
                       From => L,
                       Going => Ada.Strings.Backward);
        if Path = "/" then 
            raise Forbidden_Character_Error;
        end if;
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
                Name := SU.To_Unbounded_String(SU.Slice(Path_Unbounded ,K+1,L-1));
            else
                K := Index (Source => Path_Unbounded,
                            Pattern => "/",
                            From => L,
                            Going => Ada.Strings.Backward);
                Name := SU.To_Unbounded_String(SU.Slice(Path_Unbounded ,K+1,L));
            end if;
            Target_Path := SU.To_Unbounded_String(SU.Slice(Path_Unbounded,1,K-1));
            
            if Target_Path = "" then
                head:=Sgf.Current;
            else
                Head := Get_Node_From_Path(Sgf,SU.To_String(Target_Path), True);
            end if;
        end if;
        Verify_Directory_Name_Existence(head.all.Child,SU.To_String(Name));
        Validate_Name(SU.To_String(Name));
        new_node := new T_Node'(Name,0,True,null,Head,null,null);

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


    procedure Extract_Archive_Info (Arg : in String;
                                    Target_Path : out Unbounded_String;
                                    Zip_Name : out Unbounded_String) is

        Matcher : GNAT.Regpat.Pattern_Matcher :=
          GNAT.Regpat.Compile ("^(?:(.*/))?([^/]+)$");

        Matches : GNAT.Regpat.Match_Array (0 .. 2);

        function Ends_With (S, Suffix : String) return Boolean is
        begin
            return S'Length >= Suffix'Length
              and then S (S'Last - Suffix'Length + 1 .. S'Last) = Suffix;
        end Ends_With;

    begin
        -- Reject directory
        if Arg'Length > 0 and then Arg (Arg'Last) = '/' then
            raise Constraint_Error with "Archive name cannot be a directory";
        end if;

        GNAT.Regpat.Match (Matcher, Arg, Matches);

        if Matches (0) = GNAT.Regpat.No_Match then
            raise Constraint_Error with "Invalid archive name format";
        end if;

        -- Extract path
        if Matches (1) /= GNAT.Regpat.No_Match then
            declare
                Path_With_Slash : constant String :=
                  Arg (Matches (1).First .. Matches (1).Last);
            begin
                if Path_With_Slash = "/" then
                    Target_Path := To_Unbounded_String ("/");
                else
                    -- remove trailing '/'
                    Target_Path :=
                      To_Unbounded_String
                        (Path_With_Slash (Path_With_Slash'First
                         .. Path_With_Slash'Last - 1));
                end if;
            end;
        else
            Target_Path := To_Unbounded_String (".");
        end if;
        -- Extract filename
        Zip_Name :=
          To_Unbounded_String
            (Arg (Matches (2).First .. Matches (2).Last));

        -- Normalize extension
        if not Ends_With (To_String (Zip_Name), ".tar")
          and then not Ends_With (To_String (Zip_Name), ".tar.gz")
          and then not Ends_With (To_String (Zip_Name), ".tgz")
        then
            Zip_Name := Zip_Name & ".tar";
        end if;
    end Extract_Archive_Info;

    
    procedure Archive_Directory (Sgf : in out T_SGF;
                                 Archive_Path_Name : in String;
                                 Dir_To_Be_Archived : in String) is
        temp_node : T_Pointer_Node;
        res : Long_Long_Integer;
        archive_name, new_path_name : Unbounded_String;
    begin
        -- Verify if directory exists
        temp_node := Get_Node_From_Path(Sgf,Dir_To_Be_Archived,True);
        res := 0;
        Extract_Archive_Info(Archive_Path_Name,new_path_name,archive_name);
        Validate_Name(SU.To_String(archive_name));
        temp_node := temp_node.all.Child;
        while temp_node /= null loop
            if not temp_node.all.IsDirectory then
                res := res + temp_node.all.Size;
            end if;
            if temp_node.all.Child /= Null then
                res := res + Archive_Directory_Recursive(Sgf,temp_node,res);
            end if;
            temp_node := temp_node.all.Next;
        end loop;
        Create_File(Sgf,SU.To_String(new_path_name) & "/" & SU.To_String(archive_name), res);
    end Archive_Directory;
    
    function Archive_Directory_Recursive (Sgf : in out T_SGF;
                                          node : in T_Pointer_Node;
                                          res : in Long_Long_Integer) return Long_Long_Integer is
        temp_node : T_Pointer_Node;
        temp_res : Long_Long_Integer;
    begin
        temp_res := 0;
        temp_node := node.all.Child;
        while temp_node /= null loop
            if not temp_node.all.IsDirectory then
                temp_res := temp_res + temp_node.all.Size;
            end if;
            if temp_node.all.Child/=Null then
                temp_res := temp_res + Archive_Directory_Recursive(Sgf,temp_node,temp_res);
            end if;
            temp_node := temp_node.all.Next;
        end loop;
        return temp_res;
    end Archive_Directory_Recursive;
    
    function Get_Name (Sgf : in out T_SGF; Path : in String; IsDirectory : in Boolean) return String is
        
    begin
        return SU.To_String(Get_Node_From_Path(Sgf,Path,IsDirectory).all.Name);
    end Get_Name;
   
    function Get_Size (Sgf : in out T_SGF; Path : in String; IsDirectory : in Boolean) return Long_Long_Integer is
        temp_node : T_Pointer_Node;
    begin
        temp_node := Get_Node_From_Path(Sgf,Path,IsDirectory);
        if temp_node.all.IsDirectory then 
            return 0;
        else
            return temp_node.all.Size;
        end if;
    end Get_Size;
    
    -- if we got time, this is the structure other recursive could use instead of calling themself again
    function Get_Total_Size(Sgf : in T_SGF) return Long_Long_Integer is
        temp_node : T_Pointer_Node := Sgf.Root.all.Child;
        used_size : Long_Long_Integer := 0;
    begin
        while temp_node /= null loop
            if temp_node.all.IsDirectory then
                if temp_node.all.Child /= Null then
                    temp_node := temp_node.all.Child;
                else
                    while temp_node.all.Next /= Null and then temp_node.all.Parent /= Null loop
                        temp_node := temp_node.all.Parent;
                    end loop;
                    temp_node := temp_node.all.Next;
                end if;
            else
                used_size := used_size + temp_node.all.Size;
                while temp_node.all.Next /= Null and then temp_node.all.Parent /= Null loop
                    temp_node := temp_node.all.Parent;
                end loop;
                temp_node := temp_node.all.Next;
            end if; 
        end loop;
        return used_size;
    end Get_Total_Size;
    
    procedure Change_File_Size(SGF : in out T_SGF; path : in String; size : in Long_Long_Integer) is
        temp_node : T_Pointer_Node;
    begin
        if size < 0  then
            raise Negative_Size_Error;
        end if;
        
        temp_node := Get_Node_From_Path(Sgf,Path,False);
        temp_node.all.Size := size;
    end Change_File_Size;
    
    
end sgf;
