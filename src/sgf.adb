with GNAT.RegExp; use GNAT.RegExp;
with GNAT.RegPat; use GNAT.RegPat;
with GNAT.Spitbol; use GNAT.Spitbol;

package body sgf is

    procedure Move(SGF : in out T_SGF; path : in String) is
        regexRelatif : Regexp;
        regexAbsolute : Regexp;
    begin
        regexRelatif := Compile("^.(/[^/]+)+$");
        regexAbsolute := Compile("^(/[^/]+)+$");
        if Match(path, regex) then
            null;
        end if;
    end Move;

   function Is_Empty (Sgf : in T_SGF) return boolean is
      
   begin
      return Sgf.Current/=null;
   end Is_Empty;
   
   procedure Initialize (Sgf : out T_SGF) is 
      
   begin
      Sgf.Root := Null;
      Sgf.Current := Null;
      Sgf.Current := Sgf.Root;
      Sgf.Current.all.Child := Null;
   end Initialize;
   
   function Get_Current_Directory(Sgf : in T_SGF) return String is
      tmp_node : T_Noeud;
      path_name, current_name : String;
   begin
      tmp_node := sgf.Current;
      path_name := "";
      current_name := "";
      while tmp_node /= null loop
         current_name := tmp_sgf.all.Name;
         path_name := "/" & current_name & "/" & path_name;
         tmp_node := tmp_node.Parent;
      end loop;
      return path_name;
   end Get_Current_Directory; 
   
   procedure Create_File_Current_Directory (Sgf : in  out T_SGF;
                                            Name : in String;
                                            Size : in String) is
      Negative_Size_Error, Empty_Name_Error: Exception;
      current_child : T_Noeud;
   begin
      if Size < 0 then
         raise Negative_Size_Error;
      end if;
      Validate_Name;
      current_child := Sgf.Current.all.Child;
      Sgf.Current.all.Child := new T_Noeud(Name,Size,False,null,Sgf.Current,null,null);
      
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
      return True;
   end Validate_Name;
end sgf;
