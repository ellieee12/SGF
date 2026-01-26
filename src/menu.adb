with SGF; use SGF;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Long_Long_Integer_Text_IO; use Ada.Long_Long_Integer_Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
package body menu is
    package SU renames Ada.Strings.Unbounded;

    procedure Start_Menu (Sgf : in out T_SGF) is
        choice : Natural:=0;
        Invalid_Choice : exception;
    begin
        
        loop 
            Print_Menu_Options;
            begin
                put("Command number : ");
                get(choice);
                Skip_Line;
                case choice is
                    when 1 => Print_Current_Working_Directory(Sgf);
                    when 2 => Add_New_File(Sgf);
                    when 3 => Add_New_Directory(Sgf);
                    when 4 => Change_File_Size(Sgf);
                    when 5 => Change_Current_Directory(Sgf);
                    when 6 => Print_Directory_Content(Sgf,False);
                    when 7 => Print_Directory_Content(Sgf,True);
                    when 8 => Remove_File_Or_Directory(Sgf,False);
                    when 9 => Remove_File_Or_Directory(Sgf,True);
                    when 10 => Move_Or_Rename(Sgf);
                    when 11 => Copy_File(Sgf);
                    when 12 => Copy_Directory(Sgf);
                    when 13 => Archive_Directory(Sgf);
                    when 99 => null;
                    when others => raise Invalid_Choice;
                end case;
                        
                exit when choice = 99;
            exception
                when Data_Error => 
                    put_line("--------------------------------------------");
                    put_line("Invalid choice, please choose another option");
                    put_line("--------------------------------------------");
                    Skip_Line;
                when Invalid_Choice => 
                    put_line("--------------------------------------------");
                    put_line("Invalid choice, please choose another option");
                    put_line("--------------------------------------------");
            end;
        end loop;
    
    end Start_Menu;
   
    procedure Print_Menu_Options is 
    begin
        put_line("Available commands :");
        put_line("1 - Current working directory");
        put_line("2 - Add new file");
        put_line("3 - Add new directory");
        put_line("4 - Change file size");
        put_line("5 - Change working directory");
        put_line("6 - List directory content");
        put_line("7 - List recursively all directory content");
        put_line("8 - Remove a file");
        put_line("9 - Remove a directory");
        put_line("10 - Move or rename a file or a directory");
        put_line("11 - Copy a file");
        put_line("12 - Copy a directory");
        put_line("13 - Archive a directory");
        put_line("99 - Quit menu");
        put_line("Please enter the number to the corresponding command");
    end Print_Menu_Options;
    
    procedure Print_Current_Working_Directory (Sgf : in T_Sgf) is
    begin
        put_line("--------------------------------------------");
        put_line ("Current working directory : " & Get_Current_Directory(Sgf));
        put_line("--------------------------------------------");
    end Print_Current_Working_Directory;
    
    procedure Validate_File_Size(file_size : out Long_Long_Integer) is
    begin
        loop
            begin
                put("What is the size of the file?");
                Get(file_size);
                Skip_Line;
                if file_size<=0 then
                    put_line("--------------------------------------------");
                    put_line(" !!! Invalid file size !!!");
                    put_line("--------------------------------------------");
                end if;
                exit when file_size > 0;
            exception
                when Data_Error=>
                    put_line("--------------------------------------------");
                    put_line(" !!! Invalid file size !!!");
                    put_line("--------------------------------------------");
            end;
        end loop;
    end Validate_File_Size;
    
    function Get_User_File_or_Directory_Name(isDirectory : in Boolean) return Unbounded_String is
        name : Unbounded_String;
    begin
        if isDirectory then
            put("What is the name or path name of the new directory?");
        else
            put("What is the name of the new file?");
        end if;
        
        Get_Line(name);
        return name;
    end Get_User_File_or_Directory_Name;
    
    procedure Validate_Create_File_Or_Directory(Sgf : in out T_Sgf ;
                                                path_name: in out Unbounded_String;
                                                file_path : in out Unbounded_String;
                                                file_size : in Long_Long_Integer;
                                                isDirectory : in Boolean) is
        replacement : Unbounded_String;
    begin
        if isDirectory then
            replacement := SU.To_Unbounded_String("Directory ");
            Create_Directory(Sgf,SU.To_String(path_name));
        else
            replacement := SU.To_Unbounded_String("File ");
            Create_File(Sgf,SU.To_String(path_name & file_path), file_size);
        end if;
    exception
        when File_Exists_Error | Directory_Exists_Error=>
            put_line("--------------------------------------------");
            put_line(" !!! A "&replacement&" with the same name already exists in target directory !!!");
            put_line("--------------------------------------------");
            file_path := Get_User_File_or_Directory_Name(isDirectory);
            Validate_Create_File_Or_Directory(Sgf,path_name,file_path,file_size,isDirectory);
        when Dot_Name_Error =>
            put_line("--------------------------------------------");
            put_line(" !!! "&replacement&"name cannot be . or .. !!!");
            put_line("--------------------------------------------");
            file_path := Get_User_File_or_Directory_Name(isDirectory);
            Validate_Create_File_Or_Directory(Sgf,path_name,file_path,file_size,isDirectory);
        when Control_Character_Error =>
            put_line("--------------------------------------------");
            put_line(" !!! "&replacement&"name cannot contain control characters !!!");
            put_line("--------------------------------------------");
            file_path := Get_User_File_or_Directory_Name(isDirectory);
            Validate_Create_File_Or_Directory(Sgf,path_name,file_path,file_size,isDirectory);
        when Forbidden_Character_Error  =>
            put_line("--------------------------------------------");
            put_line(" !!! "&replacement&"name cannot contain symbols such as \ / : * ? "" < > or | !!!");
            put_line("--------------------------------------------");
            file_path := Get_User_File_or_Directory_Name(isDirectory);
            Validate_Create_File_Or_Directory(Sgf,path_name,file_path,file_size,isDirectory);
        when Empty_Path | Empty_Name_Error =>
            put_line("--------------------------------------------");
            put_line(" !!! "&replacement&"name cannot be empty !!!");
            put_line("--------------------------------------------");
            file_path := Get_User_File_or_Directory_Name(isDirectory);
            Validate_Create_File_Or_Directory(Sgf,path_name,file_path,file_size,isDirectory);
        when E : Dir_Not_Found | Not_A_File =>
            put_line("--------------------------------------------");
            put_line(" !!! "& Exception_Message(E) &" !!!");
            path_name := Get_Path_Name("What is the destination path (if empty, file will be saved in current working directory)? ");
            if SU.Length(path_name)/=0 then
                if SU.To_String(path_name)(SU.Length(path_name)) /= '/' then  
                    path_name := path_name & "/";
                end if;
                if not (SU.To_String(path_name)(1) = '.' and SU.To_String(path_name)(2) = '/') or else SU.To_String(path_name)(1) /= '/' then  
                    path_name := "./"&path_name;
                end if;
            else
                path_name := SU.To_Unbounded_String("./");
            end if;
            Validate_Create_File_Or_Directory(Sgf,path_name,file_path,file_size,isDirectory);
    end Validate_Create_File_Or_Directory;
    
        
    procedure Add_New_Directory (Sgf : in out T_Sgf) is
        path_name : Unbounded_String;
    begin
        put_line("--------------------------------------------");
        path_name := Get_User_File_or_Directory_Name(True);
        Validate_Create_File_Or_Directory(Sgf,path_name,path_name,0,True);
        put_line("Directory successfully created.");
        put_line("--------------------------------------------");
    end Add_New_Directory;
    
    procedure Add_New_File (Sgf : in out T_Sgf) is
        path_name,file_path : Unbounded_String;
        file_size : Long_Long_Integer;
    begin
        put_line("--------------------------------------------");
        put("What is the destination path (if empty, file will be saved in current working directory)? ");
        get_line(path_name);
        file_path := Get_User_File_or_Directory_Name(false);
        
        if SU.Length(path_name)/=0 then
            if SU.To_String(path_name)(SU.Length(path_name)) /= '/' then  
                path_name := path_name & "/";
            end if;
            if SU.To_String(path_name)(1) /= '/' then  
                path_name := "./"&path_name;
            end if;
        else
            path_name := SU.To_Unbounded_String("./");
        end if;
        Validate_File_Size(file_size);
        Validate_Create_File_Or_Directory(Sgf,path_name,file_path,file_size,False);
        put_line("File successfully created.");
        put_line("--------------------------------------------");
    
    end Add_New_File;
    
    procedure Change_File_Size (Sgf : in out T_SGF) is
        file_path : Unbounded_String;
        file_size : Long_Long_Integer;
    begin
        file_path := Get_Path_Name("What is the path of target file? ");
        Validate_File_Size(file_size);
        
        Change_File_Size(Sgf,SU.To_String(file_path),file_size);
        put_line("File size changed successfully");
        put_line("--------------------------------------------");
    exception
        when E : Empty_Path | Dir_Not_Found | Not_A_File =>
            put_line("--------------------------------------------");
            put_line(" !!! "& Exception_Message(E) &" !!!");
            Change_File_Size(Sgf);
    end Change_File_Size;
   
    function Get_Path_Name (msg : in String) return Unbounded_String is
        file_path : Unbounded_String;
    begin
        put(msg);
        Get_Line(file_path);
        return file_path;
    end Get_Path_Name;
    
    
    
    procedure Print_Directory_Content(Sgf : in out T_SGF; recursive : in boolean) is
        path_name : Unbounded_String;
        print_details : Unbounded_String;
        Invalid_Response : exception;
    begin
        put_line("--------------------------------------------");
        put("What is the path of target directory (if empty, content of current working directory will be printed)? ");
        Get_Line(path_name);
        put("Would you like to print the details of the content? y - yes, n - no");
        Get_Line(print_details);
        
   
        if SU.To_String(print_details) /= "Y" 
          and SU.To_String(print_details) /= "N"
          and SU.To_String(print_details) /= "y"
          and SU.To_String(print_details) /= "n"then
            raise Invalid_Response with "Invalid response to whether you would like to print content details.";
        end if;
       
        if recursive then
            if SU.Length(path_name)=0 and (SU.To_String(print_details) = "Y" or SU.To_String(print_details) = "y") then
                put_line(List_Files_Recursive(Sgf,".",True));
            elsif SU.Length(path_name)/=0 and (SU.To_String(print_details) = "Y" or SU.To_String(print_details) = "y") then
                put_line(List_Files_Recursive(Sgf,SU.To_String(path_name),True));
            elsif SU.Length(path_name)=0 and (SU.To_String(print_details) = "N" or SU.To_String(print_details) = "n") then
                put_line(List_Files_Recursive(Sgf,".",False));
            else
                put_line(List_Files_Recursive(Sgf,SU.To_String(path_name),False)); 
            end if;
        else
            if SU.Length(path_name)=0 and (SU.To_String(print_details) = "Y" or SU.To_String(print_details) = "y") then
                put_line(List_Files(Sgf,listSize=>True));
            elsif SU.Length(path_name)/=0 and (SU.To_String(print_details) = "Y" or SU.To_String(print_details) = "y") then
                put_line(List_Files(Sgf,SU.To_String(path_name),True));
            elsif SU.Length(path_name)=0 and (SU.To_String(print_details) = "N" or SU.To_String(print_details) = "n") then
                put_line(List_Files(Sgf,listSize=>False));
            else
                put_line(List_Files(Sgf,SU.To_String(path_name),False)); 
            end if;
        end if;        
        put_line("--------------------------------------------");
    exception
        when E : others =>
            put_line("--------------------------------------------");
            put_line(" !!! "& Exception_Message(E) &" !!!");
        Print_Directory_Content(Sgf,recursive);
    end Print_Directory_Content;
    
    procedure Change_Current_Directory (Sgf : in out T_Sgf) is
        path_name : Unbounded_String;
    begin
        put_line("--------------------------------------------");
        path_name := Get_Path_Name("What is the path of directory? ");
        
        if SU.Length(path_name)=0 then
            Current_Directory(Sgf);
        else
            Current_Directory(Sgf,SU.To_String(path_name));
        end if;
        put_line("Current directory successfully changed to " & Get_Current_Directory(Sgf));
        put_line("--------------------------------------------");
    exception
        when E : Empty_Path | Dir_Not_Found | Not_A_Dir =>
            put_line("--------------------------------------------");
            put_line(" !!! "& Exception_Message(E) &" !!!");
            Change_Current_Directory(Sgf);
    end Change_Current_Directory;
    
    procedure Remove_File_Or_Directory(Sgf : in out T_SGF;isDirectory : in Boolean) is
        path_name: Unbounded_String;
        msg : Unbounded_String;
    begin
        if isDirectory then
            put_line("--------------------------------------------");
            path_name := Get_Path_Name("What is the path of target directory? ");
            msg := SU.To_Unbounded_String("Directory successfully deleted.");
            Remove_Recursive(Sgf,SU.To_String(path_name));
        else
            put_line("--------------------------------------------");
            path_name := Get_Path_Name("What is the path of target file? ");
            msg := SU.To_Unbounded_String("File successfully deleted");
            Remove(Sgf,SU.To_String(path_name));
        end if;
        put_line(msg);
        put_line("--------------------------------------------");
    exception
        when E : Empty_Path | Dir_Not_Found | Not_A_File =>
            put_line("--------------------------------------------");
            put_line(" !!! "& Exception_Message(E) &" !!!");
            Remove_File_Or_Directory(Sgf,isDirectory);
    end Remove_File_Or_Directory;
    
    procedure Move_Or_Rename (Sgf : in out T_Sgf) is
        path_name, new_path_name : Unbounded_String;
    begin
        put_line("--------------------------------------------");
        path_name := Get_Path_Name("What is the initial path to file or directory? ");
        new_path_name := Get_Path_Name("What is the new path to file or directory?");
        Move(Sgf,SU.To_String(path_name),SU.To_String(new_path_name));
        put_line("Operation executed successfully");
        put_line("--------------------------------------------");
    exception
        when E : Empty_Path | Dir_Not_Found | Not_A_Dir | Not_A_File =>
            put_line("--------------------------------------------");
            put_line(" !!! "& Exception_Message(E) &" !!!");
            Move_Or_Rename(Sgf);
    end Move_Or_Rename;
    
    procedure Copy_File (Sgf : in out T_SGF) is
        src_path_name, dest_path_name : Unbounded_String;
    begin
        put_line("--------------------------------------------");
        src_path_name := Get_Path_Name("What is the source path to file to be copied? ");
        dest_path_name := Get_Path_Name("What is the destination path ? ");
        Copy(Sgf,SU.To_String(src_path_name),SU.To_String(dest_path_name));
        put_line("Operation executed successfully");
        put_line("--------------------------------------------");
    exception
        when E : Empty_Path | Dir_Not_Found | Not_A_Dir | Not_A_File =>
            put_line("--------------------------------------------");
            put_line(" !!! "& Exception_Message(E) &" !!!");
            Copy_File(Sgf);
    end Copy_File;
    
    procedure Copy_Directory (Sgf : in out T_SGF) is
        src_path_name, dest_path_name : Unbounded_String;
    begin
        put_line("--------------------------------------------");
        src_path_name := Get_Path_Name("What is the source path to directory to be copied? ");
        dest_path_name := Get_Path_Name("What is the destination path ? ");
        Copy_Recursive(Sgf,SU.To_String(src_path_name),SU.To_String(dest_path_name));
        put_line("Operation executed successfully");
        put_line("--------------------------------------------");
    exception
        when E : Empty_Path | Dir_Not_Found | Not_A_Dir =>
            put_line("--------------------------------------------");
            put_line(" !!! "& Exception_Message(E) &" !!!");
            Copy_Directory(Sgf);
    end Copy_Directory; 
    
    procedure Archive_Directory (Sgf : in out T_SGF) is
        path_name, dest_path_name : Unbounded_String;
    begin
        put_line("--------------------------------------------");
        path_name := Get_Path_Name("What is the path to the directory to be archived? ");
        dest_path_name := Get_Path_Name("What is the path and name of archived directory? (If no path given, directory is archived in currend working directory)");
        Archive_Directory(Sgf,SU.To_String(dest_path_name),SU.To_String(path_name));
    exception
        when E: others =>
            put_line("--------------------------------------------");
            put_line(" !!! "& Exception_Message(E) &" !!!");
            Archive_Directory(Sgf);
    end Archive_Directory;
end menu;
