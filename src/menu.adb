with SGF; use SGF;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
package body menu is
    package SU renames Ada.Strings.Unbounded;

    procedure Start_Menu (Sgf : in out T_SGF) is
        choice : Natural:=0;
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
                    when others => null;
                end case;
                        
                exit when choice = 99;
            exception
                when Data_Error => 
                    put_line("--------------------------------------------");
                    put_line("Invalid choice, please choose another option");
                    put_line("--------------------------------------------");
                    Skip_Line;
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
        put_line("10 - Move a file or a directory");
        put_line("11 - Rename a file or a directory");
        put_line("12 - Copy a file or a directory");
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
    
    procedure Validate_File_Size(file_size : out Integer) is
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
                                                file_size : in Integer;
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
        file_size : Integer;
    begin
        put_line("--------------------------------------------");
        put("What is the destination path (if empty, file will be saved in current working directory)? ");
        get_line(path_name);
        file_path := Get_User_File_or_Directory_Name(false);
        
        if SU.Length(path_name)/=0 then
            if SU.To_String(path_name)(SU.Length(path_name)) /= '/' then  
                path_name := path_name & "/";
            end if;
        else
            path_name := SU.To_Unbounded_String("./");
        end if;
        Validate_File_Size(file_size);
        Validate_Create_File_Or_Directory(Sgf,path_name,file_path,file_size,False);
        if path_name = "./" then
            put_line("File successfully created at " & Get_Current_Directory(Sgf));
        else
            put_line("File successfully created at " & path_name);
        end if;
        put_line("--------------------------------------------");
    
    end Add_New_File;
    
    procedure Change_File_Size (Sgf : in out T_SGF) is
        file_path : Unbounded_String;
        file_size : Integer;
    begin
        file_path := Get_File_Path_Name;
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
   
    function Get_File_Path_Name return Unbounded_String is
        file_path : Unbounded_String;
    begin
        put_line("--------------------------------------------");
        put("What is the path of target file? ");
        Get_Line(file_path);
        return file_path;
    end Get_File_Path_Name;
    
    
    
    procedure Print_Directory_Content(Sgf : in out T_SGF; recursive : in boolean) is
        path_name : Unbounded_String;
    begin
        put_line("--------------------------------------------");
        put("What is the path of target directory (if empty, content of current working directory will be printed)? ");
        Get_Line(path_name);
        if recursive then
            if SU.Length(path_name)=0 then
                put_line(List_Files_Recursive(Sgf));
            else
                put_line(List_Files_Recursive(Sgf,SU.To_String(path_name)));
            end if;
        else
            if SU.Length(path_name)=0 then
                put_line(List_Files(Sgf));
            else
                put_line(List_Files(Sgf,SU.To_String(path_name)));
            end if;
        end if;        
        put_line("--------------------------------------------");
    end Print_Directory_Content;
    
    procedure Change_Current_Directory (Sgf : in out T_Sgf) is
        path_name : Unbounded_String;
    begin
        
        path_name := Get_File_Path_Name;
        
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
    
    
end menu;
