with SGF; use SGF;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
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
                case choice is
                    when 1 => Print_Current_Working_Directory(Sgf);
                    when 2 => Add_New_File(Sgf);
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
        put_line("3 - Change working directory");
        put_line("4 - List directory content");
        put_line("5 - List recursively all directory content");
        put_line("6 - Remove a file");
        put_line("7 - Remove a directory");
        put_line("8 - Move a file or a directory");
        put_line("9 - Rename a file or a directory");
        put_line("10 - Copy a file or a directory");
        put_line("11 - Archive a directory");
        put_line("99 - Quit menu");
        put_line("Please enter the number to the corresponding command");
    end Print_Menu_Options;
    
    procedure Print_Current_Working_Directory (Sgf : in T_Sgf) is
    begin
        put_line("--------------------------------------------");
        put_line ("Current working directory : " & Get_Current_Directory(Sgf));
        put_line("--------------------------------------------");
    end Print_Current_Working_Directory;
    
    procedure Add_New_File (Sgf : in out T_Sgf) is
        path_name,file_path : Unbounded_String;
        file_size : Integer;
    begin
        put_line("--------------------------------------------");
        put("What is the destination path?");
        Skip_Line;
        get_line(path_name);
        --  Skip_Line;
        put("What is the name of the new file?");
        --  Skip_Line;
        Get_Line(file_path);
        --  Skip_Line;
        put("What is the size of the new file?");
        --  Skip_Line;
        Get(file_size);
        --  Skip_Line;
        if SU.To_String(path_name)(SU.Length(path_name)) /= '/' then 
            path_name := path_name & "/";
        end if;
        put_line(SU.To_String(path_name & file_path));
        Create_File(Sgf,SU.To_String(path_name & file_path), file_size);
    end Add_New_File;

end menu;
