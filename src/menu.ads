with Ada.Text_IO;          use Ada.Text_IO;
with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;
with SGF; use SGF;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package menu is

    procedure Start_Menu (Sgf : in out T_SGF);
    
private
    procedure Print_Menu_Options;
    procedure Print_Current_Working_Directory (Sgf : in T_Sgf);
    procedure Add_New_File (Sgf : in out T_Sgf);
    procedure Print_Directory_Content(Sgf : in out T_SGF; recursive : in boolean) ;
    procedure Validate_Create_File_Or_Directory(Sgf : in out T_Sgf ;
                                                path_name: in out Unbounded_String;
                                                file_path : in out Unbounded_String;
                                                file_size : in Integer;
                                                isDirectory : in Boolean);
    procedure Add_New_Directory (Sgf : in out T_Sgf);
    procedure Change_File_Size (Sgf : in out T_SGF) ;
    function Get_File_Path_Name return Unbounded_String;
end menu;
