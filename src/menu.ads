with Ada.Text_IO;          use Ada.Text_IO;
with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;
with SGF; use SGF;
package menu is

    procedure Start_Menu (Sgf : in out T_SGF);
    
private
    procedure Print_Menu_Options;

end menu;
