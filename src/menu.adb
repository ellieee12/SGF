package body menu is

    procedure Start_Menu (Sgf : in out T_SGF) is
        choice : Natural;
    begin
        choice := 0;
        loop 
            Print_Menu_Options;
            get(choice);
            exit when choice = 99;
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
        
    end Print_Menu_Options;
    

end menu;
