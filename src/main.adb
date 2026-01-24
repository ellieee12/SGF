with menu; use menu;
with terminal; use terminal;
with SGF; use SGF;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Main is
    package SU renames Ada.Strings.Unbounded;

    procedure Print_Interface_Options is
    begin
        put_line("Available interfaces :");
        put_line("1 - Terminal");
        put_line("2 - Menu");
        put_line("99 - Exit");
        put_line("Please enter the number to the corresponding interface that you would like to use.");
    end Print_Interface_Options;

    sgf : T_SGF;
    choice : Natural := 0;
    Invalid_Choice : exception;

begin
    Initialize(Sgf);
    loop
        Print_Interface_Options;
        begin
            choice := 0;
            put("Interface number : ");
            get(choice);
            Skip_Line;
            case choice is
            when 1 =>
                Start_Terminal(Sgf);

            when 2 =>
                Start_Menu(Sgf);
            when 99 => null;
            when others => raise Invalid_Choice;
            end case;

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
        exit when choice = 99;
    end loop;
    --  Start_Menu(Sgf);
    --  Start_Terminal(Sgf);
end Main;
