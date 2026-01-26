with Ada.Text_IO;          use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package body terminal is
    
    package SU renames Ada.Strings.Unbounded;
    
    -- Tableau de taille indéfinie (pour les commandes)
    package String_Vectors is
            new Ada.Containers.Vectors
                    (Index_Type   => Natural,
                     Element_Type => Unbounded_String);
    use String_Vectors;
   
    procedure Start_Terminal(SGF : in out T_SGF) is
        input : Unbounded_String;
        command : Vector;
    begin
        Put_Line("Select a command to execute :" & ASCII.LF &
                         "    - pwd : get current directory" & ASCII.LF &
                         "    - touch : create file" & ASCII.LF &
                         "    - size : change file size" & ASCII.LF &
                         "    - mkdir : create directory" & ASCII.LF &
                         "    - cd : change current directory" & ASCII.LF &
                         "    - ls : display all childs of given path" & ASCII.LF &
                         "    - ls -r : display all childs of given path and child directories" & ASCII.LF &
                         "    - rm : remove file" & ASCII.LF &
                         "    - rm -r : remove directory (including its childs)" & ASCII.LF &
                         "    - mv : move file (and eventually, rename it) into another directory" & ASCII.LF &
                         "    - cp : copy a file into another directory" & ASCII.LF &
                         "    - cp -r : copy a directory (including its childs) to another directory" & ASCII.LF &
                         "    - exit : leave terminal");
        Put("> ");
        Get_line(input);
        while input /= "exit" loop
            declare
                S     : constant String := To_String(input);
                Start : Positive := S'First;
            begin
                command.Clear;
                -- Récupérer la commande, les arguments et ses paramètres
                for I in S'Range loop
                    if S (I) = ' ' then
                        if Start <= I - 1 then
                            command.Append(To_Unbounded_String(S(Start .. I - 1)));
                        end if;
                        Start := I + 1;
                    end if;
                end loop;
                -- récupérere le dernier
                if Start <= S'Last then
                    command.Append(To_Unbounded_String(S(Start .. S'Last)));
                end if;
            end;
            declare
                cmd : Constant String := To_String(command(command.First_Index));
            begin
                if cmd = "pwd" then
                    Verify_Nb_Argument(command.Last_Index, 0);
                    Put_Line(Get_Current_Directory(Sgf));
                    
                elsif cmd = "touch" then
                    Verify_Nb_Argument(command.Last_Index, 2, 2);
                    Create_File(Sgf, To_String(command(command.First_Index + 1)), Long_Long_Integer'Value(To_String(command(command.First_Index + 2))));
                    
                elsif cmd = "size" then
                    Verify_Nb_Argument(command.Last_Index, 1, 1);
                    Change_File_Size(Sgf, To_String(command(command.First_Index+1)), Long_Long_Integer'Value(To_String(command(command.First_Index + 2))));
                    
                elsif cmd = "mkdir" then
                    Verify_Nb_Argument(command.Last_Index, 1, 1);
                    Create_Directory(Sgf, To_String(command(command.First_Index + 1)));
                    
                elsif cmd = "cd" then
                    Verify_Nb_Argument(command.Last_Index, 1);
                    if command.Last_Index = 1 then
                        Current_Directory(Sgf, To_String(command(command.First_Index + 1)));
                    else
                        Current_Directory(Sgf);
                    end if;
                    
                elsif cmd = "ls" then
                    Verify_Nb_Argument(command.Last_Index, 2);
                    if command.Last_Index = 2 and then To_String(command(command.First_Index + 1)) = "-r" then
                        Put_Line(List_Files_Recursive(Sgf, To_String(command(command.First_Index + 2))));
                    elsif command.Last_Index = 1 and then To_String(command(command.First_Index + 1)) = "-r" then
                        Put_Line(List_Files_Recursive(Sgf));
                    elsif command.Last_Index = 1 then
                        Put_Line(List_Files(Sgf, To_String(command(command.First_Index + 1))));
                    else
                        Put_Line(List_Files(Sgf));
                    end if;
                    
                elsif cmd = "rm" then
                    Verify_Nb_Argument(command.Last_Index, 2, 1);
                    if command.Last_Index = 2 and then To_String(command(command.First_Index + 1)) = "-r" then
                        Remove_Recursive(Sgf, To_String(command(command.First_Index + 2)));
                    else
                        
                        Remove(Sgf, To_String(command(command.First_Index + 1)));
                    end if;
                    
                elsif cmd = "mv" then
                    Verify_Nb_Argument(command.Last_Index, 2, 2);
                    Move(Sgf, To_String(command(command.First_Index + 1)), To_String(command(command.First_Index + 2)));
                    
                elsif cmd = "cp" then
                    if command.Last_Index = 3 and then To_String(command(command.First_Index + 1)) = "-r" then
                        Copy_Recursive(Sgf, To_String(command(command.First_Index + 2)), To_String(command(command.First_Index + 3)));
                    else
                        Verify_Nb_Argument(command.Last_Index, 2, 2);
                        Copy(Sgf, To_String(command(command.First_Index + 1)), To_String(command(command.First_Index + 2)));
                    end if;
                    
                else
                    Put_Line("The command '" & To_String(command(command.First_Index)) & "' was not found.");
                end if;
            exception
                when E : others =>
                    Put_Line("An error ocurred : " & Exception_Message(E));
            end;
            Put("> ");
            Get_line(input);
        end loop;
        Put_Line("Exiting terminal");
    end Start_Terminal;
    
    procedure Verify_Nb_Argument(nb : in Integer; max_nb : in Integer; min_nb : in Integer := 0) is
    begin
        if nb > max_nb then
            raise Too_Much_Argument with "there is too much argument on this command !"; 
        elsif nb < min_nb then
            raise Not_Enough_Argument with "there is not enough argument on this command !";
        end if;
    end Verify_Nb_Argument;
end terminal;
