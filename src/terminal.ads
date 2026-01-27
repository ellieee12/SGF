with SGF; use SGF;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

package terminal is
    
    package SU renames Ada.Strings.Unbounded;
    
    -- Tableau de taille indéfinie (pour les commandes)
    package String_Vectors is
            new Ada.Containers.Vectors
                    (Index_Type   => Natural,
                     Element_Type => Unbounded_String);
    use String_Vectors;

    procedure Start_Terminal (Sgf : in out T_SGF);
    
    procedure Verify_Nb_Argument(nb : in Integer; max_nb : in Integer; min_nb : in Integer := 0);
        
private
    Too_Many_Argument : exception;
    Not_Enough_Argument : exception;
end terminal;
