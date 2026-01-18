with SGF; use SGF;

package terminal is

    procedure Start_Terminal (Sgf : in out T_SGF);
    
    procedure Verify_Nb_Argument(nb : in Integer; max_nb : in Integer; min_nb : in Integer := 0);
        
private
    Too_Much_Argument : exception;
    Not_Enough_Argument : exception;
end terminal;
