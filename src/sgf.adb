with GNAT.RegExp; use GNAT.RegExp;
with GNAT.RegPat; use GNAT.RegPat;
with GNAT.Spitbol; use GNAT.Spitbol;

package body sgf is

    procedure Move(SGF : in out T_SGF; path : in String) is
        regex : Regexp;
    begin
        regex := Compile("^(/[^/]+)+$");
        if Match(path, regex) then
            
        
    end Move;

end sgf;
