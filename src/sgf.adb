with GNAT.RegExp; use GNAT.RegExp;
with GNAT.RegPat; use GNAT.RegPat;
with GNAT.Spitbol; use GNAT.Spitbol;

package body sgf is

    procedure Current_Directory(SGF : in out T_SGF; path : in String) is
        regexRelatif : Regexp;
        regexAbsolute : Regexp;
        temp_noeud : T_Pointer_Node;
    begin
        regexRelatif := Compile("^.(/[^/]+)+$");
        regexAbsolute := Compile("^(/[^/]+)+$");
        if Match(path, regexRelatif) then
            
        elsif Match(path, regexAbsolute) then
            for I in path'Range loop
                if path (I) = '/' then
                    Path(Start .. I - 1);
                end if;
            end loop;
        end if;
        
    end Current_Directory;

end sgf;
