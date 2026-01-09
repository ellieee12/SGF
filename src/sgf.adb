with GNAT.RegExp; use GNAT.RegExp;
with GNAT.RegPat; use GNAT.RegPat;
with GNAT.Spitbol; use GNAT.Spitbol;
with Ada.Exceptions; use Ada.Exceptions;

package body sgf is

    procedure Current_Directory(SGF : in out T_SGF; path : in String) is
        temp_node : T_Pointer_Node;
        start : Positive;
    begin
        if path(path'First) = '/' then
            temp_node := SGF.Root.all.Child;
            start := path'First + 1;
        elsif path'Length >= 2 and then path(path'First .. path'First + 1) = "./" then
            temp_node := SGF.Current.all.Child;
            start := path'First + 2;
        elsif path'Length >= 3 and then path(path'First .. path'First + 2) = "../" then
            temp_node := SGF.Current;
            start := path'First + 3;
        else
            raise Incorect_Path_Format with "The path given is incorrect !";
        end if;
        if temp_node = Null then
            raise Dir_Not_Found with "The path contain an unknown directory !";
        end if;
        for I in start .. path'Last + 1 loop
            if path (I) = '/' or else I = path'Last + 1 then
                part := path(Start .. I - 1);
                if part = ".." then
                    if temp_node.all.Parent /= Null then
                        temp_node := temp_node.all.Parent;
                    end if;
                else
                    while temp_node /= Null and then Temp_Node.Name /= part loop
                        temp_node := temp_node.all.Next;
                    end loop;
                    if temp_node = Null then
                        raise Dir_Not_Found with "The path contain an unknown directory !";
                    end if;
                    if not temp_node.IsDirectory then
                        raise Not_A_Dir with "File is not a directory !";
                    end if;
                    temp_node := temp_node.all.Child;
                end if;
                Start := I + 1
            end if;
        end loop;
        SGF.Current.all := temp_node.all;
    end Current_Directory;

end sgf;
