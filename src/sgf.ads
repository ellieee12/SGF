with GNAT.RegExp; use GNAT.RegExp;
with GNAT.Regpat; use GNAT.Regpat;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with Ada.Containers.Vectors;
with Ada.Text_IO;          use Ada.Text_IO;

package sgf is
    package SU renames Ada.Strings.Unbounded;
    
    type T_SGF is private;
    
    -- Initialise SGF, initialise memory block and create root as directory
    -- Post => Sgf.Root.all.Name = "" and Sgf.Root.all.Size = 0
    procedure Initialize (Sgf : out T_SGF);
    
    -- Obtain the current working directory
    -- Post => Length(Get_Current_Directory'Result) > 0
    function Get_Current_Directory(Sgf : in T_SGF) return String;
   
    -- Create a new file based on the path name given (relative and absolute path included) 
    -- Pre => Size >= 0 
    -- and (Name /= "." and else Name /= "..") 
    -- and Name'Length /= 0 
    -- and for all c in Name, c /= {/,\,:,*,?,<,>,|,&} 
    -- and not target_path.contains(Name) 
    -- and exists(target_path) and isDirectory(target_path) and not empty(target_path)
    -- and check_memory(size) = true
    -- Post => target_path.contains(Name) and total_size'new = total_size'old + size
    -- Create a new directory based on the path name given (relative and absolute path included)
    procedure Create_File(Sgf : in  out T_SGF;
                          Path : in String;
                          Size : in Long_Long_Integer);
    
    -- Create a new directory based on the path name given (relative and absolute path included)
    -- Pre => Size >= 0 
    -- and (Name /= "." and else Name /= "..") 
    -- and Name'Length /= 0 
    -- and for all c in Name, c /= {/,\,:,*,?,<,>,|,&} 
    -- and not target_path.contains(Name) 
    -- and exists(target_path) and isDirectory(target_path) and not empty(target_path)
    -- and check_memory(size) = true
    -- Post => target_path.contains(Name) and total_size'new = total_size'old + size
    procedure Create_Directory(Sgf : in  out T_SGF;
                               Path : in String);
    
    -- Change the current working directory based on a new given (relative and absolute path included)
    -- Pre => exists(target_path) and isDirectory(target_path) and not empty(target_path)
    -- Post => current'new = target_path
    procedure Current_Directory(SGF : in out T_SGF; path : in String := "/");
    
    
    -- List the files and directories of a directory as indicated by a given path name
    -- If no path name given, list the files and directories of the current working directory
    -- Pre => exists(target_path) and isDirectory(target_path) and not empty(target_path)
    function List_Files(SGF : in T_SGF; path : in String := ".";
                        listSize : in boolean := False) return String;
    
    -- List recursively all the files and directory of the current working directory 
    -- Pre => exists(target_path) and isDirectory(target_path) and not empty(target_path)
    function List_Files_Recursive(SGF : in T_SGF; path : in String := ".";
                                  listSize : in Boolean := False) return String;
    
    -- Remove a file as indicated by a given path (relative and absolute path included)
    -- Pre => exists(target_path) and not isDirectory(target_path) 
    procedure Remove(SGF : in out T_SGF; path : in String);
    
    -- Remove a directory, empty or not, as indicated by a given path (relative and absolute path included)
    -- Pre => exists(target_path) and isDirectory(target_path) 
    procedure Remove_Recursive(SGF : in out T_SGF; path : in String);
    
    -- Change the size of a file 
    -- Pre => exists(target_path) and not isDirectory(target_path) and size >= 0 
    procedure Change_File_Size(SGF : in out T_SGF; path : in String; size : in Long_Long_Integer);

    -- Move a file or rename a file
    -- Pre  =>
    --   Exists(Path) and
    --   not Exists(New_Path)
    -- Post =>
    --   not Exists(Path) and
    --   Exists(New_Path)
    procedure Move(SGF : in out T_SGF; path : in String; new_path : in String);
    
    -- Copy a file to a target destination path
    -- Pre => exists(path) and exists(new_path) and isDirectory(new_path) and not isDirectory(path)
    -- Post => exists(path_to_copied_file)
    procedure Copy(SGF : in out T_SGF; path : in String; new_path : in String);

    -- Archive a directory 
    -- Pre => exists(dir_to_be_archived) and exists(dir_to_be_archived) and 
    -- and (Name /= "." and else Name /= "..") 
    -- and Name'Length /= 0 
    -- and for all c in Name, c /= {/,\,:,*,?,<,>,|,&} 
    -- and not exists(archive_path_name+Name)
    -- Post => (Name = *.tar or Name = *.tar.gz or Name = *.tgz) and exists(archive_path_name+Name)
    procedure Archive_Directory (Sgf : in out T_SGF;
                                 Archive_Path_Name : in String;
                                 Dir_To_Be_Archived : in String) ;
    -- Get size of a directory or a file size
    -- Pre => exists(path)
    -- Post => Get_Size'Result > 0
    function Get_Size (Sgf : in out T_SGF ; Path : in String; IsDirectory : in Boolean) return Long_Long_Integer;
    
    -- Get the name of a directory or a file size
    -- Pre => exists(path)
    -- Post => Get_Name'Result = Base_Name(Path)
    function Get_Name (Sgf : in out T_SGF ;  Path : in String; IsDirectory : in Boolean) return String;
   
    -- Get the size of a memory block
    -- Pre => Block_Exists(nb)
    -- Post => Get_Block_Size'Result > 0 
    -- and Get_Block_Size'Result < SIZE_LIMIT
    function Get_Block_Size(nb : in Integer; Sgf : in T_SGF) return Long_Long_Integer;
    

    -- Copy a directory to a target destination path
    -- Pre => exists(path) and exists(new_path) and isDirectory(new_path) and isDirectory(path)
    -- Post => exists(path_to_copied_file)
    procedure Copy_Recursive(SGF : in out T_SGF; path : in String; new_path : in String);
    
    Directory_Exists_Error : exception;
    Control_Character_Error : exception;
    Forbidden_Character_Error : exception;
    Dot_Name_Error : exception;
    Invalid_Archive_Path : exception;
    File_Exists_Error : exception;
    File_Name_Is_Directory_Error : exception;
    Negative_Size_Error: Exception;
    Empty_Name_Error : exception;
    Empty_Path : exception;
    Dir_Not_Found : exception;
    Not_A_Dir : exception;
    Not_A_File : exception;
private
    SIZE_LIMIT : Constant Long_Long_Integer := 1000000000000;
    DIR_SIZE : Constant Long_Long_Integer := 10000;
    type T_Node;
    type T_Pointer_Node is access T_Node;
    type T_Node is
       record
           Name : Unbounded_String;
           Size: Long_Long_Integer;
           Address : Long_Long_Integer;
           IsDirectory : Boolean;
           Child : T_Pointer_Node;
           Parent : T_Pointer_Node;
           Next : T_Pointer_Node;
           Before : T_Pointer_Node;
       end record;
    
    type T_Memory;
    type T_Pointer_Memory is access T_Memory;
    type T_Memory is
       record
           Address : Long_Long_Integer;
           Size : Long_Long_Integer;
           Next_Block : T_Pointer_Memory;
       end record;
    
    type T_SGF is
       record
           Root : T_Pointer_Node;
           Current : T_Pointer_Node;
           Memory : T_Pointer_Memory;
       end record;
   
    Remove_Root : exception;
    Size_Limit_Reach : exception;
    
    -- Verify the name of the node
    -- Post => for all c of Name => C /= '\' and then C /= '/' and then C /= ':' and then C /= '*' and then C /= '?' and then C /= '"' and then C /= '<' and then C /= 
    -- and then C /= '>' and then C /= '|' and then C /= '&' and then C /= '.'
    -- and Character'Pos(c) >= 32 
    -- and Character'Pos(c) /= 127
    procedure Validate_Name (Name : in String);
    
    -- Get a node from a path (and precise if the node to find is a directory, or a file)
    -- Pre => exists(path)
    -- Post => onlyDirectory and isDirectory(path) or not onlyDirectory and not isDirectory(path)
    function Get_Node_From_Path(SGF : in T_SGF; path : in String; onlyDirectory : in Boolean) return T_Pointer_Node;
    
    -- Verify that the next file does not have the same name
    -- Pre => not isDirectory(Current_Node)
    -- Post => for all file of Current_Node.Next => file.name /= Name
    procedure Verify_File_Name_Existence (Current_Node : in T_Pointer_Node; 
                                          Name : in String);
    
    -- Verify that the next directory does not have the same name
    -- Pre => isDirectory(Current_Node)
    -- Post => for all directory of Current_Node.Next => directory.name /= Name
    procedure Verify_Directory_Name_Existence (Current_Node : in T_Pointer_Node; 
                                               Name : in String);
    
    -- Archive the elements of the precedent directory
    -- Pre => call by Archive_Directory_Recursive()
    function Archive_Directory_Recursive (Sgf : in T_SGF;
                                          node : in T_Pointer_Node;
                                          res : in Long_Long_Integer) return Long_Long_Integer ;
    
    -- List recursively all the files and directory of the passed node 
    -- Pre => call by List_Files_Recursive()
    function List_Files_Recursive(SGF : in T_SGF; 
                                  node : in T_Pointer_Node; 
                                  res : in Unbounded_String; 
                                  level : in Natural;
                                  listSize : in boolean := False) return Unbounded_String;
    
    -- Remove a directory, empty or not, as well as files, indicated by the given node
    -- Pre => call by Remove_Recursive()
    procedure Remove_Recursive(SGF : in out T_SGF; node : in out T_Pointer_Node);
    
    -- Copy a directory or file to a target destination node
    -- Pre => call by Copy_Recursive()
    procedure Copy_Recursive(SGF : in out T_SGF; node : in T_Pointer_Node; new_path : in String);
    
    -- Change the name of the archive ton contain the extension
    -- Pre => call by Archive_Directory_Recursive()
    -- Post => Ends_With (To_String (Zip_Name), ".tar.gz") and not Ends_With (To_String (Target_Path), "/")
    procedure Extract_Archive_Info (Arg : String;
                                    Target_Path : out Unbounded_String;
                                    Zip_Name : out Unbounded_String);
    
    -- Get the sum of all node size from a sgf
    -- Post => Get_Total_Size'Result > 0
    function Get_Total_Size(Sgf : in T_SGF) return Long_Long_Integer;
    
    -- Create memory allocation for new file or directory
    -- Pre => for some block of sgf.memory => size < block.size
    -- Post => for some block of sgf.memory => address not in range block.address .. block.address + block.size
    -- and address > 0 
    -- and address < SIZE_LIMIT
    procedure Create_Block_Memory(Sgf : in T_SGF; size : in Long_Long_Integer; address : out Long_Long_Integer);
    
    -- remove memory allocation for a file or directory
    -- Pre => for all block of sgf.memory => node.address not in range block.address .. block.address + block.size and node.address + node.size not in range block.address .. block.address + block.size
    -- Post => for all block of sgf.memory => node.address in range block.address .. block.address + block.size and node.address + node.size in range block.address .. block.address + block.size
    procedure Remove_Block_Memory(Sgf : in T_SGF; node : in out T_Pointer_Node);
    
end sgf;
