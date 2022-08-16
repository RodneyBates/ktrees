-- Copyright Rodney M. Bates, 2022. rodney.m.bates@acm.org.
-- Licensed under the MIT license. 

WITH K_Trees ; 
WITH Checked_K_Trees ; 
WITH TEXT_IO ; 

PROCEDURE Test_K_Trees 

IS 

  Forest_Size : CONSTANT NATURAL := 31_001 ; 
  One_Ct : CONSTANT := 500 ; 
--  Two_Ct : constant := 4500; 
  Two_Ct : CONSTANT := 29_500 ; 
  Three_Ct : CONSTANT := 25_000 ; 

--  Initial_Seed : constant := 7248190; 
  Initial_Seed : CONSTANT := 264_092_683 ; 
  Multiplier : CONSTANT := 7_001 ; 

  Seed : INTEGER := Initial_Seed ; 

  Next_Element : K_Trees . E := 0 ; 

  TYPE Forest_Typ IS ARRAY ( NATURAL RANGE <> ) OF K_Trees . T ; 

  Forest : Forest_Typ ( 0 .. Forest_Size - 1 ) ; 

  Next_Unused_Tree : NATURAL := Forest ' First ; 
  Next_Store : NATURAL := Forest ' First ; 

  PACKAGE Tr RENAMES Checked_K_Trees ; 

  PACKAGE Integer_Text_Io IS NEW TEXT_IO . Integer_Io ( INTEGER ) ; 

  PACKAGE Float_Text_Io IS NEW TEXT_IO . Float_Io ( FLOAT ) ; 

  Progress_Col : TEXT_IO . Positive_Count := 1 ; 
  Progress_Ct : INTEGER := 0 ; 

  FUNCTION "=" ( Left , Right : IN TEXT_IO . Count ) RETURN BOOLEAN 
    RENAMES TEXT_IO . "=" ; 

  FUNCTION ">" ( Left , Right : IN TEXT_IO . Count ) RETURN BOOLEAN 
    RENAMES TEXT_IO . ">" ; 

  FUNCTION "+" ( Left , Right : IN TEXT_IO . Count ) 
    RETURN TEXT_IO . Count 
    RENAMES TEXT_IO . "+" ; 

  PROCEDURE Ensure_Bol 

  IS 
  BEGIN -- Ensure_Bol  
    IF TEXT_IO . Col /= 1 
    THEN 
      TEXT_IO . New_Line ; 
    END IF ; 
  END Ensure_Bol ; 

  PROCEDURE Pause 

  IS 
  BEGIN -- Pause  
    Ensure_Bol ; 
    TEXT_IO . Put ( "Pause" ) ; 
    TEXT_IO . New_Line ; 
  END Pause ; 

  PROCEDURE Reset_Progress 

  IS 
  BEGIN -- Reset_Progress  
    Progress_Ct := 0 ; 
  END Reset_Progress ; 

  PROCEDURE Show_Progress 

  IS 
  BEGIN -- Show_Progress  
    IF Progress_Ct MOD 50 = 0 AND THEN TEXT_IO . Col /= 1 
    THEN 
      TEXT_IO . New_Line ; 
    END IF ; 
    IF TEXT_IO . Col = 1 
    THEN 
      Integer_Text_Io . Put ( Progress_Ct , 6 ) ; 
      TEXT_IO . Put ( ' ' ) ; 
    END IF ; 
    TEXT_IO . Put ( '.' ) ; 
    Progress_Ct := Progress_Ct + 1 ; 
  END Show_Progress ; 

  PROCEDURE Get_New_Element ( Element : OUT K_Trees . E ) 

  IS 
  BEGIN -- Get_New_Element  
    Element := Next_Element ; 
    Next_Element := Next_Element + 1 ; 
  END Get_New_Element ; 

  FUNCTION Random RETURN INTEGER 

  IS 
  BEGIN -- Random  
    WHILE Seed > INTEGER ' Last / Multiplier 
    LOOP -- WHILE  
      Seed := Seed - INTEGER ' Last / Multiplier ; 
    END LOOP ; -- WHILE  
    Seed := Seed * Multiplier ; 
    RETURN ( Seed MOD 2 ** 23 ) / 2 ** 5 ; 
  END Random ; 

  PROCEDURE Store_Tree ( Tree : IN K_Trees . T ) 

  IS 
  BEGIN -- Store_Tree  
    IF Next_Unused_Tree < Forest_Size - 1 
    THEN 
      Forest ( Next_Unused_Tree ) := Tree ; 
      Next_Unused_Tree := Next_Unused_Tree + 1 ; 
    ELSIF Next_Store < Forest_Size - 1 
    THEN 
      Forest ( Next_Store ) := Tree ; 
      Next_Store := Next_Store + 1 ; 
    ELSE Forest ( 0 ) := Tree ; Next_Store := 1 ; 
    END IF ; 
  END Store_Tree ; 

  PROCEDURE Print_Stats 

  IS 
    Leaf_Nodes , Leaf_Elements , Nonleaf_Nodes , Nonleaf_Elements 
      : INTEGER := 0 ; 
    Height_Sum , Length_Sum , Leaf_Nodes_Sum , Leaf_Elements_Sum , 
    Nonleaf_Nodes_Sum , Nonleaf_Elements_Sum 
      : INTEGER := 0 ; 
    L_Float_Tree_Ct : FLOAT := FLOAT ( Next_Unused_Tree ) ; 

  BEGIN -- Print_Stats  
    FOR I IN 0 .. Next_Unused_Tree - 1 
    LOOP -- FOR  
      Height_Sum := Height_Sum + K_Trees . Height ( Forest ( I ) ) ; 
      Length_Sum := Length_Sum + K_Trees . Length ( Forest ( I ) ) ; 
      K_Trees . Internal_Statistics 
        ( Forest ( I ) , 
          Leaf_Nodes , 
          Leaf_Elements , 
          Nonleaf_Nodes , 
          Nonleaf_Elements 
        ) ; 
      Leaf_Nodes_Sum := Leaf_Nodes_Sum + Leaf_Nodes ; 
      Leaf_Elements_Sum := Leaf_Elements_Sum + Leaf_Elements ; 
      Nonleaf_Nodes_Sum := Nonleaf_Nodes_Sum + Nonleaf_Nodes ; 
      Nonleaf_Elements_Sum := Nonleaf_Elements_Sum + Nonleaf_Elements ; 
    END LOOP ; -- FOR  

    Ensure_Bol ; 

    TEXT_IO . Put 
      ( INTEGER ' Image ( Next_Unused_Tree ) & " Sequences" ) ; 
    TEXT_IO . New_Line ; 

    Float_Text_Io . Put ( FLOAT ( Height_Sum ) / L_Float_Tree_Ct ) ; 
    TEXT_IO . Put ( " Average Height" ) ; 
    TEXT_IO . New_Line ; 

    Float_Text_Io . Put ( FLOAT ( Length_Sum ) / L_Float_Tree_Ct ) ; 
    TEXT_IO . Put ( " Average Length" ) ; 
    TEXT_IO . New_Line ; 

    Float_Text_Io . Put 
      ( FLOAT ( Leaf_Elements_Sum + Nonleaf_Elements_Sum ) 
        / FLOAT ( Leaf_Nodes_Sum + Nonleaf_Nodes_Sum ) 
      ) ; 
    TEXT_IO . Put ( " Average elements per node" ) ; 
    TEXT_IO . New_Line ; 

    Float_Text_Io . Put 
      ( FLOAT ( Leaf_Elements_Sum ) / FLOAT ( Leaf_Nodes_Sum ) ) ; 
    TEXT_IO . Put ( " Average elements per leaf node" ) ; 
    TEXT_IO . New_Line ; 

    Float_Text_Io . Put 
      ( FLOAT ( Nonleaf_Elements_Sum ) / FLOAT ( Nonleaf_Nodes_Sum ) ) ; 
    TEXT_IO . Put ( " Average elements per nonleaf node" ) ; 
    TEXT_IO . New_Line ; 

    Float_Text_Io . Put ( FLOAT ( Leaf_Nodes_Sum ) / L_Float_Tree_Ct ) ; 
    TEXT_IO . Put ( " Average Leaf_Nodes" ) ; 
    TEXT_IO . New_Line ; 

    Float_Text_Io . Put 
      ( FLOAT ( Leaf_Elements_Sum ) / L_Float_Tree_Ct ) ; 
    TEXT_IO . Put ( " Average Leaf_Elements" ) ; 
    TEXT_IO . New_Line ; 

    Float_Text_Io . Put 
      ( FLOAT ( Nonleaf_Nodes_Sum ) / L_Float_Tree_Ct ) ; 
    TEXT_IO . Put ( " Average Nonleaf_Nodes" ) ; 
    TEXT_IO . New_Line ; 

    Float_Text_Io . Put 
      ( FLOAT ( Nonleaf_Elements_Sum ) / L_Float_Tree_Ct ) ; 
    TEXT_IO . Put ( " Average Nonleaf_Elements" ) ; 
    TEXT_IO . New_Line ; 
  END Print_Stats ; 

  PROCEDURE Check_Debug_Slice 
    ( J , From , Thru : IN K_Trees . Seq_Ss_Typ ) 

  IS 
  BEGIN -- Check_Debug_Slice  
    NULL ; 
  END Check_Debug_Slice ; 

  PROCEDURE Test 

  IS 

    L_Element : K_Trees . E ; 
    L_Tree , L_Slice_1 , L_Slice_2 : K_Trees . T ; 
    J , From , Thru : K_Trees . Seq_Ss_Typ ; 
    Length : K_Trees . Seq_Ss_Typ ; 

  BEGIN -- Test  
    Checked_K_Trees . Checks_Enabled := TRUE ; 

    Ensure_Bol ; 
    TEXT_IO . Put_Line ( "Starting test" ) ; 
    Reset_Progress ; 
    L_Tree := Tr . Empty ; 
    Store_Tree ( L_Tree ) ; 
    Show_Progress ; 

    Ensure_Bol ; 
    TEXT_IO . Put_Line ( "Singletons" ) ; 
    Reset_Progress ; 
    FOR I IN 0 .. One_Ct 
    LOOP -- FOR  
      Get_New_Element ( L_Element ) ; 
      L_Tree := Tr . Singleton ( L_Element ) ; 
      Store_Tree ( L_Tree ) ; 
      Show_Progress ; 
    END LOOP ; -- FOR  

    Ensure_Bol ; 
    TEXT_IO . Put_Line ( "Cats" ) ; 
    Reset_Progress ; 
    FOR I IN 0 .. Two_Ct 
    LOOP -- FOR  
      L_Tree 
        := Tr . Cat 
             ( Forest ( Random MOD Next_Unused_Tree ) , 
               Tr . Cat 
                 ( Forest ( Random MOD Next_Unused_Tree ) , 
                   Forest ( Random MOD Next_Unused_Tree ) 
                 ) 
             ) ; 
      Store_Tree ( L_Tree ) ; 
      Show_Progress ; 
    END LOOP ; -- FOR  

    Ensure_Bol ; 
    TEXT_IO . Put_Line ( "Cats of slices" ) ; 
    Reset_Progress ; 
    FOR I IN 0 .. Three_Ct 
    LOOP -- FOR  
      J := Random MOD Next_Unused_Tree ; 
      Length := Tr . Length ( Forest ( J ) ) ; 
      IF Length <= 1 
      THEN 
        From := 0 ; 
        Thru := - 1 ; 
      ELSE 
        From := Random MOD ( Length / 2 ) ; 
        Thru := Length / 2 + Random MOD ( Length / 2 ) ; 
      END IF ; 
      Check_Debug_Slice ( J , From , Thru ) ; 
      L_Slice_1 := Tr . Slice ( Forest ( J ) , From , Thru ) ; 
      J := Random MOD Next_Unused_Tree ; 
      Length := Tr . Length ( Forest ( J ) ) ; 
      IF Length <= 1 
      THEN 
        From := 0 ; 
        Thru := - 1 ; 
      ELSE 
        From := Random MOD ( Length / 2 ) ; 
        Thru := Length / 2 + Random MOD ( Length / 2 ) ; 
      END IF ; 
      Check_Debug_Slice ( J , From , Thru ) ; 
      L_Slice_2 := Tr . Slice ( Forest ( J ) , From , Thru ) ; 
      L_Tree := Tr . Cat ( L_Slice_1 , L_Slice_2 ) ; 
      Store_Tree ( L_Tree ) ; 
      Show_Progress ; 
    END LOOP ; -- FOR  

    Print_Stats ; 

    Ensure_Bol ; 
    TEXT_IO . Put_Line ( "End of test" ) ; 
  END Test ; 

BEGIN -- Test_K_Trees  
  Test ; 
END Test_K_Trees ; 

