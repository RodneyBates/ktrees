-- Copyright Rodney M. Bates, 2022. rodney.m.bates@acm.org.
-- Licensed under the MIT license. 

WITH Assertions ; 
WITH TEXT_IO ; 
WITH UNCHECKED_CONVERSION ; 

PACKAGE BODY K_Trees 
IS 

  PACKAGE Seq_Ss_Text_Io IS NEW TEXT_IO . Integer_Io ( Seq_Ss_Typ ) ; 

  PACKAGE Height_Text_Io IS NEW TEXT_IO . Integer_Io ( Height_Typ ) ; 

  TYPE Hex_Char_Typ IS ARRAY ( 0 .. 15 ) OF CHARACTER ; 
  Hex_Char 
    : Hex_Char_Typ 
    := ( '0' , 
         '1' , 
         '2' , 
         '3' , 
         '4' , 
         '5' , 
         '6' , 
         '7' , 
         '8' , 
         '9' , 
         'A' , 
         'B' , 
         'C' , 
         'D' , 
         'E' , 
         'F' 
       ) ; 

  TYPE Leaf_Pack_State_Typ 
  IS RECORD 
       Lps_I : Elem_No_Typ ; 
       Lps_Current_New_Node : T ; 
       Lps_Current_Node_Elem_Ct : Elem_No_Typ ; 
       Lps_Total_New_Elem_Ct : Elem_No_Typ ; 
       Lps_New_K_Tree_1 : T ; 
       Lps_New_K_Tree_2 : T ; 
       Lps_New_K_Tree_3 : T ; 
     END RECORD ; 

  TYPE Nonleaf_Pack_State_Typ 
  IS RECORD 
       Nlps_I : Elem_No_Typ ; 
       Nlps_Cum_Child_Ct : Seq_Ss_Typ ; 
       Nlps_Current_New_Node : T ; 
       Nlps_Current_Node_Elem_Ct : Elem_No_Typ ; 
       Nlps_Total_New_Elem_Ct : Elem_No_Typ ; 
       Nlps_New_K_Tree_1 : T ; 
       Nlps_New_K_Tree_2 : T ; 
       Nlps_New_K_Tree_3 : T ; 
     END RECORD ; 

  FUNCTION "=" ( Left , Right : IN TEXT_IO . Count ) RETURN BOOLEAN 
    RENAMES TEXT_IO . "=" ; 

  FUNCTION Max ( Left , Right : IN Height_Typ ) RETURN Height_Typ 

  IS 
  BEGIN -- Max  
    IF Left > Right 
    THEN 
      RETURN Left ; 
    ELSE RETURN Right ; 
    END IF ; 
  END Max ; 

  FUNCTION For_Range ( Left , Right : IN Elem_No_Typ ) 
    RETURN Elem_No_Typ 

  IS 
  BEGIN -- For_Range  
    IF Right < Left 
    THEN 
      RETURN 0 ; 
    ELSE RETURN Right - Left + 1 ; 
    END IF ; 
  END For_Range ; 

  PROCEDURE Ensure_Bol 

  IS 
  BEGIN -- Ensure_Bol  
    IF TEXT_IO . Col /= 1 
    THEN 
      TEXT_IO . New_Line ; 
    END IF ; 
  END Ensure_Bol ; 

  FUNCTION Image ( Element : IN E ) RETURN STRING 

  IS 
  BEGIN -- Image  
    RETURN E ' Image ( Element ) ; 
  END Image ; 

  FUNCTION Image ( K_Tree : IN T ) RETURN STRING 

  IS 

    FUNCTION To_Int IS NEW UNCHECKED_CONVERSION ( T , INTEGER ) ; 
    L_Value : INTEGER := To_Int ( K_Tree ) ; 
    Add_16 : BOOLEAN := FALSE ; 
    L_Hex_Value : INTEGER ; 
    L_Hex_Char : Hex_Char_Typ := Hex_Char ; 
    SUBTYPE Result_Typ IS STRING ( 1 .. 8 ) ; 
    L_Result : Result_Typ ; 

  BEGIN -- Image  
    IF L_Value < 0 
    THEN 
      L_Value := L_Value + INTEGER ' Last ; 
      L_Value := L_Value + 1 ; 
      Add_16 := TRUE ; 
    END IF ; 
    FOR Place IN 1 .. 8 
    LOOP -- FOR  
      L_Hex_Value := L_Value MOD 16 ; 
      IF Place = 8 AND THEN Add_16 
      THEN 
        L_Hex_Value := L_Hex_Value + 16 ; 
      END IF ; 
      L_Value := L_Value / 16 ; 
      IF L_Value = 0 
      THEN 
        L_Hex_Char ( 0 ) := ' ' ; 
      END IF ; 
      L_Result ( 9 - Place ) := Hex_Char ( L_Hex_Value ) ; 
    END LOOP ; -- FOR  
    RETURN L_Result ; 
  END Image ; 

  FUNCTION Cum_Children_To_Left ( K_Tree : IN T ; I : IN Elem_No_Typ ) 
    RETURN Seq_Ss_Typ 

  IS 
  BEGIN -- Cum_Children_To_Left  
    IF K_Tree = NULL 
    THEN 
      RETURN 0 ; 
    ELSIF K_Tree . ALL . Height = 1 
    THEN RETURN Seq_Ss_Typ ( I ) ; 
    ELSIF I = 0 
    THEN RETURN 0 ; 
    ELSE 
      RETURN K_Tree . ALL . Nonleaf_Elems ( I - 1 ) . Nle_Cum_Child_Ct ; 
    END IF ; 
  END Cum_Children_To_Left ; 

  FUNCTION Empty RETURN T 

  IS 
  BEGIN -- Empty  
    RETURN NULL ; 
  END Empty ; 

  FUNCTION Singleton ( Element : IN E ) RETURN T 

  IS 

    L_Result : T ; 

  BEGIN -- Singleton  
    L_Result := NEW Node_Typ ( Height => 1 , Elem_Ct => 1 ) ; 
    L_Result . ALL . Leaf_Elems ( 0 ) := Element ; 
    RETURN L_Result ; 
  END Singleton ; 

  FUNCTION Elem_Ct ( K_Tree : IN T ) RETURN Elem_No_Typ 

  IS 
  BEGIN -- Elem_Ct  
    IF K_Tree = NULL 
    THEN 
      RETURN 0 ; 
    ELSE RETURN K_Tree . ALL . Elem_Ct ; 
    END IF ; 
  END Elem_Ct ; 

  FUNCTION Length ( K_Tree : IN T ) RETURN Seq_Ss_Typ 

  IS 
  BEGIN -- Length  
    IF K_Tree = NULL 
    THEN 
      RETURN 0 ; 
    ELSIF K_Tree . ALL . Height = 1 
    THEN RETURN K_Tree . ALL . Elem_Ct ; 
    ELSE 
      RETURN 
        K_Tree . ALL . Nonleaf_Elems 
          ( K_Tree . ALL . Elem_Ct - 1 ) 
          . Nle_Cum_Child_Ct ; 
    END IF ; 
  END Length ; 

  PROCEDURE Bin_Search 
    ( Nonleaf_K_Tree : IN T ; 
      Subscript : IN Seq_Ss_Typ ; 
      Elem_No : OUT Elem_No_Typ ; 
      Child_Subscript : OUT Seq_Ss_Typ ; 
      Leftmost_Elem_No : IN Elem_No_Typ := 0 
      -- caller knows the desired element can't be less than this. 
    ) 

  IS 

    L_Lo_Elem_No : Elem_No_Typ := Leftmost_Elem_No ; 
    L_Hi_Elem_No : Elem_No_Typ := Nonleaf_K_Tree . ALL . Elem_Ct - 1 ; 
    L_Probe_Elem_No : Elem_No_Typ ; 
    L_Probe_Max_Ss : Seq_Ss_Typ ; 

  BEGIN -- Bin_Search  
    Assertions . Assert 
      ( 0 <= L_Lo_Elem_No , "Bin_Search, leftmost_elem_no < 0. " ) ; 
    Assertions . Assert 
      ( L_Lo_Elem_No <= L_Hi_Elem_No , 
        "Bin_Search, empty initial range. " 
      ) ; 
    Assertions . Assert 
      ( 0 <= Subscript , "Bin_Search, low subscript. " ) ; 
    Assertions . Assert 
      ( Subscript 
        <= Nonleaf_K_Tree . ALL . Nonleaf_Elems 
             ( L_Hi_Elem_No ) 
             . Nle_Cum_Child_Ct , 
        "Bin_Search, high subscript. " 
      ) ; 
    WHILE L_Lo_Elem_No < L_Hi_Elem_No 
    LOOP -- WHILE  
      L_Probe_Elem_No := ( L_Lo_Elem_No + L_Hi_Elem_No ) / 2 ; 
      L_Probe_Max_Ss 
        := Nonleaf_K_Tree . ALL . Nonleaf_Elems 
             ( L_Probe_Elem_No ) 
             . Nle_Cum_Child_Ct 
           - 1 ; 
      IF Subscript > L_Probe_Max_Ss 
      THEN 
        L_Lo_Elem_No := L_Probe_Elem_No + 1 ; 
      ELSE 
        L_Hi_Elem_No := L_Probe_Elem_No ; 
        IF Subscript = L_Probe_Max_Ss 
        THEN 
          L_Lo_Elem_No := L_Probe_Elem_No ; 
        END IF ; 
      END IF ; 
    END LOOP ; -- WHILE  
    Elem_No := L_Lo_Elem_No ; 
    IF L_Lo_Elem_No > 0 
    THEN 
      Child_Subscript 
        := Subscript 
           - Nonleaf_K_Tree . ALL . Nonleaf_Elems 
               ( L_Lo_Elem_No - 1 ) 
               . Nle_Cum_Child_Ct ; 
    ELSE Child_Subscript := Subscript ; 
    END IF ; 
  END Bin_Search ; 

  PROCEDURE Descend_Thru_Nonleaf_Levels 
    ( K_Tree : IN T ; 
      Subscript : IN Seq_Ss_Typ ; 
      Leaf_Level : OUT T ; 
      Leaf_Elem_No : OUT Elem_No_Typ 
    ) 

  IS 

    L_K_Tree : T := K_Tree ; 
    L_Subscript : Seq_Ss_Typ := Subscript ; 
    L_Elem_No : Elem_No_Typ ; 

  BEGIN -- Descend_Thru_Nonleaf_Levels  
    IF L_K_Tree = NULL 
    THEN 
      RAISE Subscript_Out_Of_Bounds ; 
    ELSE 
      WHILE L_K_Tree . ALL . Height > 1 
      LOOP -- WHILE  
        Bin_Search 
          ( L_K_Tree , L_Subscript , L_Elem_No , L_Subscript ) ; 
        L_K_Tree 
          := L_K_Tree . ALL . Nonleaf_Elems 
               ( L_Elem_No ) 
               . Nle_Child_Ref ; 
        Assertions . Assert 
          ( Length ( L_K_Tree ) > L_Subscript , 
            "Descent_Thru_Nonleaf_Levels, bad results from Bin_Search." 
          ) ; 
      END LOOP ; -- WHILE  
      Leaf_Level := L_K_Tree ; 
      Leaf_Elem_No := L_Subscript ; 
    END IF ; 
  END Descend_Thru_Nonleaf_Levels ; 

  FUNCTION Ith_Element ( K_Tree : IN T ; Subscript : IN Seq_Ss_Typ ) 
    RETURN E 

  IS 

    L_Leaf_K_Tree : T ; 
    L_Leaf_Elem_No : Elem_No_Typ ; 

  BEGIN -- Ith_Element  
    Descend_Thru_Nonleaf_Levels 
      ( K_Tree , Subscript , L_Leaf_K_Tree , L_Leaf_Elem_No ) ; 
    RETURN L_Leaf_K_Tree . ALL . Leaf_Elems ( L_Leaf_Elem_No ) ; 
  END Ith_Element ; 

  PROCEDURE Store_Ith_Element 
    ( K_Tree : IN T ; Subscript : IN Seq_Ss_Typ ; Value : IN E ) 

  IS 

    L_Leaf_K_Tree : T ; 
    L_Leaf_Elem_No : Elem_No_Typ ; 

  BEGIN -- Store_Ith_Element  
    Descend_Thru_Nonleaf_Levels 
      ( K_Tree , Subscript , L_Leaf_K_Tree , L_Leaf_Elem_No ) ; 
    L_Leaf_K_Tree . ALL . Leaf_Elems ( L_Leaf_Elem_No ) := Value ; 
  END Store_Ith_Element ; 

  PROCEDURE Init_Repack_Leaf 
    ( State : IN OUT Leaf_Pack_State_Typ ; 
      New_Elem_Ct : IN Elem_No_Typ 
    ) 

  IS 
  BEGIN -- Init_Repack_Leaf  
    Assertions . Assert 
      ( New_Elem_Ct <= 3 * Max_Elem_Ct , 
        "Init_Repack_Leaf, total element count too big." 
      ) ; 
    State . Lps_Total_New_Elem_Ct := New_Elem_Ct ; 
    IF New_Elem_Ct > Max_Elem_Ct * 2 
    THEN 
      State . Lps_Current_Node_Elem_Ct := ( New_Elem_Ct + 1 ) / 3 ; 
    ELSIF New_Elem_Ct > Max_Elem_Ct 
    THEN State . Lps_Current_Node_Elem_Ct := New_Elem_Ct / 2 ; 
    ELSE State . Lps_Current_Node_Elem_Ct := New_Elem_Ct ; 
    END IF ; 
    Assertions . Assert 
      ( State . Lps_Current_Node_Elem_Ct <= Max_Elem_Ct , 
        "Init_Repack_Leaf, element ct too big." 
      ) ; 
    State . Lps_Current_New_Node 
      := NEW Node_Typ 
               ( Height => 1 , 
                 Elem_Ct => State . Lps_Current_Node_Elem_Ct 
               ) ; 
    State . Lps_I := 0 ; 
    State . Lps_New_K_Tree_1 := State . Lps_Current_New_Node ; 
    State . Lps_New_K_Tree_2 := NULL ; 
    State . Lps_New_K_Tree_3 := NULL ; 
  END Init_Repack_Leaf ; 

  PROCEDURE Repack_Leaf_Elem 
    ( State : IN OUT Leaf_Pack_State_Typ ; Element : IN E ) 

  IS 
  BEGIN -- Repack_Leaf_Elem  
    Assertions . Assert 
      ( State . Lps_I <= State . Lps_Current_Node_Elem_Ct , 
        "Repack_Leaf_Elem." 
      ) ; 
    IF State . Lps_I = State . Lps_Current_Node_Elem_Ct 
    THEN 
      IF State . Lps_Total_New_Elem_Ct > Max_Elem_Ct * 2 
      THEN -- three new nodes total 
        IF State . Lps_New_K_Tree_2 = NULL 
        THEN -- Ready to start 2nd new node of 3. 
          State . Lps_Current_New_Node 
            := NEW Node_Typ 
                     ( Height => 1 , 
                       Elem_Ct => State . Lps_Current_Node_Elem_Ct 
                     ) ; 
          State . Lps_New_K_Tree_2 := State . Lps_Current_New_Node ; 
        ELSE -- Ready to start 3rd new node 
          State . Lps_Current_Node_Elem_Ct 
            := State . Lps_Total_New_Elem_Ct 
               - State . Lps_Current_Node_Elem_Ct * 2 ; 
          Assertions . Assert 
            ( State . Lps_Current_Node_Elem_Ct <= Max_Elem_Ct , 
              "Repack_Leaf_Elem, 3rd element count too big." 
            ) ; 
          State . Lps_Current_New_Node 
            := NEW Node_Typ 
                     ( Height => 1 , 
                       Elem_Ct => State . Lps_Current_Node_Elem_Ct 
                     ) ; 
          State . Lps_New_K_Tree_3 := State . Lps_Current_New_Node ; 
        END IF ; 
      ELSE -- Last of two new nodes. 
        State . Lps_Current_Node_Elem_Ct 
          := State . Lps_Total_New_Elem_Ct 
             - State . Lps_Current_Node_Elem_Ct ; 
        Assertions . Assert 
          ( State . Lps_Current_Node_Elem_Ct <= Max_Elem_Ct , 
            "Repack_Leaf_Elem, 2nd element count too big." 
          ) ; 
        State . Lps_Current_New_Node 
          := NEW Node_Typ 
                   ( Height => 1 , 
                     Elem_Ct => State . Lps_Current_Node_Elem_Ct 
                   ) ; 
        State . Lps_New_K_Tree_2 := State . Lps_Current_New_Node ; 
      END IF ; 
      State . Lps_I := 0 ; 
    END IF ; 
    State . Lps_Current_New_Node . ALL . Leaf_Elems ( State . Lps_I ) 
      := Element ; 
    State . Lps_I := State . Lps_I + 1 ; 
  END Repack_Leaf_Elem ; 

  PROCEDURE Finalize_Repack_Leaf 
    ( State : IN OUT Leaf_Pack_State_Typ ) 

  IS 
  BEGIN -- Finalize_Repack_Leaf  
    Assertions . Assert 
      ( State . Lps_I = State . Lps_Current_Node_Elem_Ct 
        AND THEN 
          ( State . Lps_Total_New_Elem_Ct > Max_Elem_Ct ) 
          <= -- implies 
             ( State . Lps_New_K_Tree_2 /= NULL ) 
        AND THEN 
          ( State . Lps_Total_New_Elem_Ct > Max_Elem_Ct * 2 ) 
          <= -- implies 
             ( State . Lps_New_K_Tree_3 /= NULL ) , 
        "Finalize_Repack_Leaf, count mismatch." 
      ) ; 
  END Finalize_Repack_Leaf ; 

  PROCEDURE Init_Repack_Nonleaf 
    ( State : IN OUT Nonleaf_Pack_State_Typ ; 
      New_Elem_Ct : IN Elem_No_Typ ; 
      Height : IN Height_Typ 
    ) 

  IS 
  BEGIN -- Init_Repack_Nonleaf  
    Assertions . Assert 
      ( New_Elem_Ct <= 3 * Max_Elem_Ct , 
        "Init_Repack_Nonleaf, total element count too big." 
      ) ; 
    State . Nlps_Total_New_Elem_Ct := New_Elem_Ct ; 
    IF New_Elem_Ct > Max_Elem_Ct * 2 
    THEN 
      State . Nlps_Current_Node_Elem_Ct := ( New_Elem_Ct + 1 ) / 3 ; 
    ELSIF New_Elem_Ct > Max_Elem_Ct 
    THEN State . Nlps_Current_Node_Elem_Ct := New_Elem_Ct / 2 ; 
    ELSE State . Nlps_Current_Node_Elem_Ct := New_Elem_Ct ; 
    END IF ; 
    Assertions . Assert 
      ( State . Nlps_Current_Node_Elem_Ct <= Max_Elem_Ct , 
        "Init_Repack_Nonleaf, element ct too big." 
      ) ; 
    State . Nlps_Current_New_Node 
      := NEW Node_Typ 
               ( Height => Height , 
                 Elem_Ct => State . Nlps_Current_Node_Elem_Ct 
               ) ; 
    State . Nlps_I := 0 ; 
    State . Nlps_Cum_Child_Ct := 0 ; 
    State . Nlps_New_K_Tree_1 := State . Nlps_Current_New_Node ; 
    State . Nlps_New_K_Tree_2 := NULL ; 
    State . Nlps_New_K_Tree_3 := NULL ; 
  END Init_Repack_Nonleaf ; 

  PROCEDURE Repack_Nonleaf_Elem 
    ( State : IN OUT Nonleaf_Pack_State_Typ ; Child_Ref : IN T ) 

  IS 
  BEGIN -- Repack_Nonleaf_Elem  
    IF Child_Ref /= NULL 
    THEN 
      Assertions . Assert 
        ( State . Nlps_I <= State . Nlps_Current_Node_Elem_Ct , 
          "Repack_Nonleaf_Elem." 
        ) ; 
      IF State . Nlps_I = State . Nlps_Current_Node_Elem_Ct 
      THEN 
        IF State . Nlps_Total_New_Elem_Ct > Max_Elem_Ct * 2 
        THEN -- three new nodes total 
          IF State . Nlps_New_K_Tree_2 = NULL 
          THEN -- Ready to start 2nd new node of 3. 
            State . Nlps_Current_New_Node 
              := NEW Node_Typ 
                       ( Height 
                         => State . Nlps_New_K_Tree_1 . ALL . Height , 
                         Elem_Ct => State . Nlps_Current_Node_Elem_Ct 
                       ) ; 
            State . Nlps_New_K_Tree_2 := State . Nlps_Current_New_Node ; 
          ELSE -- Ready to start 3rd new node 
            State . Nlps_Current_Node_Elem_Ct 
              := State . Nlps_Total_New_Elem_Ct 
                 - State . Nlps_Current_Node_Elem_Ct * 2 ; 
            Assertions . Assert 
              ( State . Nlps_Current_Node_Elem_Ct <= Max_Elem_Ct , 
                "Repack_Nonleaf_Elem, 3rd element count too big." 
              ) ; 
            State . Nlps_Current_New_Node 
              := NEW Node_Typ 
                       ( Height 
                         => State . Nlps_New_K_Tree_1 . ALL . Height , 
                         Elem_Ct => State . Nlps_Current_Node_Elem_Ct 
                       ) ; 
            State . Nlps_New_K_Tree_3 := State . Nlps_Current_New_Node ; 
          END IF ; 
        ELSE -- Last of two new nodes. 
          State . Nlps_Current_Node_Elem_Ct 
            := State . Nlps_Total_New_Elem_Ct 
               - State . Nlps_Current_Node_Elem_Ct ; 
          Assertions . Assert 
            ( State . Nlps_Current_Node_Elem_Ct <= Max_Elem_Ct , 
              "Repack_Nonleaf_Elem, 2nd element count too big." 
            ) ; 
          State . Nlps_Current_New_Node 
            := NEW Node_Typ 
                     ( Height 
                       => State . Nlps_New_K_Tree_1 . ALL . Height , 
                       Elem_Ct => State . Nlps_Current_Node_Elem_Ct 
                     ) ; 
          State . Nlps_New_K_Tree_2 := State . Nlps_Current_New_Node ; 
        END IF ; 
        State . Nlps_I := 0 ; 
        State . Nlps_Cum_Child_Ct := 0 ; 
      END IF ; 
      State . Nlps_Cum_Child_Ct 
        := State . Nlps_Cum_Child_Ct + Length ( Child_Ref ) ; 
      State . Nlps_Current_New_Node . ALL . Nonleaf_Elems 
        ( State . Nlps_I ) 
        := ( Nle_Child_Ref => Child_Ref , 
             Nle_Cum_Child_Ct => State . Nlps_Cum_Child_Ct 
           ) ; 
      State . Nlps_I := State . Nlps_I + 1 ; 
    END IF ; 
  END Repack_Nonleaf_Elem ; 

  PROCEDURE Finalize_Repack_Nonleaf 
    ( State : IN OUT Nonleaf_Pack_State_Typ ) 

  IS 
  BEGIN -- Finalize_Repack_Nonleaf  
    Assertions . Assert 
      ( State . Nlps_I = State . Nlps_Current_Node_Elem_Ct 
        AND THEN 
          ( State . Nlps_Total_New_Elem_Ct > Max_Elem_Ct ) 
          <= -- implies 
             ( State . Nlps_New_K_Tree_2 /= NULL ) 
        AND THEN 
          ( State . Nlps_Total_New_Elem_Ct > Max_Elem_Ct * 2 ) 
          <= -- implies 
             ( State . Nlps_New_K_Tree_3 /= NULL ) , 
        "Finalize_Repack_Nonleaf, count mismatch." 
      ) ; 
  END Finalize_Repack_Nonleaf ; 

  FUNCTION Cat ( Left , Right : IN T ) RETURN T 

  IS 

    PROCEDURE Recurse 
      ( Height : IN Height_Typ ; Left , Right : IN OUT T ) 

    IS 

      L_Total_New_Elem_Ct : Elem_No_Typ ; 
      L_Down_Left , L_Down_Right : T ; 
      L_Returned_Left , L_Returned_Right : T ; 
      L_Left_Is_Low , L_Right_Is_Low : BOOLEAN ; 

      L_Leaf_Pack_State : Leaf_Pack_State_Typ ; 
      L_Nonleaf_Pack_State : Nonleaf_Pack_State_Typ ; 

    BEGIN -- Recurse  
      Assertions . Assert 
        ( Left /= NULL , "Cat.Recurse left = null. " ) ; 
      Assertions . Assert 
        ( Right /= NULL , "Cat.Recurse right = null. " ) ; 
      IF Height = 1 
      THEN -- These are leaf nodes. 
        Assertions . Assert 
          ( Left . ALL . Height = 1 , "Cat.Recurse left height /= 1. " ) ; 
        Assertions . Assert 
          ( Right . ALL . Height = 1 , 
            "Cat.Recurse left height /= 1. " 
          ) ; 
        L_Total_New_Elem_Ct 
          := Left . ALL . Elem_Ct + Right . ALL . Elem_Ct ; 
        IF Left . ALL . Elem_Ct = 1 
           OR ELSE Right . ALL . Elem_Ct = 1 
           OR ELSE L_Total_New_Elem_Ct <= Max_Elem_Ct 
        THEN -- repack leaf elements into one or two new nodes. 
          Init_Repack_Leaf ( L_Leaf_Pack_State , L_Total_New_Elem_Ct ) ; 
          FOR J IN 0 .. Left . ALL . Elem_Ct - 1 
          LOOP -- FOR  
            Repack_Leaf_Elem 
              ( L_Leaf_Pack_State , Left . ALL . Leaf_Elems ( J ) ) ; 
          END LOOP ; -- FOR  
          FOR J IN 0 .. Right . ALL . Elem_Ct - 1 
          LOOP -- FOR  
            Repack_Leaf_Elem 
              ( L_Leaf_Pack_State , Right . ALL . Leaf_Elems ( J ) ) ; 
          END LOOP ; -- FOR  
          Finalize_Repack_Leaf ( L_Leaf_Pack_State ) ; 
          Left := L_Leaf_Pack_State . Lps_New_K_Tree_1 ; 
          Right := L_Leaf_Pack_State . Lps_New_K_Tree_2 ; 
          Assertions . Assert 
            ( L_Leaf_Pack_State . Lps_New_K_Tree_3 = NULL , 
              "Cat.Recurse 3 leaf nodes. " 
            ) ; 
     -- else -- can just reuse the nodes as they are 
        END IF ; 
      ELSE -- Nonleaf nodes. 
        L_Left_Is_Low := Left . ALL . Height < Height ; 
        IF L_Left_Is_Low 
        THEN 
          L_Down_Left := Left ; 
        ELSE 
          L_Down_Left 
            := Left . ALL . Nonleaf_Elems 
                 ( Left . ALL . Elem_Ct - 1 ) 
                 . Nle_Child_Ref ; 
        END IF ; 
        L_Returned_Left := L_Down_Left ; 
        L_Right_Is_Low := Right . ALL . Height < Height ; 
        IF L_Right_Is_Low 
        THEN 
          L_Down_Right := Right ; 
        ELSE 
          L_Down_Right 
            := Right . ALL . Nonleaf_Elems ( 0 ) . Nle_Child_Ref ; 
        END IF ; 
        L_Returned_Right := L_Down_Right ; 
        Recurse ( Height - 1 , L_Returned_Left , L_Returned_Right ) ; 
        L_Total_New_Elem_Ct 
          := BOOLEAN ' Pos ( L_Returned_Left /= NULL ) 
             + BOOLEAN ' Pos ( L_Returned_Right /= NULL ) ; 
        IF NOT L_Left_Is_Low 
        THEN -- Count rest of elements of left node. 
          L_Total_New_Elem_Ct 
            := L_Total_New_Elem_Ct + Left . ALL . Elem_Ct - 1 ; 
        END IF ; 
        IF NOT L_Right_Is_Low 
        THEN -- Count rest of elements of right node. 
          L_Total_New_Elem_Ct 
            := L_Total_New_Elem_Ct + Right . ALL . Elem_Ct - 1 ; 
        END IF ; 
        IF L_Left_Is_Low 
           OR ELSE L_Right_Is_Low 
           -- One side's height lower than Recurse.Height means 
           -- something for that side must be packed in, even if 
           -- it was not changed by the levels below. 
           OR ELSE L_Returned_Left /= L_Down_Left 
           OR ELSE L_Returned_Right /= L_Down_Right 
           -- Level below changed something, so must repack. 
           OR ELSE L_Total_New_Elem_Ct <= Max_Elem_Ct 
        THEN -- repack into one or two new nodes 
          Init_Repack_Nonleaf 
            ( L_Nonleaf_Pack_State , L_Total_New_Elem_Ct , Height ) ; 
          IF NOT L_Left_Is_Low 
          THEN -- repack rest of elements of left node. 
            FOR J IN 0 .. Left . ALL . Elem_Ct - 2 
            LOOP -- FOR  
              Repack_Nonleaf_Elem 
                ( L_Nonleaf_Pack_State , 
                  Left . ALL . Nonleaf_Elems ( J ) . Nle_Child_Ref 
                ) ; 
            END LOOP ; -- FOR  
          END IF ; 
          IF L_Returned_Left /= NULL 
          THEN 
            Repack_Nonleaf_Elem 
              ( L_Nonleaf_Pack_State , L_Returned_Left ) ; 
          END IF ; 
          IF L_Returned_Right /= NULL 
          THEN 
            Repack_Nonleaf_Elem 
              ( L_Nonleaf_Pack_State , L_Returned_Right ) ; 
          END IF ; 
          IF NOT L_Right_Is_Low 
          THEN -- repack rest of elements of right node. 
            FOR J IN 1 .. Right . ALL . Elem_Ct - 1 
            LOOP -- FOR  
              Repack_Nonleaf_Elem 
                ( L_Nonleaf_Pack_State , 
                  Right . ALL . Nonleaf_Elems ( J ) . Nle_Child_Ref 
                ) ; 
            END LOOP ; -- FOR  
          END IF ; 
          Finalize_Repack_Nonleaf ( L_Nonleaf_Pack_State ) ; 
          Left := L_Nonleaf_Pack_State . Nlps_New_K_Tree_1 ; 
          Right := L_Nonleaf_Pack_State . Nlps_New_K_Tree_2 ; 
          Assertions . Assert 
            ( L_Nonleaf_Pack_State . Nlps_New_K_Tree_3 = NULL , 
              "Cat.Recurse, non_null New_K_Tree_3." 
            ) ; 
     -- else -- can just reuse the nodes as they are 
        END IF ; 
      END IF ; 
    END Recurse ; 

  BEGIN -- Cat  
    DECLARE 
      L_Left_Child_Ct : Seq_Ss_Typ ; 
      L_Left : T := Left ; 
      L_Right : T := Right ; 
      L_Height : Height_Typ ; 
      L_Result : T ; 
    BEGIN -- DECLARE  
      IF L_Left = NULL 
      THEN 
        RETURN L_Right ; -- Which could also be null. 
      ELSIF L_Right = NULL 
      THEN RETURN L_Left ; 
      ELSE 
        L_Height 
          := Max ( L_Left . ALL . Height , L_Right . ALL . Height ) ; 
        Recurse ( L_Height , L_Left , L_Right ) ; 
        Assertions . Assert ( L_Left /= NULL , "Cat, null result." ) ; 
        Assertions . Assert 
          ( L_Left . ALL . Height = L_Height , "Cat, bad left height." ) ; 
        IF L_Right = NULL 
        THEN 
          RETURN ( L_Left ) ; 
        ELSE 
          Assertions . Assert 
            ( L_Right . ALL . Height = L_Height , 
              "Cat, bad left height." 
            ) ; 
          L_Left_Child_Ct := Length ( L_Left ) ; 
          L_Result 
            := NEW Node_Typ ( Height => L_Height + 1 , Elem_Ct => 2 ) ; 
          L_Result . Nonleaf_Elems ( 0 ) 
            := ( Nle_Child_Ref => L_Left , 
                 Nle_Cum_Child_Ct => L_Left_Child_Ct 
               ) ; 
          L_Result . Nonleaf_Elems ( 1 ) 
            := ( Nle_Child_Ref => L_Right , 
                 Nle_Cum_Child_Ct 
                 => L_Left_Child_Ct + Length ( L_Right ) 
               ) ; 
          RETURN L_Result ; 
        END IF ; 
      END IF ; 
    END ; -- DECLARE  
  END Cat ; 

  FUNCTION Slice ( K_Tree : IN T ; From , Thru : IN Seq_Ss_Typ ) 
    RETURN T 

  IS 

    PROCEDURE Recurse_Whole_1 
      ( Rel_From_Ss , Rel_Thru_Ss : IN Seq_Ss_Typ ; K_Tree : IN OUT T ) ; 

    PROCEDURE Recurse_Whole_2 
      ( Rel_From_Ss , Rel_Thru_Ss : IN Seq_Ss_Typ ; 
        K_Tree_1 , K_Tree_2 : IN OUT T 
      ) ; 

    PROCEDURE Recurse_Whole_3 
      ( Rel_From_Ss , Rel_Thru_Ss : IN Seq_Ss_Typ ; 
        K_Tree_1 , K_Tree_2 , K_Tree_3 : IN OUT T 
      ) ; 

    PROCEDURE Recurse_Left 
      ( Rel_From_Ss : IN Seq_Ss_Typ ; K_Tree_1 , K_Tree_2 : IN OUT T ) ; 

    PROCEDURE Recurse_Right 
      ( Rel_Thru_Ss : IN Seq_Ss_Typ ; K_Tree_1 , K_Tree_2 : IN OUT T ) ; 

    PROCEDURE Recurse_Whole_1 
      ( Rel_From_Ss , Rel_Thru_Ss : IN Seq_Ss_Typ ; K_Tree : IN OUT T ) 

    IS 

      L_Old_Elem_Ct : Elem_No_Typ ; 
      L_New_Elem_Ct : Elem_No_Typ ; 
      L_From_Elem_No , L_Thru_Elem_No : Elem_No_Typ ; 
      L_Child_From_Ss , L_Child_Thru_Ss : Seq_Ss_Typ ; 
      L_Child_K_Tree_1 , L_Child_K_Tree_2 , L_Child_K_Tree_3 , 
      L_Child_K_Tree_4 
        : T ; 

      L_Leaf_Pack_State : Leaf_Pack_State_Typ ; 
      L_Nonleaf_Pack_State : Nonleaf_Pack_State_Typ ; 

    BEGIN -- Recurse_Whole_1  
      Assertions . Assert ( Rel_From_Ss <= Rel_Thru_Ss ) ; 
      Assertions . Assert ( K_Tree /= NULL ) ; 
      IF K_Tree . ALL . Height = 1 
      THEN -- Leaf level. 
        IF Rel_From_Ss = 0 
           AND THEN Rel_Thru_Ss + 1 = K_Tree . ALL . Elem_Ct 
        THEN -- Return the node unchanged. 
          RETURN ; 
        ELSE -- Must repack. 
          L_New_Elem_Ct := Rel_Thru_Ss - Rel_From_Ss + 1 ; 
          Init_Repack_Leaf ( L_Leaf_Pack_State , L_New_Elem_Ct ) ; 
          FOR I IN Rel_From_Ss .. Rel_Thru_Ss 
          LOOP -- FOR  
            Repack_Leaf_Elem 
              ( L_Leaf_Pack_State , K_Tree . ALL . Leaf_Elems ( I ) ) ; 
          END LOOP ; -- FOR  
          Finalize_Repack_Leaf ( L_Leaf_Pack_State ) ; 
          K_Tree := L_Leaf_Pack_State . Lps_New_K_Tree_1 ; 
          Assertions . Assert 
            ( L_Leaf_Pack_State . Lps_New_K_Tree_2 = NULL ) ; 
          Assertions . Assert 
            ( L_Leaf_Pack_State . Lps_New_K_Tree_3 = NULL ) ; 
        END IF ; 
      ELSE -- Nonleaf level. 
        IF Rel_From_Ss = 0 
           AND THEN 
             Rel_Thru_Ss + 1 
             = K_Tree . ALL . Nonleaf_Elems 
                 ( K_Tree . ALL . Elem_Ct - 1 ) 
                 . Nle_Cum_Child_Ct 
        THEN -- Return the node unchanged. 
          RETURN ; 
        ELSE -- Will have to repack regardless of what happens below. 
          Bin_Search 
            ( K_Tree , Rel_From_Ss , L_From_Elem_No , L_Child_From_Ss ) ; 
          Bin_Search 
            ( K_Tree , 
              Rel_Thru_Ss , 
              L_Thru_Elem_No , 
              L_Child_Thru_Ss , 
              Leftmost_Elem_No => L_From_Elem_No 
            ) ; 
          L_Old_Elem_Ct := L_Thru_Elem_No - L_From_Elem_No + 1 ; 
          CASE L_Old_Elem_Ct IS 

          WHEN 1 
          => L_Child_K_Tree_1 
               := K_Tree . ALL . Nonleaf_Elems 
                    ( L_From_Elem_No ) 
                    . Nle_Child_Ref ; 
             Recurse_Whole_1 
               ( L_Child_From_Ss , L_Child_Thru_Ss , L_Child_K_Tree_1 ) ; 
             K_Tree := L_Child_K_Tree_1 ; 

          WHEN 2 
          => L_Child_K_Tree_1 
               := K_Tree . ALL . Nonleaf_Elems 
                    ( L_From_Elem_No ) 
                    . Nle_Child_Ref ; 
             L_Child_K_Tree_2 
               := K_Tree . ALL . Nonleaf_Elems 
                    ( L_Thru_Elem_No ) 
                    . Nle_Child_Ref ; 
             Recurse_Whole_2 
               ( L_Child_From_Ss , 
                 L_Child_Thru_Ss , 
                 L_Child_K_Tree_1 , 
                 L_Child_K_Tree_2 
               ) ; 
             IF L_Child_K_Tree_2 = NULL 
             THEN 
               K_Tree := L_Child_K_Tree_1 ; 
             ELSE 
               Init_Repack_Nonleaf 
                 ( L_Nonleaf_Pack_State , 2 , K_Tree . ALL . Height ) ; 
               Repack_Nonleaf_Elem 
                 ( L_Nonleaf_Pack_State , L_Child_K_Tree_1 ) ; 
               Repack_Nonleaf_Elem 
                 ( L_Nonleaf_Pack_State , L_Child_K_Tree_2 ) ; 
               Finalize_Repack_Nonleaf ( L_Nonleaf_Pack_State ) ; 
               K_Tree := L_Nonleaf_Pack_State . Nlps_New_K_Tree_1 ; 
               Assertions . Assert 
                 ( L_Nonleaf_Pack_State . Nlps_New_K_Tree_2 = NULL , 
                   "Slice.Recurse_Whole_1, 2, non_null New_K_Tree_2." 
                 ) ; 
               Assertions . Assert 
                 ( L_Nonleaf_Pack_State . Nlps_New_K_Tree_3 = NULL , 
                   "Slice.Recurse_Whole_1, 2, non_null New_K_Tree_3." 
                 ) ; 
             END IF ; 

          WHEN 3 
          => L_Child_K_Tree_1 
               := K_Tree . ALL . Nonleaf_Elems 
                    ( L_From_Elem_No ) 
                    . Nle_Child_Ref ; 
             L_Child_K_Tree_2 
               := K_Tree . ALL . Nonleaf_Elems 
                    ( L_From_Elem_No + 1 ) 
                    . Nle_Child_Ref ; 
             L_Child_K_Tree_3 
               := K_Tree . ALL . Nonleaf_Elems 
                    ( L_Thru_Elem_No ) 
                    . Nle_Child_Ref ; 
             Recurse_Whole_3 
               ( L_Child_From_Ss , 
                 L_Child_Thru_Ss , 
                 L_Child_K_Tree_1 , 
                 L_Child_K_Tree_2 , 
                 L_Child_K_Tree_3 
               ) ; 
             IF L_Child_K_Tree_2 = NULL 
             THEN 
               Assertions . Assert ( L_Child_K_Tree_3 = NULL ) ; 
               K_Tree := L_Child_K_Tree_1 ; 
             ELSE 
               L_New_Elem_Ct 
                 := 2 + BOOLEAN ' Pos ( L_Child_K_Tree_3 /= NULL ) ; 
               Init_Repack_Nonleaf 
                 ( L_Nonleaf_Pack_State , 
                   L_New_Elem_Ct , 
                   K_Tree . ALL . Height 
                 ) ; 
               Repack_Nonleaf_Elem 
                 ( L_Nonleaf_Pack_State , L_Child_K_Tree_1 ) ; 
               Repack_Nonleaf_Elem 
                 ( L_Nonleaf_Pack_State , L_Child_K_Tree_2 ) ; 
               Repack_Nonleaf_Elem 
                 ( L_Nonleaf_Pack_State , L_Child_K_Tree_3 ) ; 
               Finalize_Repack_Nonleaf ( L_Nonleaf_Pack_State ) ; 
               K_Tree := L_Nonleaf_Pack_State . Nlps_New_K_Tree_1 ; 
               Assertions . Assert 
                 ( L_Nonleaf_Pack_State . Nlps_New_K_Tree_2 = NULL , 
                   "Slice.Recurse_Whole_1, 3, non_null New_K_Tree_2." 
                 ) ; 
               Assertions . Assert 
                 ( L_Nonleaf_Pack_State . Nlps_New_K_Tree_3 = NULL , 
                   "Slice.Recurse_Whole_1, 3, non_null New_K_Tree_3." 
                 ) ; 
               Assertions . Assert 
                 ( L_Nonleaf_Pack_State . Nlps_New_K_Tree_2 = NULL ) ; 
             END IF ; 

          WHEN OTHERS 
          => L_Child_K_Tree_1 
               := K_Tree . ALL . Nonleaf_Elems 
                    ( L_From_Elem_No ) 
                    . Nle_Child_Ref ; 
             L_Child_K_Tree_2 
               := K_Tree . ALL . Nonleaf_Elems 
                    ( L_From_Elem_No + 1 ) 
                    . Nle_Child_Ref ; 
             L_Child_K_Tree_3 
               := K_Tree . ALL . Nonleaf_Elems 
                    ( L_Thru_Elem_No - 1 ) 
                    . Nle_Child_Ref ; 
             L_Child_K_Tree_4 
               := K_Tree . ALL . Nonleaf_Elems 
                    ( L_Thru_Elem_No ) 
                    . Nle_Child_Ref ; 
             Recurse_Left 
               ( L_Child_From_Ss , L_Child_K_Tree_1 , L_Child_K_Tree_2 ) ; 
             Recurse_Right 
               ( L_Child_Thru_Ss , L_Child_K_Tree_3 , L_Child_K_Tree_4 ) ; 
             L_New_Elem_Ct 
               := 2 
                  + BOOLEAN ' Pos ( L_Child_K_Tree_2 /= NULL ) 
                  + BOOLEAN ' Pos ( L_Child_K_Tree_4 /= NULL ) 
                  + For_Range 
                      ( L_From_Elem_No + 2 , L_Thru_Elem_No - 2 ) ; 
             Init_Repack_Nonleaf 
               ( L_Nonleaf_Pack_State , 
                 L_New_Elem_Ct , 
                 K_Tree . ALL . Height 
               ) ; 
             Repack_Nonleaf_Elem 
               ( L_Nonleaf_Pack_State , L_Child_K_Tree_1 ) ; 
             Repack_Nonleaf_Elem 
               ( L_Nonleaf_Pack_State , L_Child_K_Tree_2 ) ; 
             FOR I IN L_From_Elem_No + 2 .. L_Thru_Elem_No - 2 
             LOOP -- FOR  
               Repack_Nonleaf_Elem 
                 ( L_Nonleaf_Pack_State , 
                   K_Tree . ALL . Nonleaf_Elems ( I ) . Nle_Child_Ref 
                 ) ; 
             END LOOP ; -- FOR  
             Repack_Nonleaf_Elem 
               ( L_Nonleaf_Pack_State , L_Child_K_Tree_3 ) ; 
             Repack_Nonleaf_Elem 
               ( L_Nonleaf_Pack_State , L_Child_K_Tree_4 ) ; 
             Finalize_Repack_Nonleaf ( L_Nonleaf_Pack_State ) ; 
             K_Tree := L_Nonleaf_Pack_State . Nlps_New_K_Tree_1 ; 
             Assertions . Assert 
               ( L_Nonleaf_Pack_State . Nlps_New_K_Tree_2 = NULL , 
                 "Slice.Recurse_Whole_1, >=4, non_null New_K_Tree_2." 
               ) ; 
             Assertions . Assert 
               ( L_Nonleaf_Pack_State . Nlps_New_K_Tree_3 = NULL , 
                 "Slice.Recurse_Whole_1, >=4, non_null New_K_Tree_3." 
               ) ; 

          END CASE ; 
        END IF ; 
      END IF ; 
    END Recurse_Whole_1 ; 

    PROCEDURE Recurse_Whole_2 
      ( Rel_From_Ss , Rel_Thru_Ss : IN Seq_Ss_Typ ; 
        K_Tree_1 , K_Tree_2 : IN OUT T 
      ) 

    IS 

      L_Old_Elem_Ct : Elem_No_Typ ; 
      L_New_Elem_Ct : Elem_No_Typ ; 
      L_From_Elem_No , L_Thru_Elem_No : Elem_No_Typ ; 
      L_Middle_Elem_No_1 , L_Middle_Elem_No_2 : Elem_No_Typ ; 
      L_Child_From_Ss , L_Child_Thru_Ss : Seq_Ss_Typ ; 
      L_Child_K_Tree_1 , L_Child_K_Tree_2 , L_Child_K_Tree_3 , 
      L_Child_K_Tree_4 
        : T ; 

      L_Leaf_Pack_State : Leaf_Pack_State_Typ ; 
      L_Nonleaf_Pack_State : Nonleaf_Pack_State_Typ ; 

    BEGIN -- Recurse_Whole_2  
      Assertions . Assert ( K_Tree_1 /= NULL ) ; 
      Assertions . Assert ( K_Tree_2 /= NULL ) ; 
      IF K_Tree_1 . ALL . Height = 1 
      THEN -- Leaf level. 
        IF Rel_From_Ss = 0 
           AND THEN Rel_Thru_Ss + 1 = K_Tree_2 . ALL . Elem_Ct 
        THEN -- Return the two nodes unchanged. 
          RETURN ; 
        ELSE -- Must repack. 
          L_New_Elem_Ct 
            := ( K_Tree_1 . ALL . Elem_Ct - Rel_From_Ss ) 
               + ( Rel_Thru_Ss + 1 ) ; 
          Init_Repack_Leaf ( L_Leaf_Pack_State , L_New_Elem_Ct ) ; 
          FOR I IN Rel_From_Ss .. K_Tree_1 . ALL . Elem_Ct - 1 
          LOOP -- FOR  
            Repack_Leaf_Elem 
              ( L_Leaf_Pack_State , K_Tree_1 . ALL . Leaf_Elems ( I ) ) ; 
          END LOOP ; -- FOR  
          FOR I IN 0 .. Rel_Thru_Ss 
          LOOP -- FOR  
            Repack_Leaf_Elem 
              ( L_Leaf_Pack_State , K_Tree_2 . ALL . Leaf_Elems ( I ) ) ; 
          END LOOP ; -- FOR  
          Finalize_Repack_Leaf ( L_Leaf_Pack_State ) ; 
          K_Tree_1 := L_Leaf_Pack_State . Lps_New_K_Tree_1 ; 
          K_Tree_2 := L_Leaf_Pack_State . Lps_New_K_Tree_2 ; 
          Assertions . Assert 
            ( L_Leaf_Pack_State . Lps_New_K_Tree_3 = NULL ) ; 
        END IF ; 
      ELSE -- Nonleaf level. 
        IF Rel_From_Ss = 0 
           AND THEN 
             Rel_Thru_Ss + 1 
             = K_Tree_2 . ALL . Nonleaf_Elems 
                 ( K_Tree_2 . ALL . Elem_Ct - 1 ) 
                 . Nle_Cum_Child_Ct 
        THEN -- Return the two nodes unchanged. 
          RETURN ; 
        ELSE -- Will repack regardless of what happens below. 
          Bin_Search 
            ( K_Tree_1 , 
              Rel_From_Ss , 
              L_From_Elem_No , 
              L_Child_From_Ss 
            ) ; 
          Bin_Search 
            ( K_Tree_2 , 
              Rel_Thru_Ss , 
              L_Thru_Elem_No , 
              L_Child_Thru_Ss 
            ) ; 
          L_Old_Elem_Ct 
            := ( K_Tree_1 . ALL . Elem_Ct - L_From_Elem_No ) 
               + ( L_Thru_Elem_No + 1 ) ; 
          CASE L_Old_Elem_Ct IS 

          WHEN 1 
          => Assertions . Cant_Happen ; 

          WHEN 2 
          => Assertions . Assert 
               ( L_From_Elem_No = K_Tree_1 . ALL . Elem_Ct - 1 ) ; 
             L_Child_K_Tree_1 
               := K_Tree_1 . ALL . Nonleaf_Elems 
                    ( L_From_Elem_No ) 
                    . Nle_Child_Ref ; 
             Assertions . Assert ( L_Thru_Elem_No = 0 ) ; 
             L_Child_K_Tree_2 
               := K_Tree_2 . ALL . Nonleaf_Elems ( 0 ) . Nle_Child_Ref ; 
             Recurse_Whole_2 
               ( L_Child_From_Ss , 
                 L_Child_Thru_Ss , 
                 L_Child_K_Tree_1 , 
                 L_Child_K_Tree_2 
               ) ; 
             IF L_Child_K_Tree_2 = NULL 
             THEN 
               K_Tree_1 := L_Child_K_Tree_1 ; 
               K_Tree_2 := NULL ; 
             ELSE 
               Init_Repack_Nonleaf 
                 ( L_Nonleaf_Pack_State , 2 , K_Tree_1 . ALL . Height ) ; 
               Repack_Nonleaf_Elem 
                 ( L_Nonleaf_Pack_State , L_Child_K_Tree_1 ) ; 
               Repack_Nonleaf_Elem 
                 ( L_Nonleaf_Pack_State , L_Child_K_Tree_2 ) ; 
               Finalize_Repack_Nonleaf ( L_Nonleaf_Pack_State ) ; 
               K_Tree_1 := L_Nonleaf_Pack_State . Nlps_New_K_Tree_1 ; 
               Assertions . Assert 
                 ( L_Nonleaf_Pack_State . Nlps_New_K_Tree_2 = NULL , 
                   "Slice.Recurse_Whole_2, 2, non_null New_K_Tree_2." 
                 ) ; 
               Assertions . Assert 
                 ( L_Nonleaf_Pack_State . Nlps_New_K_Tree_3 = NULL , 
                   "Slice.Recurse_Whole_2, 2, non_null New_K_Tree_3." 
                 ) ; 
               K_Tree_2 := NULL ; 
             END IF ; 

          WHEN 3 
          => L_Child_K_Tree_1 
               := K_Tree_1 . ALL . Nonleaf_Elems 
                    ( L_From_Elem_No ) 
                    . Nle_Child_Ref ; 
             IF L_From_Elem_No < K_Tree_1 . ALL . Elem_Ct - 1 
             THEN 
               L_Child_K_Tree_2 
                 := K_Tree_1 . ALL . Nonleaf_Elems 
                      ( L_From_Elem_No + 1 ) 
                      . Nle_Child_Ref ; 
             ELSE 
               Assertions . Assert ( L_Thru_Elem_No = 1 ) ; 
               L_Child_K_Tree_2 
                 := K_Tree_2 . ALL . Nonleaf_Elems 
                      ( 0 ) 
                      . Nle_Child_Ref ; 
             END IF ; 
             L_Child_K_Tree_3 
               := K_Tree_2 . ALL . Nonleaf_Elems 
                    ( L_Thru_Elem_No ) 
                    . Nle_Child_Ref ; 
             Recurse_Whole_3 
               ( L_Child_From_Ss , 
                 L_Child_Thru_Ss , 
                 L_Child_K_Tree_1 , 
                 L_Child_K_Tree_2 , 
                 L_Child_K_Tree_3 
               ) ; 
             IF L_Child_K_Tree_2 = NULL 
             THEN 
               Assertions . Assert ( L_Child_K_Tree_3 = NULL ) ; 
               K_Tree_1 := L_Child_K_Tree_1 ; 
               K_Tree_2 := NULL ; 
             ELSE 
               L_New_Elem_Ct 
                 := 2 + BOOLEAN ' Pos ( L_Child_K_Tree_3 /= NULL ) ; 
               Init_Repack_Nonleaf 
                 ( L_Nonleaf_Pack_State , 
                   L_New_Elem_Ct , 
                   K_Tree_1 . ALL . Height 
                 ) ; 
               Repack_Nonleaf_Elem 
                 ( L_Nonleaf_Pack_State , L_Child_K_Tree_1 ) ; 
               Repack_Nonleaf_Elem 
                 ( L_Nonleaf_Pack_State , L_Child_K_Tree_2 ) ; 
               Repack_Nonleaf_Elem 
                 ( L_Nonleaf_Pack_State , L_Child_K_Tree_3 ) ; 
               Finalize_Repack_Nonleaf ( L_Nonleaf_Pack_State ) ; 
               K_Tree_1 := L_Nonleaf_Pack_State . Nlps_New_K_Tree_1 ; 
               Assertions . Assert 
                 ( L_Nonleaf_Pack_State . Nlps_New_K_Tree_2 = NULL , 
                   "Slice.Recurse_Whole_2, 3, non_null New_K_Tree_2." 
                 ) ; 
               Assertions . Assert 
                 ( L_Nonleaf_Pack_State . Nlps_New_K_Tree_3 = NULL , 
                   "Slice.Recurse_Whole_2, 3, non_null New_K_Tree_3." 
                 ) ; 
               K_Tree_2 := NULL ; 
             END IF ; 

          WHEN OTHERS 
          => L_Child_K_Tree_1 
               := K_Tree_1 . ALL . Nonleaf_Elems 
                    ( L_From_Elem_No ) 
                    . Nle_Child_Ref ; 
             IF L_From_Elem_No + 1 < K_Tree_1 . ALL . Elem_Ct 
             THEN 
               L_Child_K_Tree_2 
                 := K_Tree_1 . ALL . Nonleaf_Elems 
                      ( L_From_Elem_No + 1 ) 
                      . Nle_Child_Ref ; 
               L_Middle_Elem_No_2 := 0 ; 
             ELSE 
               L_Child_K_Tree_2 
                 := K_Tree_2 . ALL . Nonleaf_Elems 
                      ( 0 ) 
                      . Nle_Child_Ref ; 
               L_Middle_Elem_No_2 := 1 ; 
             END IF ; 
             IF L_Thru_Elem_No = 0 
             THEN 
               L_Child_K_Tree_3 
                 := K_Tree_1 . ALL . Nonleaf_Elems 
                      ( K_Tree_1 . ALL . Elem_Ct - 1 ) 
                      . Nle_Child_Ref ; 
               L_Middle_Elem_No_1 := K_Tree_1 . ALL . Elem_Ct - 2 ; 
             ELSE 
               L_Child_K_Tree_3 
                 := K_Tree_2 . ALL . Nonleaf_Elems 
                      ( L_Thru_Elem_No - 1 ) 
                      . Nle_Child_Ref ; 
               L_Middle_Elem_No_1 := K_Tree_1 . ALL . Elem_Ct - 1 ; 
             END IF ; 
             L_Child_K_Tree_4 
               := K_Tree_2 . ALL . Nonleaf_Elems 
                    ( L_Thru_Elem_No ) 
                    . Nle_Child_Ref ; 
             Recurse_Left 
               ( L_Child_From_Ss , L_Child_K_Tree_1 , L_Child_K_Tree_2 ) ; 
             Recurse_Right 
               ( L_Child_Thru_Ss , L_Child_K_Tree_3 , L_Child_K_Tree_4 ) ; 
             L_New_Elem_Ct 
               := 2 
                  + BOOLEAN ' Pos ( L_Child_K_Tree_2 /= NULL ) 
                  + BOOLEAN ' Pos ( L_Child_K_Tree_4 /= NULL ) 
                  + For_Range 
                      ( L_From_Elem_No + 2 , L_Middle_Elem_No_1 ) 
                  + For_Range 
                      ( L_Middle_Elem_No_2 , L_Thru_Elem_No - 2 ) ; 
             Init_Repack_Nonleaf 
               ( L_Nonleaf_Pack_State , 
                 L_New_Elem_Ct , 
                 K_Tree_1 . ALL . Height 
               ) ; 
             Repack_Nonleaf_Elem 
               ( L_Nonleaf_Pack_State , L_Child_K_Tree_1 ) ; 
             Repack_Nonleaf_Elem 
               ( L_Nonleaf_Pack_State , L_Child_K_Tree_2 ) ; 
             FOR I IN L_From_Elem_No + 2 .. L_Middle_Elem_No_1 
             LOOP -- FOR  
               Repack_Nonleaf_Elem 
                 ( L_Nonleaf_Pack_State , 
                   K_Tree_1 . ALL . Nonleaf_Elems 
                     ( I ) 
                     . Nle_Child_Ref 
                 ) ; 
             END LOOP ; -- FOR  
             FOR I IN L_Middle_Elem_No_2 .. L_Thru_Elem_No - 2 
             LOOP -- FOR  
               Repack_Nonleaf_Elem 
                 ( L_Nonleaf_Pack_State , 
                   K_Tree_2 . ALL . Nonleaf_Elems 
                     ( I ) 
                     . Nle_Child_Ref 
                 ) ; 
             END LOOP ; -- FOR  
             Repack_Nonleaf_Elem 
               ( L_Nonleaf_Pack_State , L_Child_K_Tree_3 ) ; 
             Repack_Nonleaf_Elem 
               ( L_Nonleaf_Pack_State , L_Child_K_Tree_4 ) ; 
             Finalize_Repack_Nonleaf ( L_Nonleaf_Pack_State ) ; 
             K_Tree_1 := L_Nonleaf_Pack_State . Nlps_New_K_Tree_1 ; 
             K_Tree_2 := L_Nonleaf_Pack_State . Nlps_New_K_Tree_2 ; 
             Assertions . Assert 
               ( L_Nonleaf_Pack_State . Nlps_New_K_Tree_3 = NULL , 
                 "Slice.Recurse_Whole_2, >=4, non_null New_K_Tree_3." 
               ) ; 

          END CASE ; 
        END IF ; 
      END IF ; 
    END Recurse_Whole_2 ; 

    PROCEDURE Recurse_Whole_3 
      ( Rel_From_Ss , Rel_Thru_Ss : IN Seq_Ss_Typ ; 
        K_Tree_1 , K_Tree_2 , K_Tree_3 : IN OUT T 
      ) 

    IS 

      L_New_Elem_Ct : Elem_No_Typ ; 
      L_From_Elem_No , L_Thru_Elem_No : Elem_No_Typ ; 
      L_Middle_Elem_No_1 , L_Middle_Elem_No_2 : Elem_No_Typ ; 
      L_Child_From_Ss , L_Child_Thru_Ss : Seq_Ss_Typ ; 
      L_Child_K_Tree_1 , L_Child_K_Tree_2 , L_Child_K_Tree_3 , 
      L_Child_K_Tree_4 
        : T ; 

      L_Leaf_Pack_State : Leaf_Pack_State_Typ ; 
      L_Nonleaf_Pack_State : Nonleaf_Pack_State_Typ ; 

    BEGIN -- Recurse_Whole_3  
      Assertions . Assert ( K_Tree_1 /= NULL ) ; 
      Assertions . Assert ( K_Tree_2 /= NULL ) ; 
      Assertions . Assert ( K_Tree_3 /= NULL ) ; 
      IF K_Tree_1 . ALL . Height = 1 
      THEN -- Leaf level. 
        IF Rel_From_Ss = 0 
           AND THEN Rel_Thru_Ss + 1 = K_Tree_3 . ALL . Elem_Ct 
        THEN -- Return the three nodes unchanged. 
          RETURN ; 
        ELSE -- Must repack. 
          L_New_Elem_Ct 
            := ( K_Tree_1 . ALL . Elem_Ct - Rel_From_Ss ) 
               + K_Tree_2 . ALL . Elem_Ct 
               + Rel_Thru_Ss 
               + 1 ; 
          Init_Repack_Leaf ( L_Leaf_Pack_State , L_New_Elem_Ct ) ; 
          FOR I IN Rel_From_Ss .. K_Tree_1 . ALL . Elem_Ct - 1 
          LOOP -- FOR  
            Repack_Leaf_Elem 
              ( L_Leaf_Pack_State , K_Tree_1 . ALL . Leaf_Elems ( I ) ) ; 
          END LOOP ; -- FOR  
          FOR I IN 0 .. K_Tree_2 . ALL . Elem_Ct - 1 
          LOOP -- FOR  
            Repack_Leaf_Elem 
              ( L_Leaf_Pack_State , K_Tree_2 . ALL . Leaf_Elems ( I ) ) ; 
          END LOOP ; -- FOR  
          FOR I IN 0 .. Rel_Thru_Ss 
          LOOP -- FOR  
            Repack_Leaf_Elem 
              ( L_Leaf_Pack_State , K_Tree_3 . ALL . Leaf_Elems ( I ) ) ; 
          END LOOP ; -- FOR  
          Finalize_Repack_Leaf ( L_Leaf_Pack_State ) ; 
          K_Tree_1 := L_Leaf_Pack_State . Lps_New_K_Tree_1 ; 
          K_Tree_2 := L_Leaf_Pack_State . Lps_New_K_Tree_2 ; 
          K_Tree_3 := L_Leaf_Pack_State . Lps_New_K_Tree_3 ; 
        END IF ; 
      ELSE -- Nonleaf level. 
        IF Rel_From_Ss = 0 
           AND THEN 
             Rel_Thru_Ss + 1 
             = K_Tree_3 . ALL . Nonleaf_Elems 
                 ( K_Tree_3 . ALL . Elem_Ct - 1 ) 
                 . Nle_Cum_Child_Ct 
        THEN -- Return the three nodes unchanged. 
          RETURN ; 
        ELSE -- Will repack regardless of what happens below. 
          Bin_Search 
            ( K_Tree_1 , 
              Rel_From_Ss , 
              L_From_Elem_No , 
              L_Child_From_Ss 
            ) ; 
          Bin_Search 
            ( K_Tree_3 , 
              Rel_Thru_Ss , 
              L_Thru_Elem_No , 
              L_Child_Thru_Ss 
            ) ; 
          L_Child_K_Tree_1 
            := K_Tree_1 . ALL . Nonleaf_Elems 
                 ( L_From_Elem_No ) 
                 . Nle_Child_Ref ; 
          IF L_From_Elem_No + 1 < K_Tree_1 . ALL . Elem_Ct 
          THEN 
            L_Child_K_Tree_2 
              := K_Tree_1 . ALL . Nonleaf_Elems 
                   ( L_From_Elem_No + 1 ) 
                   . Nle_Child_Ref ; 
            L_Middle_Elem_No_1 := 0 ; 
          ELSE 
            L_Child_K_Tree_2 
              := K_Tree_2 . ALL . Nonleaf_Elems ( 0 ) . Nle_Child_Ref ; 
            L_Middle_Elem_No_1 := 1 ; 
          END IF ; 
          IF L_Thru_Elem_No = 0 
          THEN 
            L_Child_K_Tree_3 
              := K_Tree_2 . ALL . Nonleaf_Elems 
                   ( K_Tree_2 . ALL . Elem_Ct - 1 ) 
                   . Nle_Child_Ref ; 
            L_Middle_Elem_No_2 := K_Tree_2 . ALL . Elem_Ct - 2 ; 
          ELSE 
            L_Child_K_Tree_3 
              := K_Tree_3 . ALL . Nonleaf_Elems 
                   ( L_Thru_Elem_No - 1 ) 
                   . Nle_Child_Ref ; 
            L_Middle_Elem_No_2 := K_Tree_2 . ALL . Elem_Ct - 1 ; 
          END IF ; 
          L_Child_K_Tree_4 
            := K_Tree_3 . ALL . Nonleaf_Elems 
                 ( L_Thru_Elem_No ) 
                 . Nle_Child_Ref ; 
          Recurse_Left 
            ( L_Child_From_Ss , L_Child_K_Tree_1 , L_Child_K_Tree_2 ) ; 
          Recurse_Right 
            ( L_Child_Thru_Ss , L_Child_K_Tree_3 , L_Child_K_Tree_4 ) ; 
          L_New_Elem_Ct 
            := 2 
               + BOOLEAN ' Pos ( L_Child_K_Tree_2 /= NULL ) 
               + BOOLEAN ' Pos ( L_Child_K_Tree_4 /= NULL ) 
               + For_Range 
                   ( L_From_Elem_No + 2 , K_Tree_1 . ALL . Elem_Ct - 1 ) 
               + For_Range ( L_Middle_Elem_No_1 , L_Middle_Elem_No_2 ) 
               + For_Range ( 0 , L_Thru_Elem_No - 2 ) ; 
          Init_Repack_Nonleaf 
            ( L_Nonleaf_Pack_State , 
              L_New_Elem_Ct , 
              K_Tree_1 . ALL . Height 
            ) ; 
          Repack_Nonleaf_Elem 
            ( L_Nonleaf_Pack_State , L_Child_K_Tree_1 ) ; 
          Repack_Nonleaf_Elem 
            ( L_Nonleaf_Pack_State , L_Child_K_Tree_2 ) ; 
          FOR I IN L_From_Elem_No + 2 .. K_Tree_1 . ALL . Elem_Ct - 1 
          LOOP -- FOR  
            Repack_Nonleaf_Elem 
              ( L_Nonleaf_Pack_State , 
                K_Tree_1 . ALL . Nonleaf_Elems ( I ) . Nle_Child_Ref 
              ) ; 
          END LOOP ; -- FOR  
          FOR I IN L_Middle_Elem_No_1 .. L_Middle_Elem_No_2 
          LOOP -- FOR  
            Repack_Nonleaf_Elem 
              ( L_Nonleaf_Pack_State , 
                K_Tree_2 . ALL . Nonleaf_Elems ( I ) . Nle_Child_Ref 
              ) ; 
          END LOOP ; -- FOR  
          FOR I IN 0 .. L_Thru_Elem_No - 2 
          LOOP -- FOR  
            Repack_Nonleaf_Elem 
              ( L_Nonleaf_Pack_State , 
                K_Tree_3 . ALL . Nonleaf_Elems ( I ) . Nle_Child_Ref 
              ) ; 
          END LOOP ; -- FOR  
          Repack_Nonleaf_Elem 
            ( L_Nonleaf_Pack_State , L_Child_K_Tree_3 ) ; 
          Repack_Nonleaf_Elem 
            ( L_Nonleaf_Pack_State , L_Child_K_Tree_4 ) ; 
          Finalize_Repack_Nonleaf ( L_Nonleaf_Pack_State ) ; 
          K_Tree_1 := L_Nonleaf_Pack_State . Nlps_New_K_Tree_1 ; 
          K_Tree_2 := L_Nonleaf_Pack_State . Nlps_New_K_Tree_2 ; 
          K_Tree_3 := L_Nonleaf_Pack_State . Nlps_New_K_Tree_3 ; 

        END IF ; 
      END IF ; 
    END Recurse_Whole_3 ; 

    PROCEDURE Recurse_Left 
      ( Rel_From_Ss : IN Seq_Ss_Typ ; K_Tree_1 , K_Tree_2 : IN OUT T ) 

    IS 

      L_New_Elem_Ct : Elem_No_Typ ; 
      L_From_Elem_No : Elem_No_Typ ; 
      L_Middle_Elem_No : Elem_No_Typ ; 
      L_Child_From_Ss : Seq_Ss_Typ ; 
      L_Child_K_Tree_1 , L_Child_K_Tree_2 : T ; 

      L_Leaf_Pack_State : Leaf_Pack_State_Typ ; 
      L_Nonleaf_Pack_State : Nonleaf_Pack_State_Typ ; 

    BEGIN -- Recurse_Left  
      Assertions . Assert ( K_Tree_1 /= NULL ) ; 
      Assertions . Assert ( K_Tree_2 /= NULL ) ; 
      IF K_Tree_1 . ALL . Height = 1 
      THEN -- Leaf level. 
        IF Rel_From_Ss = 0 
        THEN -- Return the two nodes unchanged. 
          RETURN ; 
        ELSE -- Must repack. 
          L_New_Elem_Ct 
            := ( K_Tree_1 . ALL . Elem_Ct - Rel_From_Ss ) 
               + K_Tree_2 . ALL . Elem_Ct ; 
          Init_Repack_Leaf ( L_Leaf_Pack_State , L_New_Elem_Ct ) ; 
          FOR I IN Rel_From_Ss .. K_Tree_1 . ALL . Elem_Ct - 1 
          LOOP -- FOR  
            Repack_Leaf_Elem 
              ( L_Leaf_Pack_State , K_Tree_1 . ALL . Leaf_Elems ( I ) ) ; 
          END LOOP ; -- FOR  
          FOR I IN 0 .. K_Tree_2 . ALL . Elem_Ct - 1 
          LOOP -- FOR  
            Repack_Leaf_Elem 
              ( L_Leaf_Pack_State , K_Tree_2 . ALL . Leaf_Elems ( I ) ) ; 
          END LOOP ; -- FOR  
          Finalize_Repack_Leaf ( L_Leaf_Pack_State ) ; 
          K_Tree_1 := L_Leaf_Pack_State . Lps_New_K_Tree_1 ; 
          K_Tree_2 := L_Leaf_Pack_State . Lps_New_K_Tree_2 ; 
          Assertions . Assert 
            ( L_Leaf_Pack_State . Lps_New_K_Tree_3 = NULL ) ; 
        END IF ; 
      ELSE -- Nonleaf level. 
        IF Rel_From_Ss = 0 
        THEN -- Return the two nodes unchanged. 
          RETURN ; 
        ELSE -- Will repack regardless of what happens below. 
          Bin_Search 
            ( K_Tree_1 , 
              Rel_From_Ss , 
              L_From_Elem_No , 
              L_Child_From_Ss 
            ) ; 
          L_Child_K_Tree_1 
            := K_Tree_1 . ALL . Nonleaf_Elems 
                 ( L_From_Elem_No ) 
                 . Nle_Child_Ref ; 
          IF L_From_Elem_No + 1 < K_Tree_1 . ALL . Elem_Ct 
          THEN 
            L_Child_K_Tree_2 
              := K_Tree_1 . ALL . Nonleaf_Elems 
                   ( L_From_Elem_No + 1 ) 
                   . Nle_Child_Ref ; 
            L_Middle_Elem_No := 0 ; 
          ELSE 
            L_Child_K_Tree_2 
              := K_Tree_2 . ALL . Nonleaf_Elems ( 0 ) . Nle_Child_Ref ; 
            L_Middle_Elem_No := 1 ; 
          END IF ; 
          Recurse_Left 
            ( L_Child_From_Ss , L_Child_K_Tree_1 , L_Child_K_Tree_2 ) ; 
          L_New_Elem_Ct 
            := 1 
               + BOOLEAN ' Pos ( L_Child_K_Tree_2 /= NULL ) 
               + For_Range 
                   ( L_From_Elem_No + 2 , K_Tree_1 . ALL . Elem_Ct - 1 ) 
               + For_Range 
                   ( L_Middle_Elem_No , K_Tree_2 . ALL . Elem_Ct - 1 ) ; 
          Init_Repack_Nonleaf 
            ( L_Nonleaf_Pack_State , 
              L_New_Elem_Ct , 
              K_Tree_1 . ALL . Height 
            ) ; 
          Repack_Nonleaf_Elem 
            ( L_Nonleaf_Pack_State , L_Child_K_Tree_1 ) ; 
          Repack_Nonleaf_Elem 
            ( L_Nonleaf_Pack_State , L_Child_K_Tree_2 ) ; 
          FOR I IN L_From_Elem_No + 2 .. K_Tree_1 . ALL . Elem_Ct - 1 
          LOOP -- FOR  
            Repack_Nonleaf_Elem 
              ( L_Nonleaf_Pack_State , 
                K_Tree_1 . ALL . Nonleaf_Elems ( I ) . Nle_Child_Ref 
              ) ; 
          END LOOP ; -- FOR  
          FOR I IN L_Middle_Elem_No .. K_Tree_2 . ALL . Elem_Ct - 1 
          LOOP -- FOR  
            Repack_Nonleaf_Elem 
              ( L_Nonleaf_Pack_State , 
                K_Tree_2 . ALL . Nonleaf_Elems ( I ) . Nle_Child_Ref 
              ) ; 
          END LOOP ; -- FOR  
          Finalize_Repack_Nonleaf ( L_Nonleaf_Pack_State ) ; 
          K_Tree_1 := L_Nonleaf_Pack_State . Nlps_New_K_Tree_1 ; 
          K_Tree_2 := L_Nonleaf_Pack_State . Nlps_New_K_Tree_2 ; 
        END IF ; 
      END IF ; 
    END Recurse_Left ; 

    PROCEDURE Recurse_Right 
      ( Rel_Thru_Ss : IN Seq_Ss_Typ ; K_Tree_1 , K_Tree_2 : IN OUT T ) 

    IS 

      L_New_Elem_Ct : Elem_No_Typ ; 
      L_Thru_Elem_No : Elem_No_Typ ; 
      L_Middle_Elem_No : Elem_No_Typ ; 
      L_Child_Thru_Ss : Seq_Ss_Typ ; 
      L_Child_K_Tree_1 , L_Child_K_Tree_2 : T ; 

      L_Leaf_Pack_State : Leaf_Pack_State_Typ ; 
      L_Nonleaf_Pack_State : Nonleaf_Pack_State_Typ ; 

    BEGIN -- Recurse_Right  
      Assertions . Assert ( K_Tree_1 /= NULL ) ; 
      Assertions . Assert ( K_Tree_2 /= NULL ) ; 
      IF K_Tree_1 . ALL . Height = 1 
      THEN -- Leaf level. 
        IF Rel_Thru_Ss + 1 = K_Tree_2 . ALL . Elem_Ct 
        THEN -- Return the two nodes unchanged. 
          RETURN ; 
        ELSE -- Must repack. 
          L_New_Elem_Ct 
            := K_Tree_1 . ALL . Elem_Ct + ( Rel_Thru_Ss + 1 ) ; 
          Init_Repack_Leaf ( L_Leaf_Pack_State , L_New_Elem_Ct ) ; 
          FOR I IN 0 .. K_Tree_1 . ALL . Elem_Ct - 1 
          LOOP -- FOR  
            Repack_Leaf_Elem 
              ( L_Leaf_Pack_State , K_Tree_1 . ALL . Leaf_Elems ( I ) ) ; 
          END LOOP ; -- FOR  
          FOR I IN 0 .. Rel_Thru_Ss 
          LOOP -- FOR  
            Repack_Leaf_Elem 
              ( L_Leaf_Pack_State , K_Tree_2 . ALL . Leaf_Elems ( I ) ) ; 
          END LOOP ; -- FOR  
          Finalize_Repack_Leaf ( L_Leaf_Pack_State ) ; 
          K_Tree_1 := L_Leaf_Pack_State . Lps_New_K_Tree_1 ; 
          K_Tree_2 := L_Leaf_Pack_State . Lps_New_K_Tree_2 ; 
          Assertions . Assert 
            ( L_Leaf_Pack_State . Lps_New_K_Tree_3 = NULL ) ; 
        END IF ; 
      ELSE -- Nonleaf level. 
        IF Rel_Thru_Ss + 1 
           = K_Tree_2 . ALL . Nonleaf_Elems 
               ( K_Tree_2 . ALL . Elem_Ct - 1 ) 
               . Nle_Cum_Child_Ct 
        THEN -- Return the two nodes unchanged. 
          RETURN ; 
        ELSE -- Will repack regardless of what happens below. 
          Bin_Search 
            ( K_Tree_2 , 
              Rel_Thru_Ss , 
              L_Thru_Elem_No , 
              L_Child_Thru_Ss 
            ) ; 
          IF L_Thru_Elem_No = 0 
          THEN 
            L_Child_K_Tree_1 
              := K_Tree_1 . ALL . Nonleaf_Elems 
                   ( K_Tree_1 . ALL . Elem_Ct - 1 ) 
                   . Nle_Child_Ref ; 
            L_Middle_Elem_No := K_Tree_1 . ALL . Elem_Ct - 2 ; 
          ELSE 
            L_Child_K_Tree_1 
              := K_Tree_2 . ALL . Nonleaf_Elems 
                   ( L_Thru_Elem_No - 1 ) 
                   . Nle_Child_Ref ; 
            L_Middle_Elem_No := K_Tree_1 . ALL . Elem_Ct - 1 ; 
          END IF ; 
          L_Child_K_Tree_2 
            := K_Tree_2 . ALL . Nonleaf_Elems 
                 ( L_Thru_Elem_No ) 
                 . Nle_Child_Ref ; 
          Recurse_Right 
            ( L_Child_Thru_Ss , L_Child_K_Tree_1 , L_Child_K_Tree_2 ) ; 
          L_New_Elem_Ct 
            := 1 
               + BOOLEAN ' Pos ( L_Child_K_Tree_2 /= NULL ) 
               + For_Range ( 0 , L_Middle_Elem_No ) 
               + For_Range ( 0 , L_Thru_Elem_No - 2 ) ; 
          Init_Repack_Nonleaf 
            ( L_Nonleaf_Pack_State , 
              L_New_Elem_Ct , 
              K_Tree_1 . ALL . Height 
            ) ; 
          FOR I IN 0 .. L_Middle_Elem_No 
          LOOP -- FOR  
            Repack_Nonleaf_Elem 
              ( L_Nonleaf_Pack_State , 
                K_Tree_1 . ALL . Nonleaf_Elems ( I ) . Nle_Child_Ref 
              ) ; 
          END LOOP ; -- FOR  
          FOR I IN 0 .. L_Thru_Elem_No - 2 
          LOOP -- FOR  
            Repack_Nonleaf_Elem 
              ( L_Nonleaf_Pack_State , 
                K_Tree_2 . ALL . Nonleaf_Elems ( I ) . Nle_Child_Ref 
              ) ; 
          END LOOP ; -- FOR  
          Repack_Nonleaf_Elem 
            ( L_Nonleaf_Pack_State , L_Child_K_Tree_1 ) ; 
          Repack_Nonleaf_Elem 
            ( L_Nonleaf_Pack_State , L_Child_K_Tree_2 ) ; 
          Finalize_Repack_Nonleaf ( L_Nonleaf_Pack_State ) ; 
          K_Tree_1 := L_Nonleaf_Pack_State . Nlps_New_K_Tree_1 ; 
          K_Tree_2 := L_Nonleaf_Pack_State . Nlps_New_K_Tree_2 ; 
        END IF ; 
      END IF ; 
    END Recurse_Right ; 

  BEGIN -- Slice  
    DECLARE 
      L_K_Tree : T ; 
      L_Length : Seq_Ss_Typ ; 
    BEGIN -- DECLARE  
      IF From > Thru 
      THEN 
        RETURN Empty ; 
      ELSIF K_Tree = NULL 
      THEN 
        IF From <= Thru 
        THEN 
          RAISE Subscript_Out_Of_Bounds ; 
        ELSE RETURN NULL ; 
        END IF ; 
      ELSE 
        L_Length := Length ( K_Tree ) ; 
        IF From < 0 OR Thru >= L_Length 
        THEN 
          RAISE Subscript_Out_Of_Bounds ; 
        ELSIF From = 0 AND THEN Thru = L_Length - 1 
        THEN RETURN K_Tree ; 
        ELSE 
          L_K_Tree := K_Tree ; 
          Recurse_Whole_1 ( From , Thru , L_K_Tree ) ; 
          RETURN L_K_Tree ; 
        END IF ; 
      END IF ; 
    END ; -- DECLARE  
  END Slice ; 

--generic 
--  with procedure Visit ( Element : E ); 

  PROCEDURE Traverse ( K_Tree : IN T ) 

  IS 

    PROCEDURE Recurse ( K_Tree : IN T ) 

    IS 
    BEGIN -- Recurse  
      IF K_Tree = NULL 
      THEN 
        RETURN ; 
      ELSIF K_Tree . ALL . Height = 1 
      THEN -- leaf level 
        FOR I IN 0 .. K_Tree . ALL . Elem_Ct - 1 
        LOOP -- FOR  
          Visit ( K_Tree . ALL . Leaf_Elems ( I ) ) ; 
        END LOOP ; -- FOR  
      ELSE -- nonleaf level 
        FOR I IN 0 .. K_Tree . ALL . Elem_Ct - 1 
        LOOP -- FOR  
          Recurse 
            ( K_Tree . ALL . Nonleaf_Elems ( I ) . Nle_Child_Ref ) ; 
        END LOOP ; -- FOR  
      END IF ; 
    END Recurse ; 

  BEGIN -- Traverse  
    Recurse ( K_Tree ) ; 
  END Traverse ; 

  FUNCTION Is_Consistent ( K_Tree : IN T ) RETURN BOOLEAN 

  IS 
    L_Seq_Ss : Seq_Ss_Typ := 0 ; 
    L_Result : BOOLEAN := TRUE ; 
    L_Header_Has_Been_Written : BOOLEAN := FALSE ; 
    Quit : EXCEPTION ; 

    PROCEDURE Error ( Msg : IN STRING ) 

    IS 
    BEGIN -- Error  
      IF NOT Is_Consistent . L_Header_Has_Been_Written 
      THEN 
        Ensure_Bol ; 
        TEXT_IO . Put_Line ( "***** Inconsistency *****" ) ; 
        TEXT_IO . Put_Line 
          ( "K-tree root: " & Image ( Is_Consistent . K_Tree ) ) ; 
        Is_Consistent . L_Header_Has_Been_Written := TRUE ; 
      END IF ; 
      TEXT_IO . Put_Line ( Msg ) ; 
      Is_Consistent . L_Result := FALSE ; 
    END Error ; 

    PROCEDURE Recurse 
      ( K_Tree : IN T ; 
        Height : IN Height_Typ ; 
        Length : OUT Seq_Ss_Typ 
      ) 

    IS 

      PROCEDURE Error ( Msg : IN STRING ) 

      IS 
      BEGIN -- Error  
        IF NOT Is_Consistent . L_Header_Has_Been_Written 
        THEN 
          Ensure_Bol ; 
          TEXT_IO . Put_Line ( "***** Inconsistency *****" ) ; 
          TEXT_IO . Put_Line 
            ( "K-tree root: " & Image ( Is_Consistent . K_Tree ) ) ; 
          Is_Consistent . L_Header_Has_Been_Written := TRUE ; 
        END IF ; 
        TEXT_IO . Put_Line 
          ( "Current node: " & Image ( Recurse . K_Tree ) ) ; 
        TEXT_IO . Put_Line 
          ( "Height: " & Height_Typ ' Image ( Recurse . Height ) ) ; 
        TEXT_IO . Put_Line ( Msg ) ; 
        Is_Consistent . L_Result := FALSE ; 
      END Error ; 

    BEGIN -- Recurse  
      DECLARE 
        L_Length : Seq_Ss_Typ := 0 ; 
        L_Down_Length : Seq_Ss_Typ := 0 ; 
        L_Cum_Child_Ct : Seq_Ss_Typ := 0 ; 
        L_Ith_Element : E ; 
      BEGIN -- DECLARE  
        IF K_Tree = NULL 
        THEN 
          Error ( "Null K-tree." ) ; 
        ELSE 
          IF Height /= K_Tree . ALL . Height 
          THEN 
            Error 
              ( "Computed/stored Height mismatch" 
                & Height_Typ ' Image ( Height ) 
                & Height_Typ ' Image ( K_Tree . ALL . Height ) 
              ) ; 
          END IF ; 
          IF K_Tree . ALL . Elem_Ct < 2 
          THEN 
            Error ( "Node with < 2 elements." ) ; 
          END IF ; 
          IF K_Tree . ALL . Height = 1 
          THEN -- Leaf node. 
            FOR J IN 0 .. K_Tree . ALL . Elem_Ct - 1 
            LOOP -- FOR  
              L_Ith_Element 
                := Ith_Element 
                     ( Is_Consistent . K_Tree , 
                       Is_Consistent . L_Seq_Ss 
                     ) ; 
              IF K_Tree . ALL . Leaf_Elems ( J ) /= L_Ith_Element 
              THEN 
                Error 
                  ( "Value mismatch " 
                    & Elem_No_Typ ' Image ( J ) 
                    & Image ( K_Tree . ALL . Leaf_Elems ( J ) ) 
                    & Seq_Ss_Typ ' Image ( Is_Consistent . L_Seq_Ss ) 
                    & Image ( L_Ith_Element ) 
                  ) ; 
              END IF ; 
              Is_Consistent . L_Seq_Ss := Is_Consistent . L_Seq_Ss + 1 ; 
              L_Length := L_Length + 1 ; 
            END LOOP ; -- FOR  
          ELSE -- Nonleaf node. 
            FOR J IN 0 .. K_Tree . ALL . Elem_Ct - 1 
            LOOP -- FOR  Do some prechecks so Ith_Element won't crash 
              IF K_Tree . ALL . Nonleaf_Elems ( J ) . Nle_Cum_Child_Ct 
                 <= L_Cum_Child_Ct 
              THEN 
                Error 
                  ( "Cum_Child_Ct non-monotonic for child no " 
                    & Elem_No_Typ ' Image ( J ) 
                    & " of " 
                    & Image ( K_Tree ) 
                    & " after " 
                    & Seq_Ss_Typ ' Image ( L_Cum_Child_Ct ) 
                    & " saw " 
                    & Seq_Ss_Typ 
                        ' Image 
                        ( K_Tree . ALL . Nonleaf_Elems 
                            ( J ) 
                            . Nle_Cum_Child_Ct 
                        ) 
                    & "." 
                  ) ; 
                RAISE Quit ; 
              ELSIF 
                K_Tree . ALL . Nonleaf_Elems ( J ) . Nle_Child_Ref 
                = NULL 
              THEN 
                Error 
                  ( "Null nonleaf child for child no " 
                    & Elem_No_Typ ' Image ( J ) 
                    & " of " 
                    & Image ( K_Tree ) 
                    & "." 
                  ) ; 
                RAISE Quit ; 
              END IF ; 
              L_Cum_Child_Ct 
                := K_Tree . ALL . Nonleaf_Elems 
                     ( J ) 
                     . Nle_Cum_Child_Ct ; 
            END LOOP ; -- FOR  
            FOR J IN 0 .. K_Tree . ALL . Elem_Ct - 1 
            LOOP -- FOR  
              Recurse 
                ( K_Tree . ALL . Nonleaf_Elems ( J ) . Nle_Child_Ref , 
                  Height - 1 , 
                  L_Down_Length 
                ) ; 
              L_Length := L_Length + L_Down_Length ; 
              IF L_Length 
                 /= K_Tree . ALL . Nonleaf_Elems 
                      ( J ) 
                      . Nle_Cum_Child_Ct 
              THEN 
                Error 
                  ( "Cum_Child_Ct mismatch for child no " 
                    & Elem_No_Typ ' Image ( J ) 
                    & Seq_Ss_Typ ' Image ( L_Length ) 
                    & Seq_Ss_Typ 
                        ' Image 
                        ( K_Tree . ALL . Nonleaf_Elems 
                            ( J ) 
                            . Nle_Cum_Child_Ct 
                        ) 
                    & "." 
                  ) ; 
              END IF ; 
            END LOOP ; -- FOR  
          END IF ; 
        END IF ; 
        Length := L_Length ; 
      END ; -- DECLARE  
    END Recurse ; 

  BEGIN -- Is_Consistent  
    DECLARE 
      L_Length : Seq_Ss_Typ := 0 ; 
    BEGIN -- DECLARE  
      IF K_Tree = NULL 
      THEN -- Empty sequence. 
        RETURN TRUE ; 
      ELSIF 
        K_Tree . ALL . Height = 1 AND THEN K_Tree . ALL . Elem_Ct = 1 
      THEN -- properly formed singleton sequence. 
        RETURN TRUE ; 
      ELSE 
        Recurse ( K_Tree , K_Tree . ALL . Height , L_Length ) ; 
        Assertions . Assert ( L_Length = L_Seq_Ss ) ; 
        RETURN L_Result ; 
      END IF ; 
    EXCEPTION -- DECLARE  

      WHEN Quit 
      => RETURN L_Result ; 

      WHEN OTHERS 
      => Error ( "Exception raised inside Is_Consistent." ) ; 
         RETURN FALSE ; 

    END ; -- DECLARE  
  END Is_Consistent ; 

  PROCEDURE Dump ( K_Tree : IN T ) 

  IS 

    L_Seq_Ss : Seq_Ss_Typ := 0 ; 

    PROCEDURE Recurse 
      ( K_Tree : IN T ; Depth : IN Height_Typ ; Sum : IN Seq_Ss_Typ ) 

    IS 

      PROCEDURE Put_Left 

      IS 
      BEGIN -- Put_Left  
        Seq_Ss_Text_Io . Put ( Dump . L_Seq_Ss , 6 ) ; 
        Seq_Ss_Text_Io . Put ( Recurse . Sum , 6 ) ; 
        TEXT_IO . Put ( ' ' ) ; 
        TEXT_IO . Put ( Image ( Recurse . K_Tree ) ) ; 
        Height_Text_Io . Put ( Recurse . K_Tree . ALL . Height , 3 ) ; 
        FOR I IN 0 .. Depth 
        LOOP -- FOR  
          TEXT_IO . Put ( ' ' ) ; 
        END LOOP ; -- FOR  
        TEXT_IO . Put ( '(' ) ; 
      END Put_Left ; 

      PROCEDURE Put_Right 

      IS 
      BEGIN -- Put_Right  
        TEXT_IO . Put ( ')' ) ; 
        TEXT_IO . New_Line ; 
      END Put_Right ; 

      PROCEDURE Put_E ( Element : IN E ) 

      IS 
      BEGIN -- Put_E  
        TEXT_IO . Put ( Image ( Element ) ) ; 
      END Put_E ; 

      PROCEDURE Put_Nonleaf_Elem 
        ( Nonleaf_Elem : IN Nonleaf_Elem_Typ ) 

      IS 
      BEGIN -- Put_Nonleaf_Elem  
        TEXT_IO . Put ( '(' ) ; 
        TEXT_IO . Put ( Image ( Nonleaf_Elem . Nle_Child_Ref ) ) ; 
        TEXT_IO . Put ( ',' ) ; 
        Seq_Ss_Text_Io . Put ( Nonleaf_Elem . Nle_Cum_Child_Ct , 1 ) ; 
        TEXT_IO . Put ( ')' ) ; 
      END Put_Nonleaf_Elem ; 

    BEGIN -- Recurse  
      IF K_Tree = NULL 
      THEN 
        RETURN ; -- This is ill-formed. 
      ELSIF K_Tree . ALL . Height = 1 
      THEN -- Leaf node 
        Put_Left ; 
        FOR J IN 0 .. K_Tree . ALL . Elem_Ct - 1 
        LOOP -- FOR  
          IF J > 0 
          THEN 
            TEXT_IO . Put ( ',' ) ; 
          END IF ; 
          Put_E ( K_Tree . ALL . Leaf_Elems ( J ) ) ; 
        END LOOP ; -- FOR  
        Put_Right ; 
        L_Seq_Ss := L_Seq_Ss + K_Tree . ALL . Elem_Ct ; 
      ELSE -- Nonleaf node. 
        Put_Left ; 
        FOR J IN 0 .. K_Tree . ALL . Elem_Ct - 1 
        LOOP -- FOR  
          IF J > 0 
          THEN 
            TEXT_IO . Put ( ',' ) ; 
          END IF ; 
          Put_Nonleaf_Elem ( K_Tree . ALL . Nonleaf_Elems ( J ) ) ; 
        END LOOP ; -- FOR  
        Put_Right ; 
        FOR J IN 0 .. K_Tree . ALL . Elem_Ct - 1 
        LOOP -- FOR  
          Recurse 
            ( K_Tree . ALL . Nonleaf_Elems ( J ) . Nle_Child_Ref , 
              Depth => Depth + 1 , 
              Sum => Sum + Cum_Children_To_Left ( K_Tree , J ) 
            ) ; 
        END LOOP ; -- FOR  
      END IF ; 
    END Recurse ; 

  BEGIN -- Dump  
    Ensure_Bol ; 
    TEXT_IO . Put_Line ( "------------- K-tree dump -------------" ) ; 
    IF K_Tree = NULL 
    THEN -- Empty sequence. 
      TEXT_IO . Put_Line ( " EMPTY " ) ; 
    ELSE Recurse ( K_Tree , Depth => 0 , Sum => 0 ) ; 
    END IF ; 
    TEXT_IO . Put_Line ( "---------------------------------------" ) ; 
  END Dump ; 

  FUNCTION Height ( K_Tree : IN T ) RETURN INTEGER 

  IS 
  BEGIN -- Height  
    IF K_Tree = NULL 
    THEN 
      RETURN 0 ; 
    ELSE RETURN K_Tree . ALL . Height ; 
    END IF ; 
  END Height ; 

  PROCEDURE Internal_Statistics 
    ( K_Tree : IN T ; 
      Leaf_Nodes : OUT INTEGER ; 
      Leaf_Elements : OUT INTEGER ; 
      Nonleaf_Nodes : OUT INTEGER ; 
      Nonleaf_Elements : OUT INTEGER 
    ) 

  IS 

    L_Leaf_Nodes : INTEGER := 0 ; 
    L_Leaf_Elements : INTEGER := 0 ; 
    L_Nonleaf_Nodes : INTEGER := 0 ; 
    L_Nonleaf_Elements : INTEGER := 0 ; 

    PROCEDURE Recurse ( K_Tree : IN T ) 

    IS 

    BEGIN -- Recurse  
      IF K_Tree = NULL 
      THEN 
        RETURN ; 
      ELSIF K_Tree . ALL . Height = 1 
      THEN -- Leaf node 
        L_Leaf_Nodes := L_Leaf_Nodes + 1 ; 
        L_Leaf_Elements := L_Leaf_Elements + K_Tree . ALL . Elem_Ct ; 
      ELSE -- Nonleaf node. 
        L_Nonleaf_Nodes := L_Nonleaf_Nodes + 1 ; 
        L_Nonleaf_Elements 
          := L_Nonleaf_Elements + K_Tree . ALL . Elem_Ct ; 
        FOR J IN 0 .. K_Tree . ALL . Elem_Ct - 1 
        LOOP -- FOR  
          Recurse 
            ( K_Tree . ALL . Nonleaf_Elems ( J ) . Nle_Child_Ref ) ; 
        END LOOP ; -- FOR  
      END IF ; 
    END Recurse ; 

  BEGIN -- Internal_Statistics  
    Recurse ( K_Tree ) ; 
    Leaf_Nodes := L_Leaf_Nodes ; 
    Leaf_Elements := L_Leaf_Elements ; 
    Nonleaf_Nodes := L_Nonleaf_Nodes ; 
    Nonleaf_Elements := L_Nonleaf_Elements ; 
  END Internal_Statistics ; 

END K_Trees ; 


















