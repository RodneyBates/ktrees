-- Copyright Rodney M. Bates, 2022. rodney.m.bates@acm.org.
-- Licensed under the MIT license. 

WITH Assertions ; 
WITH TEXT_IO ; 

PACKAGE BODY Checked_K_Trees 
IS 

  FUNCTION "=" ( Left , Right : IN K_Trees . T ) RETURN BOOLEAN 
    RENAMES K_Trees . "=" ; 

  PROCEDURE Msg ( Str : IN STRING ) 

  IS 
  BEGIN -- Msg  
    TEXT_IO . Put_Line ( Str ) ; 
  END Msg ; 

  FUNCTION Empty RETURN K_Trees . T 

  IS 

    L_K_Tree : K_Trees . T := K_Trees . Empty ; 

  BEGIN -- Empty  Yes, this is a bit silly. 
    IF Checks_Enabled 
    THEN 
      IF K_Trees . Length ( L_K_Tree ) /= 0 
      THEN 
        Msg ( "Bad Empty" ) ; 
        K_Trees . Dump ( L_K_Tree ) ; 
      END IF ; 
    END IF ; 
    RETURN L_K_Tree ; 
  END Empty ; 

  FUNCTION Singleton ( Element : IN K_Trees . E ) RETURN K_Trees . T 

  IS 

    L_K_Tree : K_Trees . T := K_Trees . Singleton ( Element ) ; 

  BEGIN -- Singleton  
    IF Checks_Enabled 
    THEN 
      IF K_Trees . Length ( L_K_Tree ) /= 1 
      THEN 
        Msg ( "Singleton bad length" ) ; 
        K_Trees . Dump ( L_K_Tree ) ; 
      ELSIF K_Trees . Ith_Element ( L_K_Tree , 0 ) /= Element 
      THEN 
        Msg 
          ( "Singleton bad element value, should be " 
            & K_Trees . Image ( Element ) 
          ) ; 
        K_Trees . Dump ( L_K_Tree ) ; 
      END IF ; 
    END IF ; 
    RETURN L_K_Tree ; 
  END Singleton ; 

  FUNCTION Length ( K_Tree : IN K_Trees . T ) 
    RETURN K_Trees . Seq_Ss_Typ 

  IS 

    L_Length : K_Trees . Seq_Ss_Typ := K_Trees . Length ( K_Tree ) ; 
    L_Count : K_Trees . Seq_Ss_Typ := 0 ; 

    PROCEDURE Visit ( Element : IN K_Trees . E ) 

    IS 
    BEGIN -- Visit  
      Length . L_Count := Length . L_Count + 1 ; 
    END Visit ; 

    PROCEDURE Length_Traverse 
    IS NEW K_Trees . Traverse ( Visit => Visit ) ; 

  BEGIN -- Length  
    IF Checks_Enabled 
    THEN 
      Length_Traverse ( K_Tree ) ; 
      IF L_Length /= L_Count 
      THEN 
        Msg 
          ( "Bad Length " 
            & K_Trees . Seq_Ss_Typ ' Image ( L_Length ) 
            & K_Trees . Seq_Ss_Typ ' Image ( L_Count ) 
          ) ; 
        K_Trees . Dump ( K_Tree ) ; 
      END IF ; 
    END IF ; 
    RETURN L_Length ; 
  END Length ; 

  PROCEDURE Check_Ith_Element 
    ( K_Tree : IN K_Trees . T ; 
      Subscript : IN K_Trees . Seq_Ss_Typ ; 
      Element : IN K_Trees . E ; 
      Caller : IN STRING 
    ) 

  IS 

    L_Count : K_Trees . Seq_Ss_Typ := 0 ; 
    Quit : EXCEPTION ; 

    PROCEDURE Visit ( Element : IN K_Trees . E ) 

    IS 
    BEGIN -- Visit  
      IF Check_Ith_Element . L_Count = Subscript 
      THEN 
        IF Element /= Check_Ith_Element . Element 
        THEN 
          Msg 
            ( Caller 
              & " Bad value " 
              & K_Trees . Seq_Ss_Typ ' Image ( Subscript ) 
              & K_Trees . Image ( Element ) 
              & K_Trees . Image ( Check_Ith_Element . Element ) 
            ) ; 
          K_Trees . Dump ( K_Tree ) ; 
        END IF ; 
        RAISE Quit ; 
      END IF ; 
      Check_Ith_Element . L_Count := Check_Ith_Element . L_Count + 1 ; 
    END Visit ; 

    PROCEDURE Check_Traverse 
    IS NEW K_Trees . Traverse ( Visit => Visit ) ; 

  BEGIN -- Check_Ith_Element  
    IF Checks_Enabled 
    THEN 
      BEGIN 
        Check_Traverse ( K_Tree ) ; 
        Msg 
          ( Caller 
            & " short sequence " 
            & K_Trees . Seq_Ss_Typ ' Image ( Subscript ) 
            & K_Trees . Seq_Ss_Typ ' Image ( L_Count ) 
          ) ; 
      EXCEPTION -- BEGIN  

        WHEN Quit 
        => NULL ; 

      END ; -- BEGIN  
    END IF ; 
  END Check_Ith_Element ; 

  FUNCTION Ith_Element 
    ( K_Tree : IN K_Trees . T ; Subscript : IN K_Trees . Seq_Ss_Typ ) 
    RETURN K_Trees . E 

  IS 

    L_Element 
      : K_Trees . E := K_Trees . Ith_Element ( K_Tree , Subscript ) ; 

  BEGIN -- Ith_Element  
    IF Checks_Enabled 
    THEN 
      Check_Ith_Element 
        ( K_Tree , Subscript , L_Element , "Ith_Element" ) ; 
    END IF ; 
    RETURN L_Element ; 
  END Ith_Element ; 

  PROCEDURE Store_Ith_Element 
    ( K_Tree : IN K_Trees . T ; 
      Subscript : IN K_Trees . Seq_Ss_Typ ; 
      Value : IN K_Trees . E 
    ) 

  IS 
  BEGIN -- Store_Ith_Element  
    K_Trees . Store_Ith_Element ( K_Tree , Subscript , Value ) ; 
    IF Checks_Enabled 
    THEN 
      Check_Ith_Element 
        ( K_Tree , Subscript , Value , "Store_Ith_Element" ) ; 
    END IF ; 
  END Store_Ith_Element ; 

  PROCEDURE Check_Slice 
    ( K_Tree : IN K_Trees . T ; 
      From , Thru : IN K_Trees . Seq_Ss_Typ ; 
      Slice_K_Tree : IN K_Trees . T ; 
      Is_Ok : OUT BOOLEAN ; 
      Bad_K_Tree_Ss : OUT K_Trees . Seq_Ss_Typ 
    ) 

  IS 

    TYPE L_Slice_Array_Typ 
    IS ARRAY 
         ( INTEGER RANGE 0 .. K_Trees . Length ( Slice_K_Tree ) - 1 ) OF 
                                                                       K_Trees 
                                                                       . E ; 
    L_Slice_Array : L_Slice_Array_Typ ; 
    L_Ss : K_Trees . Seq_Ss_Typ := 0 ; 
    Quit : EXCEPTION ; 

    PROCEDURE Slice_Visit ( Element : IN K_Trees . E ) 

    IS 
    BEGIN -- Slice_Visit  
      Check_Slice . L_Slice_Array ( Check_Slice . L_Ss ) := Element ; 
      Check_Slice . L_Ss := Check_Slice . L_Ss + 1 ; 
    END Slice_Visit ; 

    PROCEDURE Fill_Slice 
    IS NEW K_Trees . Traverse ( Visit => Slice_Visit ) ; 

    PROCEDURE Operand_Visit ( Element : IN K_Trees . E ) 

    IS 
    BEGIN -- Operand_Visit  
      IF Check_Slice . L_Ss > Check_Slice . Thru 
      THEN 
        RAISE Quit ; 
      ELSIF Check_Slice . L_Ss >= Check_Slice . From 
      THEN 
        IF L_Slice_Array ( Check_Slice . L_Ss - Check_Slice . From ) 
           /= Element 
        THEN 
          Is_Ok := FALSE ; 
          Bad_K_Tree_Ss := Check_Slice . L_Ss ; 
          RAISE Quit ; 
        END IF ; 
      END IF ; 
      Check_Slice . L_Ss := Check_Slice . L_Ss + 1 ; 
    END Operand_Visit ; 

    PROCEDURE Verify 
    IS NEW K_Trees . Traverse ( Visit => Operand_Visit ) ; 

  BEGIN -- Check_Slice  
    Is_Ok := TRUE ; 
    Bad_K_Tree_Ss := 0 ; 
    IF Thru - From + 1 /= K_Trees . Length ( Slice_K_Tree ) 
    THEN 
      Is_Ok := FALSE ; 
    ELSE 
      Fill_Slice ( Slice_K_Tree ) ; 
      Assertions . Assert ( L_Ss = K_Trees . Length ( Slice_K_Tree ) ) ; 
      L_Ss := 0 ; 
      Verify ( K_Tree ) ; 
      IF L_Ss /= From + K_Trees . Length ( Slice_K_Tree ) 
      THEN 
        Is_Ok := FALSE ; 
        Bad_K_Tree_Ss := L_Ss ; 
      END IF ; 
    END IF ; 
  EXCEPTION -- Check_Slice  

    WHEN Quit 
    => NULL ; 

  END Check_Slice ; 

  FUNCTION Slice 
    ( K_Tree : IN K_Trees . T ; From , Thru : IN K_Trees . Seq_Ss_Typ ) 
    RETURN K_Trees . T 

  IS 

    L_Result : K_Trees . T := K_Trees . Slice ( K_Tree , From , Thru ) ; 
    L_Is_Ok : BOOLEAN ; 
    L_Bad_Ss : K_Trees . Seq_Ss_Typ ; 

  BEGIN -- Slice  
    IF Checks_Enabled 
    THEN 
      IF K_Trees . Is_Consistent ( L_Result ) 
      THEN 
        IF Thru - From + 1 /= K_Trees . Length ( L_Result ) 
        THEN 
          Msg 
            ( "Slice, wrong length: " 
              & K_Trees . Seq_Ss_Typ 
                  ' Image 
                  ( K_Trees . Length ( K_Tree ) ) 
              & K_Trees . Seq_Ss_Typ ' Image ( From ) 
              & K_Trees . Seq_Ss_Typ ' Image ( Thru ) 
              & K_Trees . Seq_Ss_Typ 
                  ' Image 
                  ( K_Trees . Length ( L_Result ) ) 
            ) ; 
          K_Trees . Dump ( K_Tree ) ; 
          K_Trees . Dump ( L_Result ) ; 
        ELSE 
          Check_Slice 
            ( K_Tree , From , Thru , L_Result , L_Is_Ok , L_Bad_Ss ) ; 
          IF NOT L_Is_Ok 
          THEN 
            Msg 
              ( "Slice, element mismatch at " 
                & K_Trees . Seq_Ss_Typ ' Image ( L_Bad_Ss ) 
                & K_Trees . Seq_Ss_Typ ' Image ( From ) 
                & K_Trees . Seq_Ss_Typ ' Image ( Thru ) 
              ) ; 
            K_Trees . Dump ( K_Tree ) ; 
            K_Trees . Dump ( L_Result ) ; 
          END IF ; 
        END IF ; 
      ELSE -- not consistent 
        Msg 
          ( "Slice result not consistent " 
            & K_Trees . Seq_Ss_Typ ' Image ( From ) 
            & K_Trees . Seq_Ss_Typ ' Image ( Thru ) 
          ) ; 
        K_Trees . Dump ( K_Tree ) ; 
        K_Trees . Dump ( L_Result ) ; 
      END IF ; 
    END IF ; 
    RETURN L_Result ; 
  END Slice ; 

  FUNCTION Cat ( Left , Right : IN K_Trees . T ) RETURN K_Trees . T 

  IS 

    L_Result : K_Trees . T := K_Trees . Cat ( Left , Right ) ; 
    L_Left_Length : K_Trees . Seq_Ss_Typ ; 
    L_Is_Ok : BOOLEAN ; 
    L_Bad_Ss : K_Trees . Seq_Ss_Typ ; 

  BEGIN -- Cat  
    IF Checks_Enabled 
    THEN 
      IF K_Trees . Is_Consistent ( L_Result ) 
      THEN 
        L_Left_Length := K_Trees . Length ( Left ) ; 
        Check_Slice 
          ( L_Result , 
            0 , 
            L_Left_Length - 1 , 
            Left , 
            L_Is_Ok , 
            L_Bad_Ss 
          ) ; 
        IF NOT L_Is_Ok 
        THEN 
          Msg 
            ( "Cat, left, element mismatch at " 
              & K_Trees . Seq_Ss_Typ ' Image ( L_Bad_Ss ) 
            ) ; 
          K_Trees . Dump ( Left ) ; 
          K_Trees . Dump ( Right ) ; 
          K_Trees . Dump ( L_Result ) ; 
        ELSE 
          Check_Slice 
            ( L_Result , 
              L_Left_Length , 
              L_Left_Length + K_Trees . Length ( Right ) - 1 , 
              Right , 
              L_Is_Ok , 
              L_Bad_Ss 
            ) ; 
          IF NOT L_Is_Ok 
          THEN 
            Msg 
              ( "Cat, right, element mismatch at " 
                & K_Trees . Seq_Ss_Typ ' Image ( L_Bad_Ss ) 
              ) ; 
            K_Trees . Dump ( Left ) ; 
            K_Trees . Dump ( Right ) ; 
            K_Trees . Dump ( L_Result ) ; 
          END IF ; 
        END IF ; 
      ELSE -- not consistent 
        Msg ( "cat result not consistent" ) ; 
        K_Trees . Dump ( Left ) ; 
        K_Trees . Dump ( Right ) ; 
        K_Trees . Dump ( L_Result ) ; 
      END IF ; 
    END IF ; 
    RETURN L_Result ; 
  END Cat ; 

END Checked_K_Trees ; 





