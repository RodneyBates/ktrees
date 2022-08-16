-- Copyright Rodney M. Bates, 2022. rodney.m.bates@acm.org.
-- Licensed under the MIT license. 

PACKAGE K_Trees 
IS 

  TYPE T IS PRIVATE ; 
  SUBTYPE E IS INTEGER ; 
  SUBTYPE Seq_Ss_Typ IS INTEGER RANGE - 1 .. INTEGER ' Last ; 

  Subscript_Out_Of_Bounds : EXCEPTION ; 

  FUNCTION Image ( Element : IN E ) RETURN STRING ; 

  FUNCTION Empty RETURN T ; 

  FUNCTION Singleton ( Element : IN E ) RETURN T ; 

  FUNCTION Length ( K_Tree : IN T ) RETURN Seq_Ss_Typ ; 

  FUNCTION Ith_Element ( K_Tree : IN T ; Subscript : IN Seq_Ss_Typ ) 
    RETURN E ; 

  PROCEDURE Store_Ith_Element 
    ( K_Tree : IN T ; Subscript : IN Seq_Ss_Typ ; Value : IN E ) ; 

  FUNCTION Slice ( K_Tree : IN T ; From , Thru : IN Seq_Ss_Typ ) 
    RETURN T ; 

  FUNCTION Cat ( Left , Right : IN T ) RETURN T ; 

  GENERIC WITH PROCEDURE Visit ( Element : IN E ) ; PROCEDURE Traverse 
    ( K_Tree : IN T ) ; 

  FUNCTION Is_Consistent ( K_Tree : IN T ) RETURN BOOLEAN ; 

  PROCEDURE Dump ( K_Tree : IN T ) ; 

  FUNCTION Height ( K_Tree : IN T ) RETURN INTEGER ; 

  PROCEDURE Internal_Statistics 
    ( K_Tree : IN T ; 
      Leaf_Nodes : OUT INTEGER ; 
      Leaf_Elements : OUT INTEGER ; 
      Nonleaf_Nodes : OUT INTEGER ; 
      Nonleaf_Elements : OUT INTEGER 
    ) ; 

PRIVATE -- K_Trees  

  Max_Elem_Ct : CONSTANT := 8 ; 

  SUBTYPE Elem_No_Typ IS INTEGER ; 

  SUBTYPE Height_Typ IS NATURAL ; 

  TYPE Nonleaf_Elem_Typ 
  IS RECORD 
       Nle_Child_Ref : T ; 
       Nle_Cum_Child_Ct : Seq_Ss_Typ ; 
     END RECORD ; 

  TYPE Nonleaf_Array_Typ 
  IS ARRAY ( Elem_No_Typ RANGE <> ) OF Nonleaf_Elem_Typ ; 

  TYPE Leaf_Array_Typ IS ARRAY ( Elem_No_Typ RANGE <> ) OF E ; 

  TYPE Node_Typ 
    ( Height : Height_Typ ; Elem_Ct : Elem_No_Typ ) 
  IS RECORD 
       CASE Height IS 

       WHEN 1 
       => Leaf_Elems : Leaf_Array_Typ ( 0 .. Elem_Ct ) ; 
            -- only thru Elem_Ct - 1 are used. 

       WHEN OTHERS 
       => Nonleaf_Elems : Nonleaf_Array_Typ ( 0 .. Elem_Ct ) ; 
            -- only thru Elem_Ct - 1 are used. 

       END CASE ; 
     END RECORD ; 

  TYPE T IS ACCESS Node_Typ ; 

END K_Trees ; 

