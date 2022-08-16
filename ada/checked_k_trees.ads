-- Copyright Rodney M. Bates, 2022. rodney.m.bates@acm.org.
-- Licensed under the MIT license. 

WITH K_Trees ; 

PACKAGE Checked_K_Trees 
IS 

  Checks_Enabled : BOOLEAN := TRUE ; 

  FUNCTION Empty RETURN K_Trees . T ; 

  FUNCTION Singleton ( Element : IN K_Trees . E ) RETURN K_Trees . T ; 

  FUNCTION Length ( K_Tree : IN K_Trees . T ) 
    RETURN K_Trees . Seq_Ss_Typ ; 

  FUNCTION Ith_Element 
    ( K_Tree : IN K_Trees . T ; Subscript : IN K_Trees . Seq_Ss_Typ ) 
    RETURN K_Trees . E ; 

  PROCEDURE Store_Ith_Element 
    ( K_Tree : IN K_Trees . T ; 
      Subscript : IN K_Trees . Seq_Ss_Typ ; 
      Value : IN K_Trees . E 
    ) ; 

  FUNCTION Slice 
    ( K_Tree : IN K_Trees . T ; From , Thru : IN K_Trees . Seq_Ss_Typ ) 
    RETURN K_Trees . T ; 

  FUNCTION Cat ( Left , Right : IN K_Trees . T ) RETURN K_Trees . T ; 

END Checked_K_Trees ; 


