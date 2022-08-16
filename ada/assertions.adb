-- Copyright Rodney M. Bates, 2022. rodney.m.bates@acm.org.
-- Licensed under the MIT license. 

WITH TEXT_IO ; 

PACKAGE BODY Assertions 
IS 

  PROCEDURE Assert ( Predicate : IN BOOLEAN ; Msg : IN STRING := "" ) 

  IS 
  BEGIN -- Assert  
    IF NOT Predicate 
    THEN 
      TEXT_IO . Put_Line ( "Assert failure: " & Msg ) ; 
    END IF ; 
  END Assert ; 

  PROCEDURE Cant_Happen ( Msg : IN STRING := "" ) 

  IS 
  BEGIN -- Cant_Happen  
    TEXT_IO . Put_Line ( "Cant_Happen: " & Msg ) ; 
  END Cant_Happen ; 

END Assertions ; 

