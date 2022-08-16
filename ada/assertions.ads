-- Copyright Rodney M. Bates, 2022. rodney.m.bates@acm.org.
-- Licensed under the MIT license. 

PACKAGE Assertions 
IS 

  PROCEDURE Assert ( Predicate : IN BOOLEAN ; Msg : IN STRING := "" ) ; 

  PROCEDURE Cant_Happen ( Msg : IN STRING := "" ) ; 

END Assertions ; 

