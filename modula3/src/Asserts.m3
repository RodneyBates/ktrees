
(* -----------------------------------------------------------------------1- *)
(* This is part of a reference implementation of K-trees, AKA Sequence trees.*)
(* Copyright 1994..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE Asserts
; IMPORT Stdio
; IMPORT Wr

(* EXPORTED: *)
; PROCEDURE Assert ( Condition : BOOLEAN ; Msg : TEXT := "" )
    RAISES { AssertionFailure }
  = BEGIN
      IF NOT Condition
      THEN
        CantHappen ( Msg )
      END (* IF *)
    END Assert
        
(* EXPORTED: *)
; PROCEDURE CantHappen ( Msg : TEXT := "" )
    RAISES { AssertionFailure }
  = <*FATAL ANY*> BEGIN
      Wr . PutText ( Stdio . stderr , "Assertion Failure:" & Msg )
    ; Wr . PutChar ( Stdio . stderr , '\n' )
    ; RAISE AssertionFailure
    END CantHappen

; BEGIN
  END Asserts
.
