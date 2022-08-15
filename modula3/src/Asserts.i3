
(* -----------------------------------------------------------------------1- *)
(* This is part of a reference implementation of K-trees, AKA Sequence trees.*)
(* Copyright 1994..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE Asserts
; EXCEPTION AssertionFailure
; PROCEDURE Assert ( Condition : BOOLEAN ; Msg : TEXT := "" )
    RAISES { AssertionFailure }
; PROCEDURE CantHappen ( Msg : TEXT := "" )
    RAISES { AssertionFailure }
; END Asserts
.
