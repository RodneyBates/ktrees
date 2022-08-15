
(* -----------------------------------------------------------------------1- *)
(* This is part of a reference implementation of K-trees, AKA Sequence trees.*)
(* Copyright 1994..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE IntCKTrees 
  (* An overlayer of IntKTrees, with massive, brute-force
     correctness checking.  
  *)

; IMPORT IntKTrees AS KTrees

; TYPE T = KTrees . T
; TYPE ElemT = KTrees . ElemT
; TYPE SeqSsTyp = KTrees . SeqSsTyp

; VAR ChecksEnabled : BOOLEAN := TRUE

; PROCEDURE Empty ( ) : T

; PROCEDURE Singleton ( Element : ElemT ) : T

; PROCEDURE Length ( KTree : T ) : SeqSsTyp

; PROCEDURE IthElement 
    ( KTree : T ; Subscript : SeqSsTyp ) : ElemT
  RAISES { KTrees . SubscriptOutOfBounds }

; PROCEDURE StoreIthElement
    ( KTree : T ; Subscript : SeqSsTyp ; Value : ElemT ) : T
  RAISES { KTrees . SubscriptOutOfBounds }

; PROCEDURE Slice 
    ( KTree : T ; FromSs : SeqSsTyp ; ThruSs : INTEGER ) : T
  RAISES { KTrees . SubscriptOutOfBounds }

; PROCEDURE Cat ( Left , Right : T ) : T

; END IntCKTrees
.

