
(* -----------------------------------------------------------------------1- *)
(* This is part of a reference implementation of K-trees, AKA Sequence trees.*)
(* Copyright 1994..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

GENERIC INTERFACE KTrees ( Elem )
  (* K-trees of Elem . T.
     Elem must have:
       T, which can be any type except an open array type. 
       PROCEDURE Image, of a subtype of this procedure type:
         PROCEDURE ( Elem : T ) : TEXT
   *)

; TYPE T <: ROOT (* Sequences. *)
; TYPE SeqSsTyp = [ 0 .. LAST ( INTEGER ) ] (* Sequence subscripts. *)
; EXCEPTION SubscriptOutOfBounds 

; TYPE ElemT = Elem . T (* Sequence elements renamed. *)
; CONST ElemImage = Elem . Image

; CONST DefaultMaxElemCt = 8
; CONST MaxMaxElemCt = 128 (* We need some kind of limit here! *)  

; PROCEDURE SetMaxElemCt ( Value : CARDINAL )
  (* Will be clipped to [ 3 .. MaxMaxElemCt ] *)   

; PROCEDURE Empty ( ) : T
  RAISES ANY
  (* Return an empty sequence. *)

; PROCEDURE Singleton ( Element : Elem . T ) : T
  RAISES ANY
  (* Return a sequence with one element. *)

; PROCEDURE Length ( KTree : T ) : SeqSsTyp
  RAISES ANY
  (* Number of elements in a sequence. *)

; PROCEDURE IthElement 
    ( KTree : T ; Subscript : SeqSsTyp ) : Elem . T
  RAISES ANY
  (* Return Subscript'th element of sequence. *)

; PROCEDURE StoreIthElement
    ( KTree : T ; Subscript : SeqSsTyp ; Value : Elem . T ) : T
  RAISES ANY
  (* Store Value into Subscript'th element of sequence. *)

; PROCEDURE Slice 
    ( KTree : T ; FromSs : SeqSsTyp ; ThruSs : INTEGER ) : T
  RAISES ANY
  (* Return a slice of a sequence. *)

; PROCEDURE Cat ( Left , Right : T ) : T
  RAISES ANY
  (* Concatenate two sequences. *)

; TYPE VisitElemProcTyp = PROCEDURE ( Element : Elem . T ) RAISES ANY

; PROCEDURE Traverse
    ( KTree : T ; VisitElemProc : VisitElemProcTyp )
  RAISES ANY
  (* Traverse sequence in order, calling Visit for each element. *)

; PROCEDURE IsConsistent ( KTree : T ) : BOOLEAN
  RAISES ANY
  (* Verify internal consistency. For testing KTrees. *)

; PROCEDURE Dump ( KTree : T )
  RAISES ANY
  (* Dump internal representation.  For testing KTrees. *)

; PROCEDURE Height ( KTree : T ) : INTEGER
  RAISES ANY
  (* Height of KTree.  Of internal interest only. *)

; PROCEDURE InternalStatistics
    ( KTree : T 
    ; VAR LeafNodes : INTEGER
    ; VAR LeafElements : INTEGER
    ; VAR NonleafNodes : INTEGER
    ; VAR NonleafElements : INTEGER
    )
  RAISES ANY
  (* Of internal interest only. *)

; END KTrees
.

