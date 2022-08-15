
(* -----------------------------------------------------------------------1- *)
(* This is part of a reference implementation of K-trees, AKA Sequence trees.*)
(* Copyright 1994..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

UNSAFE MODULE Image  

; IMPORT Fmt 

(* EXPORTED: *)
; PROCEDURE Ref ( R : REFANY ) : TEXT 

  = VAR V := LOOPHOLE ( R , INTEGER ) 

  ; BEGIN 
      RETURN Fmt . Int ( V , 16 ) 
    END Ref 
 
; BEGIN
  END Image 
. 

