
(* -----------------------------------------------------------------------1- *)
(* This is part of a reference implementation of K-trees, AKA Sequence trees.*)
(* Copyright 1994..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE Report

(* Some output utility procedures to 
   1) Simplify language portability
   2) Shorten some calls elsewhere
   3) Add beginning of line functions
   NOTE: Makes the reckless assumption that no client passes
         a \n to PutText or PutTextLine.
*)

; IMPORT Text

; PROCEDURE IsAtBol ( ) : BOOLEAN

; PROCEDURE EnsureBol ( )

; PROCEDURE NewLine ( )

; PROCEDURE PutText ( Value : Text . T )

; PROCEDURE PutTextLine ( Value : Text . T )

; PROCEDURE PutInt ( Value : INTEGER ; MinWidth : INTEGER := 1 )

; PROCEDURE PutReal ( Value : REAL )

; PROCEDURE Flush ( )

; PROCEDURE Close ( )

; END Report
.

