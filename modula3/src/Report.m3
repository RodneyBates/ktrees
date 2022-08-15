
(* -----------------------------------------------------------------------1- *)
(* This is part of a reference implementation of K-trees, AKA Sequence trees.*)
(* Copyright 1994..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE Report

(* Some output utility procedures to 
   1) Simplify language portability
   2) Shorten some calls elsewhere
   3) Add beginning of line functions
   NOTE: Makes the reckless assumption that no client passes
         a \n to PutText or PutTextLine.
*)

; IMPORT Text
; IMPORT Fmt
; IMPORT Wr
; IMPORT Stdio

; VAR InternalIsAtBol : BOOLEAN := TRUE
; VAR WriteStream : Wr . T := Stdio . stdout

(* EXPORTED: *)
; PROCEDURE IsAtBol  ( ): BOOLEAN

  = BEGIN
      RETURN InternalIsAtBol 
    END IsAtBol

(* EXPORTED: *)
; PROCEDURE EnsureBol ( )

  = BEGIN
      IF NOT InternalIsAtBol 
      THEN
        NewLine ( )
      END (* IF *)
    END EnsureBol

(* EXPORTED: *)
; PROCEDURE NewLine ( )

  = <*FATAL ANY*> BEGIN
      Wr . PutChar ( WriteStream , '\n' )
    ; InternalIsAtBol := TRUE
    END NewLine

(* EXPORTED: *)
; PROCEDURE PutText ( Value : Text . T )

  = <*FATAL ANY*> BEGIN
      Wr . PutText ( WriteStream , Value )
    ; InternalIsAtBol := FALSE
    END PutText

(* EXPORTED: *)
; PROCEDURE PutTextLine ( Value : Text . T )

  = BEGIN
      PutText ( Value )
    ; NewLine ( )
    END PutTextLine

(* EXPORTED: *)
; PROCEDURE PutInt ( Value : INTEGER ; MinWidth : INTEGER := 1 )

  = BEGIN
      PutText ( Fmt . Pad ( Fmt . Int ( Value ) , MinWidth ) )
    ; InternalIsAtBol := FALSE
    END PutInt

(* EXPORTED: *)
; PROCEDURE PutReal ( Value : REAL )

  = BEGIN
      PutText ( Fmt . Real ( Value ) )
    ; InternalIsAtBol := FALSE
    END PutReal

(* EXPORTED: *)
; PROCEDURE Flush ( )

  = <*FATAL ANY*> BEGIN
      Wr . Flush ( WriteStream )
    END Flush

(* EXPORTED: *)
; PROCEDURE Close ( )

  = <*FATAL ANY*> BEGIN
      EnsureBol ( )
    ; Wr . Close ( WriteStream )
    END Close

; BEGIN
  END Report
.

