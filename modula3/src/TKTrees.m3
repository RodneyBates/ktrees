
(* -----------------------------------------------------------------------1- *)
(* This is part of a reference implementation of K-trees, AKA Sequence trees.*)
(* Copyright 1994..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE TKTrees 
EXPORTS Main
; IMPORT IntKTrees 
; IMPORT IntCKTrees 
; IMPORT Report
; IMPORT Random

  ; CONST ForestSize = 5001 
  ; CONST SingletonCt = 5000 
  ; CONST CatCt = 15000 
  ; CONST StoreCt = 10000 
  ; CONST CatOfSliceCt = 10000 
  ; VAR NextElement : IntKTrees . ElemT := 0
  ; VAR Forest := NEW ( REF ARRAY OF IntKTrees . T , ForestSize )
  ; VAR NextUnusedTree : INTEGER := 0
  ; VAR NextStore : INTEGER := 0
  ; VAR ProgressCt : INTEGER := 0

; PROCEDURE ResetProgress ( )

  = BEGIN 
      ProgressCt := 0 
    END ResetProgress 

; PROCEDURE ShowProgress ( )

  = BEGIN 
      IF ( ProgressCt MOD 50 = 0 ) AND ( NOT Report . IsAtBol ( ) ) 
      THEN 
        Report . NewLine ( )
      END (* IF *) 
    ; IF Report . IsAtBol ( ) 
      THEN 
        Report . PutInt ( ProgressCt , 6 ) 
      ; Report . PutText ( " " ) 
      END (* IF *) 
    ; Report . PutText ( "." ) 
 
    ; INC ( ProgressCt )
    ; Report . Flush ( )
    END ShowProgress 

; PROCEDURE GetNewElement ( VAR Element : IntKTrees . ElemT ) 

  = BEGIN 
      Element := NextElement 
    ; INC ( NextElement )
    END GetNewElement 

; VAR Rand := NEW ( Random . Default ) . init ( fixed := TRUE ) 

; PROCEDURE RandomMod ( Mod : CARDINAL ) : CARDINAL

  = BEGIN 
      RETURN Rand . integer ( 0 , Mod - 1 ) 
    END RandomMod 

; PROCEDURE StoreTree ( Tree : IntKTrees . T ) 

  = BEGIN 
      IF NextUnusedTree < ForestSize - 1 
      THEN 
        Forest ^ [ NextUnusedTree ] := Tree 
      ; INC ( NextUnusedTree ) 
      ELSIF NextStore < ForestSize - 1 
      THEN 
        Forest ^ [ NextStore ] := Tree 
      ; INC ( NextStore ) 
      ELSE 
        Forest ^ [ 0 ] := Tree 
      ; NextStore := 1 
      END (* IF *) 
    END StoreTree 

; PROCEDURE PrintStats ( )

  = VAR LeafNodes , LeafElements , NonleafNodes , NonleafElements 
      : INTEGER := 0
  ; VAR HeightSum , LengthSum , LeafNodesSum , LeafElementsSum 
        , NonleafNodesSum , NonleafElementsSum 
      : INTEGER := 0
  ; VAR LFloatTreeCt : REAL := FLOAT ( NextUnusedTree )

  ; <*FATAL ANY*> BEGIN 
      FOR I := 0 TO NextUnusedTree - 1 
      DO 
        INC ( HeightSum , IntKTrees . Height ( Forest ^ [ I ] ) )
      ; INC ( LengthSum , IntKTrees . Length ( Forest ^ [ I ] ) )
      ; IntKTrees . InternalStatistics 
          ( Forest ^ [ I ] 
          , LeafNodes 
          , LeafElements 
          , NonleafNodes 
          , NonleafElements 
          ) 
      ; INC ( LeafNodesSum , LeafNodes )
      ; INC ( LeafElementsSum , LeafElements )
      ; INC ( NonleafNodesSum , NonleafNodes )
      ; INC ( NonleafElementsSum , NonleafElements )
      END (* FOR *) 
    ; Report . EnsureBol ( )
    ; Report . PutInt ( NextUnusedTree ) 
    ; Report . PutTextLine ( " Sequences" ) 

    ; Report . PutReal ( FLOAT ( HeightSum ) / LFloatTreeCt ) 
    ; Report . PutTextLine ( " Average Height" ) 

    ; Report . PutReal ( FLOAT ( LengthSum ) / LFloatTreeCt ) 
    ; Report . PutTextLine ( " Average Length" ) 

    ; Report . PutReal
        ( FLOAT ( LeafElementsSum + NonleafElementsSum ) 
          / FLOAT ( LeafNodesSum + NonleafNodesSum ) 
        ) 
    ; Report . PutTextLine ( " Average elements per node" ) 

    ; Report . PutReal 
        ( FLOAT ( LeafElementsSum ) / FLOAT ( LeafNodesSum ) ) 
    ; Report . PutTextLine ( " Average elements per leaf node" ) 

    ; Report . PutReal
        ( FLOAT ( NonleafElementsSum ) / FLOAT ( NonleafNodesSum ) ) 
    ; Report . PutTextLine ( " Average elements per nonleaf node" ) 

    ; Report . PutReal ( FLOAT ( LeafNodesSum ) / LFloatTreeCt ) 
    ; Report . PutTextLine ( " Average LeafNodes" ) 

    ; Report . PutReal ( FLOAT ( LeafElementsSum ) / LFloatTreeCt ) 
    ; Report . PutTextLine ( " Average LeafElements" ) 

    ; Report . PutReal ( FLOAT ( NonleafNodesSum ) / LFloatTreeCt ) 
    ; Report . PutTextLine ( " Average NonleafNodes" ) 

    ; Report . PutReal
        ( FLOAT ( NonleafElementsSum ) / LFloatTreeCt ) 
    ; Report . PutTextLine ( " Average NonleafElements" ) 
    END PrintStats 

; PROCEDURE Test ( )

  = VAR LElement : IntKTrees . ElemT 
  ; VAR LTree , LSlice1 , LSlice2 : IntKTrees . T 
  ; VAR J , FromSs : IntKTrees . SeqSsTyp 
  ; VAR ThruSs : INTEGER 
  ; VAR Length : IntKTrees . SeqSsTyp 

  ; <*FATAL ANY*> BEGIN 
      IntCKTrees . ChecksEnabled := TRUE 
    ; Report . EnsureBol ( )
    ; Report . PutTextLine ( "Starting test" ) 
    ; ResetProgress ( ) 
    ; LTree := IntCKTrees . Empty ( )
    ; StoreTree ( LTree ) 
    ; ShowProgress ( ) 

    (* Generate singletons. *)

    ; Report . EnsureBol ( ) 
    ; Report . PutTextLine ( "Singletons" ) 
    ; ResetProgress ( ) 
    ; FOR I := 0 TO SingletonCt 
      DO 
        GetNewElement ( LElement ) 
      ; LTree := IntCKTrees . Singleton ( LElement ) 
      ; StoreTree ( LTree ) 
      ; ShowProgress ( ) 
      END (* FOR *) 

    (* Concatenate some sequences. *)

    ; Report . EnsureBol ( ) 
    ; Report . PutTextLine ( "Cats" ) 
    ; ResetProgress ( ) 
    ; FOR I := 0 TO CatCt 
      DO 
        LTree 
          := IntCKTrees . Cat 
               ( Forest ^ [ RandomMod ( NextUnusedTree ) ] 
               , IntCKTrees . Cat 
                   ( Forest ^ [ RandomMod ( NextUnusedTree ) ] 
                   , Forest ^ [ RandomMod ( NextUnusedTree ) ] 
                   ) 
               ) 
      ; StoreTree ( LTree ) 
      ; ShowProgress ( ) 
      END (* FOR *) 

    (* Take slices and concatenate them, to keep sequences 
       from getting shorter. *)

    ; Report . EnsureBol ( ) 
    ; Report . PutTextLine ( "Cats of slices" ) 
    ; ResetProgress ( ) 
    ; FOR I := 0 TO CatOfSliceCt 
      DO 
        J := RandomMod ( NextUnusedTree ) 
      ; Length := IntCKTrees . Length ( Forest ^ [ J ] ) 
      ; IF Length <= 1 
        THEN 
          FromSs := 0 
        ; ThruSs := - 1 
        ELSE 
          FromSs := RandomMod ( Length DIV 2 ) 
        ; ThruSs := Length DIV 2 + RandomMod ( Length DIV 2 ) 
        END (* IF *) 
      ; LSlice1 
          := IntCKTrees . Slice ( Forest ^ [ J ] , FromSs , ThruSs ) 
      ; J := RandomMod ( NextUnusedTree )  
      ; Length := IntCKTrees . Length ( Forest ^ [ J ] ) 
      ; IF Length <= 1 
        THEN 
          FromSs := 0 
        ; ThruSs := - 1 
        ELSE 
          FromSs := RandomMod ( Length DIV 2 ) 
        ; ThruSs := Length DIV 2 + RandomMod ( Length DIV 2 ) 
        END (* IF *) 
      ; LSlice2 
          := IntCKTrees . Slice ( Forest ^ [ J ] , FromSs , ThruSs ) 
      ; LTree := IntCKTrees . Cat ( LSlice1 , LSlice2 ) 
      ; StoreTree ( LTree ) 
      ; ShowProgress ( ) 
      END (* FOR *) 

    (* Stores into sequences. *)

    ; Report . EnsureBol ( ) 
    ; Report . PutTextLine ( "Stores" ) 
    ; ResetProgress ( ) 
    ; FOR I := 0 TO StoreCt 
      DO 
        LTree := Forest ^ [ RandomMod ( NextUnusedTree ) ]
      ; Length := IntCKTrees . Length ( LTree ) 
      ; IF Length > 0
        THEN
          IF Length > 1
          THEN
            FromSs := Length DIV 2 + RandomMod ( Length DIV 2 ) 
          ELSE
            FromSs := 0
          END (* IF *)
        ; GetNewElement ( LElement ) 
        ; LTree 
            := IntCKTrees . StoreIthElement 
                 ( LTree , FromSs , LElement ) 
        ; StoreTree ( LTree ) 
        END (* IF *)
      ; ShowProgress ( ) 
      END (* FOR *) 

    ; PrintStats ( )
    ; Report . EnsureBol ( )
    ; Report . PutTextLine ( "End of test" ) 
    END Test 

; BEGIN (* TKTrees*) 
    TRY
      Test ( )
    FINALLY
      Report . Close ( )
    END (* TRY FINALLY *)
  END TKTrees 
. 






