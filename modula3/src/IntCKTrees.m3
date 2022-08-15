
(* -----------------------------------------------------------------------1- *)
(* This is part of a reference implementation of K-trees, AKA Sequence trees.*)
(* Copyright 1994..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE IntCKTrees 
  (* An overlayer of IntKTrees, with massive, brute-force
     correctness checking.  
  *)

; IMPORT IntKTrees AS KTrees
; IMPORT Text
; IMPORT Report
; IMPORT Fmt 
; CONST SeqSsImage = Fmt . Int 

(* EXPORTED: *)
; PROCEDURE Empty ( ) : T 

  = VAR LKTree : T := KTrees . Empty ( )

  ; <*FATAL ANY*> BEGIN (* Yes, This is A Bit Silly. *) 
      IF ChecksEnabled 
      THEN 
        IF KTrees . Length ( LKTree ) # 0 
        THEN 
          Report . PutTextLine ( "Bad Empty" ) 
        ; KTrees . Dump ( LKTree ) 
        END (* IF *) 
      END (* IF *) 
    ; RETURN LKTree 
    END Empty 

(* EXPORTED: *)
; PROCEDURE Singleton ( Element : ElemT ) : T 

  = VAR LKTree : T := KTrees . Singleton ( Element ) 

  ; <*FATAL ANY*> BEGIN 
      IF ChecksEnabled 
      THEN 
        IF KTrees . Length ( LKTree ) # 1 
        THEN 
          Report . PutTextLine ( "Singleton bad length" ) 
        ; KTrees . Dump ( LKTree ) 
        ELSIF KTrees . IthElement ( LKTree , 0 ) # Element 
        THEN 
          Report . PutTextLine 
            ( "Singleton bad element value, should be " 
              & KTrees . ElemImage ( Element ) 
            ) 
        ; KTrees . Dump ( LKTree ) 
        END (* IF *) 
      END (* IF *) 
    ; RETURN LKTree 
    END Singleton 

(* EXPORTED: *)
; PROCEDURE Length ( KTree : T ) : KTrees . SeqSsTyp 

  = VAR Result : KTrees . SeqSsTyp := KTrees . Length ( KTree ) 
  ; VAR Count : KTrees . SeqSsTyp := 0  

  ; PROCEDURE Visit ( <* UNUSED *> Element : KTrees . ElemT ) 

    = BEGIN 
        INC ( Count )
      END Visit 

  ; <*FATAL ANY*> BEGIN 
      IF ChecksEnabled 
      THEN 
        KTrees . Traverse ( KTree , Visit ) 
      ; IF Result # Count 
        THEN 
          Report . PutTextLine 
            ( "Bad Length " 
              & SeqSsImage ( Result ) 
              & SeqSsImage ( Count ) 
            ) 
        ; KTrees . Dump ( KTree ) 
        END (* IF *) 
      END (* IF *) 
    ; RETURN Result 
    END Length 

; EXCEPTION QuitCheckIthElement 
; PROCEDURE CheckIthElement 
    ( KTree : T 
    ; Subscript : KTrees . SeqSsTyp 
    ; Element : ElemT 
    ; Caller : Text . T
    ) 

  = VAR Count : KTrees . SeqSsTyp := 0 

  ; PROCEDURE Visit ( VisitElement : ElemT ) 
    RAISES { QuitCheckIthElement }

    = BEGIN 
        IF Count = Subscript 
        THEN 
          IF VisitElement # Element 
          THEN 
            Report . PutTextLine 
              ( Caller 
                & " Bad value " 
                & SeqSsImage ( Subscript ) 
                & KTrees . ElemImage ( VisitElement ) 
                & KTrees . ElemImage ( Element ) 
              ) 
          ; KTrees . Dump ( KTree ) 
          END (* IF *) 
        ; RAISE QuitCheckIthElement 
        END (* IF *) 
      ; INC ( Count )
      END Visit 

  ; <*FATAL ANY*> BEGIN (* CheckIthElement *)
      IF ChecksEnabled 
      THEN 
        TRY
          KTrees . Traverse ( KTree , Visit ) 
        ; Report . PutTextLine 
            ( Caller 
              & " short sequence " 
              & SeqSsImage ( Subscript ) 
              & SeqSsImage ( Count ) 
            ) 
        EXCEPT
          QuitCheckIthElement =>  
        END (* TRY EXCEPT *)
      END (* IF *) 
    END CheckIthElement 

(* EXPORTED: *)
; PROCEDURE IthElement 
    ( KTree : T ; Subscript : KTrees . SeqSsTyp ) 
  : ElemT 
  RAISES { KTrees . SubscriptOutOfBounds }

  = VAR LElement : ElemT 

  ; <*FATAL ANY*> BEGIN 
      TRY
        LElement := KTrees . IthElement ( KTree , Subscript ) 
      EXCEPT
      ELSE
        Report . PutTextLine 
          ( "Exception raised in IthElement for " 
            & SeqSsImage ( Subscript ) 
          ) 
      ; KTrees . Dump ( KTree ) 
      ; RETURN LElement
      END (* TRY-EXCEPT *)
    ; IF ChecksEnabled 
      THEN 
        CheckIthElement 
          ( KTree , Subscript , LElement , "IthElement" ) 
      END (* IF *) 
    ; RETURN LElement 
    END IthElement 

; EXCEPTION QuitTraverse
; PROCEDURE CompareSlices 
    ( KTree1 : T 
    ; FromSs1 : KTrees . SeqSsTyp 
    ; ThruSs1 : INTEGER
    ; KTree2 : T 
    ; FromSs2 : KTrees . SeqSsTyp 
    ; ThruSs2 : INTEGER
    ; VAR IsOk : BOOLEAN 
    ; VAR BadSs1 : INTEGER
    ) 

  = VAR SliceArray1 
          := NEW ( REF ARRAY OF ElemT 
                 , MAX ( 1 , ThruSs1 - FromSs1 + 1 )
                 )
  ; VAR Ss1 : KTrees . SeqSsTyp := 0 
  ; VAR Ss2 : KTrees . SeqSsTyp := 0

  ; PROCEDURE Visit1 ( Element : ElemT ) 
    RAISES { QuitTraverse }
    (* Fill SliceArray1 ^ with KTree1 [ FromSs1 .. ThruSs1 ] *)

    = BEGIN 
        IF Ss1 >= FromSs1 
        THEN
          SliceArray1 ^ [ Ss1 - FromSs1 ] := Element 
        END (* IF *)
      ; INC ( Ss1 )
      ; IF Ss1 > ThruSs1
        THEN
          RAISE QuitTraverse
        END (* IF *)
      END Visit1 

  ; PROCEDURE Visit2 ( Element : ElemT ) 
    RAISES { QuitTraverse }
    (* Compare SliceArray1 ^ to KTree2 [ FromSs2 .. ThruSs2 ] *)

    = BEGIN 
        IF Ss2 >= FromSs2 
        THEN 
          IF SliceArray1 ^ [ Ss2 - FromSs2 ] # Element 
          THEN 
            IsOk := FALSE 
          ; BadSs1 := Ss2 - FromSs2 + FromSs1
          ; RAISE QuitTraverse
          END (* IF *) 
        END (* IF *) 
      ; INC ( Ss2 )
      ; IF Ss2 > ThruSs2 
        THEN 
          RAISE QuitTraverse
        END (* IF *)
      END Visit2 

  ; <*FATAL ANY*> BEGIN (* CompareSlices *)
      IsOk := TRUE 
    ; BadSs1 := 0 
    ; IF FromSs1 > ThruSs1 
      THEN (* Slice 1 empty. *)
        IF FromSs2 <= ThruSs2
        THEN (* Slice 2 nonempty. *)
          IsOk := FALSE
        ; BadSs1 := FromSs1
        END (* IF *)
      ELSE (* Slice 1 nonempty.  *)
        IF FromSs2 > ThruSs2
        THEN (* Slice 2 empty. *)
          IsOk := FALSE
        ; BadSs1 := FromSs1
        ELSE (* Both slices nonempty. *)
          IF ThruSs1 - FromSs1 # ThruSs2 - FromSs2
          THEN (* Lengths differ. *)
            IsOk := FALSE
          ; BadSs1 := MIN ( ThruSs1 , ThruSs2 - FromSs2 + FromSs1 )
          ELSE (* Equal lengths, nonempty. *) 
            TRY
              KTrees . Traverse ( KTree1 , Visit1 ) 
            EXCEPT
              QuitTraverse =>
            END (* TRY EXCEPT *)
          ; TRY
              KTrees . Traverse ( KTree2 , Visit2 ) 
            EXCEPT
              QuitTraverse =>
            END (* TRY EXCEPT *)
          END (* IF *)
        END (* IF *)
      END (* IF *)
    END CompareSlices 

; PROCEDURE CheckSlice 
    ( KTree : T 
    ; FromSs : KTrees . SeqSsTyp 
    ; ThruSs : INTEGER
    ; SliceKTree : T 
    ; VAR IsOk : BOOLEAN 
    ; VAR BadKTreeSs : INTEGER
    ) 

  = <*FATAL ANY*> BEGIN 
      CompareSlices
        ( KTree 
        , FromSs 
        , ThruSs 
        , SliceKTree 
        , 0 
        , KTrees . Length ( SliceKTree ) - 1
        , IsOk
        , BadKTreeSs
        )
    END CheckSlice 

(* EXPORTED: *)
; PROCEDURE StoreIthElement 
    ( KTree : T 
    ; Subscript : KTrees . SeqSsTyp 
    ; Value : ElemT 
    ) 
  : T
  RAISES { KTrees . SubscriptOutOfBounds }

  = VAR LResult : T
  ; VAR LIsOk : BOOLEAN
  ; VAR LBadSs : INTEGER
  ; VAR LLength : KTrees . SeqSsTyp

  ; <*FATAL ANY*> BEGIN 
      TRY
        LResult 
          := KTrees . StoreIthElement ( KTree , Subscript , Value )  
      EXCEPT
      ELSE
        Report . PutTextLine 
          ( "Exception raised in StoreIthElement for " 
            & SeqSsImage ( Subscript ) & " "
            & KTrees . ElemImage  ( Value ) 
          ) 
      ; KTrees . Dump ( KTree ) 
      ; RETURN NIL
      END (* TRY-EXCEPT *)
    ; IF ChecksEnabled 
      THEN 
        CompareSlices
          ( KTree , 0 , Subscript - 1 
          , LResult  , 0 , Subscript - 1 
          , LIsOk , LBadSs 
          )
      ; IF NOT LIsOk 
        THEN 
          Report . PutTextLine 
            ( "Store, lower part element mismatch at " 
            & SeqSsImage ( LBadSs ) 
            ) 
        ; KTrees . Dump ( KTree ) 
        ; KTrees . Dump ( LResult ) 
        END (* IF *)
      ; CheckIthElement 
          ( LResult , Subscript , Value , "StoreIthElement" ) 
      ; LLength := KTrees . Length ( KTree )
      ; CompareSlices
          ( KTree , Subscript + 1 , LLength - 1
          , LResult  , Subscript + 1 , LLength - 1
          , LIsOk , LBadSs 
          )
      ; IF NOT LIsOk 
        THEN 
          Report . PutTextLine 
            ( "Store, upper part element mismatch at " 
            & SeqSsImage ( LBadSs ) 
            ) 
        ; KTrees . Dump ( KTree ) 
        ; KTrees . Dump ( LResult ) 
        END (* IF *)
      END (* IF *) 
    ; RETURN LResult
    END StoreIthElement 

(* EXPORTED: *)
; PROCEDURE Slice 
    ( KTree : T ; FromSs : SeqSsTyp ; ThruSs : INTEGER ) 
  : T 
  RAISES { KTrees . SubscriptOutOfBounds }

  = VAR LResult : T 
  ; VAR LIsOk : BOOLEAN  
  ; VAR LBadSs : INTEGER

  ; <*FATAL ANY*> BEGIN 
      TRY
        LResult := KTrees . Slice ( KTree , FromSs , ThruSs ) 
      EXCEPT
      ELSE
        Report . PutTextLine 
          ( "Exception raised in Slice for " 
            & SeqSsImage ( FromSs ) & " "
            & SeqSsImage ( ThruSs ) 
          ) 
      ; KTrees . Dump ( KTree ) 
      ; RETURN NIL
      END (* TRY-EXCEPT *)
    ; IF ChecksEnabled 
      THEN 
        IF KTrees . IsConsistent ( LResult ) 
        THEN 
          IF ThruSs - FromSs + 1 # KTrees . Length ( LResult ) 
          THEN 
            Report . PutTextLine 
              ( "Slice, wrong length: " 
                & SeqSsImage ( KTrees . Length ( KTree ) ) & " "
                & SeqSsImage ( FromSs ) & " "
                & SeqSsImage ( ThruSs ) & " "
                & SeqSsImage ( KTrees . Length ( LResult ) ) 
              ) 
          ; KTrees . Dump ( KTree ) 
          ; KTrees . Dump ( LResult ) 
          ELSE 
            CheckSlice 
              ( KTree , FromSs , ThruSs , LResult , LIsOk , LBadSs ) 
          ; IF NOT LIsOk 
            THEN 
              Report . PutTextLine 
                ( "Slice, element mismatch at " 
                  & SeqSsImage ( LBadSs ) & " "
                  & SeqSsImage ( FromSs ) & " "
                  & SeqSsImage ( ThruSs ) 
                ) 
            ; KTrees . Dump ( KTree ) 
            ; KTrees . Dump ( LResult ) 
            END (* IF *) 
          END (* IF *) 
        ELSE (* not Consistent*) 
          Report . PutTextLine 
            ( "Slice result not consistent " 
              & SeqSsImage ( FromSs ) & " "
              & SeqSsImage ( ThruSs ) 
            ) 
        ; KTrees . Dump ( KTree ) 
        ; KTrees . Dump ( LResult ) 
        END (* IF *) 
      END (* IF *) 
    ; RETURN LResult 
    END Slice 

(* EXPORTED: *)
; PROCEDURE Cat ( Left , Right : T ) : T 

  = VAR LResult : T 
  ; VAR LLeftLength : KTrees . SeqSsTyp  
  ; VAR LIsOk : BOOLEAN 
  ; VAR LBadSs : INTEGER

  ; <*FATAL ANY*> BEGIN 
      TRY
        LResult := KTrees . Cat ( Left , Right ) 
      EXCEPT
      ELSE
        Report . PutTextLine ( "Exception raised in Cat." ) 
      ; KTrees . Dump ( Left ) 
      ; KTrees . Dump ( Right ) 
      ; RETURN NIL
      END (* TRY-EXCEPT *)
    ; IF ChecksEnabled 
      THEN 
        IF KTrees . IsConsistent ( LResult ) 
        THEN 
          LLeftLength := KTrees . Length ( Left ) 
        ; CheckSlice 
            ( LResult , 0 , LLeftLength - 1 , Left , LIsOk , LBadSs ) 
        ; IF NOT LIsOk 
          THEN 
            Report . PutTextLine 
              ( "Cat, left, element mismatch at " 
                & SeqSsImage ( LBadSs ) 
              ) 
          ; KTrees . Dump ( Left ) 
          ; KTrees . Dump ( Right ) 
          ; KTrees . Dump ( LResult ) 
          ELSE 
            CheckSlice 
              ( LResult 
              , LLeftLength 
              , LLeftLength + KTrees . Length ( Right ) - 1 
              , Right 
              , LIsOk 
              , LBadSs 
              ) 
          ; IF NOT LIsOk 
            THEN 
              Report . PutTextLine 
                ( "Cat, right, element mismatch at " 
                  & SeqSsImage ( LBadSs ) 
                ) 
            ; KTrees . Dump ( Left ) 
            ; KTrees . Dump ( Right ) 
            ; KTrees . Dump ( LResult ) 
            END (* IF *) 
          END (* IF *) 
        ELSE (* not Consistent*) 
          Report . PutTextLine ( "cat result not consistent" ) 
        ; KTrees . Dump ( Left ) 
        ; KTrees . Dump ( Right ) 
        ; KTrees . Dump ( LResult ) 
        END (* IF *) 
      END (* IF *) 
    ; RETURN LResult 
    END Cat 

; BEGIN
  END IntCKTrees
. 



