
(* -----------------------------------------------------------------------1- *)
(* This is part of a reference implementation of K-trees, AKA Sequence trees.*)
(* Copyright 1994..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

GENERIC MODULE KTrees ( Elem ) 
 
; FROM Asserts IMPORT Assert , CantHappen 
; IMPORT Fmt 
; IMPORT Report
; IMPORT Text
; IMPORT Image 
 
; CONST SeqSsImage = Fmt.Int 
 
; CONST RefImage = Image.Ref 
 
; VAR MaxElemCt := DefaultMaxElemCt  

(* EXPORTED: *) 
; PROCEDURE SetMaxElemCt ( Value : CARDINAL ) 

  = BEGIN 
      MaxElemCt := MAX ( 3 , MIN ( Value , MaxMaxElemCt ) ) 
    END SetMaxElemCt 

; <*UNUSED*> TYPE DummyElemT = Elem . T (* Just to suppress warning. *)

; TYPE ElemNoTyp = INTEGER 
  (* Used internally for computations which could go outside
     the range of SeqSsTyp. *)
; CONST ElemNoImage = Fmt.Int 
 
; TYPE HeightTyp = [ 0 .. LAST ( INTEGER ) ] 
; CONST HeightImage = Fmt.Int 
 
; REVEAL T 
    = ROOT BRANDED OBJECT (* Abstract *) 
           METHODS 
             Height ( ) : HeightTyp 
           ; ElemCt ( ) : ElemNoTyp 
           END 
; TYPE NodeObjTyp = T 
 
; TYPE LeafArrayTyp = ARRAY OF ElemT 
; TYPE LeafArrayRefTyp = REF LeafArrayTyp 
; TYPE LeafNodeObjTyp 
    = NodeObjTyp 
        OBJECT 
          LeafElems : LeafArrayRefTyp := NIL 
        OVERRIDES 
          Height := LeafHeight 
        ; ElemCt := LeafElemCt 
        END 
 
; PROCEDURE LeafHeight 
    ( <*UNUSED*> Self : LeafNodeObjTyp ) : HeightTyp 

  = BEGIN 
    RETURN 1 
  END LeafHeight 
 
; PROCEDURE LeafElemCt ( Self : LeafNodeObjTyp ) : ElemNoTyp

  = BEGIN 
      RETURN NUMBER ( Self . LeafElems ^ ) 
    END LeafElemCt 
 
; PROCEDURE NewLeaf ( ElemCt : ElemNoTyp ) : LeafNodeObjTyp 

  = BEGIN 
      RETURN 
        NEW ( LeafNodeObjTyp 
            , LeafElems := NEW ( LeafArrayRefTyp , ElemCt ) 
            ) 
    END NewLeaf 
 
; TYPE NonleafElemTyp 
    = RECORD 
         NleChildRef : NodeObjTyp 
       ; NleCumChildCt : SeqSsTyp 
       END 
; TYPE NonleafArrayTyp = ARRAY OF NonleafElemTyp 
; TYPE NonleafArrayRefTyp = REF NonleafArrayTyp 
; TYPE NonleafNodeObjTyp 
    = NodeObjTyp 
        OBJECT 
          StoredHeight : HeightTyp := 0 
        ; NonleafElems : NonleafArrayRefTyp := NIL 
        OVERRIDES 
          Height := NonleafHeight 
        ; ElemCt := NonleafElemCt 
        END 
 
; PROCEDURE NonleafHeight ( Self : NonleafNodeObjTyp ) : HeightTyp 

  = BEGIN 
      RETURN Self . StoredHeight 
    END NonleafHeight 
 
; PROCEDURE NonleafElemCt ( Self : NonleafNodeObjTyp ) : ElemNoTyp 

  = BEGIN 
      RETURN NUMBER ( Self . NonleafElems ^ ) 
    END NonleafElemCt 
 
; PROCEDURE NewNonleaf 
    ( Height : HeightTyp ; ElemCt : ElemNoTyp ) 
  : NonleafNodeObjTyp 
  RAISES ANY
  
  = BEGIN 
      RETURN 
        NEW ( NonleafNodeObjTyp 
            , StoredHeight := Height 
            , NonleafElems := NEW ( NonleafArrayRefTyp , ElemCt ) 
            ) 
    END NewNonleaf 
 
; PROCEDURE ForRange ( Left , Right : ElemNoTyp ) : ElemNoTyp 
  RAISES ANY
  (* Number of iterations in FOR I := Left TO Right DO ... *)

  = BEGIN 
      IF Right < Left 
      THEN 
        RETURN 0 
      ELSE 
        RETURN Right - Left + 1 
      END (* IF *) 
    END ForRange 

; PROCEDURE CumChildrenToLeft 
    ( KTree : NodeObjTyp ; I : ElemNoTyp ) 
  : SeqSsTyp 
  RAISES ANY

  = BEGIN 
      IF KTree = NIL 
      THEN 
        RETURN 0 
      ELSE
        TYPECASE KTree <*NOWARN*>
        OF LeafNodeObjTyp 
          => RETURN I
        | NonleafNodeObjTyp ( TNonleafNode )
          => IF I = 0
             THEN
               RETURN 0
             ELSE
               RETURN 
                 TNonleafNode 
                 . NonleafElems ^ [ I - 1 ] . NleCumChildCt 
             END (* IF *)
        END (* TYPECASE *)
      END (* IF *)
    END CumChildrenToLeft 

(* EXPORTED: *) 
; PROCEDURE Empty ( ) : NodeObjTyp 
  RAISES ANY
  (* Return an empty sequence. *)

   = BEGIN 
      RETURN NIL 
    END Empty 

(* EXPORTED: *) 
; PROCEDURE Singleton ( Element : ElemT ) : NodeObjTyp 
  RAISES ANY
  (* Return a sequence with one element. *)

  = 
    VAR LResult : LeafNodeObjTyp 

  ; BEGIN 
      LResult := NewLeaf ( ElemCt := 1 ) 
    ; LResult . LeafElems ^ [ 0 ] := Element 
    ; RETURN LResult 
    END Singleton 

(* EXPORTED: *) 
; PROCEDURE Length ( KTree : NodeObjTyp ) : SeqSsTyp 
  RAISES ANY
  (* Number of elements in a sequence. *)

  = BEGIN 
      IF KTree = NIL 
      THEN 
        RETURN 0 
      ELSE
        TYPECASE KTree <*NOWARN*>
        OF LeafNodeObjTyp ( TLeafNode )
          => RETURN LeafNodeObjTyp . ElemCt ( TLeafNode )
        | NonleafNodeObjTyp ( TNonleafNode )
          => RETURN 
               TNonleafNode . NonleafElems 
               ^ [ NonleafNodeObjTyp . ElemCt ( TNonleafNode ) - 1 ] 
               . NleCumChildCt 
        END (* TYPECASE *)
      END (* IF *) 
    END Length 

; PROCEDURE BinSearch 
    ( NonleafKTree : NonleafNodeObjTyp 
    ; Subscript : SeqSsTyp 
    ; VAR ElemNo : ElemNoTyp 
    ; VAR ChildSubscript : SeqSsTyp 
    ; LeftmostElemNo : ElemNoTyp := 0 
      (* caller knows the desired element can't be less than this. *) 
    ) 
  RAISES ANY
  (* Binary search a nonleaf node for the subtree containing the
     Subscript'th element. *)

  = 
    VAR LLoElemNo : ElemNoTyp := LeftmostElemNo 
  ; VAR LHiElemNo : ElemNoTyp := NonleafKTree . ElemCt ( ) - 1 
  ; VAR LProbeElemNo : ElemNoTyp 
  ; VAR LProbeMaxSs : SeqSsTyp 

  ; BEGIN 
      Assert ( 0 <= LLoElemNo , "BinSearch, LeftmostElemNo < 0. " ) 
    ; Assert 
        ( LLoElemNo <= LHiElemNo 
        , "BinSearch, Empty initial range. " 
        ) 
    ; Assert ( 0 <= Subscript , "BinSearch, low Subscript. " ) 
    ; Assert 
        ( Subscript 
          <= NonleafKTree . NonleafElems 
             ^ [ LHiElemNo ] 
             . NleCumChildCt 
        , "BinSearch, high Subscript. " 
        ) 
    ; WHILE LLoElemNo < LHiElemNo 
      DO 
        LProbeElemNo := ( LLoElemNo + LHiElemNo ) DIV 2 
      ; LProbeMaxSs 
          := NonleafKTree . NonleafElems 
             ^ [ LProbeElemNo ] 
             . NleCumChildCt 
             - 1 
      ; IF Subscript > LProbeMaxSs 
        THEN 
          LLoElemNo := LProbeElemNo + 1 
        ELSE 
          LHiElemNo := LProbeElemNo 
        ; IF Subscript = LProbeMaxSs 
          THEN 
            LLoElemNo := LProbeElemNo 
          END (* IF *) 
        END (* IF *) 
      END (* WHILE *) 
    ; ElemNo := LLoElemNo 
    ; IF LLoElemNo > 0 
      THEN 
        ChildSubscript 
          := Subscript 
             - NonleafKTree . NonleafElems 
               ^ [ LLoElemNo - 1 ] 
               . NleCumChildCt 
      ELSE 
        ChildSubscript := Subscript 
      END (* IF *) 
    END BinSearch 

; PROCEDURE DescendThruNonleafLevels 
    ( KTree : NodeObjTyp 
    ; Subscript : SeqSsTyp 
    ; VAR LeafLevel : LeafNodeObjTyp 
    ; VAR LeafElemNo : ElemNoTyp 
    ) 
  RAISES ANY
  (* Descend to the leaf containing Subscript'th sequence element. *)

  = 
    VAR LKTree : NodeObjTyp
  ; VAR LSubscript : SeqSsTyp := Subscript 
  ; VAR LElemNo : ElemNoTyp 

  ; BEGIN 
      IF KTree = NIL 
      THEN 
        RAISE SubscriptOutOfBounds 
      ELSE 
        LKTree := KTree
      ; LOOP
          TYPECASE LKTree <*NOWARN*>
          OF NonleafNodeObjTyp ( TNonleafNode )
            => BinSearch 
                 ( TNonleafNode , LSubscript , LElemNo , LSubscript ) 
            ; LKTree 
                := TNonleafNode . NonleafElems ^ [ LElemNo ] 
                   . NleChildRef 
            ; Assert 
                ( Length ( LKTree ) > LSubscript 
                , "DescendThruNonleafLevels, " 
                  & "bad results from BinSearch." 
                ) 
          | LeafNodeObjTyp ( LLeafNodeKTree )
            => LeafLevel := LLeafNodeKTree 
            ; LeafElemNo := LSubscript 
            ; EXIT
          END (* TYPECASE *)
        END (* LOOP *) 
      END (* IF *) 
    END DescendThruNonleafLevels 

(* EXPORTED: *) 
; PROCEDURE IthElement 
    ( KTree : NodeObjTyp ; Subscript : SeqSsTyp ) : ElemT 
  RAISES ANY
  (* Return Subscript'th sequence element. *)

  = 
    VAR LLeafKTree : LeafNodeObjTyp 
  ; VAR LLeafElemNo : ElemNoTyp 

  ; BEGIN 
      DescendThruNonleafLevels 
        ( KTree , Subscript , LLeafKTree , LLeafElemNo ) 
    ; RETURN LLeafKTree . LeafElems ^ [ LLeafElemNo ] 
    END IthElement 

(* EXPORTED: *) 
; PROCEDURE StoreIthElement 
    ( KTree : NodeObjTyp ; Subscript : SeqSsTyp ; Value : ElemT ) 
  : NodeObjTyp 
  RAISES ANY
  (* Store Value in Subscript'th sequence element. *)

  = 
    PROCEDURE Recurse 
      ( KTree : NodeObjTyp ; Subscript : SeqSsTyp )
    : NodeObjTyp
    RAISES ANY
    
    = 
      VAR LElemNo : ElemNoTyp
    ; VAR LSubscript : SeqSsTyp
    ; VAR LKTree : NodeObjTyp
    ; VAR LLeafResult : LeafNodeObjTyp
    ; VAR LNonleafResult : NonleafNodeObjTyp
    
    ; BEGIN
        TYPECASE KTree <*NOWARN*>
        OF LeafNodeObjTyp ( TLeafNode )
          => LLeafResult 
               := NewLeaf ( ElemCt := LeafNodeObjTyp . ElemCt ( TLeafNode ) )
          ; LLeafResult . LeafElems ^ := TLeafNode . LeafElems ^
          ; LLeafResult . LeafElems ^ [ Subscript ] := Value
          ; RETURN LLeafResult
        | NonleafNodeObjTyp ( TNonleafNode )
          => BinSearch 
               ( TNonleafNode , Subscript , LElemNo , LSubscript ) 
          ; LKTree 
              := TNonleafNode . NonleafElems ^ [ LElemNo ] 
                 . NleChildRef 
          ; Assert 
              ( Length ( LKTree ) > LSubscript 
              , "StoreIthElement.Recurse, "
                & "bad results from BinSearch." 
              ) 
          ; LNonleafResult 
              := NewNonleaf 
                   ( Height := NonleafNodeObjTyp . Height ( TNonleafNode )
                   , ElemCt := NonleafNodeObjTyp . ElemCt ( TNonleafNode )
                   )
          ; LNonleafResult . NonleafElems ^ 
              := TNonleafNode . NonleafElems ^
          ; LNonleafResult . NonleafElems ^ [ LElemNo ] . NleChildRef
              := Recurse 
                   ( TNonleafNode . NonleafElems ^ [ LElemNo ] 
                     . NleChildRef 
                   , LSubscript 
                   )
          ; RETURN LNonleafResult
        END (* TYPECASE *)
      END Recurse

  ; BEGIN (* StoreIthElement *)
      IF 0 <= Subscript AND Subscript < Length ( KTree )
      THEN
        RETURN Recurse ( KTree , Subscript )
      ELSE
        RAISE SubscriptOutOfBounds
      END (* IF *)
    END StoreIthElement 

(* Internal data and procedure for repacking leaf elements
   into 1, 2, or 3 new leaf nodes. *)

; TYPE LeafPackStateTyp 
    = RECORD 
         LpsI : ElemNoTyp; 
         LpsCurrentNewNode : LeafNodeObjTyp; 
         LpsCurrentNodeElemCt : ElemNoTyp; 
         LpsTotalNewElemCt : ElemNoTyp; 
         LpsNewKTree1 : LeafNodeObjTyp; 
         LpsNewKTree2 : LeafNodeObjTyp; 
         LpsNewKTree3 : LeafNodeObjTyp; 
       END 
 
; PROCEDURE InitRepackLeaf 
    ( VAR (* IN OUT *) State : LeafPackStateTyp 
    ; NewElemCt : ElemNoTyp 
    ) 
  RAISES ANY

  = BEGIN 
      Assert 
        ( NewElemCt <= 3 * MaxElemCt 
        , "InitRepackLeaf, total element count too big." 
        ) 
    ; State . LpsTotalNewElemCt := NewElemCt 
    ; IF NewElemCt > MaxElemCt * 2 
      THEN 
        State . LpsCurrentNodeElemCt := ( NewElemCt + 1 ) DIV 3 
      ELSIF NewElemCt > MaxElemCt 
      THEN 
        State . LpsCurrentNodeElemCt := NewElemCt DIV 2 
      ELSE 
        State . LpsCurrentNodeElemCt := NewElemCt 
      END (* IF *) 
    ; Assert 
        ( State . LpsCurrentNodeElemCt <= MaxElemCt 
        , "InitRepackLeaf, element count too big." 
        ) 
    ; State . LpsCurrentNewNode 
        := NewLeaf ( ElemCt := State . LpsCurrentNodeElemCt ) 
    ; State . LpsI := 0 
    ; State . LpsNewKTree1 := State . LpsCurrentNewNode 
    ; State . LpsNewKTree2 := NIL 
    ; State . LpsNewKTree3 := NIL 
    END InitRepackLeaf 

; PROCEDURE RepackLeafElem 
    ( VAR (* IN OUT *) State : LeafPackStateTyp ; Element : ElemT ) 
  RAISES ANY

  = BEGIN 
      Assert 
        ( State . LpsI <= State . LpsCurrentNodeElemCt 
        , "RepackLeafElem, LpsI excessive." 
        ) 
    ; IF State . LpsI = State . LpsCurrentNodeElemCt 
      THEN 
        IF State . LpsTotalNewElemCt > MaxElemCt * 2 
        THEN (* three new nodes total. *) 
          IF State . LpsNewKTree2 = NIL 
          THEN (* Ready to start 2nd new node of 3. *) 
            State . LpsCurrentNewNode 
              := NewLeaf ( ElemCt := State . LpsCurrentNodeElemCt ) 
          ; State . LpsNewKTree2 := State . LpsCurrentNewNode 
          ELSE (* Ready to start 3rd new node. *) 
            State . LpsCurrentNodeElemCt 
              := State . LpsTotalNewElemCt 
                 - State . LpsCurrentNodeElemCt * 2 
          ; Assert 
              ( State . LpsCurrentNodeElemCt <= MaxElemCt 
              , "RepackLeafElem, 3rd element count too big." 
              ) 
          ; State . LpsCurrentNewNode 
              := NewLeaf ( ElemCt := State . LpsCurrentNodeElemCt ) 
          ; State . LpsNewKTree3 := State . LpsCurrentNewNode 
          END (* IF *) 
        ELSE (* Last of two new nodes. *) 
          State . LpsCurrentNodeElemCt 
            := State . LpsTotalNewElemCt 
               - State . LpsCurrentNodeElemCt 
        ; Assert 
            ( State . LpsCurrentNodeElemCt <= MaxElemCt 
            , "RepackLeafElem, 2nd element count too big." 
            ) 
        ; State . LpsCurrentNewNode 
            := NewLeaf ( ElemCt := State . LpsCurrentNodeElemCt ) 
        ; State . LpsNewKTree2 := State . LpsCurrentNewNode 
        END (* IF *) 
      ; State . LpsI := 0 
      END (* IF *) 
    ; State . LpsCurrentNewNode . LeafElems ^ [ State . LpsI ] 
        := Element 
    ; INC ( State . LpsI )
    END RepackLeafElem 

; PROCEDURE FinalizeRepackLeaf 
    ( VAR (* IN OUT *) State : LeafPackStateTyp ) 
  RAISES ANY

  = BEGIN 
      Assert 
        ( State . LpsI = State . LpsCurrentNodeElemCt 
          AND ( State . LpsTotalNewElemCt > MaxElemCt ) 
              <= (* IMPLIES *) ( State . LpsNewKTree2 # NIL ) 
          AND ( State . LpsTotalNewElemCt > MaxElemCt * 2 ) 
              <= (* IMPLIES *) ( State . LpsNewKTree3 # NIL ) 
        , "FinalizeRepackLeaf, count mismatch." 
        ) 
    END FinalizeRepackLeaf 

(* Internal data and procedure for repacking nonleaf elements
   into 1, 2, or 3 new nonleaf nodes. *)

; TYPE NonleafPackStateTyp 
    = RECORD 
         NlpsI : ElemNoTyp; 
         NlpsCumChildCt : SeqSsTyp; 
         NlpsCurrentNewNode : NonleafNodeObjTyp; 
         NlpsCurrentNodeElemCt : ElemNoTyp; 
         NlpsTotalNewElemCt : ElemNoTyp; 
         NlpsNewKTree1 : NonleafNodeObjTyp; 
         NlpsNewKTree2 : NonleafNodeObjTyp; 
         NlpsNewKTree3 : NonleafNodeObjTyp; 
       END 

; PROCEDURE InitRepackNonleaf 
    ( VAR (* IN OUT *) State : NonleafPackStateTyp 
    ; NewElemCt : ElemNoTyp 
    ; Height : HeightTyp 
    ) 
  RAISES ANY

  = BEGIN 
      Assert 
        ( NewElemCt <= 3 * MaxElemCt 
        , "InitRepackNonleaf, total element count too big." 
        ) 
    ; State . NlpsTotalNewElemCt := NewElemCt 
    ; IF NewElemCt > MaxElemCt * 2 
      THEN 
        State . NlpsCurrentNodeElemCt := ( NewElemCt + 1 ) DIV 3 
      ELSIF NewElemCt > MaxElemCt 
      THEN 
        State . NlpsCurrentNodeElemCt := NewElemCt DIV 2 
      ELSE 
        State . NlpsCurrentNodeElemCt := NewElemCt 
      END (* IF *) 
    ; Assert 
        ( State . NlpsCurrentNodeElemCt <= MaxElemCt 
        , "InitRepackNonleaf, element count too big." 
        ) 
    ; State . NlpsCurrentNewNode 
        := NewNonleaf 
             ( Height := Height 
             , ElemCt := State . NlpsCurrentNodeElemCt 
             ) 
    ; State . NlpsI := 0 
    ; State . NlpsCumChildCt := 0 
    ; State . NlpsNewKTree1 := State . NlpsCurrentNewNode 
    ; State . NlpsNewKTree2 := NIL 
    ; State . NlpsNewKTree3 := NIL 
    END InitRepackNonleaf 

; PROCEDURE RepackNonleafElem 
    ( VAR (* IN OUT *) State : NonleafPackStateTyp 
    ; ChildRef : NodeObjTyp 
    ) 
  RAISES ANY

  = BEGIN 
      IF ChildRef # NIL 
      THEN 
        Assert 
          ( State . NlpsI <= State . NlpsCurrentNodeElemCt 
          , "RepackNonleafElem, NlpsI excessive." 
          ) 
      ; IF State . NlpsI = State . NlpsCurrentNodeElemCt 
        THEN 
          IF State . NlpsTotalNewElemCt > MaxElemCt * 2 
          THEN (* Three new nodes total. *) 
            IF State . NlpsNewKTree2 = NIL 
            THEN (* Ready to start 2nd new node of 3. *) 
              State . NlpsCurrentNewNode 
                := NewNonleaf 
                     ( Height := State . NlpsNewKTree1 . Height ( ) 
                     , ElemCt := State . NlpsCurrentNodeElemCt 
                     ) 
            ; State . NlpsNewKTree2 := State . NlpsCurrentNewNode 
            ELSE (* Ready to start 3rd new node. *) 
              State . NlpsCurrentNodeElemCt 
                := State . NlpsTotalNewElemCt 
                   - State . NlpsCurrentNodeElemCt * 2 
            ; Assert 
                ( State . NlpsCurrentNodeElemCt <= MaxElemCt 
                , "RepackNonleafElem, 3rd element count too big." 
                ) 
            ; State . NlpsCurrentNewNode 
                := NewNonleaf 
                     ( Height := State . NlpsNewKTree1 . Height ( ) 
                     , ElemCt := State . NlpsCurrentNodeElemCt 
                     ) 
            ; State . NlpsNewKTree3 := State . NlpsCurrentNewNode 
            END (* IF *) 
          ELSE (* Last of two new nodes. *) 
            State . NlpsCurrentNodeElemCt 
              := State . NlpsTotalNewElemCt 
                 - State . NlpsCurrentNodeElemCt 
          ; Assert 
              ( State . NlpsCurrentNodeElemCt <= MaxElemCt 
              , "RepackNonleafElem, 2nd element count too big." 
              ) 
          ; State . NlpsCurrentNewNode 
              := NewNonleaf 
                   ( Height := State . NlpsNewKTree1 . Height ( ) 
                   , ElemCt := State . NlpsCurrentNodeElemCt 
                   ) 
          ; State . NlpsNewKTree2 := State . NlpsCurrentNewNode 
          END (* IF *) 
        ; State . NlpsI := 0 
        ; State . NlpsCumChildCt := 0 
        END (* IF *) 
      ; INC ( State . NlpsCumChildCt , Length ( ChildRef ) )  
      ; State . NlpsCurrentNewNode . NonleafElems ^ [ State . NlpsI ] 
          := NonleafElemTyp 
               { NleChildRef := ChildRef 
               , NleCumChildCt := State . NlpsCumChildCt 
               } 
      ; INC ( State . NlpsI )
      END (* IF *) 
    END RepackNonleafElem 

; PROCEDURE FinalizeRepackNonleaf 
    ( VAR (* IN OUT *) State : NonleafPackStateTyp ) 
  RAISES ANY

  = BEGIN 
      Assert 
        ( State . NlpsI = State . NlpsCurrentNodeElemCt 
          AND ( State . NlpsTotalNewElemCt > MaxElemCt ) 
              <= (* IMPLIES *) ( State . NlpsNewKTree2 # NIL ) 
          AND ( State . NlpsTotalNewElemCt > MaxElemCt * 2 ) 
              <= (* IMPLIES *) ( State . NlpsNewKTree3 # NIL ) 
        , "FinalizeRepackNonleaf, count mismatch." 
        ) 
    END FinalizeRepackNonleaf 

(* EXPORTED: *) 
; PROCEDURE Cat ( Left , Right : NodeObjTyp ) : NodeObjTyp 
  RAISES ANY
  (* Concatenate sequences. *)

  = 
    PROCEDURE Recurse 
      ( Height : HeightTyp 
      ; VAR (* IN OUT *) Left , Right : NodeObjTyp 
      ) 
    RAISES ANY

    = 
      VAR LTotalNewElemCt : ElemNoTyp 
    ; VAR LDownLeft , LDownRight : NodeObjTyp 
    ; VAR LNonleafLeft , LNonleafRight : NonleafNodeObjTyp 
    ; VAR LReturnedLeft , LReturnedRight : NodeObjTyp 
    ; VAR LLeftIsLow , LRightIsLow : BOOLEAN 
    ; VAR LLeafPackState : LeafPackStateTyp 
    ; VAR LNonleafPackState : NonleafPackStateTyp 

    ; BEGIN (* Recurse *) 
        Assert ( Left # NIL , "Cat.Recurse, left = NIL. " ) 
      ; Assert ( Right # NIL , "Cat.Recurse, right = NIL. " ) 
      ; IF Height = 1
        THEN (* These are leaf nodes. *)
          TYPECASE Left <*NOWARN*>
          OF LeafNodeObjTyp ( TLeafNodeLeft )
          => TYPECASE Right <*NOWARN*>
             OF LeafNodeObjTyp ( TLeafNodeRight )
             => LTotalNewElemCt 
                  := LeafNodeObjTyp . ElemCt ( TLeafNodeLeft ) 
                     + LeafNodeObjTyp . ElemCt ( TLeafNodeRight ) 
             ; IF LeafNodeObjTyp . ElemCt ( TLeafNodeLeft ) = 1 
                  OR LeafNodeObjTyp . ElemCt ( TLeafNodeRight ) = 1 
                  OR LTotalNewElemCt <= MaxElemCt  
               THEN (* repack leaf elements into 1 or 2 new nodes. *) 
                 InitRepackLeaf ( LLeafPackState , LTotalNewElemCt ) 
               ; FOR J := 0 TO LeafNodeObjTyp . ElemCt ( TLeafNodeLeft ) - 1 
                 DO 
                   RepackLeafElem 
                     ( LLeafPackState 
                     , TLeafNodeLeft . LeafElems ^ [ J ] 
                     ) 
                 END (* FOR *) 
               ; FOR J := 0 TO LeafNodeObjTyp . ElemCt ( TLeafNodeRight ) - 1 
                 DO 
                   RepackLeafElem 
                     ( LLeafPackState 
                     , TLeafNodeRight . LeafElems ^ [ J ] 
                     ) 
                 END (* FOR *) 
               ; FinalizeRepackLeaf ( LLeafPackState ) 
               ; Left := LLeafPackState . LpsNewKTree1 
               ; Right := LLeafPackState . LpsNewKTree2 
               ; Assert 
                   ( LLeafPackState . LpsNewKTree3 = NIL 
                   , "Cat.Recurse 3 leaf nodes. " 
                   ) 
            (* ELSE can just reuse the nodes as they are. *) 
               END (* IF *) 
             ELSE
               CantHappen
                 ( "Cat, Recurse, Height = 1 but Right is not leaf." )
             END (* TYPECASE *)
           ELSE
             CantHappen
               ( "Cat, Recurse, Height = 1 but Left is not leaf." )
           END (* TYPECASE *)
        ELSE (* Nonleaf nodes *) 
          LLeftIsLow := Left . Height ( ) < Height 
        ; IF LLeftIsLow 
          THEN 
            LDownLeft := Left 
          ELSE 
            LNonleafLeft := NARROW ( Left , NonleafNodeObjTyp )
          ; LDownLeft 
              := LNonleafLeft . NonleafElems 
                 ^ [ NonleafNodeObjTyp . ElemCt ( LNonleafLeft ) - 1 ] 
                 . NleChildRef 
          END (* IF *) 
        ; LReturnedLeft := LDownLeft 
        ; LRightIsLow := Right . Height ( ) < Height 
        ; IF LRightIsLow 
          THEN 
            LDownRight := Right 
          ELSE 
            LNonleafRight := NARROW ( Right , NonleafNodeObjTyp )
          ; LDownRight 
              := LNonleafRight . NonleafElems ^ [ 0 ] . NleChildRef 
          END (* IF *) 
        ; LReturnedRight := LDownRight 
        ; Recurse ( Height - 1 , LReturnedLeft , LReturnedRight ) 
        ; LTotalNewElemCt 
            := ORD ( LReturnedLeft # NIL ) 
               + ORD ( LReturnedRight # NIL ) 
        ; IF NOT LLeftIsLow 
          THEN (* Count rest of elements of left node. *) 
            INC ( LTotalNewElemCt 
                , NonleafNodeObjTyp . ElemCt ( LNonleafLeft ) - 1 
                )
          END (* IF *) 
        ; IF NOT LRightIsLow 
          THEN (* Count rest of elements of right node. *) 
            INC ( LTotalNewElemCt 
                , NonleafNodeObjTyp . ElemCt ( LNonleafRight ) - 1 
                )
          END (* IF *) 
        ; IF LLeftIsLow 
             OR LRightIsLow 
             (* One side's height lower than Recurse . Height means 
                something for that side must be packed in, even if 
                it was not changed by the levels below. *) 
             OR LReturnedLeft # LDownLeft 
             OR LReturnedRight # LDownRight 
             (* Level below changed something, so must repack. *) 
             OR LTotalNewElemCt <= MaxElemCt 
          THEN (* repack into one or two new nodes. *) 
            InitRepackNonleaf 
              ( LNonleafPackState , LTotalNewElemCt , Height ) 
          ; IF NOT LLeftIsLow 
            THEN (* repack rest of elements of left node. *) 
              FOR J := 0 TO NonleafNodeObjTyp . ElemCt ( LNonleafLeft ) - 2 
              DO 
                RepackNonleafElem 
                  ( LNonleafPackState 
                  , LNonleafLeft . NonleafElems ^ [ J ] . NleChildRef 
                  ) 
              END (* FOR *) 
            END (* IF *) 
          ; IF LReturnedLeft # NIL 
            THEN 
              RepackNonleafElem ( LNonleafPackState , LReturnedLeft ) 
            END (* IF *) 
          ; IF LReturnedRight # NIL 
            THEN 
              RepackNonleafElem ( LNonleafPackState , LReturnedRight ) 
            END (* IF *) 
          ; IF NOT LRightIsLow 
            THEN (* repack rest of elements of right node. *) 
              FOR J := 1 TO NonleafNodeObjTyp . ElemCt ( LNonleafRight ) - 1 
              DO 
                RepackNonleafElem 
                  ( LNonleafPackState 
                  , LNonleafRight . NonleafElems ^ [ J ] 
                    . NleChildRef 
                  ) 
              END (* FOR *) 
            END (* IF *) 
          ; FinalizeRepackNonleaf ( LNonleafPackState ) 
          ; Left := LNonleafPackState . NlpsNewKTree1 
          ; Right := LNonleafPackState . NlpsNewKTree2 
          ; Assert 
              ( LNonleafPackState . NlpsNewKTree3 = NIL 
              , "Cat.Recurse, nonNIL NewKTree3." 
              ) 
       (* ELSE can just reuse the nodes as they are. *) 
          END (* IF *) 
        END (* IF *)
      END Recurse 

  ; BEGIN (* Cat *)  
      VAR LLeftChildCt : SeqSsTyp 
    ; VAR LLeft : NodeObjTyp := Left 
    ; VAR LRight : NodeObjTyp := Right 
    ; VAR LHeight : HeightTyp 
    ; VAR LResult : NonleafNodeObjTyp 

    ; BEGIN (* Inner block *) 
        IF LLeft = NIL 
        THEN 
          RETURN LRight (* Which could also be NIL. *) 
        ELSIF LRight = NIL 
        THEN 
          RETURN LLeft 
        ELSE 
          LHeight := MAX ( LLeft . Height ( ) , LRight . Height ( ) ) 
        ; Recurse ( LHeight , LLeft , LRight ) 
        ; Assert ( LLeft # NIL , "Cat, NIL Result." ) 
        ; Assert 
            ( LLeft . Height ( ) = LHeight  
            , "Cat, bad Left height." 
            ) 
        ; IF LRight = NIL 
          THEN 
            RETURN ( LLeft ) 
          ELSE (* Left and Right non-NIL. *)
            Assert 
              ( LRight . Height ( ) = LHeight 
              , "Cat, bad Right height." 
              ) 
          ; LLeftChildCt := Length ( LLeft ) 
          ; LResult 
              := NewNonleaf 
                   ( Height := LHeight + 1 , ElemCt := 2 ) 
          ; LResult . NonleafElems ^ [ 0 ] 
              := NonleafElemTyp 
                   { NleChildRef := LLeft 
                   , NleCumChildCt := LLeftChildCt 
                   } 
          ; LResult . NonleafElems ^ [ 1 ] 
              := NonleafElemTyp 
                   { NleChildRef := LRight 
                   , NleCumChildCt 
                       := LLeftChildCt + Length ( LRight ) 
                   } 
          ; RETURN LResult 
          END (* IF *) 
        END (* IF *) 
      END (* Inner block *) 
    END Cat 

(* EXPORTED: *) 
; PROCEDURE Slice 
    ( KTree : NodeObjTyp ; FromSs : SeqSsTyp ; ThruSs : INTEGER ) 
  : NodeObjTyp 
  RAISES ANY
  (* Return a slice of a sequence. *)

  = 
    PROCEDURE RecurseWhole1 
      ( RelFromSs , RelThruSs : SeqSsTyp 
      ; VAR (* IN OUT *) KTree : NodeObjTyp 
      ) 
    RAISES ANY
    (* Called when both cuts are in the same node. KTree is
       this node, and may be replaced. *)

    = 
      VAR LOldElemCt : ElemNoTyp 
    ; VAR LNewElemCt : ElemNoTyp 
    ; VAR LFromElemNo , LThruElemNo : ElemNoTyp 
    ; VAR LChildFromSs , LChildThruSs : SeqSsTyp 
    ; VAR LChildKTree1 , LChildKTree2 , LChildKTree3 , LChildKTree4 
        : NodeObjTyp 
    ; VAR LLeafPackState : LeafPackStateTyp 
    ; VAR LNonleafPackState : NonleafPackStateTyp 

    ; BEGIN 
        Assert ( RelFromSs <= RelThruSs ) 
      ; Assert ( KTree # NIL ) 
      ; TYPECASE KTree <*NOWARN*>
        OF LeafNodeObjTyp ( TLeafNode )
        => IF RelFromSs = 0 
              AND RelThruSs + 1 = LeafNodeObjTyp . ElemCt ( TLeafNode )
           THEN (* Return the node unchanged. *) 
             RETURN 
           ELSE (* Must repack. *) 
             LNewElemCt := RelThruSs - RelFromSs + 1 
           ; InitRepackLeaf ( LLeafPackState , LNewElemCt ) 
           ; FOR I := RelFromSs TO RelThruSs 
             DO 
               RepackLeafElem 
                 ( LLeafPackState , TLeafNode . LeafElems ^ [ I ] ) 
             END (* FOR *) 
           ; FinalizeRepackLeaf ( LLeafPackState ) 
           ; KTree := LLeafPackState . LpsNewKTree1 
           ; Assert ( LLeafPackState . LpsNewKTree2 = NIL ) 
           ; Assert ( LLeafPackState . LpsNewKTree3 = NIL ) 
           END (* IF *) 
        | NonleafNodeObjTyp ( TNonleafNode )
        => IF RelFromSs = 0 
              AND RelThruSs + 1 
                  = TNonleafNode . NonleafElems       
                    ^ [ NonleafNodeObjTyp . ElemCt ( TNonleafNode ) - 1 ] 
                    . NleCumChildCt 
           THEN (* Return the node unchanged. *) 
             RETURN 
           ELSE (* Will have to repack regardless of what 
                   happens below. *) 
             BinSearch 
               ( TNonleafNode 
               , RelFromSs 
               , LFromElemNo 
               , LChildFromSs 
               ) 
           ; BinSearch 
               ( TNonleafNode 
               , RelThruSs 
               , LThruElemNo 
               , LChildThruSs 
               , LeftmostElemNo := LFromElemNo 
               ) 
           ; LOldElemCt := LThruElemNo - LFromElemNo + 1 
           ; CASE LOldElemCt 
             OF 
             | 1 
               => LChildKTree1 
                    := TNonleafNode . NonleafElems 
                       ^ [ LFromElemNo ] 
                       . NleChildRef 
               ; RecurseWhole1 
                   ( LChildFromSs , LChildThruSs , LChildKTree1 ) 
               ; KTree := LChildKTree1 
             | 2 
               => LChildKTree1 
                    := TNonleafNode . NonleafElems 
                       ^ [ LFromElemNo ] 
                       . NleChildRef 
               ; LChildKTree2 
                   := TNonleafNode . NonleafElems 
                      ^ [ LThruElemNo ] 
                      . NleChildRef 
               ; RecurseWhole2 
                   ( LChildFromSs 
                   , LChildThruSs 
                   , LChildKTree1 
                   , LChildKTree2 
                   ) 
               ; IF LChildKTree2 = NIL 
                 THEN (* Only one child returned from below. 
                         Just pass it up. *)
                   KTree := LChildKTree1 
                 ELSE (* Two childred returned from below.
                         Pack them into a new node. *)
                   InitRepackNonleaf 
                     ( LNonleafPackState 
                     , 2 
                     , NonleafNodeObjTyp . Height ( TNonleafNode ) 
                     ) 
                 ; RepackNonleafElem 
                     ( LNonleafPackState , LChildKTree1 ) 
                 ; RepackNonleafElem 
                     ( LNonleafPackState , LChildKTree2 ) 
                 ; FinalizeRepackNonleaf ( LNonleafPackState ) 
                 ; KTree := LNonleafPackState . NlpsNewKTree1 
                 ; Assert 
                     ( LNonleafPackState . NlpsNewKTree2 = NIL 
                     , "Slice.RecurseWhole1, 2, nonNIL NewKTree2." 
                     ) 
                 ; Assert 
                     ( LNonleafPackState . NlpsNewKTree3 = NIL 
                     , "Slice.RecurseWhole1, 2, nonNIL NewKTree3." 
                     ) 
                 END (* IF *) 
             | 3 
               => LChildKTree1 
                    := TNonleafNode . NonleafElems 
                       ^ [ LFromElemNo ] 
                       . NleChildRef 
               ; LChildKTree2 
                   := TNonleafNode . NonleafElems 
                      ^ [ LFromElemNo + 1 ] 
                      . NleChildRef 
               ; LChildKTree3 
                   := TNonleafNode . NonleafElems 
                      ^ [ LThruElemNo ] 
                      . NleChildRef 
               ; RecurseWhole3 
                   ( LChildFromSs 
                   , LChildThruSs 
                   , LChildKTree1 
                   , LChildKTree2 
                   , LChildKTree3 
                   ) 
               ; IF LChildKTree2 = NIL 
                 THEN (* Only one child returned from below. 
                         Just pass it up. *)
                   Assert ( LChildKTree3 = NIL ) 
                 ; KTree := LChildKTree1 
                 ELSE (* Two or three children returned from below.
                         Pack them into a new node. *)
                   LNewElemCt := 2 + ORD ( LChildKTree3 # NIL ) 
                 ; InitRepackNonleaf 
                     ( LNonleafPackState 
                     , LNewElemCt 
                     , NonleafNodeObjTyp . Height ( TNonleafNode ) 
                     ) 
                 ; RepackNonleafElem 
                     ( LNonleafPackState , LChildKTree1 ) 
                 ; RepackNonleafElem 
                     ( LNonleafPackState , LChildKTree2 ) 
                 ; RepackNonleafElem 
                     ( LNonleafPackState , LChildKTree3 ) 
                 ; FinalizeRepackNonleaf ( LNonleafPackState ) 
                 ; KTree := LNonleafPackState . NlpsNewKTree1 
                 ; Assert 
                     ( LNonleafPackState . NlpsNewKTree2 = NIL 
                     , "Slice.RecurseWhole1, 3, nonNIL NewKTree2." 
                     ) 
                 ; Assert 
                     ( LNonleafPackState . NlpsNewKTree3 = NIL 
                     , "Slice.RecurseWhole1, 3, nonNIL NewKTree3." 
                     ) 
                 ; Assert ( LNonleafPackState . NlpsNewKTree2 = NIL ) 
                 END (* IF *) 
             ELSE (* LOldElemCt > 3 *)
               LChildKTree1 
                 := TNonleafNode . NonleafElems 
                    ^ [ LFromElemNo ] 
                    . NleChildRef 
             ; LChildKTree2 
                 := TNonleafNode . NonleafElems 
                    ^ [ LFromElemNo + 1 ] 
                    . NleChildRef 
             ; LChildKTree3 
                 := TNonleafNode . NonleafElems 
                    ^ [ LThruElemNo - 1 ] 
                    . NleChildRef 
             ; LChildKTree4 
                 := TNonleafNode . NonleafElems 
                    ^ [ LThruElemNo ] 
                    . NleChildRef 
             ; RecurseLeft 
                 ( LChildFromSs , LChildKTree1 , LChildKTree2 ) 
             ; RecurseRight 
                 ( LChildThruSs , LChildKTree3 , LChildKTree4 ) 
             ; LNewElemCt 
                 := 2 
                    + ORD ( LChildKTree2 # NIL ) 
                    + ORD ( LChildKTree4 # NIL ) 
                    + ForRange ( LFromElemNo + 2 , LThruElemNo - 2 ) 
             ; InitRepackNonleaf 
                 ( LNonleafPackState 
                 , LNewElemCt 
                 , NonleafNodeObjTyp . Height ( TNonleafNode ) 
                 ) 
             ; RepackNonleafElem ( LNonleafPackState , LChildKTree1 ) 
             ; RepackNonleafElem ( LNonleafPackState , LChildKTree2 ) 
             ; FOR I := LFromElemNo + 2 TO LThruElemNo - 2 
               DO 
                 RepackNonleafElem 
                   ( LNonleafPackState 
                   , TNonleafNode . NonleafElems ^ [ I ] . NleChildRef 
                   ) 
               END (* FOR *) 
             ; RepackNonleafElem ( LNonleafPackState , LChildKTree3 ) 
             ; RepackNonleafElem ( LNonleafPackState , LChildKTree4 ) 
             ; FinalizeRepackNonleaf ( LNonleafPackState ) 
             ; KTree := LNonleafPackState . NlpsNewKTree1 
             ; Assert 
                 ( LNonleafPackState . NlpsNewKTree2 = NIL 
                 , "Slice.RecurseWhole1, >=4, nonNIL NewKTree2." 
                 ) 
             ; Assert 
                 ( LNonleafPackState . NlpsNewKTree3 = NIL 
                 , "Slice.RecurseWhole1, >=4, nonNIL NewKTree3." 
                 ) 
             END (* CASE *) 
           END (* IF *) 
        END (* TYPECASE *) 
      END RecurseWhole1 

  ; PROCEDURE RecurseWhole2 
      ( RelFromSs , RelThruSs : SeqSsTyp 
      ; VAR (* IN OUT *) KTree1 , KTree2 : NodeObjTyp 
      ) 
    RAISES ANY
    (* Called when the two cuts are in two adjacent nodes. 
       KTree1 and KTree2 are the nodes, which may be replaced
       by one (in KTree1) or two new nodes. *)

    = 
      VAR LOldElemCt : ElemNoTyp 
    ; VAR LNewElemCt : ElemNoTyp 
    ; VAR LFromElemNo , LThruElemNo : ElemNoTyp 
    ; VAR LMiddleElemNo1 , LMiddleElemNo2 : ElemNoTyp 
    ; VAR LChildFromSs , LChildThruSs : SeqSsTyp 
    ; VAR LChildKTree1 , LChildKTree2 , LChildKTree3 , LChildKTree4 
        : NodeObjTyp 
    ; VAR LLeafPackState : LeafPackStateTyp 
    ; VAR LNonleafPackState : NonleafPackStateTyp 

    ; BEGIN 
        Assert ( KTree1 # NIL ) 
      ; Assert ( KTree2 # NIL ) 
      ; TYPECASE KTree1 <*NOWARN*>
        OF LeafNodeObjTyp ( TLeafNode1 )
        => TYPECASE KTree2 <*NOWARN*>
           OF LeafNodeObjTyp ( TLeafNode2 )
           => IF RelFromSs = 0 
                 AND RelThruSs + 1 = LeafNodeObjTyp . ElemCt ( TLeafNode2 ) 
              THEN (* Return the two nodes unchanged. *) 
                RETURN 
              ELSE (* Must repack. *) 
                LNewElemCt 
                  := ( LeafNodeObjTyp . ElemCt ( TLeafNode1 ) - RelFromSs ) 
                     + ( RelThruSs + 1 ) 
              ; InitRepackLeaf ( LLeafPackState , LNewElemCt ) 
              ; FOR I := RelFromSs TO LeafNodeObjTyp . ElemCt ( TLeafNode1 ) - 1 
                DO 
                  RepackLeafElem 
                    ( LLeafPackState 
                    , TLeafNode1 . LeafElems ^ [ I ] 
                    ) 
                END (* FOR *) 
              ; FOR I := 0 TO RelThruSs 
                DO 
                  RepackLeafElem 
                    ( LLeafPackState 
                    , TLeafNode2 . LeafElems ^ [ I ] 
                    ) 
                END (* FOR *) 
              ; FinalizeRepackLeaf ( LLeafPackState ) 
              ; KTree1 := LLeafPackState . LpsNewKTree1 
              ; KTree2 := LLeafPackState . LpsNewKTree2 
              ; Assert ( LLeafPackState . LpsNewKTree3 = NIL ) 
              END (* IF *) 
           ELSE
             CantHappen
               ( "Slice, RecurseWhole2, "
                 & "KTree1 is leaf but not KTree2." 
               )
           END (* TYPECASE *)
        | NonleafNodeObjTyp ( TNonleafNode1 )
        => TYPECASE KTree2 <*NOWARN*>
           OF NonleafNodeObjTyp ( TNonleafNode2 )
           => IF RelFromSs = 0 
                 AND RelThruSs + 1 
                     = TNonleafNode2 . NonleafElems 
                       ^ [ NonleafNodeObjTyp . ElemCt ( TNonleafNode2 ) - 1 ] 
                       . NleCumChildCt 
              THEN (* Return the two nodes unchanged. *) 
                RETURN 
              ELSE (* Will repack regardless of what happens below. *) 
                BinSearch 
                  ( KTree1 , RelFromSs , LFromElemNo , LChildFromSs ) 
              ; BinSearch 
                  ( KTree2 , RelThruSs , LThruElemNo , LChildThruSs ) 
              ; LOldElemCt 
                  := ( NonleafNodeObjTyp . ElemCt ( TNonleafNode1 ) - LFromElemNo ) 
                     + ( LThruElemNo + 1 ) 
              ; CASE LOldElemCt 
                OF 
                | 1 
                  => CantHappen ( )
                | 2 
                  => Assert 
                       ( LFromElemNo 
                         = NonleafNodeObjTyp . ElemCt ( TNonleafNode1 ) - 1 
                       ) 
                  ; LChildKTree1 
                      := TNonleafNode1 . NonleafElems 
                         ^ [ LFromElemNo ] 
                         . NleChildRef 
                  ; Assert ( LThruElemNo = 0 ) 
                  ; LChildKTree2 
                      := TNonleafNode2 . NonleafElems ^ [ 0 ] 
                         . NleChildRef 
                  ; RecurseWhole2 
                      ( LChildFromSs 
                      , LChildThruSs 
                      , LChildKTree1 
                      , LChildKTree2 
                      ) 
                  ; IF LChildKTree2 = NIL 
                    THEN (* Only one child returned from below. 
                            Just pass it up. *)
                      KTree1 := LChildKTree1 
                    ; KTree2 := NIL 
                    ELSE (* Two children returned from below.
                            Pack them into a new node. *)
                      InitRepackNonleaf 
                        ( LNonleafPackState 
                        , 2 
                        , NonleafNodeObjTyp . Height ( TNonleafNode1 ) 
                        ) 
                    ; RepackNonleafElem 
                        ( LNonleafPackState , LChildKTree1 ) 
                    ; RepackNonleafElem 
                        ( LNonleafPackState , LChildKTree2 ) 
                    ; FinalizeRepackNonleaf ( LNonleafPackState ) 
                    ; KTree1 := LNonleafPackState . NlpsNewKTree1 
                    ; Assert 
                        ( LNonleafPackState . NlpsNewKTree2 = NIL 
                        , "Slice.RecurseWhole2, 2, nonNIL NewKTree2." 
                        ) 
                    ; Assert 
                        ( LNonleafPackState . NlpsNewKTree3 = NIL 
                        , "Slice.RecurseWhole2, 2, nonNIL NewKTree3." 
                        ) 
                    ; KTree2 := NIL 
                    END (* IF *) 
                | 3 
                  => LChildKTree1 
                       := TNonleafNode1 . NonleafElems 
                          ^ [ LFromElemNo ] 
                          . NleChildRef 
                  ; IF LFromElemNo 
                       < NonleafNodeObjTyp . ElemCt ( TNonleafNode1 ) - 1 
                    THEN 
                      LChildKTree2 
                        := TNonleafNode1 . NonleafElems 
                           ^ [ LFromElemNo + 1 ] 
                           . NleChildRef 
                    ELSE 
                      Assert ( LThruElemNo = 1 ) 
                    ; LChildKTree2 
                        := TNonleafNode2 . NonleafElems ^ [ 0 ] 
                           . NleChildRef 
                    END (* IF *) 
                  ; LChildKTree3 
                      := TNonleafNode2 . NonleafElems 
                         ^ [ LThruElemNo ] 
                         . NleChildRef 
                  ; RecurseWhole3 
                      ( LChildFromSs 
                      , LChildThruSs 
                      , LChildKTree1 
                      , LChildKTree2 
                      , LChildKTree3 
                      ) 
                  ; IF LChildKTree2 = NIL 
                    THEN (* Only one child returned from below. 
                            Just pass it up. *)
                      Assert ( LChildKTree3 = NIL ) 
                    ; KTree1 := LChildKTree1 
                    ; KTree2 := NIL 
                    ELSE (* Two or three children returned from below.
                            Pack them into a new node. *)
                      LNewElemCt := 2 + ORD ( LChildKTree3 # NIL ) 
                    ; InitRepackNonleaf 
                        ( LNonleafPackState 
                        , LNewElemCt 
                        , NonleafNodeObjTyp . Height ( TNonleafNode1 ) 
                        ) 
                    ; RepackNonleafElem 
                        ( LNonleafPackState , LChildKTree1 ) 
                    ; RepackNonleafElem 
                        ( LNonleafPackState , LChildKTree2 ) 
                    ; RepackNonleafElem 
                        ( LNonleafPackState , LChildKTree3 ) 
                    ; FinalizeRepackNonleaf ( LNonleafPackState ) 
                    ; KTree1 := LNonleafPackState . NlpsNewKTree1 
                    ; Assert 
                        ( LNonleafPackState . NlpsNewKTree2 = NIL 
                        , "Slice.RecurseWhole2, 3, nonNIL NewKTree2." 
                        ) 
                    ; Assert 
                        ( LNonleafPackState . NlpsNewKTree3 = NIL 
                        , "Slice.RecurseWhole2, 3, nonNIL NewKTree3." 
                        ) 
                    ; KTree2 := NIL 
                    END (* IF *) 
                ELSE (* LOldElemCt > 3 *)
                  LChildKTree1 
                    := TNonleafNode1 . NonleafElems 
                       ^ [ LFromElemNo ] 
                       . NleChildRef 
                ; IF LFromElemNo + 1 
                     < NonleafNodeObjTyp . ElemCt ( TNonleafNode1 ) 
                  THEN 
                    LChildKTree2 
                      := TNonleafNode1 . NonleafElems 
                         ^ [ LFromElemNo + 1 ] 
                         . NleChildRef 
                  ; LMiddleElemNo2 := 0 
                  ELSE 
                    LChildKTree2 
                      := TNonleafNode2 . NonleafElems ^ [ 0 ] 
                         . NleChildRef 
                  ; LMiddleElemNo2 := 1 
                  END (* IF *) 
                ; IF LThruElemNo = 0 
                  THEN 
                    LChildKTree3 
                      := TNonleafNode1 . NonleafElems 
                         ^ [ NonleafNodeObjTyp . ElemCt ( TNonleafNode1 ) - 1 ] 
                         . NleChildRef 
                  ; LMiddleElemNo1 
                      := NonleafNodeObjTyp . ElemCt ( TNonleafNode1 ) - 2 
                  ELSE 
                    LChildKTree3 
                      := TNonleafNode2 . NonleafElems 
                         ^ [ LThruElemNo - 1 ] 
                         . NleChildRef 
                  ; LMiddleElemNo1 
                      := NonleafNodeObjTyp . ElemCt ( TNonleafNode1 ) - 1 
                  END (* IF *) 
                ; LChildKTree4 
                    := TNonleafNode2 . NonleafElems 
                       ^ [ LThruElemNo ] 
                       . NleChildRef 
                ; RecurseLeft 
                    ( LChildFromSs , LChildKTree1 , LChildKTree2 ) 
                ; RecurseRight 
                    ( LChildThruSs , LChildKTree3 , LChildKTree4 ) 
                ; LNewElemCt 
                    := 2 
                       + ORD ( LChildKTree2 # NIL ) 
                       + ORD ( LChildKTree4 # NIL ) 
                       + ForRange ( LFromElemNo + 2 , LMiddleElemNo1 ) 
                       + ForRange ( LMiddleElemNo2 , LThruElemNo - 2 ) 
                ; InitRepackNonleaf 
                    ( LNonleafPackState 
                    , LNewElemCt 
                    , NonleafNodeObjTyp . Height ( TNonleafNode1 ) 
                    ) 
                ; RepackNonleafElem 
                    ( LNonleafPackState , LChildKTree1 ) 
                ; RepackNonleafElem 
                    ( LNonleafPackState , LChildKTree2 ) 
                ; FOR I := LFromElemNo + 2 TO LMiddleElemNo1 
                  DO 
                    RepackNonleafElem 
                      ( LNonleafPackState 
                      , TNonleafNode1 . NonleafElems ^ [ I ] 
                        . NleChildRef 
                      ) 
                  END (* FOR *) 
                ; FOR I := LMiddleElemNo2 TO LThruElemNo - 2 
                  DO 
                    RepackNonleafElem 
                      ( LNonleafPackState 
                      , TNonleafNode2 . NonleafElems ^ [ I ] 
                        . NleChildRef 
                      ) 
                  END (* FOR *) 
                ; RepackNonleafElem 
                    ( LNonleafPackState , LChildKTree3 ) 
                ; RepackNonleafElem 
                    ( LNonleafPackState , LChildKTree4 ) 
                ; FinalizeRepackNonleaf ( LNonleafPackState ) 
                ; KTree1 := LNonleafPackState . NlpsNewKTree1 
                ; KTree2 := LNonleafPackState . NlpsNewKTree2 
                ; Assert 
                    ( LNonleafPackState . NlpsNewKTree3 = NIL 
                    , "Slice.RecurseWhole2, >=4, nonNIL NewKTree3." 
                    ) 
                END (* CASE *) 
              END (* IF *) 
           ELSE
             CantHappen
               ( "Slice, RecurseWhole2, "
                 & "KTree1 is nonleaf but not KTree2." 
               )
           END (* TYPECASE *)
        END (* TYPECASE *) 
      END RecurseWhole2 

  ; PROCEDURE RecurseWhole3 
      ( RelFromSs , RelThruSs : SeqSsTyp 
      ; VAR (* IN OUT *) KTree1 , KTree2 , KTree3 : NodeObjTyp 
      ) 
    RAISES ANY
    (* Called when the two cuts are in the outer two of a
       range of three adjacent nodes. KTree1, KTree2, and KTree3 
       are the three nodes, which could be replaced by one
       (KTree1), two (KTree1 and KTree2) or three new nodes. *)

    = 
      VAR LNewElemCt : ElemNoTyp 
    ; VAR LFromElemNo , LThruElemNo : ElemNoTyp 
    ; VAR LMiddleElemNo1 , LMiddleElemNo2 : ElemNoTyp 
    ; VAR LChildFromSs , LChildThruSs : SeqSsTyp 
    ; VAR LChildKTree1 , LChildKTree2 , LChildKTree3 , LChildKTree4 
        : NodeObjTyp 
    ; VAR LLeafPackState : LeafPackStateTyp 
    ; VAR LNonleafPackState : NonleafPackStateTyp 

    ; BEGIN 
        Assert ( KTree1 # NIL ) 
      ; Assert ( KTree2 # NIL ) 
      ; Assert ( KTree3 # NIL ) 
      ; TYPECASE KTree1 <*NOWARN*>
        OF LeafNodeObjTyp ( TLeafNode1 )
        => TYPECASE KTree2 <*NOWARN*>
           OF LeafNodeObjTyp ( TLeafNode2 )
           => TYPECASE KTree3 <*NOWARN*>
              OF LeafNodeObjTyp ( TLeafNode3 )
              => IF RelFromSs = 0 
                    AND RelThruSs + 1 = LeafNodeObjTyp . ElemCt ( TLeafNode3 ) 
                 THEN (* Return the three nodes unchanged. *) 
                   RETURN 
                 ELSE (* Must repack. *) 
                   LNewElemCt 
                     := ( LeafNodeObjTyp . ElemCt ( TLeafNode1 ) - RelFromSs ) 
                        + LeafNodeObjTyp . ElemCt ( TLeafNode2 ) 
                        + RelThruSs 
                        + 1 
                 ; InitRepackLeaf ( LLeafPackState , LNewElemCt ) 
                 ; FOR I := RelFromSs 
                       TO LeafNodeObjTyp . ElemCt ( TLeafNode1 ) - 1 
                   DO 
                     RepackLeafElem 
                       ( LLeafPackState 
                       , TLeafNode1 . LeafElems ^ [ I ] 
                       ) 
                   END (* FOR *) 
                 ; FOR I := 0 TO LeafNodeObjTyp . ElemCt ( TLeafNode2 ) - 1 
                   DO 
                     RepackLeafElem 
                       ( LLeafPackState 
                       , TLeafNode2 . LeafElems ^ [ I ] 
                       ) 
                   END (* FOR *) 
                 ; FOR I := 0 TO RelThruSs 
                   DO 
                     RepackLeafElem 
                       ( LLeafPackState 
                       , TLeafNode3 . LeafElems ^ [ I ] 
                       ) 
                   END (* FOR *) 
                 ; FinalizeRepackLeaf ( LLeafPackState ) 
                 ; KTree1 := LLeafPackState . LpsNewKTree1 
                 ; KTree2 := LLeafPackState . LpsNewKTree2 
                 ; KTree3 := LLeafPackState . LpsNewKTree3 
                 END (* IF *) 
              ELSE
                CantHappen
                  ( "Slice, RecurseWhole3, "
                    & "KTree1 is leaf but not KTree3." 
                  )
              END (* TYPECASE *)
           ELSE
             CantHappen
               ( "Slice, RecurseWhole3, "
                 & "KTree1 is leaf but not KTree2." 
               )
           END (* TYPECASE *)
        | NonleafNodeObjTyp ( TNonleafNode1 )
        => TYPECASE KTree2 <*NOWARN*>
           OF NonleafNodeObjTyp ( TNonleafNode2 )
           => TYPECASE KTree3 <*NOWARN*>
              OF NonleafNodeObjTyp ( TNonleafNode3 )
              => IF RelFromSs = 0 
                    AND RelThruSs + 1 
                        = TNonleafNode3 . NonleafElems 
                          ^ [ NonleafNodeObjTyp . ElemCt ( TNonleafNode3 ) 
                              - 1 
                            ] 
                          . NleCumChildCt 
                 THEN (* Return the three nodes unchanged. *) 
                   RETURN 
                 ELSE (* Will repack regardless of what happens 
                         below. *) 
                   BinSearch 
                     ( KTree1 
                     , RelFromSs 
                     , LFromElemNo 
                     , LChildFromSs 
                     ) 
                 ; BinSearch 
                     ( KTree3 
                     , RelThruSs 
                     , LThruElemNo 
                     , LChildThruSs 
                     ) 
                 ; LChildKTree1 
                     := TNonleafNode1 . NonleafElems 
                        ^ [ LFromElemNo ] . NleChildRef 
                 ; IF LFromElemNo + 1 
                      < NonleafNodeObjTyp . ElemCt ( TNonleafNode1 ) 
                   THEN 
                     LChildKTree2 
                       := TNonleafNode1 . NonleafElems 
                          ^ [ LFromElemNo + 1 ] 
                          . NleChildRef 
                   ; LMiddleElemNo1 := 0 
                   ELSE 
                     LChildKTree2 
                       := TNonleafNode2 . NonleafElems ^ [ 0 ] 
                          . NleChildRef 
                   ; LMiddleElemNo1 := 1 
                   END (* IF *) 
                 ; IF LThruElemNo = 0 
                   THEN 
                     LChildKTree3 
                       := TNonleafNode2 . NonleafElems 
                          ^ [ NonleafNodeObjTyp . ElemCt ( TNonleafNode2 ) - 1 ] 
                          . NleChildRef 
                   ; LMiddleElemNo2 
                       := NonleafNodeObjTyp . ElemCt ( TNonleafNode2 ) - 2 
                   ELSE 
                     LChildKTree3 
                       := TNonleafNode3 . NonleafElems 
                          ^ [ LThruElemNo - 1 ] 
                          . NleChildRef 
                   ; LMiddleElemNo2 
                       := NonleafNodeObjTyp . ElemCt ( TNonleafNode2 ) - 1 
                   END (* IF *) 
                 ; LChildKTree4 
                     := TNonleafNode3 . NonleafElems 
                        ^ [ LThruElemNo ] . NleChildRef 
                 ; RecurseLeft 
                     ( LChildFromSs , LChildKTree1 , LChildKTree2 ) 
                 ; RecurseRight 
                     ( LChildThruSs , LChildKTree3 , LChildKTree4 ) 
                 ; LNewElemCt 
                     := 2 
                        + ORD ( LChildKTree2 # NIL ) 
                        + ORD ( LChildKTree4 # NIL ) 
                        + ForRange 
                            ( LFromElemNo + 2 
                            , NonleafNodeObjTyp . ElemCt ( TNonleafNode1 ) - 1 
                            ) 
                        + ForRange ( LMiddleElemNo1 , LMiddleElemNo2 ) 
                        + ForRange ( 0 , LThruElemNo - 2 ) 
                 ; InitRepackNonleaf 
                     ( LNonleafPackState 
                     , LNewElemCt 
                     , NonleafNodeObjTyp . Height ( TNonleafNode1 ) 
                     ) 
                 ; RepackNonleafElem 
                     ( LNonleafPackState , LChildKTree1 ) 
                 ; RepackNonleafElem 
                     ( LNonleafPackState , LChildKTree2 ) 
                 ; FOR I := LFromElemNo + 2 
                         TO NonleafNodeObjTyp . ElemCt ( TNonleafNode1 ) - 1 
                   DO 
                     RepackNonleafElem 
                       ( LNonleafPackState 
                       , TNonleafNode1 . NonleafElems ^ [ I ] 
                         . NleChildRef 
                       ) 
                   END (* FOR *) 
                 ; FOR I := LMiddleElemNo1 TO LMiddleElemNo2 
                   DO 
                     RepackNonleafElem 
                       ( LNonleafPackState 
                       , TNonleafNode2 . NonleafElems ^ [ I ] 
                         . NleChildRef 
                       ) 
                   END (* FOR *) 
                 ; FOR I := 0 TO LThruElemNo - 2 
                   DO 
                     RepackNonleafElem 
                       ( LNonleafPackState 
                       , TNonleafNode3 . NonleafElems ^ [ I ] 
                         . NleChildRef 
                       ) 
                   END (* FOR *) 
                 ; RepackNonleafElem 
                     ( LNonleafPackState , LChildKTree3 ) 
                 ; RepackNonleafElem 
                     ( LNonleafPackState , LChildKTree4 ) 
                 ; FinalizeRepackNonleaf ( LNonleafPackState ) 
                 ; KTree1 := LNonleafPackState . NlpsNewKTree1 
                 ; KTree2 := LNonleafPackState . NlpsNewKTree2 
                 ; KTree3 := LNonleafPackState . NlpsNewKTree3 
                 END (* IF *) 
              ELSE
                CantHappen
                  ( "Slice, RecurseWhole3, "
                    & "KTree1 is nonleaf but not KTree3." 
                  )
              END (* TYPECASE *)
           ELSE
             CantHappen
               ( "Slice, RecurseWhole3, "
                 & "KTree1 is nonleaf but not KTree2." 
               )
           END (* TYPECASE *)
        END (* TYPECASE *) 
      END RecurseWhole3 

  ; PROCEDURE RecurseLeft 
      ( RelFromSs : SeqSsTyp 
      ; VAR (* IN OUT *) KTree1 , KTree2 : NodeObjTyp 
      ) 
    RAISES ANY
    (* Called for the left cut, when the nodes containing the 
       two cuts are separated by at least two nodes.
       KTree1 contains the left cut, and KTree2 is its right 
       neighbor. These can be replaced by one (KTree1) or
       two new nodes. *)

    = 
      VAR LNewElemCt : ElemNoTyp 
    ; VAR LFromElemNo : ElemNoTyp 
    ; VAR LMiddleElemNo : ElemNoTyp 
    ; VAR LChildFromSs : SeqSsTyp 
    ; VAR LChildKTree1 , LChildKTree2 : NodeObjTyp 
    ; VAR LLeafPackState : LeafPackStateTyp 
    ; VAR LNonleafPackState : NonleafPackStateTyp 

    ; BEGIN 
        Assert ( KTree1 # NIL ) 
      ; Assert ( KTree2 # NIL ) 
      ; TYPECASE KTree1 <*NOWARN*>
        OF LeafNodeObjTyp ( TLeafNode1 )
        => TYPECASE KTree2 <*NOWARN*>
           OF LeafNodeObjTyp ( TLeafNode2 )
           => IF RelFromSs = 0 
              THEN (* Return the two nodes unchanged. *) 
                RETURN 
              ELSE (* Must repack. *) 
                LNewElemCt 
                  := ( LeafNodeObjTyp . ElemCt ( TLeafNode1 ) - RelFromSs ) 
                     + LeafNodeObjTyp . ElemCt ( TLeafNode2 ) 
              ; InitRepackLeaf ( LLeafPackState , LNewElemCt ) 
              ; FOR I := RelFromSs 
                    TO LeafNodeObjTyp . ElemCt ( TLeafNode1 ) - 1 
                DO 
                  RepackLeafElem 
                    ( LLeafPackState 
                    , TLeafNode1 . LeafElems ^ [ I ] 
                    ) 
                END (* FOR *) 
              ; FOR I := 0 TO LeafNodeObjTyp . ElemCt ( TLeafNode2 ) - 1 
                DO 
                  RepackLeafElem 
                    ( LLeafPackState 
                    , TLeafNode2 . LeafElems ^ [ I ] 
                    ) 
                END (* FOR *) 
              ; FinalizeRepackLeaf ( LLeafPackState ) 
              ; KTree1 := LLeafPackState . LpsNewKTree1 
              ; KTree2 := LLeafPackState . LpsNewKTree2 
              ; Assert ( LLeafPackState . LpsNewKTree3 = NIL ) 
              END (* IF *) 
           ELSE
             CantHappen
               ( "Slice, RecurseLeft, KTree1 is leaf but not KTree2." )
           END (* TYPECASE *)
        | NonleafNodeObjTyp ( TNonleafNode1 )
        => TYPECASE KTree2 <*NOWARN*>
           OF NonleafNodeObjTyp ( TNonleafNode2 )
           => IF RelFromSs = 0 
              THEN (* Return the two nodes unchanged. *) 
                RETURN 
              ELSE (* Will repack regardless of what happens below. *) 
                BinSearch 
                  ( KTree1 , RelFromSs , LFromElemNo , LChildFromSs ) 
              ; LChildKTree1 
                  := TNonleafNode1 . NonleafElems ^ [ LFromElemNo ] 
                     . NleChildRef 
              ; IF LFromElemNo + 1 
                   < NonleafNodeObjTyp . ElemCt ( TNonleafNode1 ) 
                THEN 
                  LChildKTree2 
                    := TNonleafNode1 . NonleafElems 
                       ^ [ LFromElemNo + 1 ] 
                       . NleChildRef 
                ; LMiddleElemNo := 0 
                ELSE 
                  LChildKTree2 
                    := TNonleafNode2 . NonleafElems ^ [ 0 ] 
                       . NleChildRef 
                ; LMiddleElemNo := 1 
                END (* IF *) 
              ; RecurseLeft 
                  ( LChildFromSs , LChildKTree1 , LChildKTree2 ) 
              ; LNewElemCt 
                  := 1 
                     + ORD ( LChildKTree2 # NIL ) 
                     + ForRange 
                         ( LFromElemNo + 2 
                         , NonleafNodeObjTyp . ElemCt ( TNonleafNode1 ) - 1 
                         )
                     + ForRange 
                         ( LMiddleElemNo 
                         , NonleafNodeObjTyp . ElemCt ( TNonleafNode2 ) - 1 
                         ) 
              ; InitRepackNonleaf 
                  ( LNonleafPackState 
                  , LNewElemCt 
                  , NonleafNodeObjTyp . Height ( TNonleafNode1 ) 
                  ) 
              ; RepackNonleafElem ( LNonleafPackState , LChildKTree1 ) 
              ; RepackNonleafElem ( LNonleafPackState , LChildKTree2 ) 
              ; FOR I := LFromElemNo + 2 
                      TO NonleafNodeObjTyp . ElemCt ( TNonleafNode1 ) - 1 
                DO 
                  RepackNonleafElem 
                    ( LNonleafPackState 
                    , TNonleafNode1 . NonleafElems ^ [ I ] 
                      . NleChildRef 
                    ) 
                END (* FOR *) 
              ; FOR I := LMiddleElemNo 
                      TO NonleafNodeObjTyp . ElemCt ( TNonleafNode2 ) - 1 
                DO 
                  RepackNonleafElem 
                    ( LNonleafPackState 
                    , TNonleafNode2 . NonleafElems ^ [ I ] 
                      . NleChildRef 
                    ) 
                END (* FOR *) 
              ; FinalizeRepackNonleaf ( LNonleafPackState ) 
              ; KTree1 := LNonleafPackState . NlpsNewKTree1 
              ; KTree2 := LNonleafPackState . NlpsNewKTree2 
              ; Assert ( LNonleafPackState . NlpsNewKTree3 = NIL ) 
              END (* IF *) 
           ELSE
             CantHappen
               ( "Slice, RecurseWhole2, "
                 & "KTree1 is nonleaf but not KTree2." 
               )
           END (* TYPECASE *)
        END (* TYPECASE *) 
      END RecurseLeft 

  ; PROCEDURE RecurseRight 
      ( RelThruSs : SeqSsTyp 
      ; VAR (* IN OUT *) KTree1 , KTree2 : NodeObjTyp 
      ) 
    RAISES ANY
    (* Called for the right cut, when the nodes containing the 
       two cuts are separated by at least two nodes.
       KTree2 contains the right cut, and KTree1 is its left 
       neighbor. These can be replaced by one (KTree1) or two
       new nodes. *)

    = 
      VAR LNewElemCt : ElemNoTyp 
    ; VAR LThruElemNo : ElemNoTyp 
    ; VAR LMiddleElemNo : ElemNoTyp 
    ; VAR LChildThruSs : SeqSsTyp 
    ; VAR LChildKTree1 , LChildKTree2 : NodeObjTyp 
    ; VAR LLeafPackState : LeafPackStateTyp 
    ; VAR LNonleafPackState : NonleafPackStateTyp 

    ; BEGIN 
        Assert ( KTree1 # NIL ) 
      ; Assert ( KTree2 # NIL ) 
      ; TYPECASE KTree1 <*NOWARN*>
        OF LeafNodeObjTyp ( TLeafNode1 )
        => TYPECASE KTree2 <*NOWARN*>
           OF LeafNodeObjTyp ( TLeafNode2 )
           => IF RelThruSs + 1 = LeafNodeObjTyp . ElemCt ( TLeafNode2 ) 
              THEN (* Return the two nodes unchanged. *) 
                RETURN 
              ELSE (* Must repack. *) 
                LNewElemCt 
                  := LeafNodeObjTyp . ElemCt ( TLeafNode1 ) 
                     + ( RelThruSs + 1 ) 
              ; InitRepackLeaf ( LLeafPackState , LNewElemCt ) 
              ; FOR I := 0 TO LeafNodeObjTyp . ElemCt ( TLeafNode1 ) - 1 
                DO 
                  RepackLeafElem 
                    ( LLeafPackState 
                    , TLeafNode1 . LeafElems ^ [ I ] 
                    ) 
                END (* FOR *) 
              ; FOR I := 0 TO RelThruSs 
                DO 
                  RepackLeafElem 
                    ( LLeafPackState 
                    , TLeafNode2 . LeafElems ^ [ I ] 
                    ) 
                END (* FOR *) 
              ; FinalizeRepackLeaf ( LLeafPackState ) 
              ; KTree1 := LLeafPackState . LpsNewKTree1 
              ; KTree2 := LLeafPackState . LpsNewKTree2 
              ; Assert ( LLeafPackState . LpsNewKTree3 = NIL ) 
              END (* IF *) 
           ELSE
             CantHappen
               ( "Slice, RecurseRight, "
                 & "KTree1 is leaf but not KTree2." 
               )
           END (* TYPECASE *)
        | NonleafNodeObjTyp ( TNonleafNode1 )
        => TYPECASE KTree2 <*NOWARN*>
           OF NonleafNodeObjTyp ( TNonleafNode2 )
           => IF RelThruSs + 1 
                 = TNonleafNode2 . NonleafElems 
                   ^ [ NonleafNodeObjTyp . ElemCt ( TNonleafNode2 ) - 1 ] 
                   . NleCumChildCt 
              THEN (* Return the two nodes unchanged. *) 
                RETURN 
              ELSE (* Will repack regardless of what happens below. *) 
                BinSearch 
                  ( KTree2 , RelThruSs , LThruElemNo , LChildThruSs ) 
              ; IF LThruElemNo = 0 
                THEN 
                  LChildKTree1 
                    := TNonleafNode1 . NonleafElems 
                       ^ [ NonleafNodeObjTyp . ElemCt ( TNonleafNode1 ) - 1 ] 
                       . NleChildRef 
                ; LMiddleElemNo := NonleafNodeObjTyp . ElemCt ( TNonleafNode1 ) - 2 
                ELSE 
                  LChildKTree1 
                    := TNonleafNode2 . NonleafElems 
                       ^ [ LThruElemNo - 1 ] 
                       . NleChildRef 
                ; LMiddleElemNo := NonleafNodeObjTyp . ElemCt ( TNonleafNode1 ) - 1 
                END (* IF *) 
              ; LChildKTree2 
                  := TNonleafNode2 . NonleafElems ^ [ LThruElemNo ] 
                     . NleChildRef 
              ; RecurseRight 
                  ( LChildThruSs , LChildKTree1 , LChildKTree2 ) 
              ; LNewElemCt 
                  := 1 
                     + ORD ( LChildKTree2 # NIL ) 
                     + ForRange ( 0 , LMiddleElemNo ) 
                     + ForRange ( 0 , LThruElemNo - 2 ) 
              ; InitRepackNonleaf 
                  ( LNonleafPackState 
                  , LNewElemCt , NonleafNodeObjTyp . Height ( TNonleafNode1 ) 
                  ) 
              ; FOR I := 0 TO LMiddleElemNo 
                DO 
                  RepackNonleafElem 
                    ( LNonleafPackState 
                    , TNonleafNode1 . NonleafElems ^ [ I ] 
                      . NleChildRef 
                    ) 
                END (* FOR *) 
              ; FOR I := 0 TO LThruElemNo - 2 
                DO 
                  RepackNonleafElem 
                    ( LNonleafPackState 
                    , TNonleafNode2 . NonleafElems ^ [ I ] 
                      . NleChildRef 
                    ) 
                END (* FOR *) 
              ; RepackNonleafElem ( LNonleafPackState , LChildKTree1 ) 
              ; RepackNonleafElem ( LNonleafPackState , LChildKTree2 ) 
              ; FinalizeRepackNonleaf ( LNonleafPackState ) 
              ; KTree1 := LNonleafPackState . NlpsNewKTree1 
              ; KTree2 := LNonleafPackState . NlpsNewKTree2 
              ; Assert ( LNonleafPackState . NlpsNewKTree3 = NIL ) 
              END (* IF *) 
           ELSE
             CantHappen
               ( "Slice, RecurseRight, "
                 & "KTree1 is nonleaf but not KTree2." 
               )
           END (* TYPECASE *)
        END (* TYPECASE *) 
      END RecurseRight 

  ; BEGIN (* Slice *) 
      VAR LKTree : NodeObjTyp 
    ; VAR LLength : SeqSsTyp 

    ; BEGIN (* Inner block  *) 
        IF FromSs > ThruSs 
        THEN 
          RETURN Empty ( ) 
        ELSIF KTree = NIL 
        THEN 
          RAISE SubscriptOutOfBounds 
        ELSE 
          LLength := Length ( KTree ) 
        ; IF FromSs < 0 OR ThruSs >= LLength 
          THEN 
            RAISE SubscriptOutOfBounds 
          ELSIF FromSs = 0 AND ThruSs = LLength - 1 
          THEN 
            RETURN KTree 
          ELSE 
            LKTree := KTree 
          ; RecurseWhole1 ( FromSs , ThruSs , LKTree ) 
          ; RETURN LKTree 
          END (* IF *) 
        END (* IF *) 
      END (* Inner block *) 
    END Slice 

(* EXPORTED: *) 
; PROCEDURE Traverse 
    ( KTree : NodeObjTyp ; VisitElemProc : VisitElemProcTyp ) 
  RAISES ANY
  (* Traverse sequence in order, calling Visit for each element. *)

  = 
    PROCEDURE Recurse ( KTree : NodeObjTyp ) 
    RAISES ANY

    = BEGIN 
        IF KTree # NIL 
        THEN 
          TYPECASE KTree <*NOWARN*>
          OF LeafNodeObjTyp ( TLeafNode )
          => FOR I := 0 TO LeafNodeObjTyp . ElemCt ( TLeafNode ) - 1 
             DO 
               VisitElemProc ( TLeafNode . LeafElems ^ [ I ] ) 
             END (* FOR *) 
          | NonleafNodeObjTyp ( TNonleafNode )
          => FOR I := 0 TO NonleafNodeObjTyp . ElemCt ( TNonleafNode ) - 1 
             DO 
               Recurse 
                ( TNonleafNode . NonleafElems ^ [ I ] . NleChildRef ) 
             END (* FOR *) 
          END (* TYPECASE *)
        END (* IF *) 
      END Recurse 

  ; BEGIN (* Traverse *) 
      Recurse ( KTree ) 
    END Traverse 

; EXCEPTION QuitIsConsistent 

(* EXPORTED: *) 
; PROCEDURE IsConsistent ( KTree : NodeObjTyp ) : BOOLEAN 
  RAISES ANY
  (* Verify internal consistency.  Used for testing KTrees. *)

  = 
    VAR SeqSs : SeqSsTyp := 0 
  ; VAR Result : BOOLEAN := TRUE 
  ; VAR HeaderHasBeenWritten : BOOLEAN := FALSE 

  ; PROCEDURE Recurse 
      ( RecurseKTree : NodeObjTyp 
      ; Height : HeightTyp 
      ; VAR Length : SeqSsTyp 
      ) 

    RAISES ANY

    = 
      PROCEDURE Error ( Msg : Text . T ) 
      RAISES ANY

      = BEGIN 
          IF NOT HeaderHasBeenWritten 
          THEN 
            Report . EnsureBol ( )  
          ; Report .  PutTextLine ( "***** Inconsistency *****" ) 
          ; Report . PutTextLine 
              ( "K-tree root: " & RefImage ( KTree ) ) 
          ; HeaderHasBeenWritten := TRUE 
          END (* IF *) 
        ; Report . PutTextLine 
            ( "Current node: " & RefImage ( RecurseKTree ) ) 
        ; Report . PutTextLine 
            ( "Height: " 
              & HeightImage ( Height ) 
            ) 
        ; Report . PutTextLine ( Msg ) 
        ; Result := FALSE 
        END Error 

    ; BEGIN (* Recurse *) 
        VAR LLength : SeqSsTyp := 0 
      ; VAR LDownLength : SeqSsTyp := 0 
      ; VAR LCumChildCt : SeqSsTyp := 0 
      ; VAR LIthElement : ElemT 

      ; BEGIN (* Inner block *) 
          IF RecurseKTree = NIL 
          THEN 
            Error ( "NIL K-tree." ) 
          ELSE 
            IF Height # RecurseKTree . Height ( ) 
            THEN 
              Error 
                ( "Computed/stored height mismatch" 
                  & HeightImage ( Height ) 
                  & HeightImage ( RecurseKTree . Height ( ) ) 
                ) 
            END (* IF *) 
          ; IF RecurseKTree . ElemCt ( ) < 2 
            THEN 
              Error ( "Node with < 2 elements." ) 
            END (* IF *) 
          ; TYPECASE RecurseKTree <*NOWARN*>
            OF LeafNodeObjTyp ( TLeafNode )
            => FOR J := 0 TO LeafNodeObjTyp . ElemCt ( TLeafNode ) - 1 
               DO 
                 LIthElement := IthElement ( KTree , SeqSs ) 
               ; IF TLeafNode . LeafElems ^ [ J ] # LIthElement 
                 THEN 
                   Error 
                     ( "Value mismatch " 
                       & ElemNoImage ( J ) 
                       & ElemImage ( TLeafNode . LeafElems ^ [ J ] ) 
                       & SeqSsImage ( SeqSs ) 
                       & ElemImage ( LIthElement ) 
                     ) 
                 END (* IF *) 
               ; INC ( SeqSs )
               ; INC ( LLength ) 
               END (* FOR *) 
            | NonleafNodeObjTyp ( TNonleafNode )
            => FOR J := 0 TO NonleafNodeObjTyp . ElemCt ( TNonleafNode ) - 1 
               DO (* Do some prechecks to prevent crashing *) 
                 IF TNonleafNode . NonleafElems ^ [ J ] 
                    . NleCumChildCt 
                    <= LCumChildCt 
                 THEN 
                   Error 
                     ( "CumChildCt non-monotonic for Child no "
                       & ElemNoImage ( J ) 
                       & " of " 
                       & RefImage ( TNonleafNode ) 
                       & " after " 
                       & SeqSsImage ( LCumChildCt ) 
                       & " saw " 
                       & SeqSsImage 
                           ( TNonleafNode . NonleafElems ^ [ J ] 
                             . NleCumChildCt 
                           ) 
                       & "." 
                     ) 
                 ; RAISE QuitIsConsistent 
                 ELSIF TNonleafNode . NonleafElems ^ [ J ] 
                       . NleChildRef 
                       = NIL 
                 THEN 
                   Error 
                     ( "NIL nonleaf Child for child no " 
                       & ElemNoImage ( J ) 
                       & " of " 
                       & RefImage ( TNonleafNode ) 
                       & "." 
                     ) 
                 ; RAISE QuitIsConsistent 
                 END (* IF *) 
               ; LCumChildCt 
                   := TNonleafNode . NonleafElems ^ [ J ] 
                      . NleCumChildCt 
               END (* FOR *) 
            ; FOR J := 0 TO NonleafNodeObjTyp . ElemCt ( TNonleafNode ) - 1 
              DO 
                Recurse 
                  ( TNonleafNode . NonleafElems ^ [ J ] 
                    . NleChildRef 
                  , Height - 1 
                  , LDownLength 
                  ) 
              ; LLength := LLength + LDownLength 
              ; IF LLength 
                   # TNonleafNode . NonleafElems ^ [ J ] 
                     . NleCumChildCt 
                THEN 
                  Error 
                    ( "CumChildCt mismatch for Child no " 
                      & ElemNoImage ( J ) 
                      & SeqSsImage ( LLength ) 
                      & SeqSsImage 
                          ( TNonleafNode . NonleafElems ^ [ J ] 
                            . NleCumChildCt 
                          ) 
                      & "." 
                    ) 
                END (* IF *) 
              END (* FOR *) 
            END (* TYPECASE *)
          END (* IF *) 
        ; Length := LLength 
        END (* Inner block *) 
      END Recurse 

  ; BEGIN (* IsConsistent *) 

      VAR LLength : SeqSsTyp := 0 

    ; BEGIN (* Inner block *) 
        TRY 
          IF KTree = NIL 
          THEN (* Empty sequence. *) 
            RETURN TRUE 
          ELSIF KTree . Height ( ) = 1 AND KTree . ElemCt ( ) = 1 
          THEN (* properly formed singleton sequence. *) 
            RETURN TRUE 
          ELSE 
            Recurse ( KTree , KTree . Height ( ) , LLength ) 
          ; Assert ( LLength = SeqSs ) 
          ; RETURN Result 
          END (* IF *) 
        EXCEPT
          QuitIsConsistent => RETURN Result 
        END (* TRY EXCEPT *) 
      END (* Inner block *) 
    END IsConsistent 

(* EXPORTED: *) 
; PROCEDURE Dump ( KTree : NodeObjTyp ) 
  RAISES ANY
  (* Dump internal representation.  Used for testing KTrees. *)

  = 
    VAR DumpSeqSs : SeqSsTyp := 0 

  ; PROCEDURE Recurse 
      ( KTree : NodeObjTyp ; Depth : HeightTyp ; Sum : SeqSsTyp ) 
    RAISES ANY

    = 
      PROCEDURE PutLeft ( )
      RAISES ANY

      = BEGIN 
          Report . PutText ( SeqSsImage ( DumpSeqSs ) ) 
        ; Report . PutText ( SeqSsImage ( Sum ) ) 
        ; Report . PutText ( " " ) 
        ; Report . PutText ( RefImage ( KTree ) ) 
        ; Report . PutText 
            ( HeightImage ( KTree . Height ( ) ) ) 
        ; FOR I := 0 TO Depth 
          DO Report . PutText ( " " ) 
          END (* FOR *) 
        ; Report . PutText ( "(" ) 
        END PutLeft 

    ; PROCEDURE PutRight ( )
      RAISES ANY

      = BEGIN 
        Report . PutTextLine ( ")" ) 
      END PutRight 

    ; PROCEDURE PutE ( Element : ElemT ) 
      RAISES ANY

      = BEGIN 
          Report . PutText ( ElemImage ( Element ) ) 
        END PutE 

    ; PROCEDURE PutNonleafElem ( NonleafElem : NonleafElemTyp ) 
      RAISES ANY

      = BEGIN 
          Report . PutText ( "(" ) 
        ; Report . PutText ( RefImage ( NonleafElem . NleChildRef ) ) 
        ; Report . PutText ( "," ) 
        ; Report . PutText 
            ( ElemNoImage ( NonleafElem . NleCumChildCt ) ) 
        ; Report . PutText ( ")" ) 
        END PutNonleafElem 

    ; BEGIN (* Recurse *) 
        IF KTree = NIL 
        THEN 
          RETURN (* This is ill-formed. *) 
        ELSE
          TYPECASE KTree <*NOWARN*>
          OF LeafNodeObjTyp ( TLeafNode )
          => PutLeft ( )
          ; FOR J := 0 TO LeafNodeObjTyp . ElemCt ( TLeafNode ) - 1 
            DO 
              IF J > 0 THEN Report . PutText ( "," ) END (* IF *) 
            ; PutE ( TLeafNode . LeafElems ^ [ J ] ) 
            END (* FOR *) 
          ; PutRight ( )
          ; DumpSeqSs := DumpSeqSs + LeafNodeObjTyp . ElemCt ( TLeafNode ) 
          | NonleafNodeObjTyp ( TNonleafNode )
          => PutLeft ( )
          ; FOR J := 0 TO NonleafNodeObjTyp . ElemCt ( TNonleafNode ) - 1 
            DO 
              IF J > 0 THEN Report . PutText ( "," ) END (* IF *) 
            ; PutNonleafElem ( TNonleafNode . NonleafElems ^ [ J ] ) 
            END (* FOR *) 
          ; PutRight ( )
          ; FOR J := 0 TO NonleafNodeObjTyp . ElemCt ( TNonleafNode ) - 1 
            DO 
              Recurse 
                ( TNonleafNode . NonleafElems ^ [ J ] . NleChildRef 
                , Depth := Depth + 1 
                , Sum := Sum + CumChildrenToLeft ( KTree , J )  
                ) 
            END (* FOR *) 
          END (* TYPECASE *)
        END (* IF *) 
      END Recurse 

  ; BEGIN (* Dump *) 
      Report .  EnsureBol ( )
    ; Report .  PutTextLine 
        ( "------------- K-tree Dump -------------" ) 
    ; IF KTree = NIL 
      THEN (* Empty sequence. *) 
        Report .  PutTextLine ( " Empty " ) 
      ELSE 
        Recurse ( KTree , Depth := 0 , Sum := 0 ) 
      END (* IF *) 
    ; Report .  PutTextLine 
        ( "---------------------------------------" ) 
    END Dump 

(* EXPORTED: *) 
; PROCEDURE Height ( KTree : NodeObjTyp ) : INTEGER 
  RAISES ANY
  (* Height of K-tree.  Of internal interest only. *)

  = BEGIN 
      IF KTree = NIL 
      THEN 
        RETURN 0 
      ELSE 
        RETURN KTree . Height ( ) 
      END (* IF *) 
    END Height 

(* EXPORTED: *) 
; PROCEDURE InternalStatistics 
    ( KTree : NodeObjTyp 
    ; VAR LeafNodes : INTEGER 
    ; VAR LeafElems : INTEGER 
    ; VAR NonleafNodes : INTEGER 
    ; VAR NonleafElems : INTEGER 
    ) 
  RAISES ANY
  (* Of internal interest only. *)

  = 
    VAR LLeafNodes : INTEGER := 0 
  ; VAR LLeafElems : INTEGER := 0 
  ; VAR LNonleafNodes : INTEGER := 0 
  ; VAR LNonleafElems : INTEGER := 0 

  ; PROCEDURE Recurse ( KTree : NodeObjTyp ) 

    = BEGIN 
        IF KTree = NIL 
        THEN 
          RETURN 
        ELSE
          TYPECASE KTree <*NOWARN*>
          OF LeafNodeObjTyp ( TLeafNode )
          => INC ( LLeafNodes )
          ; INC ( LLeafElems , LeafNodeObjTyp . ElemCt ( TLeafNode ) ) 
          | NonleafNodeObjTyp ( TNonleafNode )
          => INC ( LNonleafNodes ) 
          ; INC ( LNonleafElems , NonleafNodeObjTyp . ElemCt ( TNonleafNode ) )
          ; FOR J := 0 TO NonleafNodeObjTyp . ElemCt ( TNonleafNode ) - 1 
            DO 
              Recurse 
                ( TNonleafNode . NonleafElems ^ [ J ] . NleChildRef ) 
            END (* FOR *) 
          END (* TYPECASE *)
        END (* IF *) 
      END Recurse 

  ; BEGIN (* InternalStatistics *) 
      Recurse ( KTree ) 
    ; LeafNodes := LLeafNodes 
    ; LeafElems := LLeafElems 
    ; NonleafNodes := LNonleafNodes 
    ; NonleafElems := LNonleafElems 
    END InternalStatistics 

; BEGIN (* KTrees *)
  END KTrees 
. 
