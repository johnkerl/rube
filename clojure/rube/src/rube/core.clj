(ns rube.core
  (:require clojure.contrib.math))

;;;; ================================================================
;;;; RUBE.CLJ
;;;;
;;;; Constructs the cycle decomposition of sequences of moves on the Rubik's
;;;; Cube.  Faces are denoted by F, R, B, L, U and D for front, right, back,
;;;; left, up and down, respectively.  Corner pieces are denoted by their three
;;;; faces, or 'elements', edges by their two.
;;;;
;;;; Moves are denoted by their face and either: +, -, or 2, for clockwise,
;;;; counterclockwise and half-turn, or no suffix, -1 and 2, for CW, CCW and
;;;; half-turn.  Examples: U+, F-1, R2.
;;;;
;;;; Processes are given as a list of moves, e.g. '(U+ F-1 R2).  Note that the
;;;; two notation styles for turns may be intermixed; however, the first style
;;;; is higher in the association lists, resulting in faster lookup time.
;;;;
;;;; John Kerl
;;;; 12/93
;;;; Ported to Clojure Dec. 2012 ...
;;;; ================================================================
;;;; Please see LICENSE.txt in the same directory as this file.
;;;; ================================================================

;;================================================================
;; To do (porting experiment underway):
;; * docstrings & in-line examples.
;; * wiki ...
;; * notes for Clojure-from-Lisp follow.
;;----------------------------------------------------------------
;; * eval order ... different from CL.
;;----------------------------------------------------------------
;; * this is very different from CL:
;;   (class '(2 3 4))
;;   (class (cons 1 '(2 3 4)))
;;
;;----------------------------------------------------------------
;; OK in CL:
;;(defun foo (x)
;;(+ 1 (bar x)))
;;
;;(defun bar (x) (* 2 x))
;;(print (foo 10))
;;----------------------------------------------------------------
;; Not OK in Clojure:
;;
;;(defn foo [x]
;; (+ 1 (bar x)))
;;(defn bar [x] (* 2 x))
;;
;;(print (foo 10))
;;
;; but works on w/ bar defined before foo
;;----------------------------------------------------------------
;; very important re the consing et al.:  http://clojure.org/sequences

;;; =============================================================================
;;; RUBE DATA SECTION -- lookup tables.

;; The names of three adjacent faces provides the name of a corner piece.
;; However, those three faces may be combined in any of six ways, e.g.
;; URF, UFR, FRU, FUR, RFU, and RUF.
;; This variable contains all such names in which the faces are listed
;; in a clockwise direction.  This could be calculated at run-time,
;; but it seems simpler to do a table lookup.
(def table-clockwise-orient-set
  ;; Corner-piece representations which have a clockwise orientation.  Used for
  ;; determining signs of moves.
  #{'URF 'UFL 'ULB 'UBR 'DFR 'DRB 'DBL 'DLF
    'RFU 'FLU 'LBU 'BRU 'FRD 'RBD 'BLD 'LFD
    'FUR 'LUF 'BUL 'RUB 'RDF 'BDR 'LDB 'FDL})

;; Data for the image of a face under a move.  Faces are down the table; moves
;; are across.  Faces are represented as characters; moves are represented by
;; symbols.  Key into this table by face to get an array of images of that
;; face.  Index into the resulting array, using the
;; table-move-index-lookup-alist, to get the image of the specified face under
;; the specified move.
(def table-move-on-face-table
  ;;     F+ R+ B+ L+ U+ D+ F- R- B- L- U- D- F2 R2 B2 L2 U2 D2
  ;; FRBLUD going down
  {\F [ \F \U \F \D \L \R \F \D \F \U \R \L \F \B \F \B \B \B ],
   \R [ \D \R \U \R \F \B \U \R \D \R \B \F \L \R \L \R \L \L ],
   \B [ \B \D \B \U \R \L \B \U \B \D \L \R \B \F \B \F \F \F ],
   \L [ \U \L \D \L \B \F \D \L \U \L \F \B \R \L \R \L \R \R ],
   \U [ \R \B \L \F \U \U \L \F \R \B \U \U \D \D \D \D \U \R ],
   \D [ \L \F \R \B \D \D \R \B \L \F \D \D \U \U \U \U \D \D ]})

;; See above comment.
;; To do: maybe reduce table count by one, using a hash map.
(def table-move-index-lookup-alist
   {'F+    0, 'R+    1, 'B+     2, 'L+    3, 'U+    4, 'D+    5,
    'F-    6, 'R-    7, 'B-     8, 'L-    9, 'U-   10, 'D-   11,
    'F2   12, 'R2   13, 'B2    14, 'L2   15, 'U2   16, 'D2   17,
    'F     0, 'R     1, 'B      2, 'L     3, 'U     4, 'D     5,
    'F-1   6, 'R-1   7, 'B-1    8, 'L-1   9, 'U-1  10, 'D-1  11,
    'f+    0, 'r+    1, 'b+     2, 'l+    3, 'u+    4, 'd+    5,
    'f-    6, 'r-    7, 'b-     8, 'l-    9, 'u-   10, 'd-   11,
    'f2   12, 'r2   13, 'b2    14, 'l2   15, 'u2   16, 'd2   17,
    'f     0, 'r     1, 'b      2, 'l     3, 'u     4, 'd     5,
    'f-1   6, 'r-1   7, 'b-1    8, 'l-1   9, 'u-1  10, 'd-1  11})

(def table-set-of-valid-moves
  #{'F   'R   'B   'L   'U   'D
    'F+  'R+  'B+  'L+  'U+  'D+
    'F-  'R-  'B-  'L-  'U-  'D-
    'F2  'R2  'B2  'L2  'U2  'D2
    'F-1 'R-1 'B-1 'L-1 'U-1 'D-1
    'f   'r   'b   'l   'u   'd
    'f+  'r+  'b+  'l+  'u+  'd+
    'f-  'r-  'b-  'l-  'u-  'd-
    'f2  'r2  'b2  'l2  'u2  'd2
    'f-1 'r-1 'b-1 'l-1 'u-1 'd-1})

;; A table (represented as a hash map) to show the inverse of any move.
(def table-invert-move-alist
  {'F+  'F-, 'R+  'R-, 'B+  'B-, 'L+  'L-, 'U+  'U-, 'D+  'D-,
   'F-  'F+, 'R-  'R+, 'B-  'B+, 'L-  'L+, 'U-  'U+, 'D-  'D+,
   'F2  'F2, 'R2  'R2, 'B2  'B2, 'L2  'L2, 'U2  'U2, 'D2  'D2,
   'F   'F-, 'R   'R-, 'B   'B-, 'L   'L-, 'U   'U-, 'D   'D-,
   'f+  'f-, 'r+  'r-, 'b+  'b-, 'l+  'l-, 'u+  'u-, 'd+  'd-,
   'f-  'f+, 'r-  'r+, 'b-  'b+, 'l-  'l+, 'u-  'u+, 'd-  'd+,
   'f2  'f2, 'r2  'r2, 'b2  'b2, 'l2  'l2, 'u2  'u2, 'd2  'd2,
   'f   'f-, 'r   'r-, 'b   'b-, 'l   'l-, 'u   'u-, 'd   'd-})

;; A table of all the movable pieces (i.e., not including centers)
;; on the cube.
(def table-list-of-all-pieces
  (list 'UFR 'UFL 'UBL 'UBR 'DFR 'DFL 'DBL 'DBR
    'UF 'UL 'UB 'UR 'FR 'FL 'BL 'BR 'DF 'DL 'DB 'DR))
;; Do set of the seq, not seq of the set, to keep the order nice.
(def table-set-of-all-pieces
  (set table-list-of-all-pieces))

;; These equivalence classes are for piece-equal?
(def UFL_equivs #{'UFL 'ULF 'FUL 'FLU 'LUF 'LFU})
(def UFR_equivs #{'UFR 'URF 'FUR 'FRU 'RUF 'RFU})
(def UBL_equivs #{'UBL 'ULB 'BUL 'BLU 'LUB 'LBU})
(def UBR_equivs #{'UBR 'URB 'BUR 'BRU 'RUB 'RBU})
(def DFL_equivs #{'DFL 'DLF 'FDL 'FLD 'LDF 'LFD})
(def DFR_equivs #{'DFR 'DRF 'FDR 'FRD 'RDF 'RFD})
(def DBL_equivs #{'DBL 'DLB 'BDL 'BLD 'LDB 'LBD})
(def DBR_equivs #{'DBR 'DRB 'BDR 'BRD 'RDB 'RBD})

(def UF_equivs #{'UF 'FU})
(def UL_equivs #{'UL 'LU})
(def UB_equivs #{'UB 'BU})
(def UR_equivs #{'UR 'RU})
(def FR_equivs #{'FR 'RF})
(def FL_equivs #{'FL 'LF})
(def BL_equivs #{'BL 'LB})
(def BR_equivs #{'BR 'RB})
(def DF_equivs #{'DF 'FD})
(def DL_equivs #{'DL 'LD})
(def DB_equivs #{'DB 'BD})
(def DR_equivs #{'DR 'RD})

;; For each possible representation of each piece, a map from that
;; representation to the set of representations equivalent to it.
(def corner-piece-equivs
  { 'UFL  UFL_equivs, 'ULF  UFL_equivs, 'FUL  UFL_equivs, 'FLU  UFL_equivs, 'LUF  UFL_equivs, 'LFU  UFL_equivs,
    'UFR  UFR_equivs, 'URF  UFR_equivs, 'FUR  UFR_equivs, 'FRU  UFR_equivs, 'RUF  UFR_equivs, 'RFU  UFR_equivs,
    'UBL  UBL_equivs, 'ULB  UBL_equivs, 'BUL  UBL_equivs, 'BLU  UBL_equivs, 'LUB  UBL_equivs, 'LBU  UBL_equivs,
    'UBR  UBR_equivs, 'URB  UBR_equivs, 'BUR  UBR_equivs, 'BRU  UBR_equivs, 'RUB  UBR_equivs, 'RBU  UBR_equivs,
    'DFL  DFL_equivs, 'DLF  DFL_equivs, 'FDL  DFL_equivs, 'FLD  DFL_equivs, 'LDF  DFL_equivs, 'LFD  DFL_equivs,
    'DFR  DFR_equivs, 'DRF  DFR_equivs, 'FDR  DFR_equivs, 'FRD  DFR_equivs, 'RDF  DFR_equivs, 'RFD  DFR_equivs,
    'DBL  DBL_equivs, 'DLB  DBL_equivs, 'BDL  DBL_equivs, 'BLD  DBL_equivs, 'LDB  DBL_equivs, 'LBD  DBL_equivs,
    'DBR  DBR_equivs, 'DRB  DBR_equivs, 'BDR  DBR_equivs, 'BRD  DBR_equivs, 'RDB  DBR_equivs, 'RBD  DBR_equivs } )

(def edge-piece-equivs
  { 'UF  UF_equivs, 'FU  UF_equivs,
    'UL  UL_equivs, 'LU  UL_equivs,
    'UB  UB_equivs, 'BU  UB_equivs,
    'UR  UR_equivs, 'RU  UR_equivs,
    'FR  FR_equivs, 'RF  FR_equivs,
    'FL  FL_equivs, 'LF  FL_equivs,
    'BL  BL_equivs, 'LB  BL_equivs,
    'BR  BR_equivs, 'RB  BR_equivs,
    'DF  DF_equivs, 'FD  DF_equivs,
    'DL  DL_equivs, 'LD  DL_equivs,
    'DB  DB_equivs, 'BD  DB_equivs,
    'DR  DR_equivs, 'RD  DR_equivs })

;;; =============================================================================
;; RUBE PORTABILITY SECTION
;; Implementation of these functions depends on the platform.  When I originally
;; wrote a Common Lisp version, then ported it to Emacs Lisp, these functions
;; were all that needed changing.  For Clojure, of course, the differences are
;; far more pervasive.
;;
;; For Common Lisp, use "string".
;; For Emacs Lisp, use "prin1-to-string".
(defn symbol-to-string [sym]
  (.toString sym))

;; For Common Lisp, use "char".
;; For Emacs Lisp, use "aref".
(defn select-char-in-string [string elt]
  (.charAt string elt))

;; Common Lisp has symbolp, stringp, and characterp; Clojure has symbol? and
;; string?  but not character?.
(defn character? [p]
  (= (.getName (class p)) "java.lang.Character"))

;;; =============================================================================
(defn is-corner-piece? [piece]
  (= 3 (.length (symbol-to-string piece))))
(defn is-edge-piece? [piece]
  (= 2 (.length (symbol-to-string piece))))

;;; =============================================================================
;;; This function is particularly important for unit test.

;; The names of three adjacent faces provide the name of a corner piece;
;; the names of two adjacent faces provide the name of an edge piece.
;; However, a corner's three faces may be combined in any of six (3!) ways, e.g.
;; URF, UFR, FRU, FUR, RFU, and RUF; and an edge's two faces may be combined
;; in one of two (2!) ways, e.g. UF and FU.
;;
;; This function sees if two representations refer to the same piece.
;; E.g. UFR is the same as FRU; UFR is not the same as UFL; UFR is not the
;; same as UF.

(defn corner-piece-equal? [p1 p2]
  (contains? (p1 corner-piece-equivs) p2))
(defn edge-piece-equal? [p1 p2]
  (contains? (p1 edge-piece-equivs) p2))
;; To do: piece-lists-equal? can give us e.g. '(UF +).  So input (namely, the
;; '+) may not be a valid piece. Comment this well, or rework the caller.
(defn piece-equal? [p1 p2]
  "Determines whether two pieces are equal: e.g. UFR and FRU are the same piece."
  (cond (and (is-corner-piece? p1) (is-corner-piece? p2))
        (corner-piece-equal? p1 p2)
        (and (is-edge-piece? p1) (is-edge-piece? p2))
        (edge-piece-equal? p1 p2)
        :else false))

(defn piece-lists-equal? [list-1 list-2]
  ;; Principally for unit-test
  (cond (and (empty? list-1) (empty? list-2))
        true
        (and (empty? list-1) (not (empty? list-2)))
        false
        (and (empty? list-2) (not (empty? list-1)))
        false
        (not (piece-equal? (first list-1) (first list-2)))
        false
        :else
        (recur (rest list-1) (rest list-2))))

;;; -----------------------------------------------------------------------------
;; To do: cmt about return types
(defn validate-moves-aux [moves]
  (cond (empty? moves) true
        :else
        (let [move (first moves)]
          (if (not (contains?  table-set-of-valid-moves move))
            (throw
              (Exception.
                (format "Malformed move \"%s\" in sequence \"%s\"."
                        (.toString move) (.toString moves))))
            (recur (rest moves))))))

(defn validate-moves [moves]
  (if (validate-moves-aux moves)
    moves
    ;; To do: find a more elegant way to do this ... maybe use an each filter for
    ;; the aux w/ boolean rv, & do the throw from here ... ?
    "An exception should have been thrown and you should not see this."))

;;; =============================================================================
;;; RUBE LOWEST-LEVEL-MOVES SECTION
;;; Image of a single face under a move.

;; ----------------------------------------------------------------
;; Returns the image of a face under a move.
;; Faces are represented by characters; moves are represented by symbols.
;; Does a simple table lookup.
(defn find-image-of-move-on-face [move face]
  "Returns the image of a face under a move, e.g. U on F gives R."
  (let [row-vector (table-move-on-face-table face),
        vector-index (table-move-index-lookup-alist move)]
    (cond (= nil row-vector)
          (throw (format "find-image-of-move-on-face:  Rule lookup failed for face %s."
                         (cond (symbol? face) (symbol-to-string face)
                               (string? face) face
                               (character? face) (symbol-to-string face))))
          (= nil vector-index)
          (throw (format "find-image-of-move-on-face:  Rule lookup failed for move %s."
                         (cond (symbol? move) (.toString move)
                               (string? move) move
                               (character? move) (symbol-to-string move))))
          :else (nth row-vector vector-index))))

;;; =============================================================================
;;; RUBE NEXT-LOWEST-LEVEL-MOVES SECTION
;;; Image of one piece (having two or three faces, for edge or corner pieces,
;;; respectively) under a move.

;;----------------------------------------------------------------
;; Returns the image of a corner piece under a move.
;; Breaks the piece up into its component faces; looks up the image
;; of each face under the specifed move; then puts the resulting
;; faces back together.
;;
;; What the if-statement does is, if the move (denoted by a face)
;; isn't on the same face as one of the piece's faces, then the
;; piece is unaffected by the move.  E.g. URF is affected by F;
;; but URL is not.
;;
;; Both arguments are represented by symbols.

(defn find-image-of-move-on-corner-piece [move corner]
  "Returns the image of a corner piece under a move, e.g. U on UFL gives UFL."
  (let [ps   (symbol-to-string corner),
        ps0  (select-char-in-string ps 0),
        ps1  (select-char-in-string ps 1),
        ps2  (select-char-in-string ps 2),
        face (select-char-in-string (symbol-to-string move) 0)]
    (if (or (= face ps0) (= face ps1) (= face ps2))
      (symbol
        (str (symbol-to-string (find-image-of-move-on-face move ps0))
             (symbol-to-string (find-image-of-move-on-face move ps1))
             (symbol-to-string (find-image-of-move-on-face move ps2))))
      ;; else
      corner)))

;;----------------------------------------------------------------
;; Returns the image of an edge piece under a move.
;; Breaks the piece up into its component faces; looks up the image
;; of each face under the specifed move; then puts the resulting
;; faces back together by turning a string into a symbol.
;; (See Steele's CLTL for a description of the difference between
;; intern and make-symbol.  For me, the relevant difference is that
;; a symbol created with make-symbol prints with a leading #: (ugly),
;; whereas a symbol created with intern doesn't (pretty).)
;;
;; What the if-statement does is, if the move (denoted by a face)
;; isn't on the same face as one of the piece's faces, then the
;; piece is unaffected by the move.  E.g. UF is affected by F;
;; but UR is not.
;;
;; Both arguments are represented by symbols.

(defn find-image-of-move-on-edge-piece [move edge]
  "Returns the image of an edge piece under a move, e.g. U on UF gives UR."
  (let [ps   (symbol-to-string edge)
        ps0  (select-char-in-string ps 0)
        ps1  (select-char-in-string ps 1)
        face (select-char-in-string (symbol-to-string move) 0)]
    (if (or (= face ps0) (= face ps1))
      (symbol
        (str
          (symbol-to-string (find-image-of-move-on-face move ps0))
          (symbol-to-string (find-image-of-move-on-face move ps1))))
      ;; else
      edge)))

;;; =============================================================================
;;; RUBE THIRD-LOWEST-LEVEL-MOVES SECTION
;;; Image of a sequence of moves on a single (edge or corner) piece.

;; -----------------------------------------------------------------------------
(defn find-image-of-moves-on-piece [moves piece]
  "Returns the image of a list of moves on a piece, e.g. U R on UF gives BF."
  ;; Need to tune this (use an auxiliary function? moves-on-corner-piece and
  ;; moves-on-edge-piece?) so that it doesn't check the same piece over and
  ;; over for corner/edge.  Once passed in, it won't change!
  (if (is-corner-piece? piece)
    (if (empty? moves)
      piece
      (recur
        (rest moves)
        (find-image-of-move-on-corner-piece (first moves) piece)))
    (if (empty? moves)
      piece
      (recur
        (rest moves)
        (find-image-of-move-on-edge-piece (first moves) piece)))))

;;; =============================================================================
;;; RUBE FOURTH-LOWEST-LEVEL-MOVES SECTION
;;; Image of a sequence of moves on a list of pieces.

;;----------------------------------------------------------------
(defn find-image-of-moves-on-pieces [moves pieces]
  "Returns the image of a list of moves on a list of pieces."
  (map (fn [piece] (find-image-of-moves-on-piece moves piece)) pieces))

;;; =============================================================================
;;; RUBE EXPONENT SECTION

(defn invert-move [move]
  "Returns the inverse of a single move."
  (let [inv (table-invert-move-alist move)]
    (cond (nil? inv)
          (throw
            (format
              "invert-move:  Couldn't find inverse for %s\n" (.tostring move)))
          :else
          inv)))

(defn invert-moves [moves]
  "Returns the inverse of a sequence of moves."
  ;; Inverting a list of moves means reversing it and replacing each
  ;; individual move with its inverse: (A * B)^-1 = B^-1 * A^-1.
  (cond (empty? moves) '()
        (not (seq? moves)) (list moves)
        :else
        (concat
          (invert-moves (rest moves))
          (list (invert-move (first moves))))))

(defn find-power-of-sequence [moves power]
  "Returns a multiple concatenation of a sequence of moves.  Negative exponents indicate inversion."
  (cond (= power  0) '()
        (= power  1) moves
        (> power  1) (concat moves (find-power-of-sequence moves (- power 1)))
        (= power -1) (invert-moves moves)
        (< power -1) (find-power-of-sequence (invert-moves moves) (- power))))

(defn find-conjugate [moves1 moves2]
  "Returns the find-conjugate of A by B: A * B * A^-1."
  (let [lmoves1 (if (symbol? moves1) (list moves1) moves1)
        lmoves2 (if (symbol? moves2) (list moves2) moves2)]
    (concat lmoves1 lmoves2 (invert-moves lmoves1))))

(defn find-commutator [moves1 moves2]
  "Returns the commutator of A and B: A * B * A^-1 * B^-1."
  ;; The commutator of two moves or list of moves on the Rubik's Cube.  In
  ;; group theory, the commutator of A and B, written [A B], is defined to be A
  ;; * B * A^-1 * B^-1.  Then [A B] == the identity sequence precisely when A
  ;; and B commute (i.e. when A*B == B*A).
  ;;
  ;; Note however that at this we have [U D] = (U D U- D-) which clearly is the
  ;; identity on the cube but which is not pared down to such symbolically.
  ;; This fact will be discovered by cycle decomposition.
  (let [lmoves1 (if (symbol? moves1) (list moves1) moves1)
        lmoves2 (if (symbol? moves2) (list moves2) moves2)]
    (concat lmoves1 lmoves2
            (invert-moves lmoves1) (invert-moves lmoves2))))

;;; =============================================================================
;;; RUBE CYCLE SECTION

(defn find-cycle-length [cycle]
  "Accepts signed-cycle notation on input: e.g. '(UFR DFL) has length 2 and '(UF UB +) has length 4."
  ;; Supports the signed-cycle notation used by the find-cycle-decomposition logic.
  ;; E.g. the cycle '(UFR DFL) has length 2.  But '(UFR +) has length, not 1,
  ;; but 3.  Likewise '(UF UB +) has length 4.
  (if (or (= (last cycle) '+) (= (last cycle) '-))
    (let [num-faces (.length (symbol-to-string (first cycle)))]
      (cond (= num-faces 3)
            (* 3 (- (count cycle) 1))
            (= num-faces 2)
            (* 2 (- (count cycle) 1))
            :else (count cycle)))
    (count cycle)))

;; -----------------------------------------------------------------------------
(defn memtree? [atm tree]
  ;; A predicate to determine whether non-null atom ATM is rube-equivalent to
  ;; an atomic member of TREE (i.e. if the piece given by atm is the same piece
  ;; as one in the tree, not considering spin).
  (cond (and (seq? tree) (empty? tree)) false
        (not (seq? tree)) (piece-equal? atm tree)
        ;; ((equal atm tree) t) ;; I would use equal outside of Rube code.
        ;; I want to see if a piece has already been decomposed, regardless of
        ;; orientation; hence the need for piece-equal?.
        :else (or (memtree? atm (first tree)) (memtree? atm (rest tree)))))

;; -----------------------------------------------------------------------------
(defn find-sign-of-rotation [orient-1 orient-2]
  ;; To do: comment re context
  (cond
    ; Same representation of same piece -- no spin.
    (= orient-1 orient-2) '()

    ; No sign if the pieces aren't the same.
    (not (piece-equal? orient-1 orient-2)) '()

    ; Arbitrarily, say + for any differently represented edges
    ; (the rotation group only has order 2).
    (is-edge-piece? orient-1) '(+)

    ; Now they must be corner pieces.  Do a table lookup.
    (contains? table-clockwise-orient-set orient-1)
    (if (= (select-char-in-string (symbol-to-string orient-1) 1)
           (select-char-in-string (symbol-to-string orient-2) 0))
      '(+) '(-))
    :else
    (if (= (select-char-in-string (symbol-to-string orient-1) 1)
           (select-char-in-string (symbol-to-string orient-2) 0))
      '(-) '(+))))

;; -----------------------------------------------------------------------------
(defn find-cycle-aux [piece-list start-piece moves]
  ;; Returns the unsigned cycle of piece under moves, reversed.
  ;; This is an implementation detail of the cycle-finder.
  ;; To do: elaborate on that.
  (let [curr-piece (first piece-list)
        next-piece (find-image-of-moves-on-piece moves curr-piece)]
    (if (piece-equal? next-piece start-piece)
      piece-list
      (recur (cons next-piece piece-list) start-piece moves))))

(defn find-cycle [piece moves]
  (let [unsigned-reversed-cycle (find-cycle-aux (list piece) piece moves)
        next-piece (find-image-of-moves-on-piece moves (first unsigned-reversed-cycle))]
    (reverse (concat (find-sign-of-rotation piece next-piece) unsigned-reversed-cycle))))

;;;----------------------------------------------------------------
(defn find-cycles-aux [pieces moves current-cycles]
  (cond (empty? pieces)
        current-cycles

        (memtree? (first pieces) current-cycles)
        (recur (rest pieces) moves current-cycles)

        :else
        (let [new-cycle (find-cycle (first pieces) moves)
              augmented-cycles (concat (list new-cycle) current-cycles)]
          (recur (rest pieces) moves augmented-cycles))))

(defn find-cycles [pieces moves]
  (reverse (seq (find-cycles-aux pieces moves '()))))

;; -----------------------------------------------------------------------------
(defn delete-one-cycles [cycles]
  (filter
    (fn [cycle] (> (count cycle) 1))
    cycles))

;; -----------------------------------------------------------------------------
(defn find-cycle-decomposition [moves]
  ;; Given a list of Rubik's Cube pieces and a list of moves, return the cycle
  ;; decomposition of the moves, omitting trivial cycles.
  (delete-one-cycles (find-cycles table-list-of-all-pieces moves)))

;;; =============================================================================
;;; RUBE ORDER SECTION

(defn find-multi-lcm [args]
  "E.g. '(8 12 20) maps to 120  This is a built-in (namely, lcm) in Common Lisp."
  (cond (empty? args) 1
        :else (let [f (first args) r (rest args)]
                (clojure.contrib.math/lcm f (find-multi-lcm r)))))

(defn find-order [moves]
  (find-multi-lcm
    (map #(find-cycle-length %)
         (find-cycle-decomposition moves))))

;;; =============================================================================
;;; RUBE IMAGES SECTION

;; * given moves & pieces
;; * make list of images paralleling pieces
;; * make/call a func which zips up the pairs
;; * filter out non-trivial pairs: !pc=, or pc= but s-o-r != '()
;; * print that

;; To do: could be a letrec.
;; To do: standalone for UT ... ?
(defn pair-pieces-and-images [pieces images]
  (if (empty? pieces)
    '()
    (let [first-piece (first pieces)
          first-image (first images)
          sign (find-sign-of-rotation first-piece first-image)]
      ; same piece, no sign.
      ; same piece, with sign
      ; different piece
      (if (piece-equal? first-piece first-image)
        (if (empty? sign) ; To do: maybe use null?
          (pair-pieces-and-images (rest pieces) (rest images))
          (cons
            (list first-piece (first sign))
            (pair-pieces-and-images (rest pieces) (rest images))))
        (cons
          (list first-piece '-> first-image)
          (pair-pieces-and-images (rest pieces) (rest images)))))))

(defn find-images-of-pieces [moves, pieces]
  (pair-pieces-and-images pieces (find-image-of-moves-on-pieces moves pieces)))

(defn find-images-of-all-pieces [moves]
  (find-images-of-pieces moves table-list-of-all-pieces))

;;; =============================================================================
;;; RUBE PPRINT SECTION

;; Example output:
;; (tell-about '(B- U2 B2 U B- U- B- U2 F R B R- F-))
;;
;; -- Sequence is:
;; B- U2 B2 U B- U- B- U2 F R B R- F-
;; -- Image is:
;; UB +
;; UR +
;; -- Cycle decomposition is:
;; UB +
;; UR +
;; -- Order is:
;; 2

(defn print-delimited [lst delimiter]
  (if (empty? lst)
    (println '())
    (println (reduce #(str (.toString %1) delimiter (.toString %2)) (vec lst)))))

(defn print-moves [moves]
  (print-delimited moves " "))
(defn print-images [moves]
  (print-delimited (find-images-of-all-pieces moves) "\n"))
(defn print-cycle-decomposition [moves]
  (print-delimited (find-cycle-decomposition moves) "\n"))

(defn tell-about [moves]
  (validate-moves moves)
  (println "-- Sequence of moves is:")
  (print-moves moves)
  (println)
  (println "-- Image is:")
  (print-images moves)
  (println)
  (println "-- Cycle decomposition is:")
  (print-cycle-decomposition moves)
  (println)
  (print "-- Order is: ")
  (println (find-order moves))
  nil) ; To do: how to do (values)?
