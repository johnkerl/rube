;; xxx def vs. deftest ...
;; xxx the is business ...

(ns rube.test.core
  (:use [rube.core])
  (:use [clojure.test]))

;;; ================================================================
(deftest test-symbol-to-string
  (is "UFR" (symbol-to-string 'UFR)))

(deftest test-select-char-in-string
  (and
    (is \a (select-char-in-string "abc" 0))
    (is \b (select-char-in-string "abc" 1))
    (is \c (select-char-in-string "abc" 2))))

(deftest test-character?
  (and
    (is (character? \a))
    (is (not (character? 'a)))
    (is (not (character? "a")))))

;;; ================================================================
(deftest test-is-corner-piece
  (and
    (is (is-corner-piece 'UFR))
    (is (not (is-corner-piece 'UR)))))

;;; ================================================================
(deftest test-piece-equal?
  (and
    (is (piece-equal? 'UFR 'UFR))
    (is (piece-equal? 'UFR 'URF))
    (is (piece-equal? 'UFR 'RUF))
    (is (piece-equal? 'UF 'UF))
    (is (piece-equal? 'UF 'FU))
    (is (not (piece-equal? 'UFR 'DFR)))
    (is (not (piece-equal? 'UFR 'RF)))
    (is (not (piece-equal? 'UF 'URF)))))

;;; ================================================================
(deftest test-piece-lists-equal?
  (and
    (is (piece-lists-equal? '(UFR) '(UFR)))
    (is (piece-lists-equal? '(UFR) '(URF)))
    (is (piece-lists-equal? '(UFR) '(RUF)))
    (is (piece-lists-equal? '(UF)  '(UF)))
    (is (piece-lists-equal? '(UF)  '(FU)))
    (is (piece-lists-equal? '(UFR UF BR) '(UFR FU RB)))
    (is (not (piece-lists-equal? '(UFR) '(DFR))))
    (is (not (piece-lists-equal? '(UFR) '(RF))))
    (is (not (piece-lists-equal? '(UF)  '(URF))))
    (is (not (piece-lists-equal? '(UF)  '(UF UR))))
    (is (not (piece-lists-equal? '()  '(UF UR))))
    (is (not (piece-lists-equal? '(UF)  '(UR))))
    (is (piece-lists-equal? '()  '()))))

;;; ================================================================
;;; xxx to do
;;; test-validate-moves

;;; ================================================================
(deftest test-move-on-face
  (and
    (is \R (move-on-face 'U \F))
    (is \D (move-on-face 'U \D))))

;;; ================================================================
(deftest test-move-on-corner-piece
  (and
    (is (piece-equal? 'DFR (move-on-corner-piece 'F 'URF)))
    (is (piece-equal? 'URF (move-on-corner-piece 'B 'URF)))))

(deftest test-move-on-edge-piece
  (and
    (is (piece-equal? 'FR (move-on-edge-piece 'F 'UF)))
    (is (piece-equal? 'UF (move-on-edge-piece 'B 'UF)))))

;;; ================================================================
(deftest test-moves-on-piece
  (and
    (is (piece-equal? 'UBR (moves-on-piece '(U F R) 'UFR)))
    (is (piece-equal? 'URF (moves-on-piece '(F F F F) 'UFR)))))
;;; xxx more ...

;;; ================================================================
(deftest test-moves-on-pieces
  (and
    (is (piece-lists-equal?
        '(UL RU DR BF)
        (moves-on-pieces '(U F R) '(UF UR BR BF))))
    (is (piece-lists-equal? '(RFD RF) (moves-on-pieces '(F) '(UFR UF))))
    (is (piece-lists-equal? '(DFL DF) (moves-on-pieces '(F F) '(UFR UF))))
    (is (piece-lists-equal? '(LFU LF) (moves-on-pieces '(F F F) '(UFR UF))))
    (is (piece-lists-equal? '(UFR UF) (moves-on-pieces '(F F F F) '(UFR UF))))))

;; ================================================================
(deftest test-invert-move
  (and
    (is 'U- (invert-move 'U))
    (is 'U+ (invert-move 'U-))
    (is 'U2 (invert-move 'U2))))

;; ================================================================
(deftest test-invert-moves
  (and
    (is '() (invert-moves '()))
    (is '(U-) (invert-moves '(U)))
    (is '(U+) (invert-moves '(U-)))
    (is '(U2) (invert-moves '(U2)))
    (is '(F- R-) (invert-moves '(R F)))
    (is '(F- R- B- L2 U+ D2) (invert-moves '(D2 U- L2 B R F+)))))

;; ================================================================
(deftest test-power-of-sequence
  (and
    (is '()   (power-of-sequence '() 0))
    (is '()   (power-of-sequence '() 1))
    (is '()   (power-of-sequence '() 2))
    (is '()   (power-of-sequence '() -1))
    (is '()   (power-of-sequence '() -2))
    (is '()   (power-of-sequence '(F) 0))
    (is '(F)  (power-of-sequence '(F) 1))
    (is '(F2) (power-of-sequence '(F) 2))
    (is '(F-) (power-of-sequence '(F) -1))
    (is '(F-) (power-of-sequence '(F) 3))
    (is '()   (power-of-sequence '(F) 4))
    (is '()   (power-of-sequence '(F) -4))
    (is '(F2) (power-of-sequence '(F) -2))
    (is '(F R F R F R) (power-of-sequence '(F R) 3))))

;; ================================================================
(deftest test-conjugate
  (and
    (is '(F R F-)) (conjugate 'F 'R)
    (is '(F R F-)) (conjugate 'F '(R))
    (is '(F R F-)) (conjugate '(F) 'R)
    (is '(F R F-)) (conjugate '(F) '(R))
    (is '()) (conjugate '() '())
    (is '()) (conjugate '(U) '(U U U U))))

;; ================================================================
(deftest test-commutator
  (and
    (is '() (commutator '() '()))
    (is '(U R U- R-) (commutator '(U) '(R)))
    (is '(U D U- D-) (commutator '(U) '(D)))))

;; ================================================================
(deftest test-cycle-length
  (and
    (is 0 (cycle-length '()))
    (is 1 (cycle-length '(UF)))
    (is 2 (cycle-length '(UF DB)))
    (is 2 (cycle-length '(UFR DBL)))
    (is 4 (cycle-length '(UF FR +)))
    (is 4 (cycle-length '(UF FR -)))
    (is 6 (cycle-length '(UF FL FR -)))
    (is 9 (cycle-length '(UFR DFL BRU -)))))

;; ================================================================
(deftest test-memtree
  (and
    ;xxx fix me(not (is (memtree? 'UF '())))
    (is  (memtree? 'UF 'UF))
    (is  (memtree? 'UF '(UF)))
    (is  (memtree? 'UF '(UF FR BL)))
    ;xxx fix me(not (is (memtree? 'UL '(UF FR BL))))
    (is  (memtree? 'UL '(UF (FB (UL) DL) FR BL)))))

;; ================================================================
(deftest test-get-sign-of-rotation
  (and
    (is '()  (get-sign-of-rotation 'UF 'DL))
    (is '()  (get-sign-of-rotation 'UF 'FU))
    (is '(+) (get-sign-of-rotation 'UF 'UF))
    (is '()  (get-sign-of-rotation 'UFR 'UFL))
    (is '()  (get-sign-of-rotation 'UFR 'UFR))
    (is '(+) (get-sign-of-rotation 'UFR 'URF))
    (is '(-) (get-sign-of-rotation 'FUR 'FRU))
    (is '()  (get-sign-of-rotation 'UFR 'UR))))

;; ================================================================
(deftest test-find-cycle-aux
  (and
    (is (piece-lists-equal? '(UR UB UL UF) (find-cycle-aux '(UF) 'UF '(U))))
    (is (piece-lists-equal? '(URB UBL ULF URF) (find-cycle-aux '(UFR) 'UFR '(U))))
    (is (piece-lists-equal? '(RDB DLB FDR BUR FUL UFR) (find-cycle-aux '(UFR) 'UFR '(F R2 B L))))))

;; ================================================================
(deftest test-find-cycle
  (and
    (is (piece-lists-equal? '(UF UL UB UR) (find-cycle 'UF '(U))))
    (is (piece-lists-equal? '(UFR ULF UBL URB) (find-cycle 'UFR '(U))))
    (is (piece-lists-equal? '(UFR FUL BLU URB -) (find-cycle 'UFR '(F R B L U D))))))

;; ================================================================
(deftest test-find-cycles
  (and
    (is (piece-lists-equal? '((UF UL UB UR)) (find-cycles '(UF UB) '(U))))
    (is (piece-lists-equal? '((UF UB)) (find-cycles '(UF UB) '(U2))))
    (is (piece-lists-equal? '((UF) (UB)) (find-cycles '(UF UB) '(U U U U))))
    (is (piece-lists-equal? '((UF) (UB)) (find-cycles '(UF UB) '(D))))
    (is (piece-lists-equal? '((UF UL UB UR) (UFR ULF UBL URB)) (find-cycles '(UF UFR) '(U))))

    (is (piece-lists-equal? '((UFR ULF UBL URB)) (find-cycles '(UFR) '(U))))
    (is (piece-lists-equal? '((UF UL UB UR)) (find-cycles '(UF) '(U))))
    (is (piece-lists-equal? '((UFR ULF UBL URB)) (find-cycles '(UFR UFL) '(U))))
    (is (piece-lists-equal? '((UFR ULF UBL URB) (UF UL UB UR)) (find-cycles '(UFR UF) '(U))))))

;; ================================================================
(deftest test-delete-one-cycles
  (and
    (is (= '() (delete-one-cycles '())))
    (is (= '() (delete-one-cycles '((UF)))))
    (is (= '() (delete-one-cycles '((UF) (UR)))))
    (is (= '((UF UR)) (delete-one-cycles '((UF UR)))))))

;; ================================================================
;; xxx not piece-lists-equal?  equality is one level deeper than that.
;; want full elementwise comparison, but using piece-equal? for atoms.
(deftest test-cycle-decomposition
  (and
    (is (= '((UFR RFD DFL LFU) (UF RF DF LF))
           (cycle-decomposition '(F))))
    (is (= '((UFR -) (UFL RUB RBD RDF DLF +) (UF RU RB RD RF DF LF))
           (cycle-decomposition '(F R))))
    (is (= '((UFR -) (UFL -) (UBL -) (UBR -) (DFR LDB DRB -) (DFL -)
               (UF RU UB LU LF) (FR FD DL BL BD BR DR))
           (cycle-decomposition '(F R B L))))
    (is (= '((UFR DFL DBR) (UFL UBR DFR) (UF DF) (UR DR) (FR FL BR))
           (cycle-decomposition '(F2 R2))))
    (is (= '((UFR DFL) (UFL DFR) (UF DF) (FR FL))
           (cycle-decomposition '(F2))))
    (is (= '((UFR DFL) (UFL DFR) (UBL DBR) (UBR DBL) (UF DF) (UB DB) (FR FL) (BL BR))
           (cycle-decomposition '(F2 B2))))
    (is (= '()
           (cycle-decomposition '(U D U- D-))))))

;; ================================================================
(deftest test-multi-lcm
  (and
    (is (=   1 (multi-lcm '())))
    (is (=   8 (multi-lcm '(8))))
    (is (=  40 (multi-lcm '(8 10))))
    (is (= 120 (multi-lcm '(8 10 12))))
    (is (= 840 (multi-lcm '(8 10 12 14))))))

;; ================================================================
(deftest test-order
  (and
    (is (=   1 (order '())))
    (is (=   4 (order '(U))))
    (is (=   2 (order '(U2))))
    (is (=   1 (order '(U U U U))))
    (is (=  12 (order '(F R B L U D))))
    (is (=   6 (order '(F2 R2 B2 L2))))
    (is (= 315 (order '(F R B L))))))

;; ================================================================
(deftest test-pair-pieces-and-images
  (and
    (is (= (pair-pieces-and-images '() '()) '()))
    (is (= (pair-pieces-and-images '(UF) '(UF)) '()))
    (is (= (pair-pieces-and-images '(UF) '(FU)) '((UF +))))
    (is (= (pair-pieces-and-images '(UFR) '(UFR)) '()))
    (is (= (pair-pieces-and-images '(UFR) '(FRU)) '((UFR -))))
    (is (= (pair-pieces-and-images '(UF UFR) '(UF UFR)) '()))
    (is (= (pair-pieces-and-images '(UF UFR) '(UF FRU)) '((UFR -))))
    (is (= (pair-pieces-and-images '(UF UFR) '(FU UFR)) '((UF +))))
    (is (= (pair-pieces-and-images '(UF) '(BL)) '((UF -> BL))))
    (is (= (pair-pieces-and-images '(UFR UFL BL) '(FRU UFD UD))
           '((UFR -) (UFL  -> UFD) (BL -> UD))))))

;; ================================================================
(deftest test-images-of-pieces
  (and
    (is (= (images-of-pieces '() '()) '()))
    (is (= (images-of-pieces '(F R B L) '()) '()))
    (is (= (images-of-pieces '() '(UFR UF)) '()))
    (is (= (images-of-pieces '(U) '(UFR UF)) '((UFR -> ULF) (UF -> UL))))))

;;;; -----------------------------------------------------------------------------
;;;; TESTS TO BE FILED

;;	;; (mapcar #'(lambda (x)	  (move-on-face 'U x)) '(\F \R \B \L \U \D))
;;
;;	;; (setf forward-moves  '(F R B L U D))
;;	;; (setf backward-moves '(F- R- B- L- U- D-))
;;	;; (setf double-moves   '(F2 R2 B2 L2 U2 D2))
;;
;;	;; (mapcar #'(lambda (x) (move-on-edge-piece x 'UF)) forward-moves)
;;	;; (mapcar #'(lambda (x) (move-on-edge-piece x 'UF)) backward-moves)
;;	;; (mapcar #'(lambda (x) (move-on-edge-piece x 'UF)) double-moves)
;;
;;	;; (mapcar #'(lambda (x) (move-on-edge-piece x 'FU)) forward-moves)
;;	;; (mapcar #'(lambda (x) (move-on-edge-piece x 'FU)) backward-moves)
;;	;; (mapcar #'(lambda (x) (move-on-edge-piece x 'FU)) double-moves)
;;
;;	;; (mapcar #'(lambda (x) (move-on-corner-piece x 'UFR)) forward-moves)
;;	;; (mapcar #'(lambda (x) (move-on-corner-piece x 'UFR)) backward-moves)
;;	;; (mapcar #'(lambda (x) (move-on-corner-piece x 'UFR)) double-moves)
;;
;;	;; (cycle-decomposition '(B- U2 B2 U B- U- B- U2 F R B R- F-))
;;	;; (cycle-decomposition '(R L- F R- L D2 R L- F R- L))
;;	;; (cycle-decomposition (power-of-sequence '(B R- D2 R B- U2) 1))
;;	;; (cycle-decomposition (power-of-sequence '(B R- D2 R B- U2) 2))
;;	;; (cycle-decomposition (power-of-sequence '(F R) 2))
;;	;; (cycle-decomposition (power-of-sequence '(F2 R2) 6))
;;	;; (cycle-decomposition
;;	;;   '(R L- F  R- L D
;;	;;  	 R L- F  R- L D
;;	;;  	 R L- F2 R- L D
;;	;;  	 R L- F  R- L D
;;	;;  	 R L- F  R- L D2))
;;	;; (moves-on-pieces (power-of-sequence '(B R- D2 R B- U2) 2) table-list-of-all-pieces)
;;	;; (moves-on-pieces '(R L- F R- L D2 R L- F R- L) '(DR DB DL))
;;	;; (moves-on-pieces '(R L- F R- L D2 R L- F R- L) table-list-of-all-pieces)
;;	;; (moves-on-pieces (power-of-sequence '(B R- D2 R B- U2) 2) table-list-of-all-pieces)
;;	;; (pared-moves-on-all-pieces '(R L- F R- L D2 R L- F R- L))
;;	;; (pared-moves-on-all-pieces (power-of-sequence '(B R- D2 R B- U2) 2))
;;	;; (pared-moves-on-all-pieces (power-of-sequence '(F2 R2) 2))
;;	;;
;;	;; (order (power-of-sequence '(B R- D2 R B- U2) 2))
;;	;; (order '(R L- F R- L D2 R L- F R- L))
;;	;; (order '(R L- F R- L D2 R L- F R- L))
;;	;; (order (power-of-sequence '(B R- D2 R B- U2) 2))
;;	;; (order '(R L- F R- L D2 R L- F R- L))
;;	;; (order (power-of-sequence '(B R- D2 R B- U2) 2))
;;	;; (order (power-of-sequence '(F2 R2) 2))
;;	;; (order '(F R B L))
;;	;; (order '(F2 R2))
;;	;; (order '(F R))
;;	;; (order '(F R-))
;;	;; (order '(F R2))
;;	;; (order '(F U R))
;;	;; (order '(F U R B D L))
;;	;; (order '(F U R B L D))
;;	;; (order '(F))
;;	;;

;; (tell-about '(F))
;; (tell-about (power-of-sequence '(B R- D2 R B- U2) 2))
;; (tell-about '(R L- F R- L D2 R L- F R- L))
;; (tell-about (power-of-sequence '(F2 R2) 2))
;; (tell-about '(F R B L))
;; (tell-about '(F R B L D U))
;; (tell-about '(R L- F  R- L D
;; 	 R L- F  R- L D
;; 	 R L- F2 R- L D
;; 	 R L- F  R- L D
;; 	 R L- F  R- L D2))

;;(tell-about '())
;;(tell-about '(U))
;;(tell-about '(U U))
;;(tell-about '(U U U))
;;(tell-about '(U U U U))
;;(tell-about '(F R B L))
;;(tell-about '(F2 R2))
;;(tell-about '(F2 R2 B2 L2))
;;(tell-about '(F2 R2 F2 R2 F2 R2))
