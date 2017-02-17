;;;
;;; SIDEEL (SEL and SER)
;;; Adds civil engineering profile side elevations by picking.
;;;
;;; Program purpose:
;;; Provide the world's best vanilla (quick, simple, and polite)
;;; method of labeling AutoCAD civil engineering profile side elevations.
;;;
;;; Special program requirements:
;;; -You need profile point that is drawn somewhat accurately and labeled correctly.
;;; -Vertical exaggeration is assumed to be 10 as set in the sideel function.
;;; -(HAWS-MKLAYR layer_key_or_list) subroutine to make layer (included in HawsEDC edclib.lsp) or comment out (HAWS-MKLAYR) line.
;;; -(HAWS-MKTEXT j i h r s) subroutine to make a text entity.
;;; -(HAWS-ATOFX) subroutine to EXTRACT number from text.
;;;
;;; This is an open-source AutoLISP program by Thomas Gail Haws
;;;
;;; You are encouraged to modify this program
;;; subject to the following requirements:
;;;
;;; 1) You must leave this header intact
;;; 2) You must send a copy of your improvements
;;;    to:
;;;    Tom Haws (www.hawsedc.com)
;;;    
;;;    
;;;
(defun
	 se_getel	(/ en)
	(setq en (car (nentsel "\nSelect elevation text on profile: ")))
	(if	en
		(HAWS-ATOFX (cdr (assoc 1 (entget en))) "*" 1)
	) ;_ end of if
) ;_ end of defun
(defun
	 se_mktxt	(side el1 y1 pt1 / eli pti)
	(HAWS-MKLAYR '("p-elev" "m" ""))
	(setq
		eli	(+ el1 (/ (- (cadr pt1) (* ds vs) y1) hvexag));Elevation for first text
		pti	(polar pt1 (/ pi -2) (* ds vs));Insertion point for first text.
	) ;_ end of setq
	(repeat	3
		(HAWS-MKTEXT "BC" pti (* ds ts) 0 (rtos eli 2 0))
		(setq
			eli	(+ eli (/ (* vs ds) hvexag))
			pti	(polar pti (/ pi 2) (* vs ds))
		) ;_ end of setq
	) ;_ end of repeat
) ;_ end of defun
(defun
	 sideel	(side / ds el1 ho hvexag pt1 ts vs y1)
	(setq
	  hvexag 10                          ;Vertical exaggeration
	  ds (HAWS-DWGSCALE)             ;Dimscale
	  vs 0.5                             ;Vertical text spacing in plotted inches
	  ho 0.5                             ;Horizontal text offset from selected point in plotted inches
	  ts 0.2                             ;Text height in plotted inches 
 	  el1 (se_getel)
	)
	(if	el1
		(setq y1 (cadr (getpoint "\nCorresponding point: ")))
	) ;_ end of if
	(if	y1
		(setq
			pt1
			 (polar
				 (getpoint
					 (strcat "\n" (strcase (substr side 1 1)) (substr side 2) " point for middle label of three: ")
				 ) ;_ end of getpoint
				 (if (= side "Left")
					 0
					 pi
				 ) ;_ end of if
				 (* ds ho)                    ;Horizontal text offset from selected point.
			 ) ;_ end of polar
		) ;_ end of setq
	) ;_ end of if
	(se_mktxt side el1 y1 pt1)
) ;_ end of defun
(defun c:haws-sel () (sideel "Right"))
(defun c:haws-ser () (sideel "Left"))
