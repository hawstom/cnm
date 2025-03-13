(defun c:haws-dimsty ()
(haws-core-init 14)
  (defun dimVSET (varlst / dimsty dsexist val var)
    (foreach var varlst
      (setq val (cadr var) var (car var))
      (cond
        ( (= var "DIMSTYLE")(setq dimsty val))
        ( (wcmatch var "*BLK*")
          (cond
            ( (= val "")
              (VL-CMDF  "dim1" var ".")
            )
            ( T
              (cond
                ( (not (tblsearch "BLOCK" val))
                  (cond
                    ( (findfile (strcat val ".dwg"))
                      (VL-CMDF  "._insert" val)(VL-CMDF )
                    )
                    (T (setq val ""))
                  )
                )
              )
              (vl-cmdf "._dim1" var val)
            )
          )
        )
        ( (getvar var)(setvar var val))
      )
    )
    (if (tblsearch "dimstyle" dimsty)(setq dsexist T)(setq dsexist nil))
    (vl-cmdf "dim1" "save" dimsty)
    (if dsexist (vl-cmdf "y"))
    (princ)
  )
  (prompt"Use SETUP and ROMANS before DIMSTY to set up sizes and text style as needed.  Use DDIM after DIMSTY to select dimstyles.")
  ;Street dimensions
  (dimVSET
    '(
      ("DIMALT"  0);alternate units toggle
      ("DIMALTD" 2);alt. decimals
      ("DIMALTF" 25.40);alt. DIMLFAC
      ("DIMALTTZ"0);tolerances DIMZIN
      ("DIMALTU" 2);alternate DIMUNITS
      ("DIMALTZ" 0);alternate DIMZIN
      ("DIMAPOST""")
      ("DIMASO"  1)
      ("DIMATFIT"3)
      ("DIMBLK"  "")
      ("DIMBLK1" "")
      ("DIMBLK2" "")
      ("DIMCLRD" 0)
      ("DIMCLRE" 0)
      ("DIMCLRT" 0)
      ("DIMDLE"  0.00)
      ("DIMFIT"  3)
      ("DIMJUST" 0)
      ("DIMLDRBLK" "")
      ("DIMLFAC" 1.00)
      ("DIMLIM"  0)
      ("DIMLUNIT" 2)
      ("DIMLWD"  -2)
      ("DIMLWE"  -2)
      ("DIMPOST" "'")
      ("DIMRND"  0.00)
      ("DIMSAH"  0)
      ("DIMSD1"  0)
      ("DIMSD2"  0)
      ("DIMSE1"  1)
      ("DIMSE2"  1)
      ("DIMSHO"  1)
      ("DIMSOXD" 0)
      ("DIMSTYLE""STREET")
      ("DIMTAD"  0)
      ("DIMTDEC" 2);tolerance decimals
      ("DIMTFAC" 1.00)
      ("DIMTIH"  0)
      ("DIMTIX"  1)
      ("DIMTM"   0.00)
      ("DIMTmoVE" 0)
      ("DIMTOFL" 0)
      ("DIMTOH"  0)
      ("DIMTOL"  0)
      ("DIMTOLJ" 1);tolerance vertical justification
      ("DIMTP"   0.00)
      ("DIMTSZ"  0.00)
      ("DIMTVP"  1.00)
      ("DIMUPT"  0);dimension line follows user positioned text
      ("DIMZIN"  0)
    )
  )
  ;Lot dimensions, suppress extension 2
  (dimVSET
    '(
      ("DIMALT"  0);alternate units toggle
      ("DIMALTD" 2);alt. decimals
      ("DIMALTF" 25.40);alt. DIMLFAC
      ("DIMALTTZ"0);tolerances DIMZIN
      ("DIMALTU" 2);alternate DIMUNITS
      ("DIMALTZ" 0);alternate DIMZIN
      ("DIMAPOST""")
      ("DIMASO"  1)
      ("DIMATFIT"3)
      ("DIMBLK"  "")
      ("DIMBLK1" "")
      ("DIMBLK2" "")
      ("DIMCLRD" 0)
      ("DIMCLRE" 0)
      ("DIMCLRT" 0)
      ("DIMDLE"  0.00)
      ("DIMFIT"  3)
      ("DIMJUST" 0)
      ("DIMLDRBLK" "")
      ("DIMLFAC" 1.00)
      ("DIMLIM"  0)
      ("DIMLUNIT" 2)
      ("DIMLWD"  -2)
      ("DIMLWE"  -2)
      ("DIMPOST" "'")
      ("DIMRND"  0.00)
      ("DIMSAH"  0)
      ("DIMSD1"  0)
      ("DIMSD2"  0)
      ("DIMSE1"  0)
      ("DIMSE2"  1)
      ("DIMSHO"  1)
      ("DIMSOXD" 0)
      ("DIMSTYLE""LOT")
      ("DIMTAD"  0)
      ("DIMTDEC" 2);tolerance decimals
      ("DIMTFAC" 1.00)
      ("DIMTIH"  0)
      ("DIMTIX"  0)
      ("DIMTM"   0.00)
      ("DIMTmoVE" 0)
      ("DIMTOFL" 0)
      ("DIMTOH"  0)
      ("DIMTOL"  0)
      ("DIMTOLJ" 1);tolerance vertical justification
      ("DIMTP"   0.00)
      ("DIMTSZ"  0.00)
      ("DIMTVP"  1.00)
      ("DIMUPT"  0);dimension line follows user positioned text
      ("DIMZIN"  0)
    )
  )
  ;Profile dimensions
  (dimVSET
    '(
      ("DIMALT"  0);alternate units toggle
      ("DIMALTD" 2);alt. decimals
      ("DIMALTF" 25.40);alt. DIMLFAC
      ("DIMALTTZ"0);tolerances DIMZIN
      ("DIMALTU" 2);alternate DIMUNITS
      ("DIMALTZ" 0);alternate DIMZIN
      ("DIMAPOST""")
      ("DIMASO"  1)
      ("DIMATFIT"3)
      ("DIMBLK"  "")
      ("DIMBLK1" "")
      ("DIMBLK2" "")
      ("DIMCLRD" 0)
      ("DIMCLRE" 0)
      ("DIMCLRT" 0)
      ("DIMDLE"  0.00)
      ("DIMFIT"  3)
      ("DIMJUST" 0)
      ("DIMLDRBLK" "")
      ("DIMLFAC" 1.00)
      ("DIMLIM"  0)
      ("DIMLUNIT" 2)
      ("DIMLWD"  -2)
      ("DIMLWE"  -2)
      ("DIMPOST" "'")
      ("DIMRND"  0.00)
      ("DIMSAH"  0)
      ("DIMSD1"  0)
      ("DIMSD2"  0)
      ("DIMSE1"  1)
      ("DIMSE2"  1)
      ("DIMSHO"  1)
      ("DIMSOXD" 0)
      ("DIMSTYLE""PROFILE")
      ("DIMTAD"  0)
      ("DIMTDEC" 2);tolerance decimals
      ("DIMTFAC" 1.00)
      ("DIMTIH"  1)
      ("DIMTIX"  0)
      ("DIMTM"   0.00)
      ("DIMTmoVE" 0)
      ("DIMTOFL" 0)
      ("DIMTOH"  0)
      ("DIMTOL"  0)
      ("DIMTOLJ" 1);tolerance vertical justification
      ("DIMTP"   0.00)
      ("DIMTSZ"  0.00)
      ("DIMTVP"  1.00)
      ("DIMUPT"  0);dimension line follows user positioned text
      ("DIMZIN"  0)
    )
  )
  ;Standard dimension style
  (dimVSET
    '(
      ("DIMALT"  0);alternate units toggle
      ("DIMALTD" 2);alt. decimals
      ("DIMALTF" 25.40);alt. DIMLFAC
      ("DIMALTTZ"0);tolerances DIMZIN
      ("DIMALTU" 2);alternate DIMUNITS
      ("DIMALTZ" 0);alternate DIMZIN
      ("DIMAPOST""")
      ("DIMASO"  1)
      ("DIMATFIT"3)
      ("DIMBLK"  "")
      ("DIMBLK1" "")
      ("DIMBLK2" "")
      ("DIMCLRD" 0)
      ("DIMCLRE" 0)
      ("DIMCLRT" 0)
      ("DIMDLE"  0.00)
      ("DIMFIT"  3)
      ("DIMJUST" 0)
      ("DIMLDRBLK" "")
      ("DIMLFAC" 1.00)
      ("DIMLIM"  0)
      ("DIMLUNIT" 2)
      ("DIMLWD"  -2)
      ("DIMLWE"  -2)
      ("DIMPOST" "'")
      ("DIMRND"  0.00);round dimensions to nearest multiple of this amount
      ("DIMSAH"  0);use separate user defined arrow-head blocks
      ("DIMSD1"  0)
      ("DIMSD2"  0)
      ("DIMSE1"  0)
      ("DIMSE2"  0)
      ("DIMSHO"  1)
      ("DIMSOXD" 0)
      ("DIMSTYLE""STANDARD")
      ("DIMTAD"  0)
      ("DIMTDEC" 2);tolerance decimals
      ("DIMTFAC" 1.00)
      ("DIMTIH"  0)
      ("DIMTIX"  0)
      ("DIMTM"   0.00)
      ("DIMTmoVE" 0)
      ("DIMTOFL" 0)
      ("DIMTOH"  0)
      ("DIMTOL"  0)
      ("DIMTOLJ" 1);tolerance vertical justification
      ("DIMTP"   0.00)
      ("DIMTSZ"  0.00)
      ("DIMTVP"  1.00)
      ("DIMUPT"  0);dimension line follows user positioned text
      ("DIMZIN"  0)
    )
  )
)
