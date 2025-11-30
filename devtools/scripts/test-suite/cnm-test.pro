
     W E L C O M E   T O    H A W S E D C   P R O F I L E   D R A F T E R !

YOU HAVE OPENED A SAMPLE PROFILE DRAFTER DATA FILE.
BY READING THIS FILE, PROFILE DRAFTER DRAWS A CIVIL ENGINEERING PROFILE IN AUTOCAD.

THE COLUMNS IN THIS FILE ARE SEPARATED BY SPACES; THEY ARE 10 SPACES WIDE.

TRY CHANGING THE NUMBER AFTER THE WORD HVEXAG BELOW TO A 5.
THEN EXIT SAVING THIS FILE AND TYPE NEWPRO IN AUTOCAD.
WATCH THE VERTICAL EXAGGERATION CHANGE EFFORTLESSLY.

THEN COME BACK AND TRY CHANGING OTHER NUMBERS.
JUST BE SURE TO KEEP THE COLUMNS STRAIGHT.
HAVE FUN.

COLWID 10
HVEXAG    5
;PRECIS    2         2         2         3
-------------------------------------------------------------------------------
LEFT CURB
-------------------------------------------------------------------------------
*REFPNT   X DIR     Y DIR     STA       ELEV
-------------------------------------------------------------------------------
REFPNT    9825.00   20575.00  900.00    1302.00   
-------------------------------------------------------------------------------
*TITLE    HEIGHT    WALTHICK  TYPE                                 
PROTYP    6         0.00      P         ft
-------------------------------------------------------------------------------
*TITLE    STATION   OFFSET    ELEV      SLOPE     TRUE RIM  LABEL
PRFPNT    964.06    45.75     1301.95                       GB, CL CB
PRFPNT    975.41    -8.43     1302.10   0.25                BEG CURB TRANS
PROTYP    4         0.00                ft
PRFPNT    979.61    -11.15    1302.11   1000                END CURB TRANS
PRFPNT    1000                1302.17                       GC, PT
PRFPNT    1086.6              1302.94   0.889               GC
PRFPNT    1292.18             1303.70   0.37                GC
PRFPNT    1400                1304.29   0.548               MATCH SHEET PP02
-------------------------------------------------------------------------------
CENTER PAVEMENT
-------------------------------------------------------------------------------
NEWPRF
REFPNT    9825.00   20525.00  900.00    1302.00   
PROTYP    0         0.00                ft
PRFPNT    964.06    45.75     1301.95                       GB, CL CB
PRFPNT    1000      25.00     1302.82   2                   GC, BEG CROWN TRANS
PRFPNT    1086.6              1303.27   0.493               GC, END CROWN TRANS
PRFPNT    1255.68             1303.89   0.37                CL CAROL RAE LN
PRFPNT    1292.18             1304.03   0.37                GC
PRFPNT    1400                1304.62   0.548               MATCH SHEET PP02
-------------------------------------------------------------------------------
SEWER PROFILE
-------------------------------------------------------------------------------
NEWPRF
PROTYP    -12       -1        Sew      
PRFPNT    985                 1296.922            1302.40   MH, RIM=1300.1
PRFPNT    1251.001             1296.544      
NEWPRF
PRFPNT    1251                -1296.04            1303.80   MH, RIM=1303.8
NEWPRF
PRFPNT    1251.001            -1296.54
PRFPNT    1360                -1296.04  -0.33               MH, RIM=1304.5
PRFPNT    1400                1297.142  0.4       1305.12   CB, TC=1307.12
ADDITIONAL INVERT POINTS
NEWPRF
PROTYP    1000      0
PRFPNT    1251                1295.941  0                   8" INV=1296.94 N
PRFPNT    1350                1299.863  2                   INV EXIST 8" SEW=1299.86; DOESN'T PLOT AS EXIST
PRFPNT    1340                1300.863  2         8         
PROTYP    1000      0         X
PRFPNT    1200                1298.942  0                   8" SEWER
PRFPNT    1150                0.003                         8" SEWER, TOP=1295.94
-------------------------------------------------------------------------------
DIMENSIONS FOR SEWER PROFILE
-------------------------------------------------------------------------------
NEWPRF
PROTYP    -20000    0.00
PRFPNT    985                 1301.92
PRFPNT    1251                1301.04                       JOINT RESTRAINTS
NEWPRF
PRFPNT    1086.6              1306
PRFPNT    1255.68             1306                          TL=200.86'
NEWPRF
PRFPNT    1350                1299.7
PRFPNT    1350                1297.58                       MIN 1' CLEARANCE
-------------------------------------------------------------------------------
RIGHT CURB
-------------------------------------------------------------------------------
REFPNT    9825.00   20435.00  900.00    1304.00
NEWPRF
PROTYP    6         0.00      X
PRFPNT    964.06    45.75     1301.95                       GB, CL CB
PRFPNT    1033.19   49.92     1302.16   0.25                BEG CURB TRANS
PROTYP    4         0.00      X
PRFPNT    1035.94   45.75     1302.17   1000                GC, PRC, END CURB TRANS
PRFPNT    1086.6              1302.94   1.26                GC, PT
PRFPNT    1219.18             1303.43   0.37                BCR
PRFPNT    1239.181            1303.70   1000                ECR, MATCH SHEET PP11
NEWPRF
PROTYP    -10000    0.00      X
PRFPNT    1219.18             1303.43
PRFPNT    1292.18             1303.70
NEWPRF
PROTYP    4         0.00      X
PRFPNT    1272.181            1303.70                       BCR, MATCH SHEET PP11
PRFPNT    1292.18             1303.70   1000                GC, ECR
PRFPNT    1400                1304.29   0.548               MATCH SHEET PP02
