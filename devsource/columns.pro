                              PRO 5.01
                               2/16/99

                       Profile drawing program

                              Tom Haws

                              Appendix
                        Data file instructions

*This is a working sample column-delimited input file for PRO.
 You can read this file in by typing PRO Setup File Read and then
 selecting this file from the dialogue box and typing [Enter].

;PRO uses data lines that begin with certain upper-case
;title words.  It interprets all other lines as comments.
;The title words PRO uses are listed below:

-       COLWID (Column width)
-       REFPNT (Reference point)
-       PRECIS (Precision)
-       HVEXAG (Horizontal ö vertical exaggeration)
-       PROTYP (Profile type)
-       PRFPNT (Profile point)
-       NEWPRF (New profile)

>The COLWID line comes first.  It tells how the file is formatted.
>It gives the uniform data column width (tab width) if a file is
>in columns (column-delimited) or gives the delimiter character for a
>character-delimited file.
:Place the column width or delimiter character
:one blank space after the word COLWID.
:This file uses uniform 10-character columns, so COLWID is "10".
:The number 10 is placed one space after the word COLWID.

-------------------------------------------------------------------------------
COLWID 10
-------------------------------------------------------------------------------

>The REFPNT, HVEXAG, and PROTYP lines are optional setup lines which can be
done in Autocad.  If the file contains these lines, they will override any
setup done in Autocad.

>The REFPNT line anchors the profile to the AutoCAD coordinate system.
;The REFPNT line is optional if you set a reference point from the command line.
;The REFPNT line requires five fields and has two optional fields as shown below.
;1------>|2------->|3------->|4------->|5------->|6------->|7------->|
;TITLE    REFPT X   REFPT Y   REFPTSTA  REFPT EL  STA SCALE ELV SCALE
;The STA SCALE field can be used to reverse or scale a profile horizontally.
;The default scale is 1.
;The ELV SCALE field can be used to reverse or scale a profile vertically.
;The default scale is 1.
;Note: The ELV SCALE field and the HVEXAG line below are equivalent.
;You can use either method to adjust vertical scale.
;If you use both, they will multiply.
;The following line tells PRO to draw the profile in reverse
;horizontal direction using Autocad coordinate point 543.26, 22.43
;as profile Station 12+50, Elevation 101.

-------------------------------------------------------------------------------
REFPNT       543.26     22.43      1250       101         -1
-------------------------------------------------------------------------------

>The PRECIS line tells PRO how many decimal places to use for
;station, offset, elevation, and slope labels.
;The PRECIS line is always optional.
;The PRECIS line requires five fields as shown on the next line.
;1------>|2------->|3------->|4------->|5------->|
;TITLE    STPREC    OFPREC    ELPREC    SLPREC
;The following line tells PRO to draw the profile using the
;following precisions: Sta 10+00.00, 100.00' LT, TC=100.00, SLOPE=1.000%
;if you don't include a PRECIS line, PRO uses the AutoCAD LUPREC variable
;(the current units precision) for station, offset, and elevation and 3
;decimal places for slope.
-------------------------------------------------------------------------------
PRECIS    2         2         2         3
-------------------------------------------------------------------------------

>The HVEXAG line tells PRO how much to exaggerate the vertical scale.
;The HVEXAG line is always optional.
;The HVEXAG line requires two fields as shown on the next line.
;1------>|2------->|
;TITLE    HVEXAG (horizontal scale ö vertical scale)
;The following line tells PRO
;to exaggerate the vertical scale 10 times.
;If you don't include an HVEXAG line, PRO's default exaggeration is 10.

-------------------------------------------------------------------------------
HVEXAG           10
-------------------------------------------------------------------------------

>The PROTYP line tells PRO what kind of profile to draw next.
;The PROTYP line has 4 fields as shown on the next line
;1------>|2------->|3------->|4------->|5------->|
;TITLE    PROHT     -WALTHK   PROTYP    SLOPEFMT
;Field 1 is the word PROTYP.
;Field 2 tells the profile type and height or size.
    If it is 1000, PRO plots annotated utility crossing ellipses or
    points only (circles on defpoints layer). (See PROHT 1000 example.)
    If it is -10000, PRO plots a line only with no annotation.
    If it is -20000, PRO plots a labeled aligned dimension
    from point to point.
    If it is between 0 and 1000, PRO plots double-line curb and gutter.
    If it is 0, PRO plots a single-line pavement profile.
    If it is less than 0, PRO plots a pipe profile.
The value of field 2 tells the curb height or pipe size in inches.
;Field 3 is the negative wall thickness of a pipe in inches.
;Use zero or blank if no wall thickness or no pipe.
;Field 4 is the profile type for labeling and layers.
;You must use PROTYP to give the pipe type if PROHT is negative (a pipe profile)
;The pipe type must be WAT, SEW, SD, IRR, or GAS.
;You can use PROTYP to change the elevation label from G (gutter) or P
;(pavement) to whatever you want if PROHT is positive (pavement or curb profile)
;You can use PROTYP to tell PRO to draw a City of Mesa type existing curb
;profile if PROHT is positive by entering Mesa as the PROTYP.
;You can make any profile existing by prefixing the PROTYP with an X.
;(An X as the first letter of the field makes the profile existing; eg. XSD.)
;The following line tells PRO to draw a 6 inch curb & gutter profile.
;Field 5 can be used to make slopes display as ft/ft instead of percent.
;Put FT, ft, m, or anything else in Field 5 to tell PRO to label slopes
;as FT/FT, ft/ft, m/m, etc.
-------------------------------------------------------------------------------
PROTYP            6         0
-------------------------------------------------------------------------------
;The following line tells PRO to draw a single line ZQZ profile.
;(Elevations will say ZQZ=XX.XX)
-------------------------------------------------------------------------------
PROTYP            0         0 ZQZ
-------------------------------------------------------------------------------
;The following line tells PRO to draw a 48 inch exist SD pipe profile
;with 3.5 inch thick walls
-------------------------------------------------------------------------------
PROTYP          -48      -3.5 XSD
-------------------------------------------------------------------------------
;The following line tells PRO to draw a 6 inch curb profile
;with 45 degree elevation labels a la City of Mesa.
-------------------------------------------------------------------------------
PROTYP            6         0 Mesa
-------------------------------------------------------------------------------

>The PRFPNT line tells PRO to draw a profile point and connect it to
;the previous point.
;The PRFPNT line has the following 7 fields:
;1------>|2------->|3------->|4------->|5------->|6------->|7------->
*TITLE    STATION   OFFSET    ELEV1     %SLOPE    NUM2      LABEL(OPT.)

*Field 2, STATION, is always required.
*If 0.001 is added to the station, PRO doesn't label the station.

*Field 3, OFFSET, is a labeling option.  PRO reads the offset as a
*real number (plus for right offsets and minus for left offsets).  The
*program plots the station and offset as "STA XX+XX.XX, XX.XX' LT or RT."

*Field 4, ELEV1, is required except for crossing ellipses.
*If an elevation is given as negative, PRO doesn't label the elevation.
*Elevation information in ellipse labels can override field 3.

*Field 5, SLOPE, is the slope up from the previous point.
*If no slope is given, PRO will supply it as
*  slope=(elev-prevelev)/(sta-prevsta)*100.0
*If the slope is given as 220.1 (as in MAG 220A), PRO will supply it as
*  slope=(elev-prevelev-0.083)/(sta-prevsta-1.5)*100.0.
*  This calculates the true pavement slope between a pavement point
*  and a MAG 220 type A gutter point plotted 1.5 ft from lip of gutter.
*If the slope is given as 1000, PRO doesn't label the slope.
*For pipe crossing ellipses, the positive wall thickness in inches goes
*in this, the slope column.

*Field 6, NUM2, is optional.  It gives the second number required to plot
*some objects (manhole rim, catch basin top of curb, etc.) when the required
*number is not given in the label.  If NUM2 is present on a pipe
*profile point and the label for the point calls for a MH, CB, MANHOLE, INLET,
*or CATCH BASIN, PRO plots the requested object.  If the NUM2 elevation
*given conflicts with an elevation given in the label, PRO plots a broken
*manhole or catch basin with top at the NUM2 elevation.
*For pipe crossing ellipses NUM2 can give the pipe diameter in inches if you
*need to omit it from the label.

*Field 7, LABEL, is an optional label for PRO to plot at the point.
*The length of the LABEL can be longer than the normal column width.
*If LABEL is empty, PRO doesn't plot a label.
*For ellipses, PRO extracts plotting information from the label
*(see PROHT 1000 example).
*For dimensions (PROHT -20000), PRO puts the label on the dimension.

*These are profile points.

 TITLE    STATION   OFFSET    ELEV1     %SLOPE    NUM2      LABEL
          (+0.001 to (label   (minus to  (1000 to (True rim (can give MH rim
           suppress)  only)    suppress) suppress)elevation  info or ellipse
                             (overruled  OR WALL  or ellipse plotting info)
                                 for    THICKNESS diameter)
                              ellipses) (ellipses)
----------------------------------------------------------------------------------
PRFPNT         1000      24.5    101.56                     GB
PRFPNT         1200      24.5    101.65     0.365           MATCH SHEET 6
PRFPNT      1250.36      24.5    102.01     1.256           HELLO DOLLY
PRFPNT      1300.011            -101.99      1000
PRFPNT      1400.01              103.99
PRFPNT      1600.01    -0.234    104.99    -0.234
PRFPNT      1800.01     0.345    103.99     0.345
PRFPNT      2000.01     0.345    106.99     0.345
---------------------------------------------------------------------

 PROHT 1000 EXAMPLE:  Examples of PRFPNTs for ellipses

-The PROTYP line below tells PRO to plot either annotated ellipses
 (existing) or simple circles.  Field 3, WALTHK, normally 0, can increase
 the length of ellipse label leaders.

-The 1st PRFPNT line plots a circle.
-The 2nd PRFPNT line plots an exist 36" pipe ellipse with 3" wall thickness
 and the label 'EX 36" STORM DRAIN, TOP=78.24'.  PRO ignores field 4.
-The 3rd PRFPNT line plots an exist 72" pipe ellipse with 6" wall thickness
 and the label '72 INCH SD, BOTTOM=32.65'.  PRO ignores field 4.
-The 4th PRFPNT line plots an exist 18" pipe ellipse with 2" wall thickness
 and the label 'EXIST 18 IN WATER, INV=78.24'.  PRO ignores field 4.
-The 5th PRFPNT line plots an exist 2" thin wall pipe ellipse
 and the label 'TEL.'  PRO reads the elevation from field 4.

 For ellipses, PRO gets pipe size and positioning (TOP, BOTTOM, or
 INVERT) information from the label.  PRO reads the elevation from
 the elevation column (Field 4) only if no elevation information is given in
 the label (as in the fifth sample line).  For the 2" TEL, the pipe INVERT
 will be plotted at the elevation given in field 4.

 PRO looks for the following key word fragments in the label:
      " or IN          after the size in inches (required),
      TOP BOT or INV   before the elevation.

;1------>|2------->|3------->|4------->|
;TITLE    PROHT    WALTHK    PROTYP
-------------------------------------------------------------------------------
PROTYP         1000           X

;1------>|2------->|3------->|4------->|5------->|6------->7----------------------->
*TITLE    STATION   OFFSET    ELEVATION %SLOPE    NUM2     LABEL(OPT.)
PRFPNT    1206.58   1         78.24
PRFPNT    1216.58   2         100.002   3                  EX 36" STORM DRAIN TOP=78.24
PRFPNT    1226.58   3         90        6                  72 INCH SD, BOTTOM=32.65
PRFPNT    1236.58   4                   2                  EXIST 36 IN WATER, INV=79.24
PRFPNT    1246.58   5         78.24               2        TEL
--------------------------------------------------------------------

;The NEWPRF line tells PRO not to connect the next PRFPNT
;to the last one.  A new profile is starting.

-------------------------------------------------------------------------------
NEWPRF
-------------------------------------------------------------------------------
