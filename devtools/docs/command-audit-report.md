# Command Audit Report
Generated 2026-02-25

Cross-referencing three files:
- **CSV:** `devsource/CNM-Command-Reference.csv` (user command reference)
- **cnmalias:** `devsource/cnmalias.lsp` (alias definitions mapping aliases to canonical names)
- **cnmloader:** `devsource/cnmloader.lsp` (lazy/autoloader for canonical command names)

## Counts
| Source | Count |
|---|---|
| CSV commands | 271 |
| cnmalias aliases | 389 (319 Tools + 39 Custompgp + 30 Standardpgp) |
| cnmloader canonicals | 305 |
| **Commands with gaps** | **124** |

Note: Custompgp/Standardpgp aliases are simple AutoCAD command remappings (not LISP commands), so they are NOT expected in cnmloader.lsp.

---

## 1. CSV section headers mistakenly counted as commands (7)
These are category/section rows in the spreadsheet, not real commands. Exclude from further analysis.

| CSV Entry | CSV Line |
|---|---|
| BLOCK MANAGEMENT | 25 |
| CIVIL DRAFTING | 45 |
| CONSTRUCTION NOTES | 52 |
| CUSTOMIZATION | 59 |
| DIMENSIONING | 66 |
| DRAWING | 70 |
| EDITING | 74 |
| HORIZONTAL CONTROL AND COORDINATE GEOMETRY | 93 |
| INQUIRY | 98 |
| LAYER MANAGEMENT | 117 |
| PROFILES | 181 |
| SETUP & DRAWING ENVIRONMENT | 203 |
| TEXT | 224 |
| VIEWS & ZOOMS | 251 |

---

## 2. CSV uses combined alias names that split across the other files (3)
The CSV documents these as single entries, but cnmalias and cnmloader have them as separate commands.

| CSV Entry | CSV Line | Separate entries in cnmalias/cnmloader |
|---|---|---|
| GB/GC | 275 | GB, GC | Fixed haws-getlayr and moved ini-edit load to haws-config. First invocation works. On second invocation, "No function definition: HNCM-PROJ"
| LE/LLL | 120 | LE, LLL | Fixed; removed LLL. lengthen.lsp:haws-lengthen. Why not haws-lengthen.lsp? lll is defined in c3d.mnl and civil.mnl along with these others: c:za, c:zc, c:zd, c:ze, c:zf, c:zi, c:zl, c:zm, c:zo, c:zp, c:zw, c:lll, c:xclipframetoggle.
| SECL/R/T/B | 281 | OK. SECL, SECR, SECT, SECB |

---

## 3. CSV-only commands — no cnmalias entry AND no cnmloader entry (4)
Documented in user reference but not implemented as LISP commands.

| Alias | CSV Line | Description |
|---|---|---|
| BURST | 29 | Clarified. Explode attributes to text (Express Tools command) |
| DD | 63 | Donut with no hole | Removed.
| HAWSALIAS | 277 | Fixed. Load aliases in HawsAlias.lsp after editing |
| VSR | 252 | Visretain toggle | Fixed.

---

## 4. CSV + cnmalias but NOT in cnmloader (10)
These have user documentation and alias definitions but the canonical function isn't autoloaded. May be bugs, or the canonical names may be included under different groupings in the loader.

| Alias | CSV Line | Canonical (from cnmalias) | Notes |
|---|---|---|---|
| AT | 19 | haws-at | Fixed. Arc text — CSV says "Obsolete (Express Tools)" |
| CEB | 273 | hcnm-edit-bubble | Removed. Edit bubble note. Loader has `hcnm-edit-bubbles` (plural) — name mismatch? |
| CLONE | 48 | haws-clone | Fixed. Copy to current layer — cnmalias message says "Try AutoCAD CopyToLayer (CNM alias CCC)" |
| EC | 73 | haws-ec | Removed. Erase Crossing — loader has `haws-bf` etc. in `edt` file, but not `haws-ec` |
| EW | 82 | haws-ew | Removed. Erase Window — same situation, not in `edt` autoload list |
| HAM | 276 | haws-aliasmanage | Fixed. HawsAlias Manage — loader has no `haws-aliasmanage` |
| LABEL | 278 | haws-label | Seems OK. MTEXT label — loader has `haws-label` in file `haws-label`, check if name matches |
| MC | 143 | haws-mc | Removed. Move Crossing — not in `edt` autoload list |
| MW | 153 | haws-mw | Removed. Move Window — not in `edt` autoload list |
| XX | 260 | haws-xx | Fixed. Toggle dimtix — loader has `haws-dimtixtoggle` in `dms`, alias maps to `haws-xx` |

### Diagnosis notes for category 4
- **EC, EW, MC, MW:** The `edt` autoload (cnmloader line 187) loads `haws-bf`, `haws-cb`, `haws-copy`, `haws-mp`, `haws-pj`, `haws-r1`, `haws-r2`, `haws-r4`, `haws-r9`, `haws-s`, `haws-ub`, `haws-um`, `haws-vb` — but omits `haws-ec`, `haws-ew`, `haws-mc`, `haws-mw`. Either these are defined in `edt.lsp` and missing from the autoload list, or they were removed.
- **XX:** cnmalias maps `xx` → `haws-xx`, but cnmloader autoloads `haws-dimtixtoggle` from `dms`. Likely a rename where the alias wasn't updated.
- **CEB:** cnmalias maps `ceb` → `hcnm-edit-bubble` (singular), cnmloader has `hcnm-edit-bubbles` (plural). Name mismatch.
- **LABEL:** cnmloader line 125 has `(haws-autoload "haws-label" '("haws-label"))`. The regex may have treated `haws-label` as both filename and command. This one is likely a false positive — it IS in the loader.
- **HAM:** cnmalias maps to `haws-aliasmanage`. No loader entry for this canonical name. May be defined inline or in misc.lsp under a different name.
All fixed.
---

## 5. In cnmalias (Tools group) but NOT in CSV (67)
These are functional commands with aliases but undocumented in the user command reference.

| Alias | Canonical | Notes |
|---|---|---|
| A2T | haws-a2t | | Added to ods
| ATT2TXT | haws-att2txt | | Added to ods.
| BUP | hcnm-bnatu | Bubble Note Auto Text Updater | To do uncertain.
| CAM | haws-aliasmanage | CNMAlias Manage | Fixed. Added to ods.
| CMB | hcnm-replace-bubble | Change/replace bubble shape | Added to ods.
| CNM | hcnm-cnm | Main CNM tables command | Added to ods.
| CNMCOPY | hcnm-copy-bubbles | Copy bubbles | To do uncertain.
| CNMMENU | haws-cnmmenu | Load CNM menu |
| CNMOP | hcnm-cnmoptions | CNM Options |
| CONTVOL | haws-contvol | Contour volume (CV alias IS in CSV) |
| CW | haws-cw | | Removed.
| DIMSTY | haws-dimsty | Dimension style |
| FFF | haws-ffx | Freeze nested/xref layers (FFX alias IS in CSV) |
| FUNKY | haws-funky | |
| GD | haws-geodata | Geodata (CD alias IS in CSV) |
| GOTO | haws-goto | |
| IMP_EXP | haws-imp_exp | Import/Export (IMP-EXP with hyphen IS in CSV) |
| IN | haws-xin | Xref in |
| INCATT | haws-incatt | Increment attributes |
| INVL | haws-INVL | Insert invert left |
| INVR | haws-INVR | Insert invert right |
| LE | haws-lengthen | Lengthen (LE/LLL combined IS in CSV) |
| LED | haws-led | Leader |
| LLL | haws-lengthen | Lengthen (LE/LLL combined IS in CSV) |
| MAKEPOLY | haws-makepoly | |
| MC2033 | haws-mc2033 | Mesa City standard drawing |
| MODESTAT | haws-modestat | Mode status |
| NEWSCALE | haws-newscale | |
| NOTESEDIT | hcnm-notesedit | Edit notes |
| NOTESEDITOR | hcnm-noteseditor | Notes editor |
| NOTESFILEIN | haws-notesfilein | Import notes from file |
| NTPURGE | haws-ntpurge | Purge notes |
| OCTL | haws-octl | Octagon bubble note |
| OFFSETX | haws-offsetx | Offset extended (OO alias IS in CSV) |
| OFI | haws-ofi | Off isolate (freeze all but selected) |
| OUT | haws-xout | Xref out |
| PHASEEDIT | haws-phaseedit | Edit phases |
| POLARSET | haws-polarset | Set polar tracking |
| PROCB | haws-procb | Profile catch basin |
| PROMH | haws-promh | Profile manhole |
| QT | haws-qt | Quick text |
| RECL | haws-recl | Rectangle bubble note |
| ROUND | haws-round | Round numbers in text |
| RRR | haws-rotatebase | Rotate from base (R alias IS in CSV) |
| RUN | haws-loadandrun | Load and run LISP (LR alias IS in CSV) |
| SETNOTEPHASES | haws-setnotephases | Set note phases |
| SETNOTESBUBBLESTYLE | hcnm-setnotesbubblestyle | Set bubble style for notes |
| SSTL | haws-sstl | Sunburst bubble note |
| TCELEV | haws-TCELEV | TC elevation |
| TRIL | haws-tril | Triangle bubble note |
| UF0 | haws-uf0 | UCS follow off |
| UF1 | haws-uf1 | UCS follow on |
| XIN | haws-xin | Xref in (same as IN) |
| XOUT | haws-xout | Xref out (same as OUT) |
| XRA | haws-xra | Xref attach |
| XRL | haws-xrl | Xref list |
| XRO | haws-xro | Xref overlay/offset |
| XROFFET | haws-xroffet | Xref offset |
| YI | haws-ofi | Layer isolate (same as OFI) |
| ZERO | haws-zero | |
| ZV | haws-zv | Zoom view |

### Notes on category 5
Many of these are alternate aliases for commands that DO appear in the CSV under a different name (noted above). The truly undocumented ones are the CNM-specific commands (BUP, CAM, CMB, CNM, CNMCOPY, CNMOP, NOTESEDIT, NOTESEDITOR, etc.) and various utility commands.

---

## 6. In cnmloader but NOT in cnmalias (17)
These are autoloaded but have no user-facing alias defined in cnmalias.lsp.

| Canonical | In CSV? | Notes |
|---|---|---|
| haws-bx | no | Box drawing — has Custompgp alias `BX` -> `Rectang` instead |
| haws-cnmsetup | no | CNM setup |
| haws-dimtixtoggle | no | Toggle dimtix — alias `XX` maps to `haws-xx` instead |
| haws-ib | no | Has Custompgp alias `IB` -> `Insbase` instead |
| haws-mbt | no | Mirrtext toggle? |
| haws-th | TH (line 225) | Change text height — CSV has it but cnmalias maps `TH` -> `Laythw` (Custompgp conflict!) |
| haws-w | no | Wblock — has Standardpgp alias `W` -> `Wblock` instead |
| hcnm-attnoplot | no | Set attributes to no-plot |
| hcnm-attplot | no | Set attributes to plot |
| hcnm-changeprojnotes | no | Change project notes |
| hcnm-cnmkt | no | CNM keynote |
| hcnm-cnmkti | no | CNM keynote insert? |
| hcnm-cnmlayer | no | CNM layer management |
| hcnm-cnmqt | no | CNM quick text? |
| hcnm-edit-bubbles | no | Edit bubbles (plural) — CEB alias maps to singular `hcnm-edit-bubble` |
| hcnm-layereditor | no | Layer editor |
| hcnm-linkproj | no | Link project |

### Key conflict: TH
- CSV line 225: `TH` = "Change text size for a selection set"
- cnmalias line 46: `TH` -> `Laythw` (Custompgp group — layer thaw)
- cnmloader line 145: `haws-th` autoloaded from `textsize`
- The Custompgp alias overrides the Tools command. This is likely intentional (cnmalias line 46 message: empty) but means the LISP `haws-th` command has no working alias.

---

## 7. In cnmloader but NOT in CSV (78)
Full list of canonical names autoloaded but not documented in the user reference. Many overlap with categories above.

| Canonical | Has cnmalias? |
|---|---|
| haws-a2t | YES |
| haws-acres | no (alias `ac` maps to `haws-acres` but CSV has AC -> different description) |
| haws-att2txt | YES |
| haws-bx | no |
| haws-cnmmenu | YES |
| haws-cnmsetup | no |
| haws-contelev | no |
| haws-contvol | YES |
| haws-copy | no |
| haws-copyrot | no |
| haws-copyrotdrag | no |
| haws-dimsty | YES |
| haws-dimtixtoggle | no |
| haws-funky | YES |
| haws-gb | YES |
| haws-gc | YES |
| haws-geodata | no |
| haws-goto | YES |
| haws-ib | no |
| haws-imp_exp | YES |
| haws-incatt | YES |
| haws-invl | YES |
| haws-invr | YES |
| haws-lengthen | no |
| haws-loadandrun | no |
| haws-mbt | no |
| haws-mc2033 | YES |
| haws-modestat | YES |
| haws-newscale | YES |
| haws-ntpurge | YES |
| haws-octl | YES |
| haws-offsetx | YES |
| haws-ofi | YES |
| haws-pgpedit | no |
| haws-phaseedit | YES |
| haws-polarset | YES |
| haws-procb | YES |
| haws-promh | YES |
| haws-recl | YES |
| haws-rotatebase | no |
| haws-round | YES |
| haws-secb | YES |
| haws-secl | YES |
| haws-secr | YES |
| haws-sect | YES |
| haws-setnotephases | YES |
| haws-sstl | YES |
| haws-tcelev | YES |
| haws-tcelevl | no |
| haws-tcelevr | no |
| haws-tril | YES |
| haws-twz | no |
| haws-uf0 | YES |
| haws-uf1 | YES |
| haws-w | no |
| haws-xin | YES |
| haws-xout | YES |
| haws-xra | YES |
| haws-xro | YES |
| haws-xroffet | YES |
| haws-zv | YES |
| hcnm-attnoplot | no |
| hcnm-attplot | no |
| hcnm-bnatu | no |
| hcnm-changeprojnotes | no |
| hcnm-cnm | YES |
| hcnm-cnmkt | no |
| hcnm-cnmkti | no |
| hcnm-cnmlayer | no |
| hcnm-cnmoptions | no |
| hcnm-cnmqt | no |
| hcnm-edit-bubbles | no |
| hcnm-layereditor | no |
| hcnm-linkproj | no |
| hcnm-notesedit | YES |
| hcnm-noteseditor | YES |
| hcnm-replace-bubble | no |
| hcnm-setnotesbubblestyle | YES |

# Changes to consult
Any of these could be additions at least temporarily with deprecation after a transition period.
## Bubble notes
- Insertion: BIB (Box), BIC (Cir), BID (Dia), BIE (Ell), BIH (Hex), BIO (Oct), BIP (Pen), BIR (Rec), BIS (SST), BIT (Tri)
- Editing: BEE (Edit), BEM/BEF (Mirror/Flip), BEC (Copy), BEU (Update), BEV (Vport)
- Offset: O (Offset), OO/OC (Current), OM (Multiple), OX (Xref)
