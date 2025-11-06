# HawsEDC Developer Tools

**Not Implemented. Do not read.**

This directory contains developer-only tools for HawsEDC/CNM development.

## Setup

### Enable Developer Mode

1. Add this directory to AutoCAD's Support Files Search Path:
   - `Tools` → `Options` → `Files` → `Support File Search Path`
   - Add: `C:\TGHFiles\programming\hawsedc\develop\devtools`

2. Restart the drawing session or reload CNM with (load "cnmloader")

3. Verify developer mode is active:
   - Load CNM normally
   - You should see: `*** HawsEDC Developer Mode Active ***` [TGH: Where would this appear?]

### Developer Commands

Once enabled, you have access to:

- `HAWS-REBUILD-FAS` - Compile all .lsp files to .fas (faster loading and better run-time performance and memory)
- `HAWS-DISTRIB` - Package distribution files (planned) [TGH: I have a Windows batch file "C:\TGHFiles\programming\hawsedc\compile\distrib.bat" I have been using to copy all files to a distrib folder. There is a README.txt  (very old) "C:\TGHFiles\programming\hawsedc\compile\README.TXT" there to help with the setup compile steps.]

## Files

- `cnmdevloader.lsp` - Entry point loaded by cnmloader.lsp
- `haws_dev_rebuild_fas.lsp` - FAS compilation utility
- `haws_dev_distrib.lsp` - Distribution packaging (planned)

## Status

⚠️ **IN DEVELOPMENT** - Not all features implemented yet

See `HAWSEDC_DEVELOPER_GUIDE.md` section 2 for complete documentation.
