# Rename hcnm-ldrblk-* to hcnm-bubbles-* throughout codebase
# Safe to run: No external dependencies on these function names
# 
# Author: TGH with AI assistance
# Date: 2025-10-31
# Purpose: Improve code readability by using domain language ("bubbles")

$ErrorActionPreference = "Stop"

# File to process
$filePath = "C:\TGHFiles\programming\hawsedc\develop\devsource\cnm.lsp"

Write-Host "CNM Function Renaming Script" -ForegroundColor Cyan
Write-Host "=============================" -ForegroundColor Cyan
Write-Host ""
Write-Host "Renaming: hcnm-ldrblk-* → hcnm-bubbles-*" -ForegroundColor Yellow
Write-Host "File: $filePath" -ForegroundColor Gray
Write-Host ""

# Check file exists
if (-not (Test-Path $filePath)) {
    Write-Host "ERROR: File not found: $filePath" -ForegroundColor Red
    exit 1
}

# Create backup
$backupPath = "$filePath.bak-rename-ldrblk-$(Get-Date -Format 'yyyyMMdd-HHmmss')"
Write-Host "Creating backup: $backupPath" -ForegroundColor Gray
Copy-Item $filePath $backupPath

# Read file content
Write-Host "Reading file..." -ForegroundColor Gray
$content = Get-Content $filePath -Raw -Encoding UTF8

# Count occurrences before
$countBefore = ([regex]::Matches($content, "hcnm-ldrblk-")).Count
Write-Host "Found $countBefore occurrences of 'hcnm-ldrblk-'" -ForegroundColor Cyan

# Perform replacement
Write-Host "Performing replacement..." -ForegroundColor Gray
$newContent = $content -replace "hcnm-ldrblk-", "hcnm-bubbles-"

# Count occurrences after
$countAfter = ([regex]::Matches($newContent, "hcnm-bubbles-")).Count
$countRemaining = ([regex]::Matches($newContent, "hcnm-ldrblk-")).Count

# Write updated content
Write-Host "Writing updated file..." -ForegroundColor Gray
Set-Content -Path $filePath -Value $newContent -Encoding UTF8 -NoNewline

# Report results
Write-Host ""
Write-Host "Replacement complete!" -ForegroundColor Green
Write-Host "  Renamed: $countBefore occurrences" -ForegroundColor Cyan
Write-Host "  Now: $countAfter occurrences of 'hcnm-bubbles-'" -ForegroundColor Cyan
Write-Host "  Remaining 'hcnm-ldrblk-': $countRemaining" -ForegroundColor $(if ($countRemaining -eq 0) { "Green" } else { "Yellow" })
Write-Host ""
Write-Host "Backup saved: $backupPath" -ForegroundColor Gray
Write-Host ""
Write-Host "Next steps:" -ForegroundColor Yellow
Write-Host "  1. Review changes in VS Code" -ForegroundColor White
Write-Host "  2. Load cnm.lsp in AutoCAD to check for errors" -ForegroundColor White
Write-Host "  3. Run (c:pretest) to clean up before testing" -ForegroundColor White
Write-Host "  4. Test bubble insertion and editing" -ForegroundColor White
Write-Host "  5. If OK: git add + commit" -ForegroundColor White
Write-Host "  6. If issues: restore from backup" -ForegroundColor White
Write-Host ""
