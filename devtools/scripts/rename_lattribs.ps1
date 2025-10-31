# PowerShell Script: Rename lattribs functions and variables
# Date: October 29, 2025
# Purpose: Implement renaming proposal for architectural clarity

param(
    [Parameter(Mandatory=$true)]
    [string]$FilePath,
    
    [switch]$DryRun = $false
)

Write-Host "=== CNM Lattribs Renaming Script ===" -ForegroundColor Cyan
Write-Host "File: $FilePath" -ForegroundColor Yellow
if ($DryRun) {
    Write-Host "MODE: DRY RUN (no changes will be made)" -ForegroundColor Green
} else {
    Write-Host "MODE: LIVE (file will be modified)" -ForegroundColor Red
}
Write-Host ""

# Verify file exists
if (-not (Test-Path $FilePath)) {
    Write-Error "File not found: $FilePath"
    exit 1
}

# Read file content
$content = Get-Content -Path $FilePath -Raw

# Replacement counter
$replacementCount = @{}

# Function to do replacement and track
function Replace-Text {
    param($oldText, $newText, $description)
    
    $matches = [regex]::Matches($script:content, [regex]::Escape($oldText))
    $count = $matches.Count
    
    if ($count -gt 0) {
        $script:content = $script:content -replace [regex]::Escape($oldText), $newText
        $script:replacementCount[$description] = $count
        Write-Host "OK $description : $count replacements" -ForegroundColor Green
    }
}

Write-Host "=== Category 1: Data Flow Functions ===" -ForegroundColor Cyan

# Longest first to avoid partial matches
Replace-Text "hcnm-ldrblk-ensure-fields-and-adjust-formats" "hcnm-ldrblk-lattribs-validate-and-underover" "validate-and-underover"
Replace-Text "hcnm-ldrblk-initialize-attribute-list" "hcnm-ldrblk-lattribs-spec" "lattribs-spec"
Replace-Text "hcnm-ldrblk-save-attribute-to-list" "hcnm-ldrblk-lattribs-put-element" "lattribs-put-element"
Replace-Text "hcnm-ldrblk-save-auto-to-list" "hcnm-ldrblk-lattribs-put-auto" "lattribs-put-auto"
Replace-Text "hcnm-ldrblk-ensure-fields" "hcnm-ldrblk-lattribs-validate" "lattribs-validate"
Replace-Text "hcnm-save-bubble-xdata" "hcnm-ldrblk-xdata-save" "xdata-save"
Replace-Text "hcnm-read-bubble-data" "hcnm-ldrblk-dwg-to-lattribs" "dwg-to-lattribs"
Replace-Text "hcnm-save-bubble" "hcnm-ldrblk-lattribs-to-dwg" "lattribs-to-dwg"

Write-Host ""
Write-Host "=== Category 3: Format to String Functions ===" -ForegroundColor Cyan

# Longest first
Replace-Text "hcnm-ldrblk-auto-alignment-format-station" "hcnm-ldrblk-auto-al-station-to-string" "al-station-to-string"
Replace-Text "hcnm-ldrblk-auto-alignment-format-offset" "hcnm-ldrblk-auto-al-offset-to-string" "al-offset-to-string"
Replace-Text "hcnm-ldrblk-auto-pipe-format-diameter" "hcnm-ldrblk-auto-pipe-dia-to-string" "pipe-dia-to-string"
Replace-Text "hcnm-ldrblk-auto-pipe-format-slope" "hcnm-ldrblk-auto-pipe-slope-to-string" "pipe-slope-to-string"
Replace-Text "hcnm-ldrblk-auto-pipe-format-length" "hcnm-ldrblk-auto-pipe-length-to-string" "pipe-length-to-string"
Replace-Text "hcnm-ldrblk-adjust-formats" "hcnm-ldrblk-lattribs-underover" "lattribs-underover"
Replace-Text "hcnm-ldrblk-adjust-format" "hcnm-ldrblk-string-underover" "string-underover"

Write-Host ""
Write-Host "=== Category 4: Variable Names ===" -ForegroundColor Cyan

# Variable names - longest first
Replace-Text "attribute-list" "lattribs" "attribute-list to lattribs"
Replace-Text "attribute_list" "lattribs" "attribute_list to lattribs"
Replace-Text "attributeList" "lattribs" "attributeList to lattribs"
Replace-Text "attList" "lattribs" "attList to lattribs"
Replace-Text "attlist" "lattribs" "attlist to lattribs"
Replace-Text "attribute-data" "lattribs" "attribute-data to lattribs"
Replace-Text "BDAT" "lattribs" "BDAT to lattribs"

Write-Host ""
Write-Host "=== Summary ===" -ForegroundColor Cyan
Write-Host "Total categories with changes: $($replacementCount.Count)"

$totalReplacements = 0
foreach ($val in $replacementCount.Values) {
    $totalReplacements += $val
}
Write-Host "Total replacements: $totalReplacements" -ForegroundColor Yellow

if ($replacementCount.Count -eq 0) {
    Write-Host "No changes needed!" -ForegroundColor Green
    exit 0
}

Write-Host ""
Write-Host "Detailed breakdown:" -ForegroundColor Cyan
foreach ($entry in $replacementCount.GetEnumerator() | Sort-Object Value -Descending) {
    Write-Host "  $($entry.Key): $($entry.Value)"
}

# Write output
if (-not $DryRun) {
    Write-Host ""
    Write-Host "Writing changes to file..." -ForegroundColor Yellow
    Set-Content -Path $FilePath -Value $content -NoNewline
    Write-Host "OK File updated successfully!" -ForegroundColor Green
} else {
    Write-Host ""
    Write-Host "DRY RUN: No changes written" -ForegroundColor Green
    Write-Host "Remove -DryRun flag to apply changes" -ForegroundColor Yellow
}

Write-Host ""
Write-Host "=== Done ===" -ForegroundColor Cyan
