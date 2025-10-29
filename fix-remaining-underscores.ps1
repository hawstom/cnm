# Fix remaining underscore patterns that are partially converted
param(
    [Parameter(Mandatory=$true)]
    [string]$FilePath
)

$replacements = @(
    # These functions have mixed hyphen-underscore patterns now
    @{ Old = 'hcnm-ldrblk-bd_def'; New = 'hcnm-ldrblk-bd-def' }
    @{ Old = 'hcnm-ldrblk-bd_ensure_p1'; New = 'hcnm-ldrblk-bd-ensure-p1' }
    @{ Old = 'hcnm-ldrblk-bd_get'; New = 'hcnm-ldrblk-bd-get' }
    @{ Old = 'hcnm-ldrblk-bd_set'; New = 'hcnm-ldrblk-bd-set' }
    @{ Old = 'ename-bubble_'; New = 'ename-bubble-' }
    @{ Old = 'ename-leader_'; New = 'ename-leader-' }
)

if (-not (Test-Path $FilePath)) {
    Write-Error "File not found: $FilePath"
    exit 1
}

$content = Get-Content $FilePath -Raw
$changesMade = $false
$replacementCount = 0

foreach ($replacement in $replacements) {
    $oldText = $replacement.Old
    $newText = $replacement.New
    
    if ($content -match [regex]::Escape($oldText)) {
        $occurrences = ([regex]::Matches($content, [regex]::Escape($oldText))).Count
        $content = $content -replace [regex]::Escape($oldText), $newText
        Write-Host "Replaced '$oldText' with '$newText' ($occurrences occurrence(s))" -ForegroundColor Green
        $changesMade = $true
        $replacementCount += $occurrences
    } else {
        Write-Host "Not found: '$oldText'" -ForegroundColor Yellow
    }
}

if ($changesMade) {
    $backupPath = "$FilePath.backup2"
    Copy-Item $FilePath $backupPath -Force
    Write-Host "`nBackup created: $backupPath" -ForegroundColor Cyan
    
    Set-Content $FilePath $content -NoNewline
    Write-Host "File updated: $FilePath" -ForegroundColor Cyan
    Write-Host "Total replacements: $replacementCount" -ForegroundColor Cyan
} else {
    Write-Host "`nNo changes made." -ForegroundColor Yellow
}
