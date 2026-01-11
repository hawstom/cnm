# AutoLISP Parenthesis and Indentation Checker
# Validates parenthesis balance and indentation consistency in AutoLISP files
# Usage: powershell -File haws-lisp-paren-check.ps1 <filepath> [--check-precedent] [--check-level-zero]
# Default: runs both checks if no flags specified

param(
    [Parameter(Mandatory=$true, Position=0)]
    [string]$FilePath,
    [switch]$CheckPrecedent,
    [switch]$CheckLevelZero
)

# If no checks specified, run both
if (-not $CheckPrecedent -and -not $CheckLevelZero) {
    $CheckPrecedent = $true
    $CheckLevelZero = $true
}

$lines = Get-Content $FilePath

# ============================================================================
# CHECK 1: Same-Level Precedent (indent consistency at same nesting level)
# ============================================================================
if ($CheckPrecedent) {
    $nesting = 0
    $inString = $false
    $escapeNext = $false
    $precedentIndentAtLevel = @{}
    $inconsistencies = @()

    for ($i = 0; $i -lt $lines.Length; $i++) {
        $line = $lines[$i]
        $lineNum = $i + 1
        $lineIndent = ($line -replace '^( *)[^ ].*$', '$1').Length
        
        # Track if line starts with '('
        $lineStartsWithParen = ($line -match '^\s*\(')
        
        # Check BEFORE incrementing: first opening paren on line should match precedent at this level
        $firstParenOnLine = $true
        
        for ($j = 0; $j -lt $line.Length; $j++) {
            $char = $line[$j]
            
            if ($escapeNext) {
                $escapeNext = $false
                continue
            }
            
            if ($char -eq '\') {
                $escapeNext = $true
                continue
            }
            
            if ($char -eq '"') {
                $inString = -not $inString
                continue
            }
            
            if (-not $inString) {
                if ($char -eq ';') { break }
                
                if ($char -eq '(') {
                    # Check indent consistency BEFORE incrementing (at current level before opening)
                    if ($lineStartsWithParen -and $firstParenOnLine) {
                        if ($precedentIndentAtLevel.ContainsKey($nesting)) {
                            $precedentIndent = $precedentIndentAtLevel[$nesting]
                            if ($lineIndent -ne $precedentIndent) {
                                $inconsistencies += [PSCustomObject]@{
                                    LineNum = $lineNum
                                    Level = $nesting
                                    CurrentIndent = $lineIndent
                                    PrecedentIndent = $precedentIndent
                                    Content = $line
                                }
                            }
                        } else {
                            # Set precedent for this level
                            $precedentIndentAtLevel[$nesting] = $lineIndent
                        }
                        $firstParenOnLine = $false
                    }
                    
                    $nesting++
                }
                
                if ($char -eq ')') {
                    $nesting--
                    
                    # Clear precedents for DEEPER levels only (levels > current)
                    $keysToRemove = @()
                    foreach ($key in $precedentIndentAtLevel.Keys) {
                        if ($key -gt $nesting) {
                            $keysToRemove += $key
                        }
                    }
                    foreach ($key in $keysToRemove) {
                        $precedentIndentAtLevel.Remove($key)
                    }
                }
            }
        }
    }

    Write-Host "==================================================================================" -ForegroundColor Cyan
    Write-Host "CHECK 1: Same-Level Precedent (indent consistency)" -ForegroundColor Cyan
    Write-Host "==================================================================================" -ForegroundColor Cyan
    Write-Host ""
    
    if ($inconsistencies.Count -gt 0) {
        Write-Host "Found: $($inconsistencies.Count) issues" -ForegroundColor Yellow
        Write-Host ""
        foreach ($item in $inconsistencies) {
            Write-Host "Line $($item.LineNum) : Level $($item.Level) indent $($item.CurrentIndent) (precedent was $($item.PrecedentIndent))" -ForegroundColor Red
            Write-Host "$($item.Content)"
            Write-Host ""
        }
    } else {
        Write-Host "No issues found - all same-level opening parens have consistent indentation." -ForegroundColor Green
        Write-Host ""
    }
}

# ============================================================================
# CHECK 2: Level Zero / Indent Zero Consistency
# ============================================================================
if ($CheckLevelZero) {
    $nesting = 0
    $inString = $false
    $inBlockComment = $false
    $escapeNext = $false
    $topLevelBadIndent = @()
    $indentZeroBadLevel = @()
    $topLevelFormCount = 0
    $unmatchedClosingParens = @()
    $unclosedParenStack = @()
    $lastReportedLevel = $null
    $lastReportedLine = $null

    for ($i = 0; $i -lt $lines.Length; $i++) {
        $line = $lines[$i]
        $lineNum = $i + 1
        $lineIndent = ($line -replace '^( *)[^ ].*$', '$1').Length
        
        # Track if line starts with '('
        $lineStartsWithParen = ($line -match '^\s*\(')
        
        # CHECK AT START OF LINE: If line starts with paren at indent 0 but we're not at nesting level 0
        if ($lineStartsWithParen -and $lineIndent -eq 0 -and $nesting -ne 0) {
            # Only report if nesting level changed from last report
            if ($lastReportedLevel -eq $null -or $lastReportedLevel -ne $nesting) {
                $causedBy = $unclosedParenStack | Select-Object -Last 1
                $prevLevelInfo = if ($lastReportedLevel -ne $null) { " (was level $lastReportedLevel at line $lastReportedLine)" } else { "" }
                $indentZeroBadLevel += [PSCustomObject]@{
                    LineNum = $lineNum
                    Level = $nesting
                    ParenType = "opening"
                    Content = $line
                    CausedByLine = if ($causedBy) { $causedBy.LineNum } else { "unknown" }
                    PrevLevel = $prevLevelInfo
                }
                $lastReportedLevel = $nesting
                $lastReportedLine = $lineNum
            }
        }
        
        for ($j = 0; $j -lt $line.Length; $j++) {
            $char = $line[$j]
            $nextChar = if ($j + 1 -lt $line.Length) { $line[$j + 1] } else { $null }
            $charIndent = $j
            
            if ($escapeNext) {
                $escapeNext = $false
                continue
            }
            
            if ($char -eq '\') {
                $escapeNext = $true
                continue
            }
            
            if ($char -eq '"' -and -not $inBlockComment) {
                $inString = -not $inString
                continue
            }
            
            if (-not $inString) {
                # Check for block comment start ;|
                if ($char -eq ';' -and $nextChar -eq '|') {
                    $inBlockComment = $true
                    $j++
                    continue
                }
                
                # Check for block comment end |;
                if ($char -eq '|' -and $nextChar -eq ';') {
                    $inBlockComment = $false
                    $j++
                    continue
                }
                
                if ($inBlockComment) {
                    continue
                }
                
                # Single-line comment
                if ($char -eq ';') { break }
                
                if ($char -eq '(') {
                    # Count top-level forms
                    if ($nesting -eq 0) {
                        $topLevelFormCount++
                    }
                    
                    # Check #1: Top-level (level 0) opening paren NOT at indent 0
                    if ($nesting -eq 0 -and $lineIndent -ne 0 -and $lineStartsWithParen) {
                        $topLevelBadIndent += [PSCustomObject]@{
                            LineNum = $lineNum
                            Indent = $lineIndent
                            Content = $line
                        }
                    }
                    
                    # Track this opening paren
                    $unclosedParenStack += [PSCustomObject]@{
                        LineNum = $lineNum
                        Level = $nesting
                    }
                    
                    $nesting++
                }
                
                if ($char -eq ')') {
                    # Check for unmatched closing paren
                    if ($nesting -eq 0) {
                        $unmatchedClosingParens += [PSCustomObject]@{
                            LineNum = $lineNum
                            Content = $line
                        }
                    } else {
                        $nesting--
                        
                        # Pop from stack
                        if ($unclosedParenStack.Count -gt 0) {
                            $unclosedParenStack = $unclosedParenStack[0..($unclosedParenStack.Count - 2)]
                        }
                    }
                }
            }
        }
    }

    Write-Host "==================================================================================" -ForegroundColor Cyan
    Write-Host "CHECK 2: Level Zero / Indent Zero Consistency" -ForegroundColor Cyan
    Write-Host "==================================================================================" -ForegroundColor Cyan
    Write-Host ""
    Write-Host "Total lines scanned: $($lines.Length)"
    Write-Host "Final nesting level: $nesting"
    Write-Host ""
    
    # Report CHECK 2a: Top-level parens not at indent 0
    Write-Host "CHECK 2a: Top-level (level 0) opening parens NOT at indent 0" -ForegroundColor Yellow
    Write-Host "Found: $($topLevelBadIndent.Count) issues"
    Write-Host ""
    
    if ($topLevelBadIndent.Count -gt 0) {
        foreach ($item in $topLevelBadIndent) {
            Write-Host "Line $($item.LineNum) : Indent $($item.Indent)" -ForegroundColor Red
            Write-Host "$($item.Content)"
            Write-Host ""
        }
    } else {
        Write-Host "No issues found - all top-level opening parens are at indent 0." -ForegroundColor Green
        Write-Host ""
    }
    
    # Report CHECK 2b: Indent 0 parens not at level 0
    Write-Host "CHECK 2b: Indent 0 parens NOT at nesting level 0" -ForegroundColor Yellow
    Write-Host "Found: $($indentZeroBadLevel.Count) issues"
    Write-Host ""
    
    if ($indentZeroBadLevel.Count -gt 0) {
        foreach ($item in $indentZeroBadLevel) {
            Write-Host "Line $($item.LineNum) : $($item.ParenType) paren at level $($item.Level) (should be level 0)$($item.PrevLevel)" -ForegroundColor Red
            Write-Host "  Caused by unclosed paren at line $($item.CausedByLine)" -ForegroundColor Red
            Write-Host "$($item.Content)"
            Write-Host ""
        }
    } else {
        Write-Host "No issues found - all indent 0 parens are at nesting level 0." -ForegroundColor Green
        Write-Host ""
    }
    
    # Summary
    Write-Host "==================================================================================" -ForegroundColor Cyan
    Write-Host "SUMMARY" -ForegroundColor Cyan
    Write-Host "==================================================================================" -ForegroundColor Cyan
    Write-Host ""
    
    if ($unmatchedClosingParens.Count -gt 0) {
        Write-Host "SYNTAX ERRORS - Unmatched closing parens:" -ForegroundColor Red
        foreach ($item in $unmatchedClosingParens) {
            Write-Host "  Line $($item.LineNum) : Unmatched closing paren"
        }
        Write-Host ""
    }
    
    if ($nesting -eq 0) {
        Write-Host "Top-level forms found: $topLevelFormCount" -ForegroundColor Green
    } else {
        Write-Host "SYNTAX ERROR: Top level not closed at end of file (nesting level $nesting)" -ForegroundColor Red
        Write-Host "Cannot count top-level forms reliably."
    }
}
