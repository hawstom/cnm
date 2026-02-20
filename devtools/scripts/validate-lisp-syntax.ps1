# AutoLISP Syntax Validator
# Checks for balanced parentheses and quotes in .lsp files
# Usage: .\validate-lisp-syntax.ps1 -FilePath "path\to\file.lsp"

param(
    [Parameter(Mandatory=$true)]
    [string]$FilePath
)

function Test-LispBalance {
    param([string]$Content, [string]$FileName)
    
    $depth = 0
    $line = 1
    $col = 0
    $inString = $false
    $inComment = $false
    $escaped = $false
    $stringStart = @{Line=0; Col=0}
    $errors = @()
    
    for ($i = 0; $i -lt $Content.Length; $i++) {
        $char = $Content[$i]
        $col++
        
        # Track line numbers
        if ($char -eq "`n") {
            $line++
            $col = 0
            if ($inComment) { $inComment = $false }
            continue
        }
        
        # Handle escapes in strings
        if ($inString) {
            if ($escaped) { 
                $escaped = $false
                continue 
            }
            if ($char -eq '\') { 
                $escaped = $true
                continue 
            }
            if ($char -eq '"') { 
                $inString = $false 
            }
            continue
        }
        
        # Handle comments (skip to end of line)
        if ($inComment) {
            continue
        }
        
        # Check for comment start
        if ($char -eq ';') { 
            $inComment = $true
            continue 
        }
        
        # Check for string start
        if ($char -eq '"') { 
            $inString = $true
            $stringStart = @{Line=$line; Col=$col}
            continue 
        }
        
        # Count parentheses
        if ($char -eq '(') { 
            $depth++ 
        }
        if ($char -eq ')') {
            $depth--
            if ($depth -lt 0) {
                $errors += "Line $line, Column $col : Excess closing parenthesis ')'"
                $depth = 0  # Reset to continue checking
            }
        }
    }
    
    # Check for unclosed strings
    if ($inString) {
        $errors += "Line $($stringStart.Line), Column $($stringStart.Col) : Unclosed string"
    }
    
    # Check for unclosed parentheses
    if ($depth -gt 0) {
        $errors += "End of file: $depth unclosed opening parenthesis(es)"
    }
    
    return @{
        Valid = ($errors.Count -eq 0)
        Errors = $errors
    }
}

# Main execution
if (-not (Test-Path $FilePath)) {
    Write-Error "File not found: $FilePath"
    exit 1
}

$content = Get-Content $FilePath -Raw -ErrorAction Stop
$result = Test-LispBalance -Content $content -FileName (Split-Path $FilePath -Leaf)

if ($result.Valid) {
    Write-Host "✓ $FilePath - Syntax valid (parentheses and quotes balanced)" -ForegroundColor Green
    exit 0
} else {
    Write-Host "✗ $FilePath - Syntax errors found:" -ForegroundColor Red
    foreach ($error in $result.Errors) {
        Write-Host "  $error" -ForegroundColor Red
    }
    exit 1
}
