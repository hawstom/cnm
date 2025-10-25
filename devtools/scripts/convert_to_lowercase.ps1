# Convert AutoLISP code to lowercase
# Preserves: strings, comments, quoted atoms (like 'InnerDiameterOrWidth)
# Converts: all code symbols to lowercase

param(
    [Parameter(Mandatory=$true)]
    [string]$FilePath
)

function Convert-LispToLowercase {
    param([string]$Content)
    
    $result = New-Object System.Text.StringBuilder
    $i = 0
    $len = $Content.Length
    
    while ($i -lt $len) {
        $char = $Content[$i]
        
        # Handle strings - preserve everything inside "..."
        if ($char -eq '"') {
            [void]$result.Append($char)
            $i++
            while ($i -lt $len) {
                $char = $Content[$i]
                [void]$result.Append($char)
                if (($char -eq '\') -and (($i + 1) -lt $len)) {
                    # Escaped character - include next char
                    $i++
                    [void]$result.Append($Content[$i])
                }
                elseif ($char -eq '"') {
                    break
                }
                $i++
            }
            $i++
            continue
        }
        
        # Handle comments - preserve everything after ;
        if ($char -eq ';') {
            while (($i -lt $len) -and ($Content[$i] -ne "`n")) {
                [void]$result.Append($Content[$i])
                $i++
            }
            if ($i -lt $len) {
                [void]$result.Append($Content[$i])  # Include the newline
                $i++
            }
            continue
        }
        
        # Handle quoted atoms like 'InnerDiameterOrWidth
        if (($char -eq "'") -and (($i + 1) -lt $len)) {
            [void]$result.Append($char)
            $i++
            # Preserve the atom name as-is
            while (($i -lt $len) -and ($Content[$i] -match '[a-zA-Z0-9_-]')) {
                [void]$result.Append($Content[$i])
                $i++
            }
            continue
        }
        
        # Everything else - convert to lowercase
        [void]$result.Append($char.ToString().ToLower())
        $i++
    }
    
    return $result.ToString()
}

# Read file
$content = [System.IO.File]::ReadAllText($FilePath, [System.Text.Encoding]::UTF8)

# Convert
$converted = Convert-LispToLowercase $content

# Write back
[System.IO.File]::WriteAllText($FilePath, $converted, [System.Text.Encoding]::UTF8)

Write-Host "Converted: $FilePath" -ForegroundColor Green
