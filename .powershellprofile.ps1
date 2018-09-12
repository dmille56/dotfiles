$arrowSymbol = [char]0xE0B0;

function Prompt
{
    Write-Host -NoNewLine -ForeGroundColor Blue -BackGroundColor Black "PS"
    Write-Host -NoNewLine -ForeGroundColor Black -BackGroundColor DarkGray $arrowSymbol
    Write-Host -NoNewLine -ForeGroundColor Green -BackGroundColor DarkGray "$($ExecutionContext.SessionState.Path.CurrentLocation)"
    Write-Host -NoNewLine -ForeGroundColor DarkGray $arrowSymbol
    return " "
}

Set-PSReadLineKeyHandler -Chord 'Shift+Insert' -Function Paste

Import-Module PSColor
Import-Module Posh-Git
