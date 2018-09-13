$arrowSymbol = [char]0xE0B0;

function Prompt
{
    Write-Host -NoNewLine -ForeGroundColor Blue -BackGroundColor Black "PS"
    Write-Host -NoNewLine -ForeGroundColor Black -BackGroundColor DarkGray $arrowSymbol
    Write-Host -NoNewLine -ForeGroundColor Green -BackGroundColor DarkGray "$($ExecutionContext.SessionState.Path.CurrentLocation)"
    Write-Host -NoNewLine -ForeGroundColor DarkGray $arrowSymbol
    return " "
}

function Get-Weather
{
	Param (
		[Parameter(Mandatory = $false, Position = 0)]
		[string]
		$Location
    )

	(Invoke-WebRequest http://wttr.in/$Location -UserAgent curl).Content
}

Set-Alias weath Get-Weather

Set-PSReadLineOption -EditMode Windows
Set-PSReadLineKeyHandler -Chord Shift+Insert -Function Paste
Set-PSReadLineKeyHandler -Chord Control+w -Function SelectAll
Set-PSReadLineKeyHandler -Chord Control+d -Function MenuComplete

# Remove if you don't have fzf
Remove-PSReadLineKeyHandler -Chord Control+r
Import-Module PSFzf

# need to have this installed: Import-Module PowerShellCookbook
Import-Module PSColor
Import-Module Posh-Git

Set-Alias ls gci
