$arrowSymbol = [char]0xE0B0;

function Prompt
{
    Write-Host -NoNewLine -ForeGroundColor Blue -BackGroundColor Black "PS"
    Write-Host -NoNewLine -ForeGroundColor Black -BackGroundColor DarkGray $arrowSymbol
    Write-Host -NoNewLine -ForeGroundColor Green -BackGroundColor DarkGray "$($ExecutionContext.SessionState.Path.CurrentLocation)"
    Write-Host -NoNewLine -ForeGroundColor DarkGray $arrowSymbol
    return " "
}

function Install-PSColorModule
{
    $uri = 'https://github.com/Davlind/PSColor/raw/master/release/PSColor.zip'
    $hash = '6E2F25DCA53E1C9F28557FB9DEC97D50E968D3E83D3BE59A4B6F9954DA826EBB'

    #TODO: get the module path so that it works
    $modulePath = $env:PSModulePath
    $moduleLocation = $modulePath.Split(':')[0]
    $outputLocation = $moduleLocation + '/PSColor'

    $tempPath = Get-Location | ForEach-Object Path
    $tempZip = $tempPath + '/PSColor.zip'
    $tempOutput = $tempPath = '/PSColor'

    Invoke-WebRequest $uri -OutFile $tempZip

    $tempHash = Get-FileHash $tempZip -Algorithm SHA256 | ForEach-Object Hash
    if ($hash -eq $tempHash) {
        Expand-Archive $tempZip
        Copy-Item -Recurse $tempOutput $outputLocation
        Remove-Item -Recurse -Force $tempOutput
    } else {
        Write-Error "Couldn't install PSColor: File hash did not match the expected hash!"
    }

    Remove-Item -Force $tempZip
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

function Copy-WorkingDirectoryToClipboard
{
    Get-Location | ForEach-Object Path | clip
}

Set-Alias weath Get-Weather
Set-Alias cwd Copy-WorkingDirectoryToClipboard

Set-PSReadLineOption -EditMode Windows
Set-PSReadLineKeyHandler -Chord Shift+Insert -Function Paste
Set-PSReadLineKeyHandler -Chord Control+w -Function SelectAll
Set-PSReadLineKeyHandler -Chord Control+d -Function MenuComplete

# Turn off the bell because it's annoying
Set-PSReadLineOption -BellStyle None

if (Get-Command fzf -ErrorAction SilentlyContinue)
{
    Remove-PSReadLineKeyHandler -Chord Control+r
    Import-Module PSFzf
}

# need to have this installed on powershell core (for PSColor): Import-Module PowerShellCookbook
Import-Module PSColor
Import-Module Posh-Git

$global:PSColor.File.Code.Pattern = '\.(java|c|cpp|cs|js|css|html|hs|fs|fsi)$'
$global:PSColor.File.Text.Pattern = '\.(txt|cfg|conf|ini|csv|log|config|xml|yml|md|markdown|yaml|cabal)$'

if ($IsLinux -or $IsMacOS)
{
    Set-Alias ls gci
}
