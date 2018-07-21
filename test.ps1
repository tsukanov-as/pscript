
. .\src\parser.ps1

$Src = Get-Content ".\test.bsl" -Raw
$p = New-Object Parser($Src)

# do {
#     $p.next()
# } while ($p.Chr -ne "`0")

$p.ParseModule()
$module = $p.Package

# $module | ConvertTo-Json -Depth 50 > .\ast.json