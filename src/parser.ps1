
using namespace System.Collections.Generic

#region Enums

enum Tokens {

    # Keywords

    If; Then; ElsIf; Else; EndIf; For; Each; In; To; While; Do; EndDo;
    Procedure; EndProcedure; Function; EndFunction; Var; Val; Return; Continue; Break;
    And; Or; Not; Try; Except; Raise; EndTry; New;
    Export; True; False; Undefined; Case; When; EndCase; End;

    # Literals

    Ident; Number; String; DateTime;
    # parts of strings
    StringBeg; StringMid; StringEnd;

    # Operators

    # =   <>    <    >   <=   >=    +    -    *    /    %
    Eql; Neq; Lss; Gtr; Leq; Geq; Add; Sub; Mul; Div; Mod;
    #    (       )       [       ]       {       }
    Lparen; Rparen; Lbrack; Rbrack; Lbrace; Rbrace;
    #     ?      ;       .      :          ;
    Ternary; Comma; Period; Colon; Semicolon;

    # New Stmts
    #      +=
    AddAssign;

    # Other

    #         //          &      ~
    Eof; Comment; Directive; Label;
}

enum SelectKinds {
    Ident
    Index
    Call
}

#endregion Enums

#region Parser

class Parser {

    static $Keywords = @{
        If =           [Tokens]::If;           Если              = [Tokens]::If
        Then =         [Tokens]::Then;         Тогда             = [Tokens]::Then
        ElsIf =        [Tokens]::ElsIf;        ИначеЕсли         = [Tokens]::ElsIf
        Else =         [Tokens]::Else;         Иначе             = [Tokens]::Else
        EndIf =        [Tokens]::EndIf;        КонецЕсли         = [Tokens]::EndIf
        For =          [Tokens]::For;          Для               = [Tokens]::For
        Each =         [Tokens]::Each;         Каждого           = [Tokens]::Each
        In =           [Tokens]::In;           Из                = [Tokens]::In
        To =           [Tokens]::To;           По                = [Tokens]::To
        While =        [Tokens]::While;        Пока              = [Tokens]::While
        Do =           [Tokens]::Do;           Цикл              = [Tokens]::Do
        EndDo =        [Tokens]::EndDo;        КонецЦикла        = [Tokens]::EndDo
        Procedure =    [Tokens]::Procedure;    Процедура         = [Tokens]::Procedure
        EndProcedure = [Tokens]::EndProcedure; КонецПроцедуры    = [Tokens]::EndProcedure
        Function =     [Tokens]::Function;     Функция           = [Tokens]::Function
        EndFunction =  [Tokens]::EndFunction;  КонецФункции      = [Tokens]::EndFunction
        Var =          [Tokens]::Var;          Перем             = [Tokens]::Var
        Val =          [Tokens]::Val;          Знач              = [Tokens]::Val
        Return =       [Tokens]::Return;       Возврат           = [Tokens]::Return
        Continue =     [Tokens]::Continue;     Продолжить        = [Tokens]::Continue
        Break =        [Tokens]::Break;        Прервать          = [Tokens]::Break
        And =          [Tokens]::And;          И                 = [Tokens]::And
        Or =           [Tokens]::Or;           Или               = [Tokens]::Or
        Not =          [Tokens]::Not;          Не                = [Tokens]::Not
        Try =          [Tokens]::Try;          Попытка           = [Tokens]::Try
        Except =       [Tokens]::Except;       Исключение        = [Tokens]::Except
        Raise =        [Tokens]::Raise;        ВызватьИсключение = [Tokens]::Raise
        EndTry =       [Tokens]::EndTry;       КонецПопытки      = [Tokens]::EndTry
        New =          [Tokens]::New;          Новый             = [Tokens]::New
        Export =       [Tokens]::Export;       Экспорт           = [Tokens]::Export
        True =         [Tokens]::True;         Истина            = [Tokens]::True
        False =        [Tokens]::False;        Ложь              = [Tokens]::False
        Undefined =    [Tokens]::Undefined;    Неопределено      = [Tokens]::Undefined
        Case =         [Tokens]::Case;         Выбор             = [Tokens]::Case
        When =         [Tokens]::When;         Когда             = [Tokens]::When
        EndCase =      [Tokens]::EndCase;      КонецВыбора       = [Tokens]::EndCase
        End =          [Tokens]::End;          Конец             = [Tokens]::End
    }

    static [Object] $Map = @{
        [char]'='  = [Tokens]::Eql
        [char]'+'  = [Tokens]::Add
        [char]'-'  = [Tokens]::Sub
        [char]'*'  = [Tokens]::Mul
        [char]'%'  = [Tokens]::Mod
        [char]'('  = [Tokens]::Lparen
        [char]')'  = [Tokens]::Rparen
        [char]'['  = [Tokens]::Lbrack
        [char]']'  = [Tokens]::Rbrack
        [char]'{'  = [Tokens]::Lbrace
        [char]'}'  = [Tokens]::Rbrace
        [char]'?'  = [Tokens]::Ternary
        [char]','  = [Tokens]::Comma
        [char]'.'  = [Tokens]::Period
        [char]':'  = [Tokens]::Colon
        [char]';'  = [Tokens]::Semicolon
        [char]"`0" = [Tokens]::Eof
    }

    static $BasicLitNoString =
    [Tokens]::Number, [Tokens]::DateTime, [Tokens]::True,
    [Tokens]::False, [Tokens]::Undefined, [Tokens]::Null

    static $RelOperators =
    [Tokens]::Eql, [Tokens]::Neq, [Tokens]::Lss,
    [Tokens]::Gtr, [Tokens]::Leq, [Tokens]::Geq

    static $AddOperators = [Tokens]::Add, [Tokens]::Sub

    static $MulOperators = [Tokens]::Mul, [Tokens]::Div, [Tokens]::Mod

    static $InitOfExpression =
    [Tokens]::Add, [Tokens]::Sub, [Tokens]::Not,
    [Tokens]::Ident, [Tokens]::Lparen, [Tokens]::Number,
    [Tokens]::String, [Tokens]::StringBeg, [Tokens]::DateTime,
    [Tokens]::Ternary, [Tokens]::New, [Tokens]::True,
    [Tokens]::False, [Tokens]::Undefined, [Tokens]::Null

    [string] $Src
    [int]    $Len
    [int]    $Pos
    [Tokens] $Tok
    [string] $Lit = ''
    [char]   $Chr
    [Object] $Val
    [int]    $Line
    [int]    $EndLine
    [int]    $BegPos
    [int]    $Endpos
    [Object] $Scope = $null
    [Object] $Package

    $Vars = @{}
    $Methods = @{}
    $Unknown = @{}
    $IsFunc = $false
    $Interface = [List[Object]]::new()
    $AllowVar = $true

    Parser($Src) {
        $this.Src = $Src
        $this.Len = $Src.Length
        $this.Pos = 0
        $this.Chr = $this.Src[$this.Pos]
        $this.Val = $null
        $this.Line = 1
        $this.EndLine = 1
        $this.BegPos = 0
        $this.EndPos = 0
        $this.OpenScope();
    }

    next() {

        $this.EndPos = $this.Pos; $this.EndLine = $this.Line

        if ( $this.Lit[-1] -eq "`n") { $this.Line++ }

        do {

            $Skip = $False

            # skip space
            while ($null -ne $this.Chr -and [char]::IsWhiteSpace($this.Chr)) {
                if ($this.Chr -eq "`n") {
                    $this.Line++
                }
                $this.Chr = $this.Src[++$this.Pos]
            }

            $this.BegPos = $this.Pos

            if ( [char]::IsLetter($this.Chr) -or $this.Chr -eq '_' ) {

                # scan ident
                $Beg = $this.Pos
                do {
                    $this.Chr = $this.Src[++$this.Pos]
                } while ([char]::IsLetterOrDigit($this.Chr) -or $this.Chr -eq '_')
                $this.Lit = $this.Src.Substring($Beg, $this.Pos - $Beg)

                # lookup
                $T = $this::Keywords[$this.Lit]
                $this.Tok = if ($null -ne $T) { $T } else { [Tokens]::Ident }

            }
            elseif ([char]::IsDigit($this.Chr)) {

                # scan number
                $Beg = $this.Pos
                do {
                    $this.Chr = $this.Src[++$this.Pos]
                } while ([char]::IsDigit($this.Chr))
                if ($this.Chr -eq '.') {
                    do {
                        $this.Chr = $this.Src[++$this.Pos]
                    } while ([char]::IsDigit($this.Chr))
                }
                $this.Lit = $this.Src.Substring($Beg, $this.Pos - $Beg)
                $this.Tok = [Tokens]::Number

            }
            elseif ($this.Chr -eq '"') {

                # scan string
                $Beg = $this.Pos
                do {
                    do { $this.Chr = $this.Src[++$this.Pos] } while ($null -ne $this.Chr -and $this.Chr -ne '"' -and $this.Chr -ne "`n")
                    $this.Chr = $this.Src[++$this.Pos]
                } while ($this.Chr -eq '"')
                $this.Lit = $this.Src.Substring($Beg, $this.Pos - $Beg)
                $this.Tok = if ($this.Lit[-1] -eq '"') { [Tokens]::String } else { [Tokens]::StringBeg }

            }
            elseif ($this.Chr -eq '|') {

                # scan string
                $Beg = $this.Pos
                do {
                    do { $this.Chr = $this.Src[++$this.Pos] } while ($null -ne $this.Chr -and $this.Chr -ne '"' -and $this.Chr -ne "`n")
                    $this.Chr = $this.Src[++$this.Pos]
                } while ($this.Chr -eq '"')
                $this.Lit = $this.Src.Substring($Beg, $this.Pos - $Beg)
                $this.Tok = if ($this.Lit[-1] -eq '"') { [Tokens]::StringEnd } else { [Tokens]::StringMid }

            }
            elseif ($this.Chr -eq "'") {

                # scan datetime
                $Beg = $this.Pos
                do { $this.Chr = $this.Src[++$this.Pos] } while ($null -ne $this.Chr -and $this.Chr -ne "'")
                $this.Chr = $this.Src[++$this.Pos]
                $this.Lit = $this.Src.Substring($Beg, $this.Pos - $Beg)
                $this.Tok = [Tokens]::DateTime

            }
            else {

                $T = [Parser]::map[$this.Chr]

                if ($null -ne $T) {
                    $this.Tok = $T
                    $this.Lit = $this.Chr
                    $this.Chr = $this.Src[++$this.Pos]
                }
                elseif ($this.Chr -eq '/') {
                    $this.Chr = $this.Src[++$this.Pos]
                    if ($this.Chr -eq '/') {
                        $this.Pos = $this.Src.IndexOf([char]"`n", ++$this.Pos)
                        $this.Chr = if ($this.Pos -eq -1) { '' } else { "`n" }
                        $Skip = $True
                    }
                    else {
                        $this.Lit = '/'; $this.Tok = [Tokens]::Div
                    }
                }
                elseif ($this.Chr -eq '+') {
                    $this.Chr = $this.Src[++$this.Pos]
                    if ($this.Chr -eq '=') {
                        $this.Lit = '+='
                        $this.Tok = [Tokens]::AddAssign
                        $this.Chr = $this.Src[++$this.Pos]
                    }
                    else {
                        $this.Lit = '>'
                        $this.Tok = [Tokens]::Add
                    }
                }
                elseif ($this.Chr -eq '<') {
                    $this.Chr = $this.Src[++$this.Pos]
                    if ($this.Chr -eq '=') {
                        $this.Lit = '<='
                        $this.Tok = [Tokens]::Leq
                        $this.Chr = $this.Src[++$this.Pos]
                    }
                    elseif ($this.Chr -eq '>') {
                        $this.Lit = '<>'
                        $this.Tok = [Tokens]::Neq
                        $this.Chr = $this.Src[++$this.Pos]
                    }
                    else {
                        $this.Lit = '<'
                        $this.Tok = [Tokens]::Lss
                    }
                }
                elseif ($this.Chr -eq '>') {
                    $this.Chr = $this.Src[++$this.Pos]
                    if ($this.Chr -eq '=') {
                        $this.Lit = '>='
                        $this.Tok = [Tokens]::Geq
                        $this.Chr = $this.Src[++$this.Pos]
                    }
                    else {
                        $this.Lit = '>'
                        $this.Tok = [Tokens]::Gtr
                    }
                }
                elseif ($this.Chr -eq '~') {
                    # skip space
                    while ($this.Chr -and [char]::IsWhiteSpace($this.Chr)) {
                        if ($this.Chr -eq "`n") { $this.Line++ }
                        $this.Chr = $this.Src[++$this.Pos]
                    }
                    # skip label
                    while ([char]::IsLetterOrDigit($this.Chr) -or $this.Chr -eq '_') {
                        $this.Chr = $this.Src[++$this.Pos]
                    }
                    $Skip = $True
                }
                elseif ($this.Chr -eq '&') {
                    $this.Pos = $this.Src.IndexOf([char]"`n", ++$this.Pos)
                    $this.Chr = if ($this.Pos -eq -1) { "`0" } else { "`n" }
                    $Skip = $True
                }
                elseif ($this.Chr -eq '#') {
                    $this.Pos = $this.Src.IndexOf([char]"`n", ++$this.Pos)
                    $this.Chr = if ($this.Pos -eq -1) { "`0" } else { "`n" }
                    $Skip = $True
                }
                elseif ($this.Chr -eq "`0") {
                    $this.Lit = ''; $this.Tok = [Tokens]::Eof
                }
                else {
                    throw 'Неизвестный символ'
                }
            }

        } while ($Skip)

        if ($this.Tok -eq [Tokens]::Number) {
            $this.Val = [decimal]$this.Lit
        }
        elseif ($this.Tok -eq [Tokens]::True) {
            $this.Val = $true
        }
        elseif ($this.Tok -eq [Tokens]::False) {
            $this.Val = $false
        }
        elseif ($this.Tok -eq [Tokens]::DateTime) {
            $this.Val = $this.Lit
        }
        elseif ($this.Tok -eq [Tokens]::String) {
            $this.Val = $this.Lit
        }
        else {
            $this.Val = $null
        }

    } #next()

    [Object] FindItem($Name) {
        $S = $this.Scope;
        $Item = $S.Items[$Name]
        while ($null -eq $Item -and $null -ne $S.Outer) {
            $S = $S.outer
            $Item = $S.items[$Name]
        }
        return $Item
    }

    [Object] OpenScope() {
        $S = @{
            Outer = $this.Scope
            Items = @{}
            Auto  = [List[Object]]::new()
        }
        $this.Scope = $S
        $this.Vars = $S.Items
        return $S
    }

    CloseScope() {
        $S = $this.Scope.Outer
        $this.Scope = $S
        $this.Vars = $S.Items
    }

    Error($Note, $Pos, $Stop) {
        if ($Stop) {
            throw $Note + ' ' + $this.Line
        }
        else {
            Write-Host $Note
        }
    }

    Expect($Token) {
        if ($this.Tok -ne $Token) {
            $this.Error("Expected $Token", 0, $True)
        }
    }

    [Object] Place() {
        $L = $this.Lit.Length
        $P = $this.Pos - $L
        return @{
            Pos     = $P
            Len     = $L
            BegLine = $this.Line
            EndLine = $this.EndLine
        }
    }

    [Object] Place($Pos, $Line) {
        return @{
            Pos     = $Pos
            Len     = $this.EndPos - $Pos
            BegLine = $Line
            EndLine = $this.EndLine
        }
    }

    ParseModule() {
        $this.Next()
        $Decls = $this.ParseDecls()
        $Stmts = $this.ParseStmts()
        $Auto = [List[Object]]::new()
        foreach ($Item in $this.Scope.auto) {
            $Auto.Add($Item)
        }
        $this.Package = @{
            Type  = "Package"
            Decls = $Decls
            Stmts = $Stmts
            Auto  = $Auto
        }
        foreach ($Item in $this.Unknown.GetEnumerator()) {
            Write-Host "Undeclared method $($Item.Name)"
        }
        $this.Expect([Tokens]::Eof)
    }

    #Region Expr

    [Object] ParseExpression() {
        $P = $this.BegPos; $L = $this.Line
        $Expr = $this.ParseAndExpr()
        while ($this.Tok -eq [Tokens]::Or) {
            $Operator = $this.Tok
            $this.Next()
            $Expr = @{
                Type     = "BinaryExpr"
                Left     = $Expr
                Operator = $Operator
                Right    = $this.ParseAndExpr()
                Place    = $this.Place($P, $L)
            }
        }
        return $Expr
    }

    [Object] ParseAndExpr() {
        $P = $this.BegPos; $L = $this.Line
        $Expr = $this.ParseNotExpr()
        while ($this.Tok -eq [Tokens]::And) {
            $Operator = $this.Tok
            $this.Next()
            $Expr = @{
                Type     = "BinaryExpr"
                Left     = $Expr
                Operator = $Operator
                Right    = $this.ParseNotExpr()
                Place    = $this.Place($P, $L)
            }
        }
        return $Expr
    }

    [Object] ParseNotExpr() {
        $P = $this.BegPos; $L = $this.Line
        if ($this.Tok -eq [Tokens]::Not) {
            $this.Next()
            $Expr = @{
                Type  = "NotExpr"
                Expr  = $this.ParseRelExpr()
                Place = $this.Place($P, $L)
            }
        }
        else {
            $Expr = $this.ParseRelExpr()
        }
        return $Expr
    }

    [Object] ParseRelExpr() {
        $P = $this.BegPos; $L = $this.Line
        $Expr = $this.ParseAddExpr()
        while ($this::RelOperators.IndexOf($this.Tok) -ne -1) {
            $Operator = $this.Tok
            $this.Next()
            $Expr = @{
                Type     = "BinaryExpr"
                Left     = $Expr
                Operator = $Operator
                Right    = $this.ParseAddExpr()
                Place    = $this.Place($P, $L)
            }
        }
        return $Expr
    }

    [Object] ParseAddExpr() {
        $P = $this.BegPos; $L = $this.Line
        $Expr = $this.ParseMulExpr()
        while ($this::AddOperators.IndexOf($this.Tok) -ne -1) {
            $Operator = $this.Tok
            $this.Next()
            $Expr = @{
                Type     = "BinaryExpr"
                Left     = $Expr
                Operator = $Operator
                Right    = $this.ParseMulExpr()
                Place    = $this.Place($P, $L)
            }
        }
        return $Expr
    }

    [Object] ParseMulExpr() {
        $P = $this.BegPos; $L = $this.Line
        $Expr = $this.ParseUnaryExpr()
        while ($this::MulOperators.IndexOf($this.Tok) -ne -1) {
            $Operator = $this.Tok
            $this.Next()
            $Expr = @{
                Type     = "BinaryExpr"
                Left     = $Expr
                Operator = $Operator
                Right    = $this.ParseUnaryExpr()
                Place    = $this.Place($P, $L)
            }
        }
        return $Expr
    }

    [Object] ParseUnaryExpr() {
        $P = $this.BegPos; $L = $this.Line
        $Operator = $this.Tok
        if ($this::AddOperators.IndexOf($this.Tok) -ne -1) {
            $this.Next()
            $Expr = @{
                Type     = "UnaryExpr"
                Operator = $Operator
                Operand  = $this.ParseUnaryExpr()
                Place    = $this.Place($P, $L)
            }
        }
        elseif ($this.Tok -eq [Tokens]::Eof) {
            $Expr = $null
        }
        else {
            $Expr = $this.ParseOperand()
        }
        return $Expr
    }

    [Object] ParseOperand() {
        $T = $this.Tok
        $Operand = $null
        if ($T -eq [Tokens]::String -or $T -eq [Tokens]::StringBeg) {
            $Operand = $this.ParseStringExpr()
        }
        elseif ($this::BasicLitNoString.IndexOf($T) -ne -1) {
            $Operand = @{
                Type  = "BasicLitExpr"
                Kind  = $T
                Value = $this.Val
                Place = $this.Place()
            }
            $this.Next()
        }
        elseif ($T -eq [Tokens]::Ident) {
            $Operand = $this.ParseDesigExpr()
        }
        elseif ($T -eq [Tokens]::Lparen) {
            $Operand = $this.ParseParenExpr()
        }
        elseif ($T -eq [Tokens]::New) {
            $Operand = $this.ParseNewExpr()
        }
        elseif ($T -eq [Tokens]::Ternary) {
            $Operand = $this.ParseTernaryExpr()
        }
        else {
            $this.Error("Expected operand", 0, $True)
        }
        return $Operand
    }

    [Object] ParseStringExpr() {
        $P = $this.BegPos; $L = $this.Line
        $ExprList = [List[Object]]::new()
        while ($True) {
            if ($this.Tok -eq [Tokens]::String) {
                do {
                    $ExprList.Add(@{
                            Type  = "BasicLitExpr"
                            Kind  = $this.Tok
                            Value = $this.Val
                            Place = $this.Place()
                        })
                    $this.Next()
                } while ($this.Tok -eq [Tokens]::String)
            }
            elseif ($this.Tok -eq [Tokens]::StringBeg) {
                do {
                    $ExprList.Add(@{
                            Type  = "BasicLitExpr"
                            Kind  = $this.Tok
                            Value = $this.Val
                            Place = $this.Place()
                        })
                    $this.Next()
                } while ($this.Tok -eq [Tokens]::StringMid)
                if ($this.Tok -ne [Tokens]::StringEnd) {
                    $this.Error("Expected """, 0, $True)
                }
                $ExprList.Add(@{
                        Type  = "BasicLitExpr"
                        Kind  = $this.Tok
                        Value = $this.Val
                        Place = $this.Place()
                    })
                $this.Next()
            }
            else {
                break
            }
        }
        return @{
            Type  = "StringExpr"
            List  = $ExprList
            Place = $this.Place($P, $L)
        }
    }

    [Object] ParseNewExpr() {
        $P = $this.BegPos; $L = $this.Line
        $this.Next()
        $Name = ""
        $Argums = [List[Object]]::new()
        if ($this.Tok -eq [Tokens]::Ident) {
            $Name = $this.Lit
            $this.Next()
        }
        if ($this.Tok -eq [Tokens]::Lparen) {
            $this.Next()
            if ($this.Tok -ne [Tokens]::Rparen) {
                $Argums = $this.ParseArguments()
                $this.Expect([Tokens]::Rparen)
            }
            $this.Next()
        }
        if ($null -eq $Name -and $null -eq $Argums) {
            $this.Error("Expected constructor", $this.EndPos, $True)
        }
        return @{
            Type   = "NewExpr"
            Name   = $Name
            Argums = $Argums
            Place  = $this.Place($P, $L)
        }
    }

    [Object] ParseDesigExpr() {
        $AllowNewVar = $false
        $NewVar = $null
        return $this.ParseDesigExpr($AllowNewVar, [ref]$NewVar)
    }

    [Object] ParseDesigExpr($AllowNewVar, [ref]$NewVar) {
        $P = $this.BegPos; $L = $this.Line
        $Name = $this.Lit
        $this.Next()
        $SelectExpr = $this.ParseSelectExpr()
        $Item = $null
        $Kind = $null
        if ($null -eq $SelectExpr) {
            $Item = $this.FindItem($Name)
            $List = [List[Object]]::new()
        }
        else {
            $AllowNewVar = $False
            $Kind = $SelectExpr.Kind
            if ($Kind -eq [SelectKinds]::Call) {
                if (-not ($Item = $this.Methods[$Name])) {
                    if (-not ($Item = $this.Unknown[$Name])) {
                        $Item = @{
                            Type = "Unknown"
                            Name = $Name
                        }
                        $this.Unknown.Add($Name, $Item)
                    }
                }
            }
            else {
                $Item = $this.FindItem($Name)
            }
            $List = [List[Object]]::new()
            $List.Add($SelectExpr)
            $SelectExpr = $this.ParseSelectExpr()
            while ($null -ne $SelectExpr) {
                $Kind = $SelectExpr.Kind
                $List.Add($SelectExpr)
                $SelectExpr = $this.ParseSelectExpr()
            }
        }
        if ($null -eq $Item) {
            if ($AllowNewVar) {
                $Item = @{
                    Type = "VarItem"
                    Name = $Name
                    Auto = $True
                }
                $NewVar.Value = $Item
            }
            else {
                $Item = @{
                    Type = "Unknown"
                    Name = $Name
                }
                # TODO:
                $this.Error("Undeclared identifier $Name", $P, $False)
            }
        }
        $Call = ($Kind -eq [SelectKinds]::Call)
        return @{
            Type   = "DesigExpr"
            Item   = $Item
            Select = $List
            Call   = $Call
            Place  = $this.Place($P, $L)
        }
    }


    [Object] ParseSelectExpr() {
        $P = $this.BegPos; $L = $this.Line
        $T = $this.Tok
        $SelectExpr = $null
        if ($T -eq [Tokens]::Period) {
            $this.Next()
            if ($null -eq $this::Keywords[$this.Lit]) {
                $this.Expect([Tokens]::Ident)
            }
            $Value = $this.Lit
            $this.Next()
            $SelectExpr = @{
                Type  = "SelectExpr"
                Kind  = [SelectKinds]::Ident
                Value = $Value
                Place = $this.Place($P, $L)
            }
        }
        elseif ($T -eq [Tokens]::Lbrack) {
            $this.Next()
            if ($this.Tok -eq [Tokens]::Rbrack) {
                $this.Error("Expected expression", $P, $True)
            }
            $Value = $this.ParseExpression()
            $this.Expect([Tokens]::Rbrack)
            $this.Next()
            $SelectExpr = @{
                Type  = "SelectExpr"
                Kind  = [SelectKinds]::Index
                Value = $Value
                Place = $this.Place($P, $L)
            }
        }
        elseif ($this.Tok -eq [Tokens]::Lparen) {
            $this.Next()
            if ($T -eq [Tokens]::Rparen) {
                $Value = [System.Collections.ArrayList]::new()
            }
            else {
                $Value = $this.ParseArguments()
            }
            $this.Expect([Tokens]::Rparen)
            $this.Next()
            $SelectExpr = @{
                Type  = "SelectExpr"
                Kind  = [SelectKinds]::Call
                Value = $Value
                Place = $this.Place($P, $L)
            }
        }
        return $SelectExpr
    }

    [List[Object]] ParseExprList($HeadExpr = $null) {
        if ($null -eq $HeadExpr) {
            $HeadExpr = $this.ParseExpression()
        }
        $ExprList = [List[Object]]::new()
        $ExprList.Add($HeadExpr)
        while ($this.Tok -eq [Tokens]::Comma) {
            $this.Next()
            if ($this::InitOfExpression.IndexOf($this.Tok) -ne -1) {
                $ExprList.Add($this.ParseExpression())
            }
            else {
                break
            }
        }
        return $ExprList
    }

    [List[Object]] ParseArguments() {
        $ExprList = [List[Object]]::new()
        while ($true) {
            if ($this::InitOfExpression.IndexOf($this.Tok) -ne -1) {
                $ExprList.Add($this.ParseExpression())
            }
            else {
                $ExprList.Add($null)
            }
            if ($this.Tok -eq [Tokens]::Comma) {
                $this.Next()
            }
            else {
                break
            }
        }
        return $ExprList
    }

    [Object] ParseTernaryExpr() {
        $P = $this.BegPos; $L = $this.Line
        $this.Next()
        $this.Expect([Tokens]::Lparen)
        $this.Next()
        $Cond = $this.ParseExpression()
        $this.Expect([Tokens]::Comma)
        $this.Next()
        $ThenPart = $this.ParseExpression()
        $this.Expect([Tokens]::Comma)
        $this.Next()
        $ElsePart = $this.ParseExpression()
        $this.Expect([Tokens]::Rparen)
        $this.Next()
        if ($this.Tok -eq [Tokens]::Period) {
            $SelectList = [List[Object]]::new()
            $SelectExpr = $this.ParseSelectExpr()
            while ($null -ne $SelectExpr) {
                $SelectList.Add($SelectExpr)
                $SelectExpr = $this.ParseSelectExpr()
            }
        }
        else {
            $SelectList = [List[Object]]::new()
        }
        return @{
            Type   = "TernaryExpr"
            Cond   = $Cond
            Then   = $ThenPart
            Else   = $ElsePart
            Select = $SelectList
            Place  = $this.Place($P, $L)
        }
    }

    [Object] ParseParenExpr() {
        $P = $this.BegPos; $L = $this.Line
        $this.Next()
        $Expr = $this.ParseExpression()
        $this.Expect([Tokens]::Rparen)
        $this.Next()
        return @{
            Type  = "ParenExpr"
            Expr  = $Expr
            Place = $this.Place($P, $L)
        }
    }

    #EndRegion

    #Region Decl

    [List[Object]] ParseDecls() {
        $T = $this.Tok
        $Decls = [List[Object]]::new()
        while ($True) {
            if ($T -eq [Tokens]::Var -and $this.AllowVar) {
                $Decls.Add($this.ParsePropDecl())
            }
            elseif ($T -eq [Tokens]::Function) {
                $Decls.Add($this.ParseFuncDecl())
                $this.AllowVar = $False
            }
            elseif ($T -eq [Tokens]::Procedure) {
                $Decls.Add($this.ParseProcDecl())
                $this.AllowVar = $False
            }
            else {
                break
            }
            $T = $this.Tok
        }
        return $Decls
    }

    [Object] ParsePropDecl() {
        $this.Next()
        $P = $this.BegPos; $L = $this.Line
        $PropList = [List[Object]]::new()
        $PropList.Add($this.ParseProp())
        while ($this.Tok -eq [Tokens]::Comma) {
            $this.Next()
            $PropList.Add($this.ParseProp())
        }
        $this.Expect([Tokens]::Semicolon)
        $this.Next()
        while ($this.Tok -eq [Tokens]::Semicolon) {
            $this.Next()
        }
        return @{
            Type  = "PropDecl"
            List  = $PropList
            Place = $this.Place($P, $L)
        }
    }

    [Object] ParseProp() {
        $P = $this.BegPos
        $this.Expect([Tokens]::Ident)
        $Name = $this.Lit
        $this.Next()
        if ($this.Tok -eq [Tokens]::Export) {
            $Exported = $true
            $this.Next()
        }
        else {
            $Exported = $False
        }
        $Item = @{
            Type   = "PropItem"
            Name   = $Name
            Export = $Exported
        }
        if ($Exported) {
            $this.Interface.Add($Item)
        }
        if ($null -ne $this.Vars[$Name]) {
            $this.Error("Identifier already declared", $P, $true)
        }
        $this.Vars.Add($Name, $Item)
        return $Item
    }

    [List[Object]] ParseVarDecls() {
        $Decls = [List[Object]]::new()
        while ($this.Tok -eq [Tokens]::Var) {
            $this.Next()
            $Decls.Add($this.ParseVarDecl())
            $this.Expect([Tokens]::Semicolon)
            $this.Next()
        }
        return $Decls
    }

    [Object] ParseVarDecl() {
        $P = $this.BegPos; $L = $this.Line
        $VarList = [List[Object]]::new()
        $VarList.Add($this.ParseVar())
        while ($this.Tok -eq [Tokens]::Comma) {
            $this.Next()
            $VarList.Add($this.ParseVar())
        }
        return @{
            Type  = "VarDecl"
            List  = $VarList
            Place = $this.Place($P, $L)
        }
    }

    [Object] ParseVar() {
        $P = $this.BegPos
        $this.Expect([Tokens]::Ident)
        $Name = $this.Lit
        $Item = @{
            Type = "VarItem"
            Name = $Name
            Auto = $false
        }
        if ($this.Vars[$Name]) {
            $this.Error("Identifier already declared", $P, $True)
        }
        $this.Vars.Add($Name, $Item)
        $this.Next()
        return $Item
    }

    [Object] ParseFuncDecl() {
        $Item = $null
        $P = $this.BegPos; $L = $this.Line
        $Exported = $False
        $this.Next()
        $this.Expect([Tokens]::Ident)
        $Name = $this.Lit
        $this.Next()
        $this.OpenScope()
        $ParamList = $this.ParseParamList()
        if ($this.Tok -eq [Tokens]::Export) {
            $Exported = $True
            $this.Next()
        }
        if ($Item = $this.Unknown[$Name]) {
            $Item.Type = $this::Nodes.Func
            $Item["Params"] = $ParamList
            $Item["Export"] = $Exported
            $this.Unknown.Remove($Name)
        }
        else {
            $Item = @{
                Type   = "FuncItem"
                Name   = $Name
                Params = $ParamList
                Export = $Exported
            }
        }
        if ($null -ne $this.Methods[$Name]) {
            $this.Error("Method already declared", $P, $True)
        }
        $this.Methods.Add($Name, $Item)
        if ($Exported) {
            $this.Interface.Add($Item)
        }
        $Decls = $this.ParseVarDecls()
        $this.IsFunc = $True
        $Stmts = $this.ParseStmts()
        $this.IsFunc = $False
        $this.Expect([Tokens]::EndFunction)
        $Auto = [List[Object]]::new()
        foreach ($VarObj in $this.Scope.Auto) {
            $Auto.Add($VarObj)
        }
        $this.CloseScope()
        $this.Next()
        return @{
            Type  = "FuncDecl"
            Item  = $Item
            Decls = $Decls
            Auto  = $Auto
            Body  = $Stmts
            Place = $this.Place($P, $L)
        }
    }

    [Object] ParseProcDecl() {
        $Item = $null
        $P = $this.BegPos; $L = $this.Line
        $Exported = $False
        $this.Next()
        $this.Expect([Tokens]::Ident)
        $Name = $this.Lit
        $this.Next()
        $this.OpenScope()
        $ParamList = $this.ParseParamList()
        if ($this.Tok -eq [Tokens]::Export) {
            $Exported = $True
            $this.Next()
        }
        if ($Item = $this.Unknown[$Name]) {
            $Item.Type = "ProcItem"
            $Item["Params"] = $ParamList
            $Item["Export"] = $Exported
            $this.Unknown.Remove($Name)
        }
        else {
            $Item = @{
                Type   = "ProcItem"
                Name   = $Name
                Params = $ParamList
                Export = $Exported
            }
        }
        if ($this.Methods[$Name]) {
            $this.Error("Method already declared", $P, $true)
        }
        $this.Methods.Add($Name, $Item)
        if ($Exported) {
            $this.Interface.Add($Item)
        }
        $Decls = $this.ParseVarDecls()
        $Stmts = $this.ParseStmts()
        $this.Expect([Tokens]::EndProcedure)
        $Auto = [List[Object]]::new()
        foreach ($VarObj in $this.Scope.Auto) {
            $Auto.Add($VarObj)
        }
        $this.CloseScope()
        $this.Next()
        return @{
            Type  = "ProcDecl"
            Item  = $Item
            Decls = $Decls
            Auto  = $Auto
            Body  = $Stmts
            Place = $this.Place($P, $L)
        }
    }

    [List[Object]] ParseParamList() {
        $this.Expect([Tokens]::Lparen)
        $this.Next()
        if ($this.Tok -eq [Tokens]::Rparen) {
            $ParamList = [List[Object]]::new()
        }
        else {
            $ParamList = [List[Object]]::new()
            $ParamList.Add($this.ParseParameter())
            while ($this.Tok -eq [Tokens]::Comma) {
                $this.Next()
                $ParamList.Add($this.ParseParameter())
            }
        }
        $this.Expect([Tokens]::Rparen)
        $this.Next()
        return $ParamList
    }

    [Object] ParseParameter() {
        $P = $this.BegPos
        $ByVal = $False
        if ($this.Tok -eq [Tokens]::Val) {
            $ByVal = $True
            $this.Next()
        }
        $this.Expect([Tokens]::Ident)
        $Name = $this.Lit
        $this.Next()
        if ($this.Tok -eq [Tokens]::Eql) {
            $this.Next()
            $Item = @{
                Type  = "ParamItem"
                Name  = $Name
                ByVal = $ByVal
                Value = $this.ParseUnaryExpr()
            }
        }
        else {
            $Item = @{
                Type  = "ParamItem"
                Name  = $Name
                ByVal = $ByVal
                Value = $null
            }
        }
        if ($null -ne $this.Vars[$Name]) {
            $this.Error("Identifier already declared", $P, $True)
        }
        $this.Vars.Add($Name, $Item)
        return $Item
    }

    #EndRegion

    #Region Stmt

    [List[Object]] ParseStmts() {
        $Stmts = [List[Object]]::new()
        $Stmt = $this.ParseStmt()
        if ($null -ne $Stmt) {
            $Stmts.Add($Stmt)
        }
        while ($this.Tok -eq [Tokens]::Semicolon) {
            $this.Next()
            $Stmt = $this.ParseStmt()
            if ($null -ne $Stmt) {
                $Stmts.Add($Stmt)
            }
        }
        return $Stmts
    }

    [Object] ParseStmt() {
        $Stmt = $null
        if ($this.Tok -eq [Tokens]::Ident) {
            $Stmt = $this.ParseAssignOrCallStmt()
        }
        elseif ($this.Tok -eq [Tokens]::if) {
            $Stmt = $this.ParseIfStmt()
        }
        elseif ($this.Tok -eq [Tokens]::Try) {
            $Stmt = $this.ParseTryStmt()
        }
        elseif ($this.Tok -eq [Tokens]::While) {
            $Stmt = $this.ParseWhileStmt()
        }
        elseif ($this.Tok -eq [Tokens]::For) {
            $this.Next()
            if ($this.Tok -eq [Tokens]::Each) {
                $Stmt = $this.ParseForEachStmt()
            }
            else {
                $Stmt = $this.ParseForStmt()
            }
        }
        elseif ($this.Tok -eq [Tokens]::Return) {
            $Stmt = $this.ParseReturnStmt()
        }
        elseif ($this.Tok -eq [Tokens]::Break) {
            $Stmt = $this.ParseBreakStmt()
        }
        elseif ($this.Tok -eq [Tokens]::Continue) {
            $Stmt = $this.ParseContinueStmt()
        }
        elseif ($this.Tok -eq [Tokens]::Raise) {
            $Stmt = $this.ParseRaiseStmt()
        }
        elseif ($this.Tok -eq [Tokens]::Label) {
            $Stmt = @{
                Type  = "LabelStmt"
                Label = $this.Lit
                # Place = TODO:
            }
            $this.Next()
            $this.Expect([Tokens]::Colon)
            $this.Tok = [Tokens]::Semicolon
        }
        elseif ($this.Tok -eq [Tokens]::Semicolon) {
            # NOP
        }
        return $Stmt
    }

    [Object] ParseRaiseStmt() {
        $P = $this.BegPos; $L = $this.Line
        $Expr = $null
        $this.Next()
        if ($this::InitOfExpression.IndexOf($this.Tok) -ne -1) {
            $Expr = $this.ParseExpression()
        }
        return @{
            Type  = "RaiseStmt"
            Expr  = $Expr
            Place = $this.Place($P, $L)
        }
    }

    [Object] ParseBreakStmt() {
        $P = $this.BegPos; $L = $this.Line
        $Label = $null
        $this.Next()
        return @{
            Type  = "BreakStmt"
            Label = $Label
            Place = $this.Place($P, $L)
        }
    }

    [Object] ParseContinueStmt() {
        $P = $this.BegPos; $L = $this.Line
        $this.Next()
        return @{
            Type  = "ContinueStmt"
            Place = $this.Place($P, $L)
        }
    }

    [Object] ParseAssignOrCallStmt() {
        $P = $this.BegPos; $L = $this.Line
        $NewVar = $null
        $Left = $this.ParseDesigExpr($True, [ref]$NewVar)
        if ($Left.Call) {
            $Stmt = @{
                Type  = "CallStmt"
                Desig = $Left
                Place = $this.Place($P, $L)
            }
        }
        else {
            $this.Expect([Tokens]::Eql)
            $this.Next()
            $Right = $this.ParseExpression()
            if ($null -ne $NewVar) {
                $this.Vars.Add($NewVar.Name, $NewVar)
                $this.Scope.Auto.Add($NewVar)
            }
            $Stmt = @{
                Type  = "AssignStmt"
                Left  = $Left
                Right = $Right
                Place = $this.Place($P, $L)
            }
        }
        return $Stmt
    }

    [Object] ParseIfStmt() {
        $P = $this.BegPos; $L = $this.Line
        $ElsIfPart = $null; $ElsePart = $null
        $this.Next()
        $Cond = $this.ParseExpression()
        $this.Expect([Tokens]::Then)
        $this.Next()
        $ThenPart = $this.ParseStmts()
        $T = $this.Tok
        if ($T -eq [Tokens]::Elsif) {
            $ElsIfPart = [List[Object]]::new()
            while ($T -eq [Tokens]::Elsif) {
                $P = $this.BegPos; $L = $this.Line
                $this.Next()
                $ElsIfCond = $this.ParseExpression()
                $this.Expect([Tokens]::Then)
                $this.Next()
                $ElsIfThen = $this.ParseStmts()
                $ElsIfPart.Add(@{
                        Type  = "ElsIfStmt"
                        Cond  = $ElsIfCond
                        Then  = $ElsIfThen
                        Place = $this.Place($P, $L)
                    })
                $T = $this.Tok
            }
        }
        if ($T -eq [Tokens]::Else) {
            $this.Next()
            $ElsePart = $this.ParseStmts()
        }
        $this.Expect([Tokens]::EndIf)
        $this.Next()
        return @{
            Type  = "IfStmt"
            Cond  = $Cond
            Then  = $ThenPart
            ElsIf = $ElsIfPart
            Else  = $ElsePart
            Place = $this.Place($P, $L)
        }
    }

    [Object] ParseTryStmt() {
        $P = $this.BegPos; $L = $this.Line
        $TryPart = $null; $ExceptPart = $null
        $this.Next()
        TryPart = $this.ParseStmts()
        $this.Expect([Tokens]::Except)
        $this.Next()
        ExceptPart = $this.ParseStmts()
        $this.Expect([Tokens]::EndTry)
        $this.Next()
        return @{
            Type   = "TryStmt"
            Try    = $TryPart
            Except = $ExceptPart
            Place  = $this.Place($P, $L)
        }
    }

    [Object] ParseWhileStmt() {
        $P = $this.BegPos; $L = $this.Line
        $this.Next()
        $Cond = $this.ParseExpression()
        $this.Expect([Tokens]::Do)
        $this.Next()
        $Stmts = $this.ParseStmts()
        $this.Expect([Tokens]::EndDo)
        $this.Next()
        return @{
            Type  = "WhileStmt"
            Cond  = $Cond
            Body  = $Stmts
            Place = $this.Place($P, $L)
        }
    }

    [Object] ParseForStmt() {
        $P = $this.BegPos; $L = $this.Line
        $NewVar = $null
        $this.Expect([Tokens]::Ident)
        $VarPos = $this.BegPos
        $DesigExpr = $this.ParseDesigExpr($True, [ref]$NewVar)
        if ($DesigExpr.Call) {
            $this.Error("Expected variable", $VarPos, $True)
        }
        $this.Expect([Tokens]::Eql)
        $this.Next()
        $From = $this.ParseExpression()
        $this.Expect([Tokens]::To)
        $this.Next()
        $Until = $this.ParseExpression()
        if ($null -ne $NewVar) {
            $this.Vars.Add($NewVar.Name, $NewVar)
            $this.Scope.Auto.Add($NewVar)
        }
        $this.Expect([Tokens]::Do)
        $this.Next()
        $Stmts = $this.ParseStmts()
        $this.Expect([Tokens]::EndDo)
        $this.Next()
        return @{
            Type  = "ForStmt"
            Desig = $DesigExpr
            From  = $From
            To    = $Until
            Body  = $Stmts
            Place = $this.Place($P, $L)
        }
    }

    [Object] ParseForEachStmt() {
        $P = $this.BegPos; $L = $this.Line
        $NewVar = $null
        $this.Next()
        $this.Expect([Tokens]::Ident)
        $VarPos = $this.BegPos
        $DesigExpr = $this.ParseDesigExpr($True, [ref]$NewVar)
        if ($DesigExpr.Call) {
            $this.Error("Expected variable", $VarPos, $True)
        }
        $this.Expect([Tokens]::In)
        $this.Next()
        $Collection = $this.ParseExpression()
        if ($null -ne $NewVar) {
            $this.Vars.Add($NewVar.Name, $NewVar)
            $this.Scope.Auto.Add($NewVar)
        }
        $this.Expect([Tokens]::Do)
        $this.Next()
        $Stmts = $this.ParseStmts()
        $this.Expect([Tokens]::EndDo)
        $this.Next()
        return @{
            Type  = "ForEachStmt"
            Desig = $DesigExpr
            In    = $Collection
            Body  = $Stmts
            Place = $this.Place($P, $L)
        }
    }

    [Object] ParseReturnStmt() {
        $P = $this.BegPos; $L = $this.Line
        $Expr = $null
        $P = $this.BegPos; $L = $this.Line
        $this.Next()
        if ($this.IsFunc) {
            $Expr = $this.ParseExpression()
        }
        return @{
            Type  = "ReturnStmt"
            Expr  = $Expr
            Place = $this.Place($P, $L)
        }
    }

    #EndRegion

}

#endregion Parser