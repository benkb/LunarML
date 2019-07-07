structure DamepoMLLrVals = DamepoMLLrValsFun(structure Token = LrParser.Token)
structure DamepoMLLex = DamepoMLLexFun(structure Tokens = DamepoMLLrVals.Tokens)
structure DamepoMLParser = Join(structure Lex = DamepoMLLex
                                structure ParserData = DamepoMLLrVals.ParserData
                                structure LrParser = LrParser)
structure SimpleParser = struct
fun print_error (s,p1,p2) = print s
fun parse str = #2 (Fixity.doDecs(Fixity.initialFixity, #1 (DamepoMLParser.parse(0, DamepoMLParser.makeLexer(DamepoMLLex.makeInputFromString str), print_error, ()))))
end
