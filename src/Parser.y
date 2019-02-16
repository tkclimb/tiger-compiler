-- original author: unnohideyuki
-- referenced from: https://github.com/unnohideyuki/Tiger-in-Haskell/blob/master/chap5/Parser.y
{

module Parser (parse) where
import Lexer
import qualified Ast as A

}

%name parse
%tokentype { Token }
%error { parseError }

%token
  while   { While $$ }
  for     { For $$ }
  to      { To _ }
  break   { Break $$ }
  let     { Let $$ }
  in      { In _ }
  end     { End _ }
  function { Function _ }
  var     { Var _ }
  type    { Type $$ }
  array   { Array $$ }
  if      { If $$ }
  then    { Then _ }
  else    { Else _ }
  do      { Do _ }
  of      { Of _ }
  nil     { Nil _ }
  ','     { Comma _ }
  ':'     { Colon _ }
  ';'     { Semicolon _ }
  '('     { Lparen _ }
  ')'     { Rparen _ }
  '['     { Lbrack $$ }
  ']'     { Rbrack _ }
  '{'     { Lbrace $$ }
  '}'     { Rbrace _ }
  '.'     { Dot $$ }
  '+'     { Plus $$ }
  '-'     { Minus $$ }
  '*'     { Mul $$ }
  '/'     { Div $$ }
  '='     { Eq $$ }
  '<>'    { Neq $$ }
  '<'     { Lt $$ }
  '<='    { Le $$ }
  '>'     { Gt $$ }
  '>='    { Ge $$ }
  '&'     { And $$ }
  '|'     { Or $$ }
  ':='    { Assign $$ }
  STRING  { Str $$ }
  INT     { Int $$ }
  ID      { Id $$ }

%right    of
%nonassoc do
%nonassoc else
%nonassoc ':='
%left     '&' '|'
%nonassoc '=' '<>' '<' '<=' '>' '>=' 
%left     '+' '-'
%left     '*' '/'
%left     UMINUS

%%

program:        exp     { $1 }

decs:           tydec decs   { $1 : $2 }
 |              vardec decs  { $1 : $2 }
 |              fundec decs  { $1 : $2 }
 |              {- empty -}  { [] }

tydec:          type ID '=' ty { typedec $2 $4 $1 }

ty:             ID                      { namety $1 }
 |              '{' tyfields '}'        { recordty $2 $1 }
 |              array of ID             { arrayty $1 $3 }

tyfields:       ID ':' ID tyfs_tail     { field $1 $3 : $4 }
 |              {- empty -}             { [] }

tyfs_tail:      ',' ID ':' ID tyfs_tail { field $2 $4 : $5 }
 |              {- empty -}             { [] }

vardec:         var ID ':=' exp         { vardec' $2 $4 }
 |              var ID ':' ID ':=' exp  { vardec $2 $4 $6 }

fundec:         function ID '(' tyfields ')' '=' exp            { funcdec' $2 $4 $7 }
 |              function ID '(' tyfields ')' ':' ID '=' exp     { funcdec $2 $4 $7 $9 }

lvalue:         ID                      { simplevar $1 }
 |              lval2                   { $1 }

lval2:          ID '.' ID               { fieldvar (simplevar $1) $3 $2 }
 |              lval2 '.' ID            { fieldvar $1 $3 $2 }
 |              ID '[' exp ']'          { subscriptvar (simplevar $1) $3 $2 }
 |              lval2 '[' exp ']'       { subscriptvar $1 $3 $2 }

exp:            lvalue                                          { A.VarExpr $1 }
 |              nil                                             { A.NilExpr }
 |              '(' expseq ')'                                  { $2 }
 |              INT                                             { intexp $1 }
 |              STRING                                          { stringexp $1 }
 |              '-' exp %prec UMINUS                            { opexp A.MinusOp zero $2 $1 }
 |              ID '(' args ')'                                 { callexp $1 $3 }
 |              exp '+' exp                                     { opexp A.PlusOp $1 $3 $2 }
 |              exp '-' exp                                     { opexp A.MinusOp $1 $3 $2 }
 |              exp '*' exp                                     { opexp A.MulOp $1 $3 $2 }
 |              exp '/' exp                                     { opexp A.DivOp $1 $3 $2 }
 |              exp '=' exp                                     { opexp A.EqOp $1 $3 $2 }
 |              exp '<>' exp                                    { opexp A.NeqOp $1 $3 $2 }
 |              exp '<' exp                                     { opexp A.LtOp $1 $3 $2 }
 |              exp '>' exp                                     { opexp A.GtOp $1 $3 $2 }
 |              exp '<=' exp                                    { opexp A.LeOp $1 $3 $2 }
 |              exp '>=' exp                                    { opexp A.GeOp $1 $3 $2 }
 |              exp '&' exp                                     { ifexp $1 $3 zero $2 }
 |              exp '|' exp                                     { ifexp $1 one $3 $2 }
 |              ID '{' rcd '}'                                  { recordexp $1 $3 }
 |              ID '[' exp ']' of exp                           { arrayexp $1 $3 $6 }
 |              lvalue ':=' exp                                 { assignexp $1 $3 $2 } 
 |              if exp then exp else exp                        { ifexp $2 $4 $6 $1 }
 |              if exp then exp %prec do                        { ifexp' $2 $4 $1 }
 |              while exp do exp                                { whileexp $2 $4 $1 }      
 |              for ID ':=' exp to exp do exp                   { forexp $2 $4 $6 $8 $1 }
 |              break                                           { breakexp $1 }
 |              let decs in expseq end                          { letexp $2 $4 $1 }


expseq:         exp expseq_tail         { seqexp_concat $1 $2 }
 |              {- empty -}             { A.SeqExpr [] }

expseq_tail:    ';' exp expseq_tail     { seqexp_concat $2 $3 }
 |              {- empty -}             { A.SeqExpr [] }

args:           exp args_tail           { $1 : $2 }
 |              {- empty -}             { [] }

args_tail:      ',' exp args_tail       { $2 : $3 }
 |              {- empty -}             { [] }

rcd:            ID '=' exp rcd_tail     { rec_field $1 $3 : $4 }
 |              {- empty -}             { [] }

rcd_tail:       ',' ID '=' exp rcd_tail { rec_field $2 $4 : $5 }
 |              {- empty -}             { [] }


{

parseError :: [Token] -> a
parseError [] = error "Parse Error at EOF"
parseError (x:xs) = error ("Parse Error at token " ++ prettyToken x)

simplevar ((AlexPn _ l c), s) = A.SimpleVar s $ A.Pos l c
fieldvar v (_, s) (AlexPn _ l c) = A.FieldVar v s $ A.Pos l c
subscriptvar v e (AlexPn _ l c) = A.SubscriptVar v e $ A.Pos l c

zero = A.IntExpr 0 $ A.Pos 0 0
one = A.IntExpr 1 $ A.Pos 0 0

intexp ((AlexPn _ l c), i) = A.IntExpr i $ A.Pos l c
stringexp ((AlexPn _ l c), s) = A.StringExpr s $ A.Pos l c
callexp ((AlexPn _ l c), f) args = A.CallExpr f args $ A.Pos l c
breakexp (AlexPn _ l c) = A.BreakExpr $ A.Pos l c
opexp op lhs rhs (AlexPn _ l c) = A.OpExpr op lhs rhs $ A.Pos l c
rec_field ((AlexPn _ l c), s) exp = (s, exp, A.Pos l c)
recordexp ((AlexPn _ l c), s) rs = A.RecordExpr rs s $ A.Pos l c
assignexp var exp (AlexPn _ l c) = A.AssignExpr var exp $ A.Pos l c
ifexp test' then' else' (AlexPn _ l c) = A.IfExpr test' then' (Just else') (A.Pos l c)
ifexp' test' then' (AlexPn _ l c) = A.IfExpr test' then' Nothing $ A.Pos l c
whileexp t b (AlexPn _ l c) = A.WhileExpr t b $ A.Pos l c
forexp (_, s) lo hi body (AlexPn _ l c) = A.ForExpr s True lo hi body $ A.Pos l c

letexp decs body (AlexPn _ l c) = 
  let
    merge_decs [] dec = [dec]
    merge_decs ds d2 =
      let
        d1 = last ds
        ds' = init ds
      in
        case (d1, d2) of
          (A.FunctionDec fdecs, A.FunctionDec fdecs') -> ds' ++ [A.FunctionDec (fdecs ++ fdecs')]
          (A.TypeDec tdecs, A.TypeDec tdecs') -> ds' ++ [A.TypeDec (tdecs ++ tdecs')]
          _ -> ds ++ [d2]
    decs' = foldl merge_decs [] decs
  in                                      
    A.LetExpr decs' body $ A.Pos l c

arrayexp ((AlexPn _ l c), s) sz exp = A.ArrayExpr s sz exp $ A.Pos l c

seqexp_concat exp (A.SeqExpr es) = A.SeqExpr (exp : es)

namety ((AlexPn _ l c), s) = A.NameTy s $ A.Pos l c
recordty fs (AlexPn _ l c) = A.RecordTy fs $ A.Pos l c
arrayty (AlexPn _ l c) (_, s) = A.ArrayTy s $ A.Pos l c
field ((AlexPn _ l c), f) (_, t) = A.Field f True t $ A.Pos l c

typedec (_, s) t (AlexPn _ l c) = A.TypeDec [(s, t, A.Pos l c)] -- should be merged later?
funcdec ((AlexPn _ l c), s) params (_, ty) body = A.FunctionDec [A.FuncDec s params (Just ty) body $ A.Pos l c]
funcdec' ((AlexPn _ l c), s) params body = A.FunctionDec [A.FuncDec s params Nothing body $ A.Pos l c]
vardec ((AlexPn _ l c), s) (_, ty) init = A.VarDec s True (Just ty) init $ A.Pos l c
vardec' ((AlexPn _ l c), s) init = A.VarDec s True Nothing init $ A.Pos l c

}
