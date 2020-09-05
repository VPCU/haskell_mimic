module MiCParser
(readProgram
,CVal(Idef, IntC, StringC, Address, BaseEbp)
,CValExpr(CVal, Binary, Unary, Assign, AssignTo, AssignToAddr, FuncCall, LoadAddr, CValFromAddr)
,BinOp(Add, Substract, Multiply, Divide, Greater, Less, Equel, GreaterEquel, LessEquel, LogicAnd, LogicOr)
,UOp(VarPP, PPVar, VarMM, MMVar, Minus, LogicNot)
,Statement(StList, IfElse, If, While, For, CValExpr, Var, Return)
,Function(Fun)
,Program(Program)
,readMiC
,parseCValExpr
,parseStatement
,parseFunction
) where

import System.Environment
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

miCRNames = ["if", "then", "else", "while", "do", "for", 
             "break", "continue", "return", "sizeof",
             "int", "long", "short", "float", "double", "char", "viod"]

miOpNames = ["+", "-", "*", "/", "%", "$",
          "++", "--",
          "=", "+=", "-=", "*=", "/=", "%=",
          "^", "&", "|", 
          "!", "&&", "||", 
          "==", ">", "<", ">=", "<=", "!="]

languageDef = emptyDef{
            Token.commentStart      = "/*",
            Token.commentEnd        = "*/",
            Token.commentLine       = "//",
            Token.identStart        = letter <|> char '_',
            Token.identLetter       = letter <|> char '_' <|> digit,
            Token.reservedNames     = miCRNames,
            Token.reservedOpNames   = miOpNames
            }

lexer = Token.makeTokenParser languageDef

identifier      = Token.identifier      lexer
reserved        = Token.reserved        lexer
reservedOp      = Token.reservedOp      lexer
parens          = Token.parens          lexer
m_integer       = Token.integer         lexer
m_string        = Token.stringLiteral   lexer 
semi            = Token.semi            lexer
whiteSpace      = Token.whiteSpace      lexer
commaSep        = Token.commaSep        lexer
stringLiteral   = Token.stringLiteral   lexer

skipSemis :: Parser m -> Parser m
skipSemis p = do many semi
                 a <- p
                 many semi
                 return a

data CVal = Idef VarIdf
          | IntC Integer
          | StringC String
          | Address VarIdf
          | BaseEbp Integer
          deriving (Show)

type VarIdf = String
type VarType = String
type VarStmt = (VarType, VarIdf)
type FunCallParaList = [CValExpr]


data CValExpr = CVal CVal
              | Binary BinOp CValExpr CValExpr
              | Unary UOp CValExpr
              | Assign VarIdf CValExpr
              | AssignTo CVal CValExpr
              | AssignToAddr CValExpr CValExpr
              | FuncCall VarIdf FunCallParaList
              | LoadAddr CVal
              | CValFromAddr CValExpr
                deriving (Show)

data BinOp = Add
           | Substract
           | Multiply
           | Divide
           | Greater
           | GreaterEquel
           | LessEquel
           | Less
           | Equel
           | LogicAnd
           | LogicOr
             deriving (Show)

parseIdef :: Parser CVal
parseIdef = Idef <$> identifier

parseAddress :: Parser CVal
parseAddress = do
         a <- reservedOp "&" >> identifier
         return $ Address a

parseIntC :: Parser CVal
parseIntC = IntC <$> m_integer

parseStringC :: Parser CVal
parseStringC = liftM StringC stringLiteral


data UOp = LogicNot
         | Minus
         | VarPP
         | PPVar
         | VarMM
         | MMVar
           deriving (Show)

parseCVal :: Parser CVal
parseCVal = parseIdef <|> parseAddress <|> parseIntC <|> parseStringC

parseCValExpr :: Parser CValExpr
parseCValExpr = buildExpressionParser cValExprOps cValExprTerm

parseAssign :: Parser CValExpr
parseAssign = do var <- identifier
                 reservedOp "="
                 expr <- parseCValExpr
                 return $ Assign var expr

parseAddrAssign :: Parser CValExpr
parseAddrAssign = do reservedOp "$"
                     addr <- parseCValExpr
                     reservedOp "="
                     val  <- parseCValExpr
                     return $ AssignToAddr addr val

parseArrayAssign :: Parser CValExpr
parseArrayAssign = do idf <- identifier
                      reservedOp "["
                      expr <- parseCValExpr
                      reservedOp "]"
                      reservedOp "="
                      val  <- parseCValExpr
                      let idfaddr = CVal $ Address idf
                      let p = Binary Multiply expr (CVal $ IntC 4)
                      let addr = Binary Add idfaddr p
                      return $ AssignToAddr addr val

parseArrayVal :: Parser CValExpr
parseArrayVal = do idf <- identifier
                   reservedOp "["
                   expr <- parseCValExpr
                   reservedOp "]"
                   let idfaddr = CVal $ Address idf
                   let p = Binary Multiply expr (CVal $ IntC 4)
                   let addr = Binary Add idfaddr p
                   return $ CValFromAddr addr

parseFuncCall :: Parser CValExpr
parseFuncCall = do fun <- identifier
                   paras <- parens $ commaSep parseCValExpr
                   return $ FuncCall fun paras

cValExprOps = [[Prefix (reservedOp "-" >> return  (Unary Minus))]
              ,[Prefix (reservedOp "*" >> return  (CValFromAddr))]
              ,[Postfix(reservedOp "++" >> return (Unary VarPP))
              , Postfix(reservedOp "--" >> return (Unary VarMM))]
              ,[Prefix (reservedOp "++" >> return (Unary PPVar))
              , Prefix (reservedOp "--" >> return (Unary MMVar))]
              ,[Prefix (reservedOp "!"  >> return (Unary LogicNot))]
              ,[binary "*" Multiply   AssocLeft
              , binary "/" Divide     AssocLeft]
              ,[binary "+" Add        AssocLeft
              , binary "-" Substract  AssocLeft]
              ,[binary ">" Greater    AssocLeft
              , binary "<" Less       AssocLeft
              , binary ">=" GreaterEquel AssocLeft
              , binary "<=" LessEquel  AssocLeft
              , binary "==" Equel     AssocLeft]
              ,[binary "&&" LogicAnd  AssocLeft]
              ,[binary "||" LogicOr   AssocLeft]]
    where binary op e ass = Infix (reservedOp op >> return (Binary e)) ass

cValExprTerm :: Parser CValExpr
cValExprTerm = parens parseCValExpr
            <|>try parseFuncCall
            <|>parseAddrAssign
            <|>try parseArrayAssign
            <|>try parseAssign
            <|>try parseArrayVal
            <|>CVal <$> parseCVal
            <?> "bad expression "


parseExpr = parseCValExpr


data Statement = StList [Statement]
               | IfElse CValExpr Statement Statement
               | If CValExpr Statement
               | While CValExpr Statement
               | For CValExpr CValExpr CValExpr Statement
               | CValExpr CValExpr
               | Var VarStmt Integer
               | Return CValExpr

instance Show Statement where
    show (StList l) = show_ l++""
    show (IfElse c s1 s2) = "if (" ++ show c ++ ") then\n" ++ show s1 ++ "else\n" ++ show s2 ++ "endif"
    show (If c s) = "if (" ++ show c ++ ") then\n" ++ show s++"endif"
    show (While c s) = "while ("++show c++") do\n" ++ show s ++ "endwhile"
    show (For c1 c2 c3 s) = "for ("++show c1++" "++show c2++" "++show c3++"\n"++show s++"endfor"
    show (CValExpr c) = show c
    show (Var st i) = "var "++show st++" len:"++show i++""
    show (Return c) = "retrun "++show c++""

parseStatement' :: Parser Statement
parseStatement' = parseList
              <|>try parseIfElse
              <|>parseIf
              <|>parseWhile
              <|>parseFor
              <|>try parseVarArrayStmt
              <|>parseVarStmt
              <|>parseCValExprStmt
              <|>parseReturn
              <?> "error statement"

parseStatement :: Parser Statement
parseStatement = skipSemis parseStatement'

parseList :: Parser Statement
parseList = do reservedOp "{"
               stList <- many parseStatement
               reservedOp "}"
               return $ StList stList

parseIfElse :: Parser Statement
parseIfElse = do reserved "if"
                 reservedOp "("
                 cond <- parseCValExpr
                 reservedOp ")"
                 st1 <- parseStatement
                 reserved "else"
                 st2 <- parseStatement
                 return $ IfElse cond st1 st2

parseIf :: Parser Statement
parseIf = do reserved "if"
             reservedOp "("
             cond <- parseCValExpr
             reservedOp ")"
             st <- parseStatement
             return $ If cond st

parseWhile :: Parser Statement
parseWhile = do reserved "while"
                reservedOp "("
                cond <- parseCValExpr
                reservedOp ")"
                st <- parseStatement
                return $ While cond st

parseFor :: Parser Statement
parseFor = do reserved "for"
              reservedOp "("
              p1 <- parseCValExpr
              reservedOp ";"
              p2 <- parseCValExpr
              reservedOp ";"
              p3 <- parseCValExpr
              reservedOp ")"
              st <- parseStatement
              return $ For p1 p2 p3 st

parseReturn :: Parser Statement
parseReturn = do reserved "return"
                 r <- parseCValExpr
                 return $ Return r

parseVarType :: Parser VarType
parseVarType = do reserved "int"
                  return "int"
            <|>do reserved "char"
                  return "char"
            <|>do reserved "viod"
                  return "viod"
            <?> "type"

parseVarArrayStmt :: Parser Statement
parseVarArrayStmt = do t <- parseVarType
                       name <- identifier
                       reservedOp "["
                       len <- m_integer
                       reservedOp "]"
                       return $ Var (t, name) len
                       

parseVarStmt :: Parser Statement
parseVarStmt = do t <- parseVarType
                  name <- identifier
                  return $ Var (t, name) 1

parseCValExprStmt :: Parser Statement
parseCValExprStmt = liftM CValExpr parseCValExpr

type FunctionPara = (VarType, VarIdf)
type FunctionParaList = [FunctionPara]

data Function = Fun VarIdf VarType FunctionParaList Statement

instance Show Function where
    show (Fun idf tp paras st) = tp++" "++idf++" "++show paras++" "++"\n"++show st++"\n_____\n"

functionParaStmt :: Parser FunctionPara
functionParaStmt = do t <- parseVarType
                      name <- identifier
                      return $ (t, name)

functionParaStmtList :: Parser FunctionParaList
functionParaStmtList = commaSep functionParaStmt 

parseFunction :: Parser Function
parseFunction = do t <- maybe "void" (\x -> x) <$> (optionMaybe parseVarType) 
                   fname <- identifier
                   paras <- parens functionParaStmtList
                   st <- parseStatement
                   return $ Fun fname t paras st 

data Program = Program [VarStmt] [Function]

show_ a = unlines(show `map` a)

instance Show Program where
    show (Program vars funcs) = "Program\n_____\n\n" ++ show_ vars ++ "\n" ++ show_ funcs

proAddFunction :: Program -> Parser Program
proAddFunction (Program var fun) = do
    f <- parseFunction
    return $ Program var (f:fun)

glbStmt :: Parser VarStmt
glbStmt = do t <- parseVarType
             i <- identifier
             return (t, i)

proAddStmt :: Program -> Parser Program
proAddStmt (Program var fun) = do
    s <- glbStmt
    return $ Program (s:var) fun

proAdd :: Program -> Parser Program
proAdd p = try (proAddFunction p)
        <|>(proAddStmt p)

skipThings :: Parser ()
skipThings = whiteSpace >> (( do char '#'; skipMany $ (noneOf "\n"); whiteSpace;  ) <|> whiteSpace)

parseProgram' :: Program -> Parser Program
parseProgram' p = do
    skipThings
    result <- optionMaybe eof
    case result of
        Just _  -> return p
        Nothing -> skipSemis (proAdd p) >>= parseProgram'

reverseFuncs :: Program -> Program
reverseFuncs (Program vars funcs) = Program vars (reverse funcs)

parseProgram :: Parser Program
parseProgram = do 
                  p <- parseProgram' $ Program [] []
                  return $ reverseFuncs p

readExpr :: String -> String
readExpr input = case parse parseExpr "miC" input of
    Left err -> "No match: " ++ show err
    Right val -> show val 

readMiC :: Parser a -> String -> Either String a
readMiC pa code =
    case parse pa "miC" code of
         Left err -> Left $ "No match: " ++ show err
         Right val -> Right $ val

readProgram :: String -> Either String Program
readProgram program = readMiC parseProgram program

readProgramF :: String -> IO String
readProgramF file = 
    do program <- readFile file
       case readProgram program of
            Left err -> return $ "No match: " ++ err
            Right val -> return $ show val 


main :: IO ()
main = do
         (file:_) <- getArgs
         readProgramF file >>= putStrLn
