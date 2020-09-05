{-# LANGUAGE FlexibleInstances #-}

import System.Environment
import qualified MiCParser as P
import Data.List (intercalate)
import Text.Printf
import Data.IORef

type Program = P.Program

type VarIdf = String
type VarType = String
type VarStmt = (VarIdf, VarType)
type VarStmtList = [VarStmt]
type NewVar = (VarStmt, Integer)
type FunctionParaList = [VarStmt]


data Statement = StList [NewVar] [Statement]
               | IfElse CValExpr Statement Statement
               | If CValExpr Statement
               | While CValExpr Statement
               | For CValExpr CValExpr CValExpr Statement
               | CValExpr CValExpr
               | Return CValExpr
                 deriving (Show)

type CValExpr = P.CValExpr

data Function = Fun VarIdf VarType FunctionParaList Statement
                deriving (Show)

isVar :: P.Statement -> Bool
isVar (P.Var _ _) = True
isVar _ = False

getVars :: P.Statement -> VarStmtList
getVars (P.StList s) = map (\(P.Var v _) -> v) $ filter isVar s

getArrayVars :: P.Statement -> [NewVar]
getArrayVars (P.StList s) = map (\(P.Var v q) -> (v, q)) $ filter isVar s

getCValExpr :: VarVm -> P.CValExpr -> CValExpr
getCValExpr vm (P.CVal (P.Idef idf))    = P.CVal (P.BaseEbp $ getVarPose vm idf)
getCValExpr vm (P.CVal (P.Address idf)) = P.LoadAddr (P.BaseEbp $ getVarPose vm idf)
getCValExpr vm (P.CValFromAddr c)       = P.CValFromAddr $ getCValExpr vm c
getCValExpr vm (P.AssignToAddr c1 c2)   = P.AssignToAddr (getCValExpr vm c1) (getCValExpr vm c2)
getCValExpr vm a@(P.CVal _)             = a
getCValExpr vm (P.Binary op a b)        = P.Binary op (getCValExpr vm a) (getCValExpr vm b)
getCValExpr vm (P.Unary op a)           = P.Unary op (getCValExpr vm a)
getCValExpr vm (P.Assign idf a)         = P.AssignTo (P.BaseEbp $ getVarPose vm idf) (getCValExpr vm a)
getCValExpr vm (P.FuncCall idf paras)   = P.FuncCall idf $ map (getCValExpr vm) paras

getStatement :: VarVm -> P.Statement -> Statement
getStatement vm st@(P.StList s)     = StList (getArrayVars st) (map (getStatement vm') $ filter (not . isVar) s)
    where vm' = varVmAddVars vm (getArrayVars st)
getStatement vm (P.IfElse c s1 s2)  = IfElse (getCValExpr vm c) (getStatement vm s1) (getStatement vm s2)
getStatement vm (P.If c s)          = If (getCValExpr vm c) (getStatement vm s)
getStatement vm (P.While c s)       = While (getCValExpr vm c) (getStatement vm s)
getStatement vm (P.For c1 c2 c3 s)  = For (g c1) (g c2) (g c3) (getStatement vm s)
    where g c = getCValExpr vm c
getStatement vm (P.CValExpr c)      = CValExpr $ getCValExpr vm c
getStatement vm (P.Return c)        = Return (getCValExpr vm c)

getFunction :: VarVm -> P.Function -> Function
getFunction vm (P.Fun name rt paras st) = Fun name rt paras (getStatement vm' st)
    where vm' = varVmAddParas vm paras

data MiCProgram = Program VarStmtList [Function]
                  deriving (Show)

getMiCProgram :: P.Program -> MiCProgram
getMiCProgram (P.Program vars funcs) = Program vars (map (getFunction emptyVarVm) funcs)

type RegName = String

data VarOp = VarIdf VarIdf
           | Imdt Integer
           | Direct String
           | Register RegName
           | RegBase RegName Integer
           | VsStack Integer
             deriving (Show)

data VarVm = VarVm VarList Integer -- vars size

type VarRef = IORef Integer

emptyVarRef :: IO VarRef
emptyVarRef = newIORef 0

newLabel :: VarRef -> IO String
newLabel v = do
    i <- readIORef v
    writeIORef v (i+1)
    return $ "L"++show (i+1)

emptyVarVm :: VarVm
emptyVarVm = VarVm [] 0

type TypeInfo = [(VarType, Integer)]

baseTypeInfo :: TypeInfo
baseTypeInfo = [("int", 4)]

varLen :: VarType -> Integer
varLen tp = maybeJust $ lookup tp baseTypeInfo

vmSize :: VarVm -> Integer
vmSize (VarVm _ size) = size

type VarInfo = (VarIdf, VarType, VarPose)
type VarPose = Integer
type VarList = [(VarIdf, VarInfo)]

maybeJust :: Maybe a -> a
maybeJust (Just a) = a
maybeJust (Nothing) = error "unkown type"

getVarInfo :: VarVm -> VarIdf -> VarInfo
getVarInfo vm@(VarVm list size) idf = maybeJust $ lookup idf list

getVarPose :: VarVm -> VarIdf -> VarPose
getVarPose vm idf = getPose $ getVarInfo vm idf
    where getPose (i, t, p) = p

getVarType :: VarVm -> VarIdf -> VarType
getVarType vm idf = getType $ getVarInfo vm idf
    where getType (i, t, p) = t



varVmAddVar :: VarVm -> NewVar -> VarVm
varVmAddVar vm@(VarVm list size) ((tp, idf), q) = VarVm ( (idf, (idf,tp,negate size')):list ) size'
    where size' = size + varLen tp * q

varVmAddVars :: VarVm -> [NewVar] -> VarVm
varVmAddVars vm vars = foldl varVmAddVar vm vars

varVmAddPara :: (VarVm, VarPose) -> VarStmt -> (VarVm, VarPose)
varVmAddPara (VarVm list size, pos) (tp, idf) = ( VarVm ( (idf, (idf,tp,pos)) : list )  size , pos + varLen tp )

varVmAddParas :: VarVm -> VarStmtList -> VarVm
varVmAddParas vm vars = first $ foldl varVmAddPara (vm, 8) vars

data CValSf = CValF P.CVal
            | BinF P.BinOp
            | UinF P.UOp
            | VarPP P.UOp P.CVal
            | AssignF P.CVal
            | AssignAdr
            | FuncF VarIdf [VarType]
            | LoadAddr P.CVal
            | LoadFromAddr
              deriving (Show)

getCValSf_ :: CValExpr -> [CValSf]
getCValSf_ (P.CVal x)                   = [CValF x]
getCValSf_ (P.Binary op x y)            = [BinF op] ++ getCValSf_ y ++ getCValSf_ x
getCValSf_ (P.Unary P.VarPP (P.CVal b)) = [VarPP P.VarPP b]
getCValSf_ (P.Unary P.VarMM (P.CVal b)) = [VarPP P.VarMM b]
getCValSf_ (P.Unary P.PPVar (P.CVal b)) = [VarPP P.PPVar b]
getCValSf_ (P.Unary P.MMVar (P.CVal b)) = [VarPP P.MMVar b]
getCValSf_ (P.Unary P.Minus b)          = getCValSf_ (P.Binary P.Substract (P.CVal $ P.IntC 0) b)
getCValSf_ (P.Unary op x)               = [UinF op] ++ getCValSf_ x
getCValSf_ (P.AssignTo (c@(P.BaseEbp _)) cval) = AssignF  c : getCValSf_ cval
getCValSf_ (P.AssignToAddr addr cval)   = [AssignAdr] ++ getCValSf_ addr ++ getCValSf_ cval
getCValSf_ (P.FuncCall t paras)         = FuncF t (map (\a -> "int") paras) : ( concat $ map getCValSf_ paras )
getCValSf_ (P.LoadAddr c)               = [LoadAddr c]
getCValSf_ (P.CValFromAddr c)           = [LoadFromAddr] ++ getCValSf_ c


getCValSf :: CValExpr -> [CValSf]
getCValSf c = reverse $ getCValSf_ c

first :: (a, b) -> a
first (a, _) = a

second :: (a, b) -> b
second (_, a) = a

second3 :: (a, b, c) -> b
second3 (_, a, _) = a

type VsStack = ([Integer], Integer) --(stack, maxsize)

stFirstInt :: VarVm -> Integer
stFirstInt vm = negate (vmSize vm) - varLen "int"

emptyVsSt :: VsStack
emptyVsSt = ([0], 0)

pushVsSt :: VsStack -> Integer -> VsStack
pushVsSt (stack, maxsize) len = (t:stack, max t maxsize)
    where t = head stack + len

pushVsIdf :: VsStack -> VarIdf -> VsStack
pushVsIdf vs idf = pushVsSt vs (varLen "int")

popVsSt :: VsStack -> VsStack
popVsSt (stack, maxsize) = (tail stack, maxsize)

popnVsSt :: Int -> VsStack -> VsStack
popnVsSt n (stack, maxsize) = (drop (fromIntegral n) stack, maxsize)


cVSf2Asm' :: [CValSf] -> AsmProg
cVSf2Asm' sf = first (foldl foldFuncC ([], emptyVsSt) sf)

cVsSize :: [CValSf] -> Integer
cVsSize sf = f (foldl foldFuncC ([], emptyVsSt) sf)
    where f (_,(_,a)) = a


uoo :: VsStack -> Integer -> VsStack-- push pop pop vsStack
uoo = pushVsSt.popVsSt.popVsSt

stTop :: VsStack -> Integer
stTop vs@(v, _) = head v

pb2b (P.BaseEbp x) = RegBase "ebp" x

foldFuncC :: (AsmProg, VsStack) -> CValSf -> (AsmProg, VsStack)
foldFuncC (asmProg, vs) (BinF P.Add)            = ( (AddInt (VsStack x) (VsStack y) ):asmProg,          uoo vs (varLen "int"))
    where x = stTop vs ; y = (stTop.popVsSt) vs 
foldFuncC (asmProg, vs) (BinF P.Substract)      = ( (SubInt (VsStack x) (VsStack y) ):asmProg,          uoo vs (varLen "int"))
    where x = stTop vs ; y = (stTop.popVsSt) vs 
foldFuncC (asmProg, vs) (BinF P.Multiply)       = ( (MulInt (VsStack x) (VsStack y) ):asmProg,          uoo vs (varLen "int"))
    where x = stTop vs ; y = (stTop.popVsSt) vs 
foldFuncC (asmProg, vs) (BinF P.Divide)         = ( (DivInt (VsStack x) (VsStack y) ):asmProg,          uoo vs (varLen "int"))
    where x = stTop vs ; y = (stTop.popVsSt) vs 
foldFuncC (asmProg, vs) (BinF P.Greater)        = ( (GreaterInt (VsStack x) (VsStack y) ):asmProg,      uoo vs (varLen "int"))
    where x = stTop vs ; y = (stTop.popVsSt) vs
foldFuncC (asmProg, vs) (BinF P.Less)           = ( (LessInt (VsStack x) (VsStack y) ):asmProg,      uoo vs (varLen "int"))
    where x = stTop vs ; y = (stTop.popVsSt) vs
foldFuncC (asmProg, vs) (BinF P.Equel)          = ( (EquelInt (VsStack x) (VsStack y) ):asmProg,      uoo vs (varLen "int"))
    where x = stTop vs ; y = (stTop.popVsSt) vs
foldFuncC (asmProg, vs) (BinF P.GreaterEquel)   = ( (GreaterEInt (VsStack x) (VsStack y) ):asmProg,      uoo vs (varLen "int"))
    where x = stTop vs ; y = (stTop.popVsSt) vs
foldFuncC (asmProg, vs) (BinF P.LessEquel)      = ( (LessEInt (VsStack x) (VsStack y) ):asmProg,      uoo vs (varLen "int"))
    where x = stTop vs ; y = (stTop.popVsSt) vs
foldFuncC (asmProg, vs) (BinF P.LogicAnd)       = ( (LgAndInt (VsStack x) (VsStack y) ):asmProg,        uoo vs (varLen "int"))
    where x = stTop vs ; y = (stTop.popVsSt) vs
foldFuncC (asmProg, vs) (BinF P.LogicOr)        = ( (LgOrInt (VsStack x) (VsStack y) ):asmProg,         uoo vs (varLen "int"))
    where x = stTop vs ; y = (stTop.popVsSt) vs
foldFuncC (asmProg, vs) (UinF P.LogicNot)       = ( (LgNotInt (VsStack x)) : asmProg,                   vs)
    where x = stTop vs
foldFuncC (asmProg, vs) (VarPP P.VarPP x)       = ([MovInt (Register "eax") (pb2b x),
                                                            AddInt (Imdt 1) (Register "eax"),
                                                            MovInt (pb2b x) (VsStack $ stTop newVs)] ++ asmProg, newVs)
    where newVs = pushVsSt vs (varLen "int")
foldFuncC (asmProg, vs) (VarPP P.PPVar x)       = ([MovInt (Register "eax") (VsStack $ stTop newVs),
                                                            AddInt (Imdt 1) (Register "eax"),
                                                            MovInt (pb2b x) (Register "eax")] ++ asmProg, newVs)
    where newVs = pushVsSt vs (varLen "int")
foldFuncC (asmProg, vs) (VarPP P.VarMM x)       = ([MovInt (Register "eax") (pb2b x),
                                                            SubInt (Imdt 1) (Register "eax"),
                                                            MovInt (pb2b x) (VsStack $ stTop newVs)] ++ asmProg, newVs)
    where newVs = pushVsSt vs (varLen "int")
foldFuncC (asmProg, vs) (VarPP P.MMVar x)       = ([MovInt (Register "eax") (VsStack $ stTop newVs),
                                                            SubInt (Imdt 1) (Register "eax"),
                                                            MovInt (pb2b x) (Register "eax")] ++ asmProg, newVs)
    where newVs = pushVsSt vs (varLen "int")
foldFuncC (asmProg, vs ) (CValF (P.BaseEbp x))  = ( (MovInt (RegBase "ebp" x) (VsStack $ stTop newVs)) : asmProg,  newVs)
    where newVs = pushVsSt vs (varLen "int")
foldFuncC (asmProg, vs ) (CValF (P.IntC x))     = ( (MovInt (Imdt x) (VsStack $ stTop newVs)) : asmProg,    newVs)
    where newVs = pushVsSt vs (varLen "int") 
foldFuncC (asmProg, vs ) (CValF (P.StringC x))  = ( (MovInt (Direct x) (VsStack $ stTop newVs)) : asmProg,  newVs)
    where newVs = pushVsSt vs (varLen "int")
foldFuncC (asmProg, vs ) (LoadAddr (P.BaseEbp x))   = ( (LeaInt (RegBase "ebp" x) (VsStack $ stTop newVs)) : asmProg, newVs)
    where newVs = pushVsSt vs (varLen "int")
foldFuncC (asmProg, vs ) (AssignF (P.BaseEbp x))    = ( (MovInt (VsStack $ stTop vs) (RegBase "ebp" x)) : asmProg,   vs)
foldFuncC (asmProg, vs ) (AssignAdr)            = ( [MovInt (VsStack y) (RegBase "ebx" 0),
                                                     MovInt (VsStack x) (Register "ebx")] ++ asmProg, uoo vs (varLen "int"))
    where x = stTop vs ; y = (stTop.popVsSt) vs
foldFuncC (asmProg, vs ) (LoadFromAddr)         = ( [MovInt (RegBase "ebx" 0) (VsStack $ stTop newVs), 
                                                     MovInt (VsStack x) (Register "ebx")] ++ asmProg, newVs)
    where x = stTop vs ; newVs = vs
foldFuncC (asmProg, vs ) (FuncF idf paras )     = ( [MovInt (Register "eax") (VsStack $ stTop newVs), 
                                                            SubInt (Imdt $ toInteger (length paras) * varLen "int") (Register "esp"), 
                                                            CallInt $ "_" ++ idf]++
                                                            funcPushParas vs paras ++ 
                                                            asmProg, 
                                                       newVs)
    where newVs = pushVsSt ( popnVsSt (length paras) vs ) (varLen "int")


funcPushParas' :: [Integer] -> AsmProg
funcPushParas' [] = []
funcPushParas' l = PushInt (VsStack $ head l) : funcPushParas' (tail l)

funcPushParas :: VsStack -> [VarType] -> AsmProg
funcPushParas st@(s, _) types = funcPushParas' $ take (length types) s

cVSf2Asm :: [CValSf] -> AsmProg
cVSf2Asm sf = reverse $ cVSf2Asm' sf

data AsmCode = VarOp VarOp
             | AddInt VarOp VarOp
             | SubInt VarOp VarOp
             | MulInt VarOp VarOp
             | DivInt VarOp VarOp
             | ModInt VarOp VarOp
             | Imull VarOp VarOp
             | Idivl VarOp
             | Cltd
             | EquelInt VarOp VarOp
             | LessInt VarOp VarOp
             | GreaterInt VarOp VarOp
             | GreaterEInt VarOp VarOp
             | LessEInt VarOp VarOp
             | CmpInt VarOp VarOp
             | MovInt VarOp VarOp
             | LeaInt VarOp VarOp
             | Setg VarOp
             | Setl VarOp
             | Sete VarOp
             | Setge VarOp
             | Setle VarOp
             | Setne VarOp
             | MovZbl VarOp VarOp
             | PushInt VarOp
             | CallInt VarIdf
             | LeaveA
             | RetA
             | FunBegin VarIdf Integer -- function name , stack size
             | FunHead VarIdf
             | FunEnd
             | Label String
             | ProHead String
             | ProEnd
             | Ascii String
             | Je VarOp
             | Jmp VarOp
             | LgAndInt VarOp VarOp
             | LgOrInt VarOp VarOp
             | LgNotInt VarOp
               deriving (Show)

movIntRegReg :: RegName -> RegName -> AsmCode
movIntRegReg a b = MovInt (Register a) (Register b)

type AsmProg = [AsmCode]

showAsmProg :: AsmProg -> String
showAsmProg p = intercalate "\n" (map show p)

class Asm a where
    asmCode :: VarVm -> VarRef -> a -> IO AsmProg

instance Asm CValExpr where
    asmCode vm r c = return $ cVSf2Asm $ getCValSf c

val2rb :: VarVm -> VarOp -> VarOp
val2rb vm (VsStack i)  = (RegBase "ebp" $ negate i - vmSize vm)
val2rb vm a = a


aVa :: VarVm -> AsmCode -> AsmProg
aVa vm (MovInt x@(RegBase _ _) y@(VsStack _)) = [MovInt x (Register "eax"), 
                                          MovInt (Register "eax") (val2rb vm y)]
aVa vm (MovInt x@(VsStack _) y@(RegBase _ _)) = [MovInt (val2rb vm x) (Register "eax"),
                                              MovInt (Register "eax") y]
aVa vm (MovInt x@(Imdt _) y)                = [MovInt x (val2rb vm y)]
aVa vm (MovInt x@(VsStack _) y@(Register _)) = [MovInt (val2rb vm x) y]
aVa vm (MovInt x@(Direct _) y)  = [MovInt x (val2rb vm y)]
aVa vm (MovInt x@(Register _) y) = [MovInt x (val2rb vm y)]
aVa vm (MovInt x@(RegBase _ _) y@(Register _)) = [MovInt x y]
aVa vm (LeaInt x@(RegBase _ _) y@(VsStack _)) = [LeaInt x (Register "eax"),
                                              MovInt (Register "eax") (val2rb vm y)]
aVa vm (AddInt x@(VsStack _) y@(VsStack _)) = [MovInt (val2rb vm y) (Register "eax"), 
                                               AddInt (val2rb vm x) (Register "eax"), 
                                               MovInt (Register "eax") (val2rb vm y)]
aVa vm (SubInt x@(VsStack _) y@(VsStack _)) = [MovInt (val2rb vm y) (Register "eax"), 
                                               SubInt (val2rb vm x) (Register "eax"), 
                                               MovInt (Register "eax") (val2rb vm y)]
aVa vm (MulInt x@(VsStack _) y@(VsStack _)) = [MovInt (val2rb vm y) (Register "eax"),
                                               Imull  (val2rb vm x) (Register "eax"),
                                               MovInt (Register "eax") (val2rb vm y)]
aVa vm (DivInt x@(VsStack _) y@(VsStack _)) = [MovInt (val2rb vm y) (Register "eax"),
                                               Cltd,
                                               Idivl (val2rb vm x),
                                               MovInt (Register "eax") (val2rb vm y)]
aVa vm (GreaterInt x@(VsStack _) y@(VsStack _)) = [MovInt (val2rb vm y) (Register "eax"),
                                                   CmpInt (val2rb vm x) (Register "eax"),
                                                   Setg   (Register "al"),
                                                   MovZbl (Register "al") (Register "eax"),
                                                   MovInt (Register "eax") (val2rb vm y)]
aVa vm (LessInt x@(VsStack _) y@(VsStack _)) = [MovInt (val2rb vm y) (Register "eax"),
                                                   CmpInt (val2rb vm x) (Register "eax"),
                                                   Setl   (Register "al"),
                                                   MovZbl (Register "al") (Register "eax"),
                                                   MovInt (Register "eax") (val2rb vm y)]
aVa vm (EquelInt x@(VsStack _) y@(VsStack _)) = [MovInt (val2rb vm y) (Register "eax"),
                                                   CmpInt (val2rb vm x) (Register "eax"),
                                                   Sete   (Register "al"),
                                                   MovZbl (Register "al") (Register "eax"),
                                                   MovInt (Register "eax") (val2rb vm y)]
aVa vm (GreaterEInt x@(VsStack _) y@(VsStack _)) = [MovInt (val2rb vm y) (Register "eax"),
                                                    CmpInt (val2rb vm x) (Register "eax"),
                                                    Setge   (Register "al"),
                                                    MovZbl (Register "al") (Register "eax"),
                                                    MovInt (Register "eax") (val2rb vm y)]
aVa vm (LessEInt x@(VsStack _) y@(VsStack _)) = [MovInt (val2rb vm y) (Register "eax"),
                                                 CmpInt (val2rb vm x) (Register "eax"),
                                                 Setle  (Register "al"),
                                                 MovZbl (Register "al") (Register "eax"),
                                                 MovInt (Register "eax") (val2rb vm y)]
aVa vm (LgAndInt x@(VsStack _) y@(VsStack _)) = [MovInt (Imdt 0) (Register "eax"),
                                                 MovInt (Imdt 0) (Register "ebx"),
                                                 CmpInt (val2rb vm y) (Register "eax"),
                                                 Setne  (Register "al"),
                                                 CmpInt (val2rb vm x) (Register "ebx"),
                                                 Setne  (Register "bl"),
                                                 AddInt (Register "ebx") (Register "eax"),
                                                 MovInt (Imdt 0) (Register "ebx"),
                                                 CmpInt (Imdt 2) (Register "eax"),
                                                 Sete   (Register "bl"),
                                                 MovInt (Register "ebx") (val2rb vm y)]
aVa vm (LgOrInt x@(VsStack _) y@(VsStack _)) =  [MovInt (Imdt 0) (Register "eax"),
                                                 MovInt (Imdt 0) (Register "ebx"),
                                                 CmpInt (val2rb vm y) (Register "eax"),
                                                 Setne  (Register "al"),
                                                 CmpInt (val2rb vm x) (Register "ebx"),
                                                 Setne  (Register "bl"),
                                                 AddInt (Register "ebx") (Register "eax"),
                                                 MovInt (Imdt 0) (Register "ebx"),
                                                 CmpInt (Imdt 0) (Register "eax"),
                                                 Setne  (Register "bl"),
                                                 MovInt (Register "ebx") (val2rb vm y)]
aVa vm (LgNotInt x@(VsStack _))              = [MovInt (Imdt 0) (Register "eax"),
                                                CmpInt (val2rb vm x) (Register "eax"),
                                                Setne  (Register "al"),
                                                MovInt (Register "eax") (val2rb vm x)]
aVa vm (PushInt x@(VsStack _))               = [PushInt (val2rb vm x)]
aVa vm c = [c]

instance Asm Statement where
    asmCode = statement2Asm

movSFtoEax :: VarVm -> AsmCode
movSFtoEax vm = MovInt (RegBase "ebp" $ stFirstInt vm) (Register "eax")

movSFCmp :: VarVm -> AsmProg
movSFCmp vm = movSFtoEax vm : [CmpInt (Imdt 0) (Register "eax")]

statement2Asm :: VarVm -> VarRef -> Statement -> IO AsmProg
statement2Asm vm r (CValExpr c) = asmCode vm r c
statement2Asm vm r (StList vars st) = do
    t <- fmap' (asmCode vm' r) st
    return $ concat $ map (aVa vm') $ concat t
        where vm' = varVmAddVars vm vars
statement2Asm vm r (Return c) = do
    a <- asmCode vm r c
    return $ a ++ [movSFtoEax vm] 
statement2Asm vm r (IfElse c s1 s2) = do
    c' <- asmCode vm r c
    l1 <- newLabel r
    l2 <- newLabel r
    s1' <- asmCode vm r s1
    s2' <- asmCode vm r s2
    return $ c' ++ movSFCmp vm ++ [Je (Direct l1)]
                ++ s1' ++ [Jmp (Direct l2)] ++ [Label l1] ++ s2' ++ [Label l2]
statement2Asm vm r (If c s) = do
    c' <- asmCode vm r c
    l <- newLabel r
    s' <- asmCode vm r s
    return $ c' ++ movSFCmp vm ++ [Je (Direct l)]
                ++ s' ++ [Label l]
statement2Asm vm r (While c s) = do
    c' <- asmCode vm r c
    l1 <- newLabel r
    l2 <- newLabel r
    s' <- asmCode vm r s
    return $ [Label l1] ++ c' ++ movSFCmp vm ++ [Je (Direct l2)]
                        ++ s' ++ [Jmp (Direct l1), Label l2]
statement2Asm vm r (For c1 c2 c3 s) = do
    l1  <- newLabel r
    l2  <- newLabel r
    c1' <- asmCode vm r c1
    c2' <- asmCode vm r c2
    c3' <- asmCode vm r c3
    s'  <- asmCode vm r s
    return $ c1' ++ [Label l1] ++ c2' ++ movSFCmp vm ++ [Je (Direct l2)]
                        ++ s' ++ c3' ++ [Jmp (Direct l1), Label l2]

statementVarVmMaxSize :: VarVm -> Statement -> Integer
statementVarVmMaxSize vm (CValExpr c)       = vmSize vm + cVsSize (getCValSf c)
statementVarVmMaxSize vm (StList vars st)   = max (vmSize vm') (maximum $ map (statementVarVmMaxSize vm') st)
        where vm' = varVmAddVars vm vars
statementVarVmMaxSize vm (Return c)         = vmSize vm + cVsSize (getCValSf c)
statementVarVmMaxSize vm (IfElse c s1 s2)   = max (statementVarVmMaxSize vm (CValExpr c)) $ max (statementVarVmMaxSize vm s1) (statementVarVmMaxSize vm s2)
statementVarVmMaxSize vm (If c s)           = max (statementVarVmMaxSize vm (CValExpr c)) (statementVarVmMaxSize vm s)
statementVarVmMaxSize vm (While c s)        = max (statementVarVmMaxSize vm (CValExpr c)) (statementVarVmMaxSize vm s)
statementVarVmMaxSize vm (For c1 c2 c3 s)   = max (statementVarVmMaxSize vm (CValExpr c1)) $ 
                                              max (statementVarVmMaxSize vm (CValExpr c2)) $ 
                                              max (statementVarVmMaxSize vm (CValExpr c3)) (statementVarVmMaxSize vm s)

instance Asm Function where
    asmCode = func2Asm

func2Asm :: VarVm -> VarRef -> Function -> IO AsmProg
func2Asm vm r (Fun fname _ paras st) = do
    t <- asmCode vm' r st
    return $ fBgAsm ( FunBegin fname (statementVarVmMaxSize vm' st) ) ++ t ++ fBgAsm FunEnd
        where vm' = varVmAddParas emptyVarVm paras

fBgAsm :: AsmCode -> AsmProg
fBgAsm (FunBegin fname stsize) = [FunHead $ "_"++fname, 
                                  Label $ "_"++fname, 
                                  PushInt (Register "ebp"), 
                                  MovInt (Register "esp") (Register "ebp"), 
                                  SubInt (Imdt stsize) (Register "esp")]
fBgAsm (FunEnd) = [LeaveA, RetA]

instance Asm MiCProgram where
    asmCode = program2Asm

program2Asm :: VarVm -> VarRef -> MiCProgram -> IO AsmProg
program2Asm vm r (Program gvars funcs) = do
    t <- fmap' (asmCode emptyVarVm r) funcs
    return $ concat t ++ [ProEnd]

type Diction = IORef (Integer, [String])


newDiction :: IO Diction
newDiction = newIORef (0, [])

addString :: Diction -> String -> IO Integer
addString dic s = do (n, l) <- readIORef dic
                     writeIORef dic (n+1, s : l)
                     return n 


preloadString :: Diction -> MiCProgram -> IO MiCProgram
preloadString = pls_1

pls_1 :: Diction -> MiCProgram -> IO MiCProgram
pls_1 dic (Program a funcs) = do funcs' <- pls_2' dic funcs
                                 return (Program a funcs')

pls_2' :: Diction -> [Function] -> IO [Function]
pls_2' dic funcs = fmap' (pls_2 dic) funcs

pls_2 :: Diction -> Function -> IO Function
pls_2 dic (Fun a b c st) = do st' <- pls_3 dic st
                              return (Fun a b c st')

pls_3 :: Diction -> Statement -> IO Statement
pls_3 dic (StList v l) = do
        l' <- fmap' (pls_3 dic) l
        return (StList v l')
pls_3 dic (CValExpr c) = do
        c' <- pls_4 dic c
        return (CValExpr c')
pls_3 dic (IfElse c s1 s2) = do
        c' <- pls_4 dic c
        s1' <- pls_3 dic s1
        s2' <- pls_3 dic s2
        return (IfElse c' s1' s2')
pls_3 dic (If c s) = do
        c' <- pls_4 dic c
        s' <- pls_3 dic s
        return (If c' s')
pls_3 dic (While c s) = do
        c' <- pls_4 dic c
        s' <- pls_3 dic s
        return (While c' s')
pls_3 dic (For c1 c2 c3 s) = do
        c1' <- pls_4 dic c1
        c2' <- pls_4 dic c2
        c3' <- pls_4 dic c3
        s'  <- pls_3 dic s
        return (For c1' c2' c3' s')
pls_3 dic (Return c) = do
        c' <- pls_4 dic c
        return (Return c')

pls_4 :: Diction -> CValExpr -> IO CValExpr
pls_4 dic (P.CVal (P.StringC s)) = do
        i <- addString dic s
        return $ P.CVal (P.StringC $ "$LC" ++ show i)
pls_4 dic a@(P.CVal _) = return a
pls_4 dic (P.Binary op c1 c2) = do
        c1' <- pls_4 dic c1
        c2' <- pls_4 dic c2
        return (P.Binary op c1' c2')
pls_4 dic (P.Unary op c) = do
        c' <- pls_4 dic c
        return (P.Unary op c')
pls_4 dic (P.Assign v c) = do
        c' <- pls_4 dic c
        return (P.Assign v c')
pls_4 dic (P.FuncCall v c) = do
        c' <- fmap' (pls_4 dic) c
        return (P.FuncCall v c')
pls_4 dic (P.AssignToAddr c1 c2) = do
        c1' <- pls_4 dic c1
        c2' <- pls_4 dic c2
        return (P.AssignToAddr c1' c2')
pls_4 dic (P.AssignTo a c) = do
        c' <- pls_4 dic c
        return (P.AssignTo a c')
pls_4 dic a@(P.LoadAddr c) = return a
pls_4 dic a@(P.CValFromAddr _) = return a



fmap' :: (a -> IO b) -> [a] -> IO [b]
fmap' f [] = return []
fmap' f (a:b) = do 
    b' <- fmap' f b
    a' <- f a
    return (a':b')

class ShowAsm a where
    showAsm :: a -> String

instance ShowAsm AsmCode where
    showAsm = showA

instance ShowAsm VarOp where
    showAsm = showV

showV :: VarOp -> String
showV (Imdt x) = "$" ++ show x
showV (Direct x) = x
showV (Register x) = "%" ++ x
showV (RegBase r 0) = "(%" ++ r ++ ")"
showV (RegBase r x) = show x ++ "(%" ++ r ++ ")"
showV a = show a

s2a :: (ShowAsm a) => String -> a -> a -> String
s2a s x y = "\t" ++ s ++ "\t" ++ showAsm x ++ ", " ++ showAsm y

s1a' :: String -> String -> String
s1a' s x = "\t" ++ s ++ "\t" ++ x

s1a :: (ShowAsm a) => String -> a -> String
s1a s x = "\t" ++ s ++ "\t" ++ showAsm x


showA :: AsmCode -> String
showA (AddInt x y)  = s2a "addl" x y
showA (SubInt x y)  = s2a "subl" x y 
showA (Imull  x y)  = s2a "imull"x y
showA (CmpInt x y)  = s2a "cmpl" x y
showA (MovInt x y)  = s2a "movl" x y
showA (LeaInt x y)  = s2a "leal" x y
showA (Idivl x)     = s1a "idivl" x
showA (PushInt x)   = s1a "pushl" x
showA (Setg x)      = s1a "setg" x
showA (Setl x)      = s1a "setl" x
showA (Sete x)      = s1a "sete" x
showA (Setge x)     = s1a "setge" x
showA (Setle x)     = s1a "setle" x
showA (Setne x)     = s1a "setne" x
showA (MovZbl x y)  = s2a "movzbl" x y
showA (CallInt x)   = s1a' "call" x
showA (Je x)        = s1a "je" x
showA (Jmp x)       = s1a "jmp" x
showA (Cltd)        = "\tcltd"
showA (LeaveA)      = "\tleave"
showA (RetA)        = "\tret"
--showA (FunHead x)   = printf "\t.globl\t%s\n\t.def\t%s;\t.scl\t2;\t.type\t32;\t.endef" x x
showA (FunHead x)   = printf "\t.globl\t%s" x
showA (Label x)    = x ++ ":"
showA (ProEnd)      = "\t.ident\t\"MiC: 0.1\""
showA (ProHead x)   = printf "\t.file\t\"%s\"\n\t.text" x
showA (Ascii x)     = printf "\t.ascii\t\"%s\\0\"" $ showS_ x



showS_ :: String -> String
showS_ s = tail $ init $ show s

instance ShowAsm AsmProg where
    showAsm = showP

showP :: AsmProg -> String
showP p = intercalate "\n" (map showAsm p)

pProgramToAsm :: P.Program -> IO String
pProgramToAsm p = do
        let pro = getMiCProgram p
        dic <- newDiction
        pro' <- preloadString dic pro
        ss <- staticString dic
        ref <- emptyVarRef
        t <- asmCode emptyVarVm ref pro'
        return $ showAsm ss ++ "\n" ++  showAsm t

staticString :: Diction -> IO AsmProg
staticString dic = do
    (_, l) <- readIORef dic
    return $ addStr 0 (reverse l)

addStr :: Integer -> [String] -> AsmProg
addStr i [] = []
addStr i (s:l)  = [Label ("LC"++show i), Ascii s] ++ addStr (i+1) l

readFrom :: String -> IO String
readFrom file =
    do p <- readFile file
       case P.readProgram p of
       --case P.readMiC P.parseCValExpr p of
       --case P.readMiC P.parseStatement p of
       --case P.readMiC P.parseFunction p of
            Left err -> return err
            --Right val -> return $ show $ getMiCProgram val
            --Right val -> return $ show (getStatement val) ++ "\n\n" ++ showAsmProg ( asmCode emptyVarVm $ getStatement val)
            --Right val -> return $ show (getFunction val) ++ "\n\n" ++ showAsmProg ( asmCode emptyVarVm $ getFunction val)
            --Right val -> return $ show (getMiCProgram val) ++ "\n\n" ++ showAsmProg ( asmCode emptyVarVm $ getMiCProgram val)
            --Right val -> return $ showAsm (ProHead file) ++ "\n" ++ showAsm ( asmCode emptyVarVm $ getMiCProgram val)
            Right val -> do 
                            asm <- pProgramToAsm val
                            return $ showAsm (ProHead file) ++ "\n" ++ asm
            --Right val -> return ( show (getCValSf val) ++ "\n" ++ show val ++ "\n"  ++ showAsmProg ( asmCode emptyVarVm val))

writeToFile ::String -> IO ()
writeToFile file = do
    do p <- readFile file
       case P.readProgram p of
            Left err -> do let errmsg = "error" ++ "\n\n---------------------------\n\n" ++ err
                           writeFile (file ++ ".s") errmsg
                           writeFile (file ++ ".dic") errmsg
                           writeFile (file ++ ".pro") errmsg
                           writeFile (file ++ ".tem") errmsg
            Right val -> do
                            writeFile (file++".parse") $ show val
                            let pro = getMiCProgram val
                            dic <- newDiction
                            dic' <- readIORef dic
                            pro' <- preloadString dic pro
                            ss <- staticString dic
                            ref <- emptyVarRef
                            t <- asmCode emptyVarVm ref pro'
                            writeFile (file++".dic") $ show dic' ++ "\n\n" ++ show ss
                            writeFile (file++".pro") $ show pro'
                            writeFile (file++".tem") $ showAsmProg t
                            asm <- pProgramToAsm val
                            writeFile (file ++ ".s") $ showAsm (ProHead file) ++ "\n" ++ asm

main :: IO ()
main = do
         (file:_) <- getArgs
         readFrom file >>= putStrLn
         writeToFile file
         
