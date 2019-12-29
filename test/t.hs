{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

import System.Process
import System.FilePath
import System.IO.Temp
import Data.Bits
import Data.Word
import Data.Int
import Prelude hiding ((/))
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Control.Arrow
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import Text.RawString.QQ

data Expr
    = EOp Op Expr Expr
    | EInt Int32
    deriving Show

data Op = Add | Sub | Mul | Div | Mod
    deriving (Eq, Show)

instance Num Expr where
    (+) = EOp Add
    (-) = EOp Sub
    (*) = EOp Mul
    abs = undefined
    signum = undefined
    fromInteger = EInt . fromInteger

infixl 7 %
(%) :: Expr -> Expr -> Expr
(%) = EOp Mod

infixl 7 /
(/) :: Expr -> Expr -> Expr
(/) = EOp Div

instance Arbitrary Expr where
    arbitrary = sized expr
    shrink (EOp op a b) = [a, b] ++ [EOp op a' b' | (a', b') <- shrink (a, b)]
    shrink _ = []

instance Arbitrary Op where
    arbitrary = frequency . map (second pure) $ [(1,Add),(1,Sub),(1,Mul),(1,Div),(1,Mod)]

expr :: Int -> Gen Expr
expr 0 = EInt <$> arbitrary
expr n | n > 0 = frequency $
    [ (2, EOp <$> arbitrary <*> subexpr <*> subexpr)
    , (1, EInt <$> arbitrary)
    ]
  where
    subexpr = expr (n - 1)

depth :: Expr -> Int
depth (EOp _ a b) = max (depth a) (depth b) + 1
depth _ = 0

pExpr :: Expr -> String
pExpr (EInt i) = show i
pExpr (EOp op a b) = "(" ++ pExpr a ++ " " ++ pOp op ++ " " ++ pExpr b ++ ")"

pOp :: Op -> String
pOp = \case
    Add -> "+"
    Sub -> "-"
    Mul -> "*"
    Div -> "/"
    Mod -> "%"

hasDivByZero :: Expr -> Bool
hasDivByZero (EInt _) = False
hasDivByZero (EOp op a b)
    =  hasDivByZero a
    || hasDivByZero b
    || ((op == Div || op == Mod) && eval b == 0)

eval :: Expr -> Int32
eval (EInt i) = i
eval (EOp op a b) = evalOp op (eval a) (eval b)

data Instr
    = IPush Word8
    | IShift Word8
    | IOp Op
    | IPop
    | IDup
    | ISwap
    deriving Show

newtype Prog = Prog [Instr]
    deriving Show

instance Arbitrary Prog where
    arbitrary = pure $ Prog example

pProg :: Prog -> String
pProg (Prog p) = unlines $ map pInstr p

pInstr :: Instr -> String
pInstr = \case
    IPush w  -> "push " ++ show w
    IShift w -> "shift " ++ show w
    IOp op   -> "op " ++ show op
    IPop     -> "pop"
    IDup     -> "dup"
    ISwap    -> "swap"

data CState = CState
    { stack  :: [Int32]
    , outErr :: Int
    }

type Z a = StateT CState (Writer [String]) a

runZ :: Z () -> [String]
runZ m = snd $ runWriter $ evalStateT m (CState [] 0)

runEZ :: ExceptT () (StateT CState (Writer [String])) () -> Z ()
runEZ m = runExceptT m >>= \case
    Left  _ -> modify $ \s -> s { outErr = 1 }
    Right _ -> modify $ \s -> s { outErr = 0 }

emit :: String -> Z ()
emit s = tell [s]

exec :: [Instr] -> Z ()
exec p = mapM_ (\(k, i) -> execInstr i >> assertState k) $ zip [1..] p

execInstr :: Instr -> Z ()
execInstr = \case
    IPush w -> do
        execPush w
        emit $ "#4 sw = " ++ show w ++ "; btn[1] = 1;"
        emit $ "#4 btn[1] = 0;"
    IShift w -> do
        execShift w
        emit $ "#4 sw = " ++ show w ++ "; btn[2] = 1;"
        emit $ "#4 btn[2] = 0;"
    IOp op -> do
        execOp op
        emit $ "#4 sw = " ++ opVal op ++ "; btn[3] = 1;"
        emit $ "#4 btn[3] = 0;"
        emit $ "#" ++ if elem op [Div, Mod] then "136" else "4"
    IPop -> do
        execPop
        emit $ "#4 sw = 3'b101; btn[3] = 1;"
        emit $ "#4 btn[3] = 0;"
    IDup -> do
        execDup
        emit $ "#4 sw = 3'b110; btn[3] = 1;"
        emit $ "#4 btn[3] = 0;"
    ISwap -> do
        execSwap
        emit $ "#4 sw = 3'b111; btn[3] = 1;"
        emit $ "#4 btn[3] = 0;"
        emit $ "#4"

execPush :: Word8 -> Z ()
execPush w = runEZ $ do
    s@CState{..} <- get
    when (length stack == 511) $
        throwError ()
    put $ s { stack = extendZ w : stack }

execShift :: Word8 -> Z ()
execShift w = runEZ $ do
    s@CState{..} <- get
    when (null stack) $
        throwError ()
    put $ s { stack = extendL w (head stack) : tail stack }

execOp :: Op -> Z ()
execOp op = runEZ $ do
    s@CState{..} <- get
    when (length stack < 1) $
        throwError ()
    when (elem op [Div, Mod] && head stack == 0) $
        throwError ()
    put $ s { stack = evalOp op (head $ tail stack) (head stack) : tail (tail stack) }

execPop :: Z ()
execPop = runEZ $ do
    s@CState{..} <- get
    when (null stack) $
        throwError ()
    put $ s { stack = tail stack }

execDup :: Z ()
execDup = runEZ $ do
    s@CState{..} <- get
    when (null stack || length stack == 511) $
        throwError ()
    put $ s { stack = head stack : stack }

execSwap :: Z ()
execSwap = runEZ $ do
    s@CState{..} <- get
    when (length stack < 1) $
        throwError ()
    put $ s { stack = head (tail stack) : head stack : tail (tail stack) }

evalOp :: Op -> Int32 -> Int32 -> Int32
evalOp = \case
    Add -> (+)
    Sub -> (-)
    Mul -> (*)
    Div -> quot
    Mod -> rem

opVal :: Op -> String
opVal = \case
    Add -> "3'b000"
    Sub -> "3'b001"
    Mul -> "3'b010"
    Div -> "3'b011"
    Mod -> "3'b100"

extendZ :: Word8 -> Int32
extendZ w = foldr (\b -> if testBit w b then flip setBit b else id) zeroBits [0..7]

extendL :: Word8 -> Int32 -> Int32
extendL w i = shiftL i 8 .|. extendZ w

assertState :: Int -> Z ()
assertState i = do
    CState{..} <- get
    emit $ "`assert(out_err == " ++ show outErr ++ ", " ++ show i ++ ")"
    when (not $ null stack) $
        emit $ "`assert(out_top == " ++ show (head stack) ++ ", " ++ show i ++ ")"
    emit $ "`assert(out_size == " ++ show (length stack) ++ ", " ++ show i ++ ")"
    emit $ "`assert(out_empty == " ++ (if null stack then "1" else "0") ++ ", " ++ show i ++ ")"

mkTest :: [Instr] -> [String]
mkTest p = runZ $ assertState 0 >> exec p

mkTest' :: Prog -> String
mkTest' (Prog p) = prologue ++ unlines (map ("    " ++) $ mkTest p) ++ epilogue

example :: [Instr]
example =
    [ IPush 7
    , IPush 5
    , IOp Div
    ]

compile :: Expr -> Prog
compile = undefined

runTest
    :: String     -- the test
    -> [String]   -- paths to module files
    -> IO String -- test output
runTest test mods = withSystemTempDirectory "testrun" $ \dir -> do
    let vPath = dir </> "tb.v"
        tbPath = dir </> "tb"
    writeFile vPath test
    callProcess "iverilog" $ ["-o", tbPath, vPath] ++ mods
    --(code, stdout, stderr) <- readProcessWithExitCode "vvp" [tbPath] ""
    readProcess "vvp" [tbPath] ""

main :: IO ()
main = quickCheck $ prop_RunProg

prop_RunProg :: Prog -> Property
prop_RunProg p@(Prog p') =
    collect (length p') $
    counterexample ("program:\n" ++ pProg p) $
    monadicIO $ do
        let testVerilog = mkTest' p
        vvpOut <- run $ runTest testVerilog ["../calc.v", "../div.v"]
        monitor $ counterexample $ "verilog:\n" ++ testVerilog
        monitor $ counterexample $ "test output:\n" ++ vvpOut
        assert $ last (lines vvpOut) == "PASS"

prop_CalcExpr :: Expr -> Property
prop_CalcExpr e = let p = compile e in
    collect (depth e) $
    not (hasDivByZero e) && depth e > 1 ==>
    counterexample ("expression: " ++ pExpr e) $
    counterexample ("program:\n" ++ pProg p) $
    monadicIO $ do
        let testVerilog = mkTest' p
        vvpOut <- run $ runTest testVerilog ["../calc.v", "../div.v"]
        monitor $ counterexample $ "verilog:\n" ++ testVerilog
        monitor $ counterexample $ "test output:\n" ++ vvpOut
        assert $ last (lines vvpOut) == "PASS"

prologue :: String
prologue = [r|
`define assert(expr, i) \
        if (!(expr)) begin \
            $display("ASSERTION FAILED: expr at line %d after instruction %d", `__LINE__, i); \
            $finish; \
        end

module test;

reg clk = 0;
reg [7:0] sw = 0;
reg [3:0] btn = 0;

wire out_err;
wire [31:0] out_top;
wire [6:0] out_size;
wire out_empty;

calc c (clk, sw, btn, out_err, out_top, out_size, out_empty);

always #2 clk = !clk;
initial begin
    $monitor("At time %t, top = %0d, size = %0d, sw = %0d, btn = %0b", $time, out_top, out_size, sw, btn);
end

initial begin
    #1
|]

epilogue :: String
epilogue = [r|
    $display("PASS"); $finish;
end

endmodule
|]
