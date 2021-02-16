module AST where

import Data.Fix
import Data.String
import Text.Show.Deriving

type Value = Double
type Name = String

data ExprF a = Value Value
             | Variable Name
             | Plus a a
             | Minus a a
             | Multiply a a
             | Divide a a
             | Sin a
             | Cos a
             | Exp a
             | Log a
             deriving (Functor, Foldable, Traversable)

type Expr = Fix ExprF

deriving instance (Show a) => Show (ExprF a)
$(deriveShow1 ''ExprF)

instance Num Expr where
    fromInteger = Fix . Value . fromInteger
    x + y = Fix $ Plus x y
    x - y = Fix $ Minus x y
    x * y = Fix $ Multiply x y
    abs = id -- supress warning
    signum = id -- supress warning

instance Fractional Expr where
    fromRational = Fix . Value . fromRational
    x / y = Fix $ Divide x y

instance Floating Expr where
    pi   = Fix $ Value pi
    sin  = Fix . Sin
    cos  = Fix . Cos
    exp  = Fix . Exp
    log  = Fix . Log

instance IsString Expr where
    fromString = Fix . Variable

eval :: Expr -> Expr
eval = foldFix $ Fix . \case
    Plus     (Fix (Value x)) (Fix (Value y)) -> Value $ x + y
    Plus     (Fix (Value 0)) (Fix x)         -> x
    Plus     (Fix x)         (Fix (Value 0)) -> x
    Minus    (Fix (Value x)) (Fix (Value y)) -> Value $ x - y
    Minus    (Fix x)         (Fix (Value 0)) -> x
    Multiply (Fix (Value x)) (Fix (Value y)) -> Value $ x * y
    Multiply (Fix (Value 0)) _               -> Value 0
    Multiply _               (Fix (Value 0)) -> Value 0
    Multiply (Fix (Value 1)) (Fix x)         -> x
    Multiply (Fix x)         (Fix (Value 1)) -> x
    Divide   (Fix (Value x)) (Fix (Value y)) -> Value $ x / y
    Divide   (Fix (Value 0)) _               -> Value 0
    Sin      (Fix (Value x))                 -> Value $ sin x
    Cos      (Fix (Value x))                 -> Value $ cos x
    Exp      (Fix (Value x))                 -> Value $ exp x
    Log      (Fix (Value x))                 -> Value $ log x
    x -> x

diff :: Expr -> Name -> Expr
diff (Fix e) withRespectTo = eval $
    case e of
      Value _      -> 0
      Variable x   -> if withRespectTo == x then 1 else 0
      Plus     x y -> d x + d y
      Minus    x y -> d x - d y
      Multiply x y -> d x * y + x * d y
      Divide   x y -> (d x * y - x * d y) / (y ** 2)
      Sin      x   -> cos x * d x
      Cos      x   -> negate (sin x) * d x
      Exp      x   -> exp x * d x
      Log      x   -> d x / x
    where d = flip diff withRespectTo
