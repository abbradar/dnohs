module Codegen.Utils where

import Data.Word
import LLVM.General.AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.CallingConvention as CC
import LLVM.General.AST.Type
import LLVM.General.AST.IntegerPredicate

type Instructions = [Named Instruction]

defBits :: Word32
defBits = 64

int :: Type
int = IntegerType defBits

intv :: Integer -> C.Constant
intv = C.Int defBits

intv8 :: Integer -> C.Constant
intv8 = C.Int 8

istr' :: Integer -> C.Constant
istr' = C.Int 32

istr :: Integer -> Operand
istr = constant . istr'

getElementPtr :: Instruction
getElementPtr = GetElementPtr { inBounds = True
                              , address = error "No address specified"
                              , indices = []
                              , metadata = []
                              }

getElementPtr' :: Operand -> [Operand] -> Instruction
getElementPtr' from inds = getElementPtr { address = from, indices = istr 0 : inds }

getElementPtrUnsafe' :: Operand -> [Operand] -> Instruction
getElementPtrUnsafe' from inds = (getElementPtr' from inds) { inBounds = False }

load :: Instruction
load = Load { volatile = False
            , address = error "No address specified"
            , maybeAtomicity = Nothing
            , alignment = 0
            , metadata = []
            }

load' :: Operand -> Instruction
load' a = load { address = a }

store :: Instruction
store = Store { volatile = False
              , address = error "No address specified"
              , value = error "No value specified"
              , maybeAtomicity = Nothing
              , alignment = 0
              , metadata = []
              }

store' :: Operand -> Operand -> Instruction
store' to from = store { address = to, value = from }

call :: Instruction
call = Call { isTailCall = False
            , callingConvention = CC.C
            , returnAttributes = []
            , function = error "No function specified"
            , arguments = []
            , functionAttributes = []
            , metadata = []
            }

call' :: Operand -> [Operand] -> Instruction
call' fun args = call { function = Right fun
                      , arguments = map (, []) args
                      }

add :: Instruction
add = Add { nsw = False
          , nuw = False
          , operand0 = error "No first operand supplied"
          , operand1 = error "No second operand supplied"
          , metadata = []
          }

add' :: Operand -> Operand -> Instruction
add' a b = add { operand0 = a, operand1 = b }

sub :: Instruction
sub = Sub { nsw = False
          , nuw = False
          , operand0 = error "No first operand supplied"
          , operand1 = error "No second operand supplied"
          , metadata = []
          }

sub' :: Operand -> Operand -> Instruction
sub' a b = add { operand0 = a, operand1 = b }

mul :: Instruction
mul = Mul { nsw = False
          , nuw = False
          , operand0 = error "No first operand supplied"
          , operand1 = error "No second operand supplied"
          , metadata = []
          }

mul' :: Operand -> Operand -> Instruction
mul' a b = mul { operand0 = a, operand1 = b }

icmp :: Instruction
icmp = ICmp { iPredicate = error "No predicate specified"
            , operand0 = error "No first operand supplied"
            , operand1 = error "No second operand supplied"
            , metadata = []
            }

icmp' :: IntegerPredicate -> Operand -> Operand -> Instruction
icmp' pr a b = icmp { iPredicate = pr
                    , operand0 = a
                    , operand1 = b
                    }

bitcast :: Instruction
bitcast = BitCast { operand0 = error "No operand specified"
                  , type' = error "No type specified"
                  , metadata = []
                  }

bitcast' :: Operand -> Type -> Instruction
bitcast' op typ = bitcast { operand0 = op
                          , type' = typ
                          }

select :: Instruction
select = Select { condition' = error "No condition specified"
                , trueValue = error "No first operand supplied"
                , falseValue = error "No second operand supplied"
                , metadata = []
                }

select' :: Operand -> Operand -> Operand -> Instruction
select' cond a b = select { condition' = cond
                          , trueValue = a
                          , falseValue = b
                          }

assign :: Type -> Name -> Instruction
assign typ from = bitcast' (local typ from) typ

local :: Type -> Name -> Operand
local = LocalReference

constant :: C.Constant -> Operand
constant = ConstantOperand

retBlock :: Name -> Instructions -> BasicBlock
retBlock name cmds = BasicBlock name cmds (Do $ Ret Nothing [])

brBlock :: Name -> Instructions -> Name -> BasicBlock
brBlock name cmds to = BasicBlock name cmds (Do $ Br to [])

sizeof :: Type -> [C.Constant] -> C.Constant
sizeof typ inds = C.PtrToInt szp int
  where szp = C.GetElementPtr { C.inBounds = False
                              , C.address = C.Null $ ptr typ
                              , C.indices = istr' 0 : inds
                              }

sizeof' :: Type -> Integer -> C.Constant
sizeof' typ n = C.PtrToInt szp int
  where szp = C.GetElementPtr { C.inBounds = False
                              , C.address = C.Null $ ptr typ
                              , C.indices = [istr' n]
                              }

globalRef :: Type -> Name -> Operand
globalRef typ nam = ConstantOperand $ C.GlobalReference typ nam

voidptr :: Type
voidptr = ptr i8
