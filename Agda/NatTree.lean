inductive NatTree where
| End : NatTree
| Mk : (Nat -> NatTree) -> NatTree

#print NatTree.rec

def drill : List Nat -> NatTree -> NatTree
| [], t => t
| _, .End => .End
| n :: ns, .Mk f => drill ns (f n)

def zerodepth : NatTree -> Nat
| .End => .zero
| .Mk f => .succ (zerodepth (f .zero))

def pathdepth (choice : (Nat -> Nat)) : (t : NatTree) -> Nat
| .End => .zero
| .Mk f => .succ (pathdepth (λ d => choice (.succ d)) (f (choice .zero)))
