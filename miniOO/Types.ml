type ast =
| DecVar      of (string * ast)
| ProcCall    of (ast * ast)
| Malloc      of (string)
| VarAssign   of (string * ast)
| FieldAssign of (ast * ast * ast)
| Skip
| FirstThen   of (ast * ast)
| WhileLoop   of (ast * ast)
| IfThenElse  of (ast * ast * ast)
| Parallel    of (ast * ast)
| Atomic      of (ast)

| FieldIdt    of (string)
| LiteralNum  of (int)
| Minus       of (ast * ast)
| Null
| VarIdt      of (string)
| FieldSeek   of (ast * ast)
| ProcDef     of (string * ast)

| LiteralBool of (bool)
| IsEqual     of (ast * ast)
| IsLessThan  of (ast * ast)
;;
