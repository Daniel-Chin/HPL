type varAnnotation = | VarAnnotation of (string * int);;

type ast =
| DecVar      of (varAnnotation * ast)
| ProcCall    of (ast * ast)
| Malloc      of (varAnnotation)
| VarAssign   of (varAnnotation * ast)
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
| VarIdt      of (varAnnotation)
| FieldSeek   of (ast * ast)
| ProcDef     of (varAnnotation * ast)

| LiteralBool of (bool)
| IsEqual     of (ast * ast)
| IsLessThan  of (ast * ast)
| Block       of (ast)
| Hold
;;
