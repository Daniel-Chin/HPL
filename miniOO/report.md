Make a PPT. 
fix test example 1 - 3

Specialties:
    Var IDT starts with uppercase while Field IDT starts with lowercase. 
    ignores \n. THE_END. This is to allow line break that makes things prettier. 
    no library importing
    ||| (parallelism) randomly swaps the two sub-controls for each transition. Decision is based on random bools from the OS. 
maybe specialties:
    `val` field is exposed to users of miniOO. 
    Parsing error
    ```
    Unknown error. Prabably parsing error?
    lexbuf is at char # 120
    (Use ctrl+alt+G in VSCode to seek char pos.)
    Fatal error: exception MENHIR.MenhirBasics.Error
    ```
    find typos this way. 
tricks
    var_name is replaced with var_id
    *Loc* is object_id
        -1 means *null*
    *Env* is var_id -> object_id
    States and Configurations are not explicitly represented - they are implicit in the ocaml-based interpreter state. 
    Control is represented as a node in the AST. 
    Heap is a list of heapRows. The list index is the object_id. A heapRow is JustValue | Everything. JustValue has only field `val`. Everything is a map of field_name -> *Tva*, but lookup failure returns *null*. 
    heapGet out-of-*dom* returns *error*. 
    heapSet out-of-*dom* raises HeapOutOfDom. 
    "Hold" AST node for stack printing. 
remarks:
    every recursive function turned out to be accidentally tail-recursive...

todo:
    test association precedences. 
