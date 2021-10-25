Specialties:
    Var IDT starts with uppercase while Field IDT starts with lowercase. 
    ignores \n. THE_END. This is to allow line break that makes things prettier. 
    no library importing
maybe specialties:
    `val` field is exposed to users of miniOO. 
tricks
    var_name is replaced with var_id
    *Loc* is object_id
        -1 means *null*
    *Env* is var_id -> object_id
    States and Configurations are not explicitly represented - they are implicit in the ocaml-based interpreter state. 
    Control is represented as a node in the AST. 
    Heap is a list of heapRows. The list index is the object_id. A heapRow is JustValue | map. Maps are field_name -> *Tva*. JustValue has `val`. 
    heapGet out-of-*dom* returns *error*. 

todo:
    test association precedences. 
