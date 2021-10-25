Specialties:
    Var IDT starts with uppercase while Field IDT starts with lowercase. 
    ignores \n. THE_END. This is to allow line break that makes things prettier. 
    no library importing
tricks
    var_name is replaced with var_id
    `val` field implemented as empty string field_name
    *Loc* is object_id
        -1 means *null*
    *Env* is var_id -> object_id
    States and Configurations are not explicitly represented - they are implicit in the ocaml-based interpreter state. 
    Control is represented as a node in the AST. 
    Heap is a list of maps. The list index is the object_id. Maps are field_name -> *Tva*. 
    heapGet out-of-*dom* returns *error*. 

todo:
    test association precedences. 
