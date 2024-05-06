datatype tree = empty | node of int * tree * tree

fun arrange filename =
    let 
        val ins = TextIO.openIn filename
        val _ = TextIO.inputLine ins
        val line = TextIO.inputLine ins
        val _ = TextIO.closeIn ins
        val treelist = case line of
            NONE => []
            | SOME s => map (fn x => valOf (Int.fromString x)) (String.tokens (fn c => c = #" ") s)

        (* Creating the tree from the list *)
        fun create_tree [] = (empty, [])
          | create_tree (0::rest) = (empty, rest)
          | create_tree (n::rest) =
                let
                    val (left, xsAfterLeft) = create_tree rest
                    val (right, xsAfterRight) = create_tree xsAfterLeft
                in
                    (node(n, left, right), xsAfterRight)
                end

        (* Function arrange_tree *)
        fun arrange_tree (empty) = (empty, NONE)
          | arrange_tree (node(x, left, right)) =
                let
                    val (l', minl) = arrange_tree left
                    val (r', minr) = arrange_tree right
                in  
                    case (minl, minr) of
                        (NONE, NONE) => (node (x, l', r'), SOME x)
                      | (NONE, SOME minr) =>
                            if minr < x then (node (x, r', l'), SOME minr)
                            else (node (x, l', r'), SOME x)
                      | (SOME minl, NONE) =>
                            if minl < x then (node (x, l', r'), SOME minl)
                            else (node (x, r', l'), SOME x)
                      | (SOME minl, SOME minr) =>
                          if minr < minl then (node (x, r', l'), SOME minr)
                          else (node (x, l', r'), SOME minl)
                end

        (* Print the in-order traversal of the tree *)
        fun print_tree empty = ()
          | print_tree (node(n,left,right)) = (
                print_tree left;
                print (Int.toString n ^ " ");
                print_tree right
            )

        val (tree,_) = create_tree treelist
        val (arranged_tree, _) = arrange_tree tree
    in
        print_tree arranged_tree
    end
