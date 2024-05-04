datatype tree = empty | node of int * tree * tree

fun arrange filename =
    let 
            val ins = TextIO.openIn filename (*open the file*)
            val _ = TextIO.inputLine ins (*population not needed*)
            val line = TextIO.inputLine ins (*read the line*)				
            val _ = TextIO.closeIn ins (*close the file*)
            val treelist = case line of
                NONE => []
                | SOME s => map (fn x => valOf (Int.fromString x)) (String.tokens (fn c => c = #" ") s)
            (*done until now: read file, created int list *)
            
            fun create_tree [] = (empty, [])
            | create_tree (0::rest) = (empty, rest)
            | create_tree (n::rest) =
                    let
                        val (left, xsAfterLeft) = create_tree rest
                        val (right, xsAfterRight) = create_tree xsAfterLeft
                    in
                        (node(n, left, right), xsAfterRight)
                    end

            
            fun lexsmallest empty = empty
            | lexsmallest (node(n, left, right)) =
                    let
                        val newleft = lexsmallest left
                        val newright = lexsmallest right

                        fun extract_int empty = NONE
                        | extract_int (node(n, _, _)) = SOME n

                        val left_val = extract_int newleft
                        val right_val = extract_int newright

                        val swapped = 
                            (case (left_val, right_val) of
                                (NONE, NONE) => false
                            | (NONE, SOME _) => false
                            | (SOME _, NONE) => true
                            | (SOME l, SOME r) => l > r)
                    in
                        if swapped then
                            node(n, newright, newleft)
                        else
                            node(n, newleft, newright)
                    end


            fun print_tree empty = ()
                | print_tree (node(n,left,right)) = (
                    print_tree left;
                    print (Int.toString n ^ " ");
                    print_tree right
                )

            val (tree,_) = create_tree treelist
            val lexsmallesttree = lexsmallest tree
            
            in
            print_tree lexsmallesttree
            end