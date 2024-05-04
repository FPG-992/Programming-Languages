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
