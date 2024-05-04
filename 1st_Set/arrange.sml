datatype tree = empty | node of int * tree * tree

fun arrange filename =
    let 
            val ins = TextIO.openIn filename (*open the file*)
            val _ = TextIO.inputLine ins (*population not needed*)
            val line = TextIO.inputLine ins (*read the line*)				
            val _ = TextIO.closeIn ins (*close the file*)
            val treelist = case line of
                NONE => []
                | SOME s => List.mapPartial Int.fromString (String.tokens (fn c => c = #" ") s) (*split the line into a list of integers*)
            (*done until now: read file, created int list *)
            
            fun create_tree treelist =
                case treelist of
                    [] => (empty,[])
                    | 0::rest => (empty,rest)
                    | n::rest => let 
                        val (left,rest1) = create_tree rest
                        val (right,rest2) = create_tree rest1
                    in 
                        (node(n,left,right),rest2)
                    end
            
            fun print_tree empty = ()
                | print_tree (node(n,left,right)) = (
                    print_tree left;
                    print (Int.toString n ^ " ");
                    print_tree right
                )
            
            val (tree,_) = create_tree treelist (*create the tree*)
            
            in
                print_tree tree
            end

            