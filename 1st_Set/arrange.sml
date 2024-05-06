datatype tree = empty | node of int * tree * tree

fun arrange filename =
    let 
        val ins = TextIO.openIn filename
        val _ = TextIO.inputLine ins (* Ignore the first line population n is not needed*)
        val line = TextIO.inputLine ins
        val _ = TextIO.closeIn ins
        val treelist = case line of
            NONE => [] (* If the line is empty, return an empty list *)
            | SOME s => map (fn x => valOf (Int.fromString x)) (String.tokens (fn c => c = #" ") s) (* Parse the line into a list of integers *)

        (* Creating the tree from the list | Given the tree in a preorder list *) 
        fun create_tree [] = (empty, [])
          | create_tree (0::rest) = (empty, rest)
          | create_tree (n::rest) =
                let
                    val (left, xsAfterLeft) = create_tree rest
                    val (right, xsAfterRight) = create_tree xsAfterLeft
                in
                    (node(n, left, right), xsAfterRight)
                end

(* Arranging the tree in order to get the smallest value lexicographically tree in in order form *)
fun arrange_tree empty = (empty, NONE)
  | arrange_tree (node(v, left_subtree, right_subtree)) =
        let
            val (arranged_left, left_min) = arrange_tree left_subtree
            val (arranged_right, right_min) = arrange_tree right_subtree

            fun swap () = (node(v, arranged_right, arranged_left), right_min)
            fun retain () = (node(v, arranged_left, arranged_right), left_min)

            val result =
                case (left_min, right_min) of
                    (NONE, NONE) => (node(v, arranged_left, arranged_right), SOME v)
                  | (NONE, SOME rm) => if rm < v then swap () else retain ()
                  | (SOME lm, NONE) => if lm < v then retain () else swap ()
                  | (SOME lm, SOME rm) => if rm < lm then swap () else retain ()
        in
            result
        end

        (* Print the in-order traversal of the tree *)
        fun print_tree empty = ()
          | print_tree (node(n,left,right)) = (
                print_tree left;
                print (Int.toString n ^ " ");
                print_tree right
            )

        val (tree, _) = create_tree treelist
        val (arranged_tree, _) = arrange_tree tree
    in
        print_tree arranged_tree
    end
