datatype tree = empty | node of int * tree * tree (*prwto vima pou kanoume panta otan grafoume ena programma einai na grafoume tis domes dedomenwn*)

fun arrange filename =
    let 
        val ins = TextIO.openIn filename (*open file*)
        val _ = TextIO.inputLine ins (* Ignore the first line population n is not needed*)
        val line = TextIO.inputLine ins (* Read the second line *)
        val _ = TextIO.closeIn ins (* Close the file *)
        val treelist = case line of (* Parse the line into a list of integers *)
            NONE => [] (* If the line is empty, return an empty list *) 
            | SOME s => map (fn x => valOf (Int.fromString x)) (String.tokens (fn c => c = #" ") s) (* Parse the line into a list of integers *)

        (* Creating the tree from the list | Given the tree in a preorder list *) 
        fun create_tree [] = (empty, []) (* If the list is empty, return an empty tree and the empty list *)
          | create_tree (0::rest) = (empty, rest) (* If the first element is 0, return an empty tree and the rest of the list *)
          | create_tree (n::rest) = (* If the first element is not 0, create a node with the value of the first element and create the left and right subtrees *)
                let
                    val (left, xsAfterLeft) = create_tree rest (* Create the left subtree *)
                    val (right, xsAfterRight) = create_tree xsAfterLeft (* Create the right subtree *)
                in
                    (node(n, left, right), xsAfterRight) (* Return the node and the rest of the list *)
                end

    (* Arranging the tree in order to get the smallest value lexicographically tree in in order form *)

    fun smallest_lex_tree tree = (* The function that arranges the tree in order to get the smallest value lexicographically tree in in order form *)
        let
            fun process_children (v, arranged_left, left_min, arranged_right, right_min) = (* The function that processes the children of the node *)
                case (left_min, right_min) of (* Check if the left and right children are empty or not *)
                    (NONE, NONE) => (node(v, arranged_left, arranged_right), SOME v)
                | (NONE, SOME rm) => (* If the left child is empty and the right child is not empty *)
                        if rm < v then (node(v, arranged_right, arranged_left), right_min) (* If the right child is smaller than the node, return the node with the right child as the left child *)
                        else (node(v, arranged_left, arranged_right), left_min) (* If the right child is not smaller than the node, remain*)
                | (SOME lm, NONE) => (* If the left child is not empty and the right child is empty *)
                        if lm < v then (node(v, arranged_left, arranged_right), left_min) (* If the left child is smaller than the node, remain *)
                        else (node(v, arranged_right, arranged_left), right_min) (* If the left child is not smaller than the node, return the node with the left child as the right child *)
                | (SOME lm, SOME rm) => (* If the left and right children are not empty *)
                        if rm < lm then (node(v, arranged_right, arranged_left), right_min) (* if the right child is smaller than the left child, swap*)
                        else (node(v, arranged_left, arranged_right), left_min) (* if the left child is smaller than the right child, remain*)

            (* what this function does is that it takes a tree and returns the smallest lexicographically tree *)
            fun helper empty = (empty, NONE) (* If the tree is empty, return an empty tree and NONE *)
            | helper (node(v, left_subtree, right_subtree)) = (* If the tree is not empty, process the children of the node *)
                    let 
                        val (arranged_left, left_min) = helper left_subtree (* Arrange the left subtree *)
                        val (arranged_right, right_min) = helper right_subtree (* Arrange the right subtree *)
                    in
                        process_children(v, arranged_left, left_min, arranged_right, right_min) (* Process the children of the node *)
                    end
        in
            helper tree (* Return the smallest value lexicographically tree *)
        end


    (* Print the in-order traversal of the tree *)
    fun print_tree empty = ()
    | print_tree (node(n,left,right)) = (
            print_tree left;
            print (Int.toString n ^ " ");
            print_tree right
        )

        val (tree, _) = create_tree treelist (* Create the tree from the list *)
        val (arranged_tree, _) = smallest_lex_tree tree (* Arrange the tree *)
    in
        print_tree arranged_tree (* Print the in-order traversal of the tree *)
    end
