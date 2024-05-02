fun fairseq filename =
    let
        val ins = TextIO.openIn filename (*open the file given*)
        val _ = TextIO.inputLine ins (* we dont care about the population of the numbers *)
        val line = TextIO.inputLine ins (* get the line with the numbers *)
        val numbers = case line of
                          NONE => []
                        | SOME line => List.mapPartial Option.valOf (List.map Int.fromString (String.tokens (fn c => c = #" ") line))
        val totalSum = foldl (fn (x, acc) => x + acc) 0 numbers
        fun findMinDiff (_, minDiff) [] = minDiff
          | findMinDiff (currentSum, minDiff) (x::xs) =
            let
                val newSum = currentSum + x
                val newDiff = abs (totalSum - 2 * newSum)
            in
                findMinDiff (newSum, Int.min (minDiff, newDiff)) xs
            end
        val result = findMinDiff (0, totalSum) numbers
    in
        TextIO.closeIn ins;
        result
    end;
