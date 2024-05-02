Calculating the minimum difference between the sum of any subset of an unsorted list and the sum of the remaining numbers is a complex problem that can be solved using dynamic programming.

Here's a step-by-step plan:

Calculate the total sum of the list.
Create a boolean array dp of size total_sum + 1 and initialize all elements as false. The purpose of this array is to store whether a sum i can be formed using the elements of the list.
Set dp[0] to true, because a sum of 0 can always be formed without including any elements.
Iterate over the list and for each number num, iterate over the dp array from total_sum to num (in reverse order) and set dp[i] to true if dp[i - num] is true.
After filling the dp array, iterate over the first total_sum / 2 elements of dp (from total_sum / 2 to 0) and find the maximum i such that dp[i] is true. This i is the sum of the subset that gives the minimum difference.
The minimum difference is total_sum - 2 * i.
Here's how you can implement this in SML:

This function calculates the minimum difference for an unsorted list by considering all possible subsets. However, it can be computationally expensive for large lists because it has a time complexity of O(n * sum) and a space complexity of O(sum), where n is the number of elements in the list and sum is the total sum of the elements.

fun min_diff_subset numberslist =
    let
        val total_sum = foldl (op +) 0 numberslist
        val dp = Array.array (total_sum + 1, false)
        val _ = Array.update (dp, 0, true)
        val _ = List.app (fn num =>
            for i = total_sum downto num do
                if Array.sub (dp, i - num) then Array.update (dp, i, true) else ()
            ) numberslist
        val subset_sum = foldl (fn (i, max_i) =>
            if Array.sub (dp, i) then i else max_i
        ) 0 (List.tabulate ((total_sum div 2) + 1, fn i => i))
    in
        total_sum - 2 * subset_sum
    end

fun fairseq filename =
    let 
        val ins = TextIO.openIn filename
        val _ = TextIO.inputLine ins
        val line = TextIO.inputLine ins
        val _ = TextIO.closeIn ins
        val numberslist = case line of
            NONE => []
          | SOME s => List.mapPartial Int.fromString (String.tokens (fn c => c = #" ") s)
        val result = min_diff_subset numberslist
    in
        print (Int.toString result ^ "\n")
    end