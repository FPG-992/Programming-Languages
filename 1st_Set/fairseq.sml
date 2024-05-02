fun fairseq filename =
	let 
		val ins = TextIO.openIn filename (*open the file*)
		val _ = TextIO.inputLine ins (*population not needed*)
		val line = TextIO.inputLine ins (*read the line*)				
		val _ = TextIO.closeIn ins (*close the file*)
		val numberslist = case line of
			NONE => []
			| SOME s => List.mapPartial Int.fromString (String.tokens (fn c => c = #" ") s) (*split the line into a list of integers*)
		val total_sum = foldl (op +) 0 numberslist (*sum the list of integers into total_sum*)
		fun find_min_diff (_, min_diff) [] = min_diff
		  | find_min_diff(current_sum,min_diff) (x::xs) =
			let 
				val new_sum = current_sum + x
				val new_diff = abs(total_sum - 2*new_sum)
			in
				find_min_diff(new_sum, Int.min(min_diff, new_diff)) xs
			end
		val result = find_min_diff(0, total_sum) numberslist
		in
			print (Int.toString result ^ "\n")
		end