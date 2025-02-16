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
		(*done until now: read file, created int list and calculated total_sum*)
		
		val dp = Array.array(total_sum + 1 , false)
		val _ = Array.update (dp,0,true) (*we can have a sum of 0 with an empty list, always true*)
		fun initializeDP num i =
			if i < num then ()
			else (
				if Array.sub (dp,i-num) then Array.update (dp,i,true) else ();
				initializeDP num (i-1) (*do this recursively since we have no for*)
			)
		val _ = List.app (fn num => initializeDP num total_sum) numberslist
		val result = let
			fun find_min_diff i =
				if Array.sub (dp,i) then abs(total_sum - 2 * i) else find_min_diff(i-1)
			in
				find_min_diff(total_sum div 2)
			end
					
		in
			print (Int.toString result ^ "\n")
		end