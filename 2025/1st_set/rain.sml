(* gia kathe thesi i,
sthn arxh: gnwrizume pws den mporei na exei nero an aristera tou den uparxei hill
efoson uparxei aristera tou hill, to i den mporei na einai megalutero apo to aristero kai deksi hill giati tha uparxei uperxilish.
uparxei h periptwsh na exw aristera ena hill, deksia ena hill to opoio na einai iso h mikrotero apo to aristero kai na uparxei ena deksia 
>= apo ta aristerotero hill, opote na dhmiourgeitai mia "limnh" opou uparxei ena hill to opoio exei nero apo panw tou, deksia h kai aristera tou

pairnoume ena sequence apo N hills kai ta antistoixa height tous, opou stis koilades tous anamesa uparxei nero
thelume ton ogko tou nerou anamesa sta hills

sto position i, o ogkos tou nerou einai max to min height tou aristera kai deksia tou hill.

water = max(0,min(Left[i], Right[i]) - Height[i])

LeftHillI einai to hill me to megalutero height apo to i kai aristera tou
RightHillI einai to hill me to megalutero height apo to i kai deksia tou
 *)

(* grafoume thn sunarthsh pou gia kathe position i, tha vriskei to max aristero tou *)
fun MaxHillLeft hs =
    let
        fun loop ([], _, acc) = List.rev acc
            | loop (h::t, m, acc) =
                let
                    val m' = Int.max(h, m) (* to megalutero height apo ta duo *)
                in 
                    loop(t, m', m'::acc)
                end
    in
        loop (hs, 0, [])
    end

(* grafoume thn sunarthsh pou gia kathe position i, tha vriskei to max deksio tou *)
fun MaxHillRight hs =
    let
        val revHS = List.rev hs
        fun loop ([], _, acc) = acc
            | loop (h::t, m, acc) =
                let
                    val m' = Int.max(h, m) (* to megalutero height apo ta duo *)
                in 
                    loop(t, m', m'::acc)
                end
    in
        loop (revHS, 0, [])
    end            

fun WaterTrapped ([],[],[]) total = total
    | WaterTrapped (h::hs, l::ls, r::rs) total =
        let 
            val water= Int.max(0, Int.min(l, r) - h) (* to water pou uparxei sto i *)
        in 
            WaterTrapped (hs, ls, rs) (total + water) (* prosthetoume to water sto total *)
        end
    | WaterTrapped _ _ = raise Fail "WaterTrapped: Invalid input"

fun WaterTrappedMain hs =
    let 
        val leftHills = MaxHillLeft hs (* vriskei ta max aristera hills *)
        val rightHills = MaxHillRight hs (* vriskei ta max deksia hills *)
    in
        WaterTrapped (hs, leftHills, rightHills) 0 (* ypologizei to water pou uparxei *)
    end
        
fun readInts ins =
  let
    val line = valOf (TextIO.inputLine ins)
    val toks = String.tokens Char.isSpace line
  in
    List.mapPartial Int.fromString toks
  end


fun rain filename =
  let
    val ins     = TextIO.openIn filename
    val _       = TextIO.inputLine ins
    val heights = readInts ins
    val ()      = TextIO.closeIn ins
    val result  = WaterTrappedMain heights
  in
    print (Int.toString result ^ "\n")
  end