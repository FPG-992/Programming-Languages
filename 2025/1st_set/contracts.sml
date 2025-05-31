(* O tropos pou tha antimetopisoume to provlima einai:
    Dinoume se olous B kai athroizoume to kerdos ws Bprofit
    an allaksw to B me to A, to kerdos tha einai Profiti = Ai-Bi
    
    To zhthma einai pws thelw akrivos M apo ta Profiti gia na kanw max to sunoliko profit.
    MaxProfit = Bprofit + Sum(First M MaxProfitI)

    Oi times pou mporei na parei to ProfitI einai apo [-999999, 999999] = Max 200001 different values
 *)

 val shift : int = 99999
 val size : int = 200001

 (* int slicing *)
 fun intsfromline =
    String.tokens Char.isSpace line
    |> List.mapPartial Int.fromString

val buckets : int Array.array = array (size, 0)

fun addToBucket idx = 
    Array.update (buckets, idx, Array.sub (buckets, idx) + 1)

fun ReadFile filename = 
    let
        val ins = TextIO.openIn filename
        val header = intsOfline (valOf (TextIO.inputLine ins))
        val [M,K] = header
        val N = M + K

        fun loop 0 baseB = (M, baseB)
            | loop n baseB =
                let
                    val SOME line = TextIO.inputLine ins
                    val [a,b] = intsfromline line
                    val d = a - b
                    val _ = addToBucket (d + shift)
                in 
                    loop (n - 1) (baseB + d)
                end

        val (Mfinal, baseB) = loop N 0
        val () = TextIO.closeIn ins
    in
        (Mfinal, baseB)
    end

fun collectTopM M =
  let
    fun step idx mLeft acc =
      if mLeft = 0            then acc                
      else if idx < 0         then raise Fail "bug"
      else
        let
          val cnt   = Array.sub (buckets, idx)        
        in
          if cnt = 0 then
               step (idx-1) mLeft acc                 
          else
            let
              val take   = Int.min (cnt, mLeft)       
              val value  = idx - shift                
            in
              step (idx-1) (mLeft - take) (acc + take * value)
            end
        end
  in
    step (bucketSize-1) M 0
  end

fun hire filename =
  let
    val (M, baseB) = loadFile filename     
    val gain       = collectTopM M         
    val answer     = baseB + gain
  in
    print (Int.toString answer ^ "\n")
  end

