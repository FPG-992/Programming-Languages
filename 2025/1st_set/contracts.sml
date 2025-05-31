(* O tropos pou tha antimetopisoume to provlima einai:
    Dinoume se olous B kai athroizoume to kerdos ws Bprofit
    an allaksw to B me to A, to kerdos tha einai Profiti = Ai-Bi
    
    To zhthma einai pws thelw akrivos M apo ta Profiti gia na kanw max to sunoliko profit.
    MaxProfit = Bprofit + Sum(First M MaxProfitI)

    Oi times pou mporei na parei to ProfitI einai apo [-999999, 999999] = Max 200001 different values
 *)

val shift      = 99999                       
val size       = 2*shift + 1                  


val buckets : int Array.array = Array.array (size, 0)

fun bump idx =
  Array.update (buckets, idx,
                Array.sub (buckets, idx) + 1)

fun intsFromLine line =
  List.mapPartial Int.fromString
                 (String.tokens Char.isSpace line)

fun readFile filename =
  let
    val ins       = TextIO.openIn filename
    val header    = intsFromLine (valOf (TextIO.inputLine ins))
    val [M, K]    = header
    val N         = M + K

    fun loop 0 baseB = (M, baseB)              
      | loop n baseB =
          let
            val SOME line = TextIO.inputLine ins
            val [a, b]    = intsFromLine line
            val d         = a - b
            val _         = bump (d + shift)
          in
            loop (n-1) (baseB + b)             
          end

    val (Mfinal, baseB) = loop N 0
    val ()              = TextIO.closeIn ins
  in
    (Mfinal, baseB)
  end

fun collectTopM M =
  let
    fun step idx mLeft acc =
      if mLeft = 0 then acc
      else if idx < 0 then raise Fail "collect bug"
      else
        let
          val cnt = Array.sub (buckets, idx)
        in
          if cnt = 0 then
            step (idx-1) mLeft acc
          else
            let
              val take  = Int.min (cnt, mLeft)
              val value = idx - shift
            in
              step (idx-1) (mLeft - take)
                          (acc + take * value)
            end
        end
  in
    step (size-1) M 0
  end

fun hire filename =
  let
    val (M, baseB) = readFile filename   
    val gain       = collectTopM M       
    val answer     = baseB + gain
  in
    print (Int.toString answer ^ "\n")
  end
