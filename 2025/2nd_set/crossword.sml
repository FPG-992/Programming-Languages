datatype cell = Empty | Black | Letter of char

type grid = cell Array2.array
type position = int * int
type slot = {start: position, length: int, horizontal: bool}
type placement = {word: string, slot: slot}

fun explode s = List.tabulate(String.size s, fn i => String.sub(s, i))
fun implode cs = String.implode cs

fun mergeSort compare [] = []
  | mergeSort compare [x] = [x]
  | mergeSort compare xs =
    let
        val len = length xs
        val mid = len div 2
        val (left, right) = (List.take(xs, mid), List.drop(xs, mid))
        
        fun merge [] ys = ys
          | merge xs [] = xs
          | merge (x::xs) (y::ys) =
            if compare(x, y) then x :: merge xs (y::ys)
            else y :: merge (x::xs) ys
    in
        merge (mergeSort compare left) (mergeSort compare right)
    end

fun readFile filename =
    let
        val infile = TextIO.openIn filename
        fun readLines acc =
            case TextIO.inputLine infile of
                NONE => (TextIO.closeIn infile; List.rev acc)
              | SOME line => 
                    let
                        val cleanLine = if String.size line > 0 andalso String.sub(line, String.size line - 1) = #"\n"
                                       then String.substring(line, 0, String.size line - 1)
                                       else line
                    in
                        readLines (cleanLine :: acc)
                    end
    in
        readLines []
    end handle Io => (print ("Error reading file: " ^ filename ^ "\n"); raise Fail ("Cannot read file: " ^ filename))

fun parseInput lines =
    let
        val firstLine = hd lines
        val tokens = String.tokens (fn c => c = #" ") firstLine
        
        fun safeIntFromString s =
            case Int.fromString s of
                SOME i => i
              | NONE => (print ("Error parsing integer: " ^ s ^ "\n"); raise Fail ("Invalid integer: " ^ s))
        
        val (rows, cols, numBlacks, numWords) = 
            case List.map safeIntFromString tokens of
                [r, c, nb, nw] => (r, c, nb, nw)
              | _ => (print ("Invalid token count: " ^ Int.toString (length tokens) ^ "\n");
                     print ("Tokens: " ^ String.concatWith " | " tokens ^ "\n");
                     raise Fail "Invalid input format")
        
        val blackLines = List.take(List.drop(lines, 1), numBlacks)
        val wordLines = List.drop(lines, 1 + numBlacks)
        
        val blackSquares = List.map (fn line =>
            let
                val lineTokens = String.tokens (fn x => x = #" ") line
            in
                case List.map safeIntFromString lineTokens of
                    [r, c] => (r-1, c-1)
                  | _ => (print ("Invalid black square line: " ^ line ^ "\n");
                         raise Fail "Invalid black square format")
            end) blackLines
    in
        (rows, cols, blackSquares, wordLines)
    end

fun initGrid rows cols blackSquares =
    let
        val grid = Array2.array(rows, cols, Empty)
        fun markBlack (r, c) = Array2.update(grid, r, c, Black)
    in
        List.app markBlack blackSquares;
        grid
    end

fun findSlots grid =
    let
        val rows = Array2.nRows grid
        val cols = Array2.nCols grid
        
        fun findHorizontalSlots r =
            let
                fun findInRow c start acc =
                    if c >= cols then
                        if c - start >= 2 then
                            {start=(r, start), length=c-start, horizontal=true} :: acc
                        else acc
                    else
                        case Array2.sub(grid, r, c) of
                            Black => 
                                if c - start >= 2 then
                                    findInRow (c+1) (c+1) ({start=(r, start), length=c-start, horizontal=true} :: acc)
                                else
                                    findInRow (c+1) (c+1) acc
                          | _ => findInRow (c+1) start acc
            in
                findInRow 0 0 []
            end
        
        fun findVerticalSlots c =
            let
                fun findInCol r start acc =
                    if r >= rows then
                        if r - start >= 2 then
                            {start=(start, c), length=r-start, horizontal=false} :: acc
                        else acc
                    else
                        case Array2.sub(grid, r, c) of
                            Black => 
                                if r - start >= 2 then
                                    findInCol (r+1) (r+1) ({start=(start, c), length=r-start, horizontal=false} :: acc)
                                else
                                    findInCol (r+1) (r+1) acc
                          | _ => findInCol (r+1) start acc
            in
                findInCol 0 0 []
            end
        
        val horizontalSlots = List.concat (List.tabulate(rows, findHorizontalSlots))
        val verticalSlots = List.concat (List.tabulate(cols, findVerticalSlots))
    in
        horizontalSlots @ verticalSlots
    end

fun canPlaceWord grid word slot =
    let
        val {start=(sr, sc), length, horizontal} = slot
        val wordChars = explode word
        
        fun checkPosition i =
            if i >= length then true
            else
                let
                    val (r, c) = if horizontal then (sr, sc + i) else (sr + i, sc)
                    val gridCell = Array2.sub(grid, r, c)
                    val wordChar = List.nth(wordChars, i)
                in
                    case gridCell of
                        Empty => checkPosition (i + 1)
                      | Letter ch => ch = wordChar andalso checkPosition (i + 1)
                      | Black => false
                end
    in
        String.size word = length andalso checkPosition 0
    end

fun placeWord grid word slot =
    let
        val {start=(sr, sc), length, horizontal} = slot
        val wordChars = explode word
        
        fun placeChar i =
            if i < length then
                let
                    val (r, c) = if horizontal then (sr, sc + i) else (sr + i, sc)
                    val wordChar = List.nth(wordChars, i)
                in
                    Array2.update(grid, r, c, Letter wordChar);
                    placeChar (i + 1)
                end
            else ()
    in
        placeChar 0
    end

fun removeWord grid slot =
    let
        val {start=(sr, sc), length, horizontal} = slot
        
        fun removeChar i =
            if i < length then
                let
                    val (r, c) = if horizontal then (sr, sc + i) else (sr + i, sc)
                in
                    Array2.update(grid, r, c, Empty);
                    removeChar (i + 1)
                end
            else ()
    in
        removeChar 0
    end

fun slotsIntersect slot1 slot2 =
    let
        val {start=(sr1, sc1), length=len1, horizontal=h1} = slot1
        val {start=(sr2, sc2), length=len2, horizontal=h2} = slot2
    in
        if h1 = h2 then false
        else
            let
                val (hSlot, vSlot) = if h1 then (slot1, slot2) else (slot2, slot1)
                val {start=(hr, hc), length=hlen, ...} = hSlot
                val {start=(vr, vc), length=vlen, ...} = vSlot
            in
                hr >= vr andalso hr < vr + vlen andalso
                vc >= hc andalso vc < hc + hlen
            end
    end

fun solve grid words slots =
    let
        fun findPlacements word =
            List.filter (canPlaceWord grid word) slots
        
        fun sortWordsByConstraints words =
            let
                val wordPlacements = List.map (fn w => (w, length (findPlacements w))) words
                val sorted = mergeSort (fn ((_, p1), (_, p2)) => p1 <= p2) wordPlacements
            in
                List.map #1 sorted
            end
        
        fun backtrack remainingWords usedSlots =
            case remainingWords of
                [] => true
              | word :: restWords =>
                    let
                        val validSlots = List.filter (fn s => not (List.exists (fn us => us = s) usedSlots))
                                                    (findPlacements word)
                        
                        fun trySlot [] = false
                          | trySlot (slot :: restSlots) =
                                if canPlaceWord grid word slot then
                                    (placeWord grid word slot;
                                     if backtrack restWords (slot :: usedSlots) then true
                                     else (removeWord grid slot; trySlot restSlots))
                                else
                                    trySlot restSlots
                    in
                        trySlot validSlots
                    end
        
        val sortedWords = sortWordsByConstraints words
    in
        backtrack sortedWords []
    end

fun extractWords grid =
    let
        val rows = Array2.nRows grid
        val cols = Array2.nCols grid
        
        fun extractRow r =
            let
                fun getChar c =
                    case Array2.sub(grid, r, c) of
                        Letter ch => Char.toString ch
                      | _ => " "
                
                fun buildRowString c acc =
                    if c >= cols then acc
                    else buildRowString (c + 1) (acc ^ getChar c)
                
                val rowString = buildRowString 0 ""
                
                fun extractWordsFromString s =
                    let
                        val tokens = String.tokens (fn c => c = #" ") s
                        val words = List.filter (fn w => String.size w >= 2) tokens
                    in
                        String.concatWith " " words
                    end
            in
                extractWordsFromString rowString
            end
    in
        List.tabulate(rows, extractRow)
    end

fun crossword filename =
    let
        val lines = readFile filename
        val (rows, cols, blackSquares, words) = parseInput lines
        val grid = initGrid rows cols blackSquares
        val slots = findSlots grid
        
        val solutionFound = solve grid words slots
    in
        if solutionFound then
            let
                val resultRows = extractWords grid
            in
                List.app print (List.map (fn s => s ^ "\n") resultRows)
            end
        else
            print "IMPOSSIBLE\n"
    end