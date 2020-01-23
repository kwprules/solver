(* the things that should have been in the basis *)
fun max f [x] = f x
  | max f (x::xs) = Int.max (f x, max f xs)
  | max _ _ = raise Match

(* foldr that returns array of intermediate values *)
fun foldrAccum f z xs =
  #2 (foldr (fn (a, (b, xs)) =>
              let val b' = f (a, b)
              in
                (b', b'::xs)
              end)
            (z, []) xs)


(* the real functions *)

fun maxSquareFromRow xs =
  let fun areaFromNewBar (n, (stack, i, currMax)) =
    (case stack
      of [] => ([(n, i)], i + 1, currMax)
       | ((n', i')::numIndices) =>
         case (Int.compare (n, n'), numIndices)
           of (LESS, []) =>
                ((n, i)::[], i + 1, Int.max(currMax, Int.min(n', i')))
            | (LESS, (_, i'')::_) =>
                  let val newSideLength = Int.min(n', i - i'' - 1)
                      val (newStack, newIndex, newMax) =
                        areaFromNewBar (n, (numIndices, i, currMax))
                  in
                    (newStack, newIndex, Int.max(newMax, newSideLength))
                  end
            | _ => ((n, i)::stack, i + 1, currMax))
       val (_, _, sideLength) = foldr areaFromNewBar ([(0, 0)], 1, 0) (0::xs)
    in
      sideLength * sideLength
    end


fun maxSquareArea [] = 0
  | maxSquareArea (first::rest) =
    max maxSquareFromRow
        (foldrAccum (ListPair.foldrEq (fn (b, n, xs) => (n + b) * b::xs) []) first rest)
