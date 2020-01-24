(* the things that should have been in the basis *)
fun max f [x] = f x
  | max f (x::xs) = Int.max (f x, max f xs)
  | max _ _ = raise Match

(* same as above but tail recursive (but uglier) *)
fun maxOn _ [] = raise Match
  | maxOn f (x::xs) =
      let fun max (f, y, []) = y
            | max (f, y, x::xs) = max (f, Int.max (y, f x), xs)
      in
        max (f, f x, xs)
      end

fun squared x = x * x

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
  let fun areaFromNewBar (n, ([], i, currMax)) = ([(n, i)], i + 1, currMax)
        | areaFromNewBar (n, (stack as (n', i')::numIndices, i, currMax)) =
           case (Int.compare (n, n'), numIndices)
             of (LESS, []) =>
                  ((n, i)::[], i + 1, squared (Int.max(currMax, Int.min(n', i'))))
              | (LESS, (_, i'')::_) =>
                    let val newSideLength = Int.min(n', i - i'' - 1)
                        val (newStack, newIndex, newMax) =
                          areaFromNewBar (n, (numIndices, i, currMax))
                    in
                      (newStack, newIndex, Int.max(newMax, newSideLength))
                   end
              | _ => ((n, i)::stack, i + 1, currMax)
    in
      #3 (foldr areaFromNewBar ([(0, 0)], 1, 0) (0::xs))
    end


fun maxSquareArea [] = 0
  | maxSquareArea (first::rest) =
    maxOn maxSquareFromRow
          (foldrAccum (ListPair.mapEq (fn (b, n) => (n + b) * b)) first rest)
