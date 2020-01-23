fun maxArea xs =
  let fun areaFromNewBar (n, ([], i, currMax)) = ([(n, i)], i + 1, currMax)
        | areaFromNewBar (n, (stack as (n', i')::numIndices, i, currMax)) =
           case (Int.compare (n, n'), numIndices)
             of (LESS, []) =>
                  ((n, i)::[], i + 1, Int.max(currMax, Int.min(n', i')))
              | (LESS, (_, i'')::_) =>
                  let val newmaxArea = n' * (i - i'' - 1)
                      val (newStack, newIndex, newMax) =
                        areaFromNewBar (n, (numIndices, i, currMax))
                  in
                    (newStack, newIndex, Int.max(newMax, newmaxArea))
                  end
              | _ => ((n, i)::stack, i + 1, currMax)
    in #3 (foldr areaFromNewBar ([(0, 0)], 1, 0) (0::xs))
    end
