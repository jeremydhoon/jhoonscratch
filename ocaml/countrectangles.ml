let countrectangles width height =
  let bigger, smaller = max width height, min width height in
  let rec outer sum big =
    if big = 0 then sum else
    let rec inner sum2 small =
      if small = 0 then sum2 else
      inner (sum2 + (bigger - (big - 1) -
    