module Optional

    //sum op fra m op til n
    let sum m n =
        let rec aux m n =
            match n with
            | 0 -> m
            | x -> aux (m + x) (x - 1)
        aux m n

    let length list = 
        let rec aux acc list = 
            match list with
            | [] -> acc
            | x::xs -> aux (acc + 1) xs
        aux 0 list