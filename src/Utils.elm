module Utils exposing (unique)


unique : List a -> List a
unique list =
    let 
        uniqueItem item list =
            if (List.member item list) == False then
                item :: list
            else 
                list 
    in
        List.foldl uniqueItem [] list
