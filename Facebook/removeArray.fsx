(*http://codercareer.blogspot.com/2012/02/no-32-remove-numbers-in-array.html*)
let removeArray (arr : int []) v =
    let res = 
        [|
            for a in arr do
                if a <> v then yield a
        |]

    res, res.Length

