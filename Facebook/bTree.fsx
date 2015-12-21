open System.Collections.Generic

type 'a Tree =
| Empty
| Node of left : 'a Tree * value : 'a * right : 'a Tree

let rec insert (tree : 'a Tree) (value : 'a) =
    match tree with
    | Empty -> Node(Empty, value, Empty)
    | Node (left, v, right) -> if value < v then Node((insert left value), v, right) else Node(left, v, (insert right value))

let walk (tree : 'a Tree) =
    let path = Stack<'a Tree>()
    let lst = List<'a>()

    let rec move (t : 'a Tree) isRight=
        match t with
        | Empty -> Empty
        | Node (left, v, right) ->
            if isRight then right else left

    let getValue = function
        | Empty -> failwith "empty"
        | Node (_, v, _) -> v

    let rec keepMoving t =
        if t = Empty then t
        else
            let t = move t false
            path.Push t
            keepMoving t

    let rec createPath tree =
        if path.Count = 0 then lst
        else
            match tree with
            | Empty -> lst
            | Node(left, v, right) ->
                while keepMoving tree <> Empty do
                    ignore true

                lst.Add(path.Pop() |> getValue)

                let tree = path.Pop()
                lst.Add(getValue tree)
                
                createPath (move tree true)

    path.Push tree       
    createPath tree        

