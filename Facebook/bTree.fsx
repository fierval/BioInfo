open System.Collections.Generic

type 'a Tree =
| Empty
| Node of left : 'a Tree * value : 'a * right : 'a Tree

let rec insert (value : 'a) (tree : 'a Tree) =
    match tree with
    | Empty -> Node(Empty, value, Empty)
    | Node (left, v, right) -> if value < v then Node((insert value left), v, right) else Node(left, v, (insert value right))

let walkSorted (tree : 'a Tree) =
    let path = Stack<'a Tree>()
    let lst = List<'a>()

    let rec move (t : 'a Tree) isRight=
        match t with
        | Empty -> Empty
        | Node (left, v, right) -> if isRight then right else left

    let getValue = function
        | Empty -> failwith "empty"
        | Node (_, v, _) -> v
    
    let hasRight = function
    |Empty -> false
    |Node(_, _, right) -> right <> Empty

    let rec keepMoving t =
        if t = Empty then t
        else
            let t = move t false
            if t <> Empty then 
                path.Push t
            keepMoving t

    let rec createPath tree =
        if path.Count = 0 then lst
        else
            match tree with
            | Empty -> lst
            | _ ->
                while keepMoving tree <> Empty do
                    ignore true
                
                let tree = path.Pop()
                lst.Add(tree |> getValue)
                let tree = 
                    if hasRight tree then
                        tree
                    else
                        lst.Add(path.Peek() |> getValue)
                        path.Pop()

                let tree = move tree true
                path.Push tree
                createPath tree


    path.Push tree       
    createPath tree |> Seq.toArray

let tree = insert 20 Empty|> insert 35 |> insert 15 |> insert 21 |> insert 17 |> insert 25 |> insert 13
let path = Stack<int Tree>()
let lst = List<int>()
