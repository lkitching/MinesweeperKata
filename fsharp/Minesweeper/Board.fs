module Board

type Board<'t> = 't [,]     //board is a 2d array
[<Measure>] type r          //row index type
[<Measure>] type c          //column index type
[<Measure>] type w          //board with type
[<Measure>] type h          //board height type

//initialises a board with the given dimensions
//The function f is given the position on the board and returns the value at that position
let init<'T> (width: int<w>) (height: int<h>) f : Board<'T> =
    Array2D.init (int height) (int width) (fun row col -> f (row * 1<r>) (col * 1<c>))

//gets the width of the board
let getWidth<'T> (board: Board<'T>) = Array2D.length2 board

//gets the height of the board
let getHeight<'T> (board: Board<'T>) = Array2D.length1 board

//get the neighbouring squares for a given board position
let getNeighbours<'T> (board: Board<'T>) (row: int<r>) (col: int<c>) =
    let height = getHeight board |> int
    let width = getWidth board |> int
    seq {
        for ro in [-1; 0; 1] do
            for co in [-1; 0; 1] do
                let (r, c) = ((int row) + ro, (int col) + co)
                if r >= 0 && r < height && c >= 0 && c < width then yield (r * 1<r>, c * 1<c>)
    }

//get the value at the given position
let get<'T> (board: Board<'T>) (row: int<r>) (col: int<c>) = Array2D.get board (int row) (int col)

//maps this board to another with the same dimensions
let mapi<'T, 'U> (f: int<r> -> int<c> -> 'T -> 'U) (board: Board<'T>) : Board<'U> = Array2D.mapi (fun row col e -> f (row * 1<r>) (col * 1<c>) e) board

//gets the row with the given row index
let getRow<'T> (board: Board<'T>) (row: int<r>) = 
    let widthi = getWidth board |> int
    Array.init widthi (fun i -> get board row (i * 1<c>))

//maps the rows on the board with a given mapping function
let mapRows<'T, 'U> (f: 'T array -> 'U) (board: Board<'T>) : 'U seq =
    let rowCount = getHeight board
    Seq.init rowCount (fun r -> getRow board (r * 1<r>) |> f)
