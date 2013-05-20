open System.IO
open Board
open Utils

type Square = Bomb | Clear              //inpur board square type
type AdjSquare = Bomb | Adj of int      //output board square type with number of adjacent bombs

//converts an input square to an output square given the square and a list of its neighbours
let toAdj neighbours = function
| Square.Bomb -> AdjSquare.Bomb
| Square.Clear -> let adjBombs = neighbours |> Seq.filter ((=)Square.Bomb) |> Seq.length in AdjSquare.Adj(adjBombs)

//maps an input board to an output board
let mapAdjacancies (board: Square Board) =
    let mapSquare ri ci square =
        let neighbours = Board.getNeighbours board ri ci |> Seq.map (fun (r, c) -> Board.get board r c)
        toAdj neighbours square
    Board.mapi mapSquare board

//parsing
let parseSquare = function
| '*' -> Square.Bomb
| '.' -> Square.Clear
| c -> failwith (sprintf "Unexpected board character %c" c)

//parses a board header line. This should a line containing two ints (height width) separated by a space
let parseHeader (header: string) =
    let [| heightStr; widthStr |] = header.Split(' ')
    let height = int heightStr
    let width = int widthStr
    (width * 1<w>, height * 1<h>)

//parses a board of given dimensions contained in an array of strings whose first line is found at the given index
let parseBoard width height (lines: string array) startIndex =
    let getSquare (ri: int<r>) (ci: int<c>) =
        lines.[startIndex + (int ri)].[int ci] |> parseSquare
    Board.init width height getSquare

//reads all the input boards from an array of input lines
//the first line should be the header line for the first board
let readAll (lines: string array) =
    let readBoardAt (idx, num) =
        let header = lines.[idx]
        match parseHeader header with
        | (0<w>, 0<h>) -> None
        | (w, h) ->
            let board = parseBoard w h lines (idx + 1)
            let nextIndex = idx + (int h) + 1
            Some((num, board), (nextIndex, num + 1))
    Seq.unfold readBoardAt (0, 1)

//formats a single output board square as a char
let displaySquare = function
| AdjSquare.Bomb -> '*'
| AdjSquare.Adj(i) -> i.ToString().[0]

//formats a single output board row as a string
let displayRow row = Array.map displaySquare row |> (fun chars -> new System.String(chars))

//formats an entire output board as a string, with a header row and each row on its own line
let displayField (num: int) board =
    let sb = new System.Text.StringBuilder()
    sb.AppendLine(sprintf "Field #%d:" num) |> ignore
    let boardLines = Board.mapRows displayRow board
    let boardStr = System.String.Join(System.Environment.NewLine, boardLines)
    sb.Append(boardStr) |> ignore
    sb.ToString()

//writes a list of formatted field strings to a text writer, separated by empty lines
let rec writeFieldsTo (tr: System.IO.TextWriter) = function
| [] -> ()
| [last: string] -> 
    tr.Write(last)
| (str::strs) ->
    tr.Write(str)
    tr.WriteLine()
    tr.WriteLine()
    writeFieldsTo tr strs

let readAndWrite (reader: System.IO.TextReader) (writer: System.IO.TextWriter) =
    let fileLines = readLines reader
    let boards = readAll fileLines
    let outLines = boards |> Seq.map (fun (n, board) -> displayField n (mapAdjacancies board)) |> List.ofSeq
    writeFieldsTo writer outLines
    writer.Flush()

[<EntryPoint>]
let main argv = 
    match argv with
    | [| path |] ->
        use reader = new System.IO.StreamReader(path)
        readAndWrite reader System.Console.Out
        0
    | _ -> 
        printf "Usage: Minesweeper file\n"
        -1
