module Utils

let readLines (reader: System.IO.TextReader) =
    let readLine (tr: System.IO.TextReader) =
        let line = tr.ReadLine()
        if line = Unchecked.defaultof<string> then None else Some(line, tr)
    Seq.unfold readLine reader |> Array.ofSeq

