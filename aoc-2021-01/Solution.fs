module Solution

let parse (input:string) = input.Split "\r\n" |> Seq.map int

let increasingDepth (a,b) = a < b

let analyze = Seq.pairwise >> Seq.filter increasingDepth >> Seq.length

let solve  = parse >> analyze
let solve2 = parse >> Seq.windowed 3 >> Seq.map Seq.sum >> analyze

