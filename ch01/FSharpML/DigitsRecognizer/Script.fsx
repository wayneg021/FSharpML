open System.IO
type Observation = {Label:string; Pixels: int[]}
type Distance = int[] * int[] -> int

let toObservation (csvData: string) =
    let columns = csvData.Split(',')
    let label = columns.[0]
    let pixels = columns.[1..] |> Array.map int
    { Label = label; Pixels = pixels}

let reader path =
    let data = File.ReadAllLines path
    data.[1..]
    |> Array.map toObservation

let trainingPath = @"C:\Source Code\FSharpML\DigitsRecognizer\trainingsample.csv"
let trainingData = reader trainingPath

let manhatonDistance (pixels1, pixels2) =
    Array.zip pixels1 pixels2
    |> Array.map (fun (x, y) -> abs(x - y) )
    |> Array.sum

let euclideanDistance (pixels1, pixels2) =
    Array.zip pixels1 pixels2
    |> Array.map (fun (x, y) -> pown (x - y) 2)
    |> Array.sum

let train (trainingSet: Observation[]) (dist: Distance) =
    let classify (pixels: int[]) =
        trainingSet
        |> Array.minBy (fun x -> dist (x.Pixels, pixels))
        |> fun x -> x.Label
    classify

let classifier = train trainingData

let validationPath = @"C:\Source Code\FSharpML\DigitsRecognizer\validationsample.csv"
let validationData = reader validationPath

validationData
|> Array.averageBy (fun x -> if classifier manhatonDistance x.Pixels = x.Label then 1. else 0.)
|> printfn "Correct: %.3f"

validationData
|> Array.averageBy (fun x -> if classifier euclideanDistance x.Pixels = x.Label then 1. else 0.)
|> printfn "Correct: %.3f"


