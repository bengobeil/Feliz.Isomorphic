namespace Feliz.Isomorphic.Generator

open System.Text

[<AutoOpen>]
module FSharpPlus =
    open FSharpPlus
    
    let inline exclude f col =
        filter (not << f) col

[<AutoOpen>]
module StringBuilder =
    // http://fssnip.net/7WR
    open System.Text
    
    let append (sb: StringBuilder) (str: string) = sb.Append str |> ignore; sb
    
    
    let inline (+=|) (sb: StringBuilder) (str: string) = sb.AppendLine str
    type StringBuffer = StringBuilder -> unit

    type StringBuilderBuilder () =
        member inline __.Yield (txt: string) = fun (b: StringBuilder) -> Printf.bprintf b "%s" txt
        member inline __.Yield (c: char) = fun (b: StringBuilder) -> Printf.bprintf b "%c" c
        member inline __.Yield (strings: #seq<string>) =
            fun (b: StringBuilder) -> for s in strings do Printf.bprintf b "%s\n" s
        member inline __.YieldFrom (f: StringBuffer) = f
        member __.Combine (f, g) = fun (b: StringBuilder) -> f b; g b
        member __.Delay f = fun (b: StringBuilder) -> (f()) b
        member __.Zero () = ignore
        
        member __.For (xs: 'a seq, f: 'a -> StringBuffer) =
            fun (b: StringBuilder) ->
                use e = xs.GetEnumerator ()
                while e.MoveNext() do
                    (f e.Current) b
        
        member __.While (p: unit -> bool, f: StringBuffer) =
            fun (b: StringBuilder) -> while p () do f b    

    let stringBuilder = new StringBuilderBuilder ()
    
    type StringBuilderBuilder with
      member inline __.Yield (b: byte) = fun (sb: StringBuilder) -> Printf.bprintf sb "%02x " b
      
    let (++) (s1: string) (s2: string) = stringBuilder {
        s1
        s2
    }
    
module StringBuffer =
    open FSharpPlus
    let append b1 b2 = stringBuilder {
        yield! b1
        yield! b2
    }
    
    let appendLine buffer (str: string): StringBuffer = stringBuilder {
        yield! buffer
        "\n"
        str
    }
    let appendToNewLine b1 b2 = stringBuilder {
        yield! b1
        "\n"
        yield! b2
    }
    
    let inline reduce (buffers: StringBuffer seq) =
        fold appendToNewLine ignore buffers
    
    let run f =
        let sb = StringBuilder()
        f sb
        sb.ToString()

[<RequireQualifiedAccess>]
module String =
    let split (str:string) (delimiter:string): string array =
        str.Split delimiter
        
[<RequireQualifiedAccess>]
module Seq =
    let exclude f =
        Seq.filter (f >> not)
        
[<RequireQualifiedAccess>]
module Tuple =
    let reduceTuplesOfLists tuples =
        tuples
        |> List.reduce (fun (cAcc,sAcc) (cCurr,sCurr)-> (cAcc @ cCurr, sAcc @ sCurr))
        
    let map f = function (x,y) -> (f x,f y)
    let map2 f1 f2 = function (x,y) -> (f1 x,f2 y)