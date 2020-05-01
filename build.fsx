#if !FAKE
#r "netstandard"
#endif

#r "paket:
nuget Fake.DotNet.Cli
nuget Fake.IO.FileSystem
nuget Fake.Core.Target //"

#load ".fake/build.fsx/intellisense.fsx"
open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators

Target.initEnvironment ()
let projectPath = "./src/Feliz.Isomorphic"
let generatorPath = "./src/Generator"

let platformTool tool winTool =
  let tool = if Environment.isUnix then tool else winTool
  tool
  |> ProcessUtils.tryFindFileOnPath
  |> function Some t -> t | _ -> failwithf "%s not found" tool
  
let runTool cmd args workingDir =
    let arguments = args |> String.split ' ' |> Arguments.OfArgs
    Command.RawCommand (cmd, arguments)
    |> CreateProcess.fromCommand
    |> CreateProcess.withWorkingDirectory workingDir
    |> CreateProcess.ensureExitCode
    |> Proc.run
    |> ignore
    
let runDotNet cmd workingDir =
    let result =
        DotNet.exec (DotNet.Options.withWorkingDirectory workingDir) cmd ""
    if result.ExitCode <> 0 then failwithf "'dotnet %s' failed in %s" cmd workingDir
    
let delete file =
    if System.IO.File.Exists(file)
    then System.IO.File.Delete file
    else ()
  
Target.create "Clean" (fun _ ->
    !! "src/**/bin"
    ++ "src/**/obj"
    |> Shell.cleanDirs 
)

Target.create "Generate" (fun _ ->
    runDotNet "run" generatorPath 
)

Target.create "Build" (fun _ ->
    runDotNet "build" projectPath
)

let publish projectPath = fun _ ->
    runDotNet "restore --no-cache" projectPath
    runDotNet "pack -c Release" projectPath
    let nugetKey =
        match Environment.environVarOrNone "FELIZ_ISOMORPHIC_NUGET_KEY" with
        | Some nugetKey -> nugetKey
        | None -> failwith "The Nuget API key must be set in a FELIZ_ISOMORPHIC_NUGET_KEY environmental variable"
    let nupkg =
        Directory.findFirstMatchingFile "*.nupkg" (projectPath </> "bin" </> "Release")
        
    let pushCmd = sprintf "nuget push %s -s https://www.nuget.org/api/v2/package/ -k %s" nupkg nugetKey
    runDotNet pushCmd projectPath
    
Target.create "Publish" (publish projectPath)
    

Target.create "All" ignore

"Clean"
  ==> "Generate"
  ==> "Build"
  ==> "Publish"

Target.runOrDefault "Build"
