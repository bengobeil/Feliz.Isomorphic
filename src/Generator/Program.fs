namespace Feliz.Isomorphic.Generator

open System
open FSharpPlus
open System.Reflection
open Microsoft.FSharp.Reflection

type DiscoverableAssembly = {Name: string; Exclusions: string list; IncludeAliases: string list}
type IsomorphicAssemblyPair = {Client: DiscoverableAssembly; Server: DiscoverableAssembly}

module Assembly =
    let getRootTypes (t: Assembly) =
        t.GetTypes()
        |> Seq.exclude (fun t -> t.IsNested)
    
    let getNestedTypes (t: Type) =
        t.GetNestedTypes()
        
type QualifiedName = QualifiedName of string

type ObjectTypeName = ObjectTypeName of QualifiedName
type FieldName = FieldName of string
type FunctionName = FunctionName of QualifiedName

type FieldInfo =
    | Literal of FieldName
    | NonLiteral of FieldName
    
module FieldInfo =
    let fromFieldInfo (fieldInfo: System.Reflection.FieldInfo) =
        match fieldInfo.GetCustomAttributes<LiteralAttribute>() |> Seq.tryExactlyOne with
        | Some _ -> Literal <| FieldName fieldInfo.Name
        | _ -> NonLiteral <| FieldName fieldInfo.Name

type ModuleFullName = ModuleFullName of QualifiedName

type Module = { FullName: ModuleFullName; Fields: FieldInfo seq; Functions: FunctionName seq; NestedTypes: TypeInfo seq }

and TypeInfo =
    | ObjectType of ObjectTypeName
    | Module of Module
        
module TypeInfo =
    let (|IsModule|_|) (t: Type) =
        match FSharpType.IsModule t with
        | true -> Some <| IsModule
        | false -> None
        
    let fromTypes (types: Type seq) =
        let rec fromType (t: Type) =
            match t with
            | IsModule ->
                let fields =
                    t.GetFields()
                    |> filter (fun fieldInfo -> fieldInfo.Attributes.HasFlag (FieldAttributes.Public &&& FieldAttributes.Static))
                    |> map FieldInfo.fromFieldInfo
                    
                let functions =
                    t.GetMethods()
                    |> filter (fun method -> method.Attributes.HasFlag (MethodAttributes.Public &&& MethodAttributes.Static))
                    |> exclude (fun method -> method.Attributes.HasFlag (MethodAttributes.HideBySig))
                    |> map (fun method -> FunctionName <| QualifiedName method.Name)
                    
                let nestedTypes =
                    t
                    |> Assembly.getNestedTypes
                    |> map fromType
                    
                Module <| {FullName = ModuleFullName <| QualifiedName t.FullName; Fields = fields; Functions = functions; NestedTypes = nestedTypes }
                
            | t -> (ObjectType << ObjectTypeName << QualifiedName) t.FullName
            
        types
        |> filter (fun t -> t.IsPublic)
        |> map fromType
    

module AssemblyDefinitions =
    let felizBulma = 
        { Name = "Feliz.Bulma"
          Exclusions = [
              "Feliz.Bulma.ElementBuilders"
              "Feliz.Bulma.PropertyBuilders"
              "Feliz.Bulma.ClassLiterals"
              "Feliz.Bulma.Operators"
              "Feliz.Bulma.ElementLiterals"
          ]
          IncludeAliases = [] }

    let felizBulmaViewEngine =
        { Name = "Feliz.Bulma.ViewEngine"
          Exclusions = [
              "Feliz.Bulma.ViewEngine.ElementBuilders"
              "Feliz.Bulma.ViewEngine.PropertyBuilders"
              "Feliz.Bulma.ViewEngine.ClassLiterals"
              "Feliz.Bulma.ViewEngine.Operators"
              "Feliz.Bulma.ViewEngine.ElementLiterals"
          ]
          IncludeAliases = []}
        
    let feliz = {
        Name = "Feliz"
        Exclusions = [
            
        ]
        IncludeAliases = [
            "Feliz.ReactElement"
        ]
    }

    let felizViewEngine = {
        Name = "Feliz.ViewEngine"
        Exclusions = [
            
        ]
        IncludeAliases = []
    }

    let felizPair =
        { Client = feliz
          Server = felizViewEngine }

    let felizBulmaPair =
        { Client = felizBulma
          Server = felizBulmaViewEngine }

module Program =
    open AssemblyDefinitions
    
    [<EntryPoint>]
    let main argv =
            
        let workflow {Name = name} =
            Assembly.Load name
            |> Assembly.getRootTypes
            |> exclude (fun t -> t.FullName.StartsWith "<StartupCode$")
            |> exclude (fun t -> t.FullName.Contains "@")
            |> sortBy (fun x -> String.toLower x.FullName)
            |> TypeInfo.fromTypes
            
        workflow feliz
        |> iter (printfn "%A")
        0
