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
        
type RawQualifiedName = RawQualifiedName of string
type FormattedQualifiedName = FormattedQualifiedName of string

type ObjectTypeName<'Format> = ObjectTypeName of 'Format
module ObjectTypeName =
    let format (ObjectTypeName (RawQualifiedName name)) =
        name
        |> String.replace "+" "."
        |> String.replace "Module." "."
        |> FormattedQualifiedName
        |> ObjectTypeName

type FieldName<'Format> = FieldName of 'Format
module FieldName =
    let format (FieldName (RawQualifiedName name)) =
        name
        |> String.replace "get_" ""
        |> FormattedQualifiedName
        |> FieldName
type FunctionName<'Format> = FunctionName of 'Format
module FunctionName =
    let fromFieldInfo (fieldInfo: System.Reflection.MethodInfo) =
        (FunctionName << RawQualifiedName) <| fieldInfo.DeclaringType.FullName ++ "." ++ fieldInfo.Name
        
    let format (FunctionName (RawQualifiedName name)) =
        name
        |> String.replace "get_" ""
        |> FormattedQualifiedName
        |> FunctionName

type FieldInfo<'Format> =
    | Literal of FieldName<'Format>
    | NonLiteral of FieldName<'Format>
    with
        static member Map (x, f) =
            match x with
            | Literal name -> (Literal << f) name
            | NonLiteral name -> (NonLiteral << f) name
    
module FieldInfo =
    let hasLiteralAttribute (fieldInfo: System.Reflection.FieldInfo) =
        fieldInfo.GetCustomAttributes<LiteralAttribute>() |> Seq.tryExactlyOne
        
    let toFieldName (fieldInfo: System.Reflection.FieldInfo) =
        (FieldName << RawQualifiedName) <| fieldInfo.DeclaringType.FullName ++ "." ++ fieldInfo.Name
        
    let fromFieldInfo (fieldInfo: System.Reflection.FieldInfo) =
        hasLiteralAttribute fieldInfo
        |> (function Some _ -> Literal | _ -> NonLiteral) <| (toFieldName fieldInfo)
        
type ModuleFullName<'Format> = ModuleFullName of 'Format
module ModuleFullName =
    let format (ModuleFullName (RawQualifiedName name)) =
        name
        |> String.replace "Module" ""
        |> FormattedQualifiedName
        |> ModuleFullName

type Module<'Format> = { FullName: ModuleFullName<'Format>; Fields: FieldInfo<'Format> seq; Functions: FunctionName<'Format> seq; NestedTypes: TypeInfo<'Format> seq }
and TypeInfo<'Format> =
    | ObjectType of ObjectTypeName<'Format>
    | Module of Module<'Format>

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
                    |> map FunctionName.fromFieldInfo
                    
                let nestedTypes =
                    t
                    |> Assembly.getNestedTypes
                    |> map fromType
                    
                Module <| {FullName = ModuleFullName <| RawQualifiedName t.FullName; Fields = fields; Functions = functions; NestedTypes = nestedTypes }
                
            | t -> (ObjectType << ObjectTypeName << RawQualifiedName) t.FullName
            
        types
        |> filter (fun t -> t.IsPublic)
        |> map fromType
        
    let formatTypeNames (typeInfos: TypeInfo<RawQualifiedName> seq) =
        let rec formatTypeName = function
            | ObjectType objectTypeName ->
                ObjectType <| ObjectTypeName.format objectTypeName
            | Module {FullName = fullName; Fields = fields; Functions = functions; NestedTypes = nestedTypes} ->
                let fullName = ModuleFullName.format fullName
                
                let fields =
                    fields
                    |> map (map FieldName.format)
                
                let functions =
                    functions
                    |> map FunctionName.format
                    
                let nestedTypes =
                    nestedTypes
                    |> map formatTypeName
                    
                Module {FullName = fullName; Fields = fields; Functions = functions; NestedTypes = nestedTypes}
                
        typeInfos
        |> map formatTypeName

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
            |> TypeInfo.formatTypeNames
            
        workflow feliz
        |> iter (printfn "%A")
        0
