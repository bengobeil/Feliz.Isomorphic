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
type AliasDefinition = AliasDefinition of string
module FormattedQualifiedName =
    let (|SignificantName|) (FormattedQualifiedName name) =
        name.Split '.'
        |> Array.last
        |> fun name -> SignificantName name

type ObjectTypeName<'Format> = ObjectTypeName of 'Format with
    static member Map (ObjectTypeName x, f) = ObjectTypeName (f x)
    
module ObjectTypeName =
    let format (ObjectTypeName (RawQualifiedName name)) =
        name
        |> String.replace "+" "."
        |> String.replace "Module." "."
        |> FormattedQualifiedName
        |> ObjectTypeName
        
    let toDefinition objectTypeName: ObjectTypeName<AliasDefinition> =
        map (fun (FormattedQualifiedName.SignificantName significantName & FormattedQualifiedName fullName) -> stringBuilder {
            "type"
            " "
            significantName
            " = "
            fullName
        } >> AliasDefinition) objectTypeName

type FunctionName<'Format> = FunctionName of 'Format with
    static member Map (FunctionName x, f) = FunctionName (f x)
module FunctionName =
    let fromFieldInfo (fieldInfo: System.Reflection.MethodInfo) =
        (FunctionName << RawQualifiedName) <| fieldInfo.DeclaringType.FullName ++ "." ++ fieldInfo.Name
        
    let format (FunctionName (RawQualifiedName name)) =
        name
        |> String.replace "get_" ""
        |> FormattedQualifiedName
        |> FunctionName
        
    let toDefinition functionName =
        map (fun (FormattedQualifiedName.SignificantName significantName & FormattedQualifiedName fullName) -> stringBuilder {
            "let"
            " "
            significantName
            " = "
            fullName
        } >> AliasDefinition) functionName

type FieldName<'Format> =
    | Literal of 'Format
    | NonLiteral of 'Format
    with
        static member Map (x, f) =
            match x with
            | Literal name -> (Literal << f) name
            | NonLiteral name -> (NonLiteral << f) name

module FieldName =
    let format (fieldName: FieldName<RawQualifiedName>) =
        let format (RawQualifiedName name) =
            name
            |> String.replace "get_" ""
            |> FormattedQualifiedName
            
        format <!> fieldName
        
    let hasLiteralAttribute (fieldInfo: System.Reflection.FieldInfo) =
        fieldInfo.GetCustomAttributes<LiteralAttribute>() |> Seq.tryExactlyOne
        
    let fromFieldInfo (fieldInfo: System.Reflection.FieldInfo) =
        (fieldInfo.DeclaringType.FullName ++ "." ++ fieldInfo.Name)
        |> RawQualifiedName
        |> (hasLiteralAttribute fieldInfo |> (function Some _ -> Literal | _ -> NonLiteral))
        
    let toDefinition fieldName =
        fieldName
        |> map (fun (FormattedQualifiedName.SignificantName significantName & FormattedQualifiedName fullName) ->
            AliasDefinition <| stringBuilder {
            "let"
            " "
            match fieldName with
            | Literal _ -> "[<Literal>] "
            | _ -> ""
            significantName
            " = "
            fullName
        })
        
type ModuleFullName<'Format> = ModuleFullName of 'Format with
    static member Map (ModuleFullName x, f) = ModuleFullName (f x)
    

type Module<'Format> = { FullName: ModuleFullName<'Format>; Fields: FieldName<'Format> seq; Functions: FunctionName<'Format> seq; NestedTypes: TypeInfo<'Format> seq }
and TypeInfo<'Format> =
    | ObjectType of ObjectTypeName<'Format>
    | Module of Module<'Format> with
    static member Map (x, f) =
        match x with
        | ObjectType x -> map f x |> ObjectType    
        | Module {FullName = fullName; Fields = fields; Functions = functions; NestedTypes = nestedTypes} ->
            Module {FullName = map f fullName; Fields = map (map f) fields; Functions = map (map f) functions; NestedTypes = map (map f) nestedTypes}
    
module ModuleFullName =
    let format (ModuleFullName (RawQualifiedName name)) =
        name
        |> String.replace "Module" ""
        |> FormattedQualifiedName
        |> ModuleFullName
    

module TypeInfo =
    let (|IsModule|_|) (t: Type) =
        match FSharpType.IsModule t with
        | true -> Some <| IsModule
        | false -> None
        
    let rec fromType (t: Type) =
        match t with
        | IsModule ->
            let fields =
                t.GetFields()
                |> filter (fun fieldInfo -> fieldInfo.Attributes.HasFlag (FieldAttributes.Public &&& FieldAttributes.Static))
                |> map FieldName.fromFieldInfo
                
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
        
    let rec formatTypeName = function
        | ObjectType objectTypeName ->
            ObjectType <| ObjectTypeName.format objectTypeName
        | Module {FullName = fullName; Fields = fields; Functions = functions; NestedTypes = nestedTypes} ->
            let fullName = ModuleFullName.format fullName
            
            let fields =
                fields
                |> map FieldName.format
            
            let functions =
                functions
                |> map FunctionName.format
                
            let nestedTypes =
                nestedTypes
                |> map formatTypeName
                
            Module {FullName = fullName; Fields = fields; Functions = functions; NestedTypes = nestedTypes}
        
    let rec toDefinition typeInfo =
        let rec toModuleDefinition {FullName = (ModuleFullName (FormattedQualifiedName.SignificantName name) as fullName); Fields = fields; Functions = functions; NestedTypes = nestedTypes} =
            let fullName = 
                map (fun (FormattedQualifiedName.SignificantName name) -> stringBuilder {
                    "module"
                    " "
                    name
                    " ="
                } >> AliasDefinition) fullName
                
            let fields =
                fields
                |> map FieldName.toDefinition
                
            let functions =
                functions
                |> map FunctionName.toDefinition
                
            let nestedTypes =
                nestedTypes
                |> map toDefinition
                
            {FullName = fullName; Fields = fields; Functions = functions; NestedTypes = nestedTypes}
            
        match typeInfo with
        | ObjectType objectName ->
            ObjectType <| ObjectTypeName.toDefinition objectName
        | Module module' ->
            Module <| toModuleDefinition module'

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
            |> filter (fun t -> t.IsPublic)
            |> sortBy (fun x -> String.toLower x.FullName)
            |> map TypeInfo.fromType
            |> map TypeInfo.formatTypeName
            |> map TypeInfo.toDefinition
            
        workflow feliz
        |> iter (printfn "%A")
        0
