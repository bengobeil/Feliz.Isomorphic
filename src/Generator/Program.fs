namespace Feliz.Isomorphic.Generator

open System
open System.Text
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
module AliasDefinition =
    let value (AliasDefinition def) = def
    
module FormattedQualifiedName =
    let (|SignificantName|) (FormattedQualifiedName name) =
        name.Split '.'
        |> Array.last
        |> fun name -> SignificantName name

type TypeName<'Format> = TypeName of 'Format with
    static member Map (TypeName x, f) = TypeName (f x)
    
module ObjectTypeName =
    let format (TypeName (RawQualifiedName name)) =
        name
        |> String.replace "+" "."
        |> String.replace "Module." "."
        |> FormattedQualifiedName
        |> TypeName
        
    let toDefinition objectTypeName: TypeName<AliasDefinition> =
        map (fun (FormattedQualifiedName.SignificantName significantName & FormattedQualifiedName fullName) -> stringBuilder {
            "type"
            " "
            significantName
            " = "
            fullName
        } >> (AliasDefinition << StringBuffer.run)) objectTypeName

type FunctionName<'Format> = FunctionName of 'Format with
    static member Map (FunctionName x, f) = FunctionName (f x)
    
module FunctionName =
    let fromFieldInfo (fieldInfo: System.Reflection.MethodInfo) =
        (FunctionName << RawQualifiedName) <| fieldInfo.DeclaringType.FullName ++ "." ++ fieldInfo.Name
        
    let format (FunctionName (RawQualifiedName name)) =
        name
        |> String.replace "+" "."
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
        } >> (AliasDefinition << StringBuffer.run)) functionName

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
            |> String.replace "+" "."
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
            (AliasDefinition << StringBuffer.run) <| stringBuilder {
            "let"
            " "
            match fieldName with
            | Literal _ -> "[<Literal>] "
            | _ -> ""
            significantName
            " = "
            fullName
        })
        
type ModuleName<'Format> = ModuleName of 'Format with
    static member Map (ModuleName x, f) = ModuleName (f x)
    
type ConstructName<'Format> =
    | Field of FieldName<'Format>
    | Function of FunctionName<'Format>
    | Type of TypeName<'Format>
    | Module of ModuleName<'Format>
    with
        static member Map (x,f) =
            match x with
            | Field r -> Field <| map f r
            | Function r -> Function <| map f r
            | Type r -> Type <| map f r
            | Module r -> Module <| map f r
    
module ConstructName =
    let value (Field (Literal name | NonLiteral name) | Function (FunctionName name) | Type (TypeName name) | Module (ModuleName name)) =
        name

type Module<'Format> = { FullName: ModuleName<'Format>; Fields: FieldName<'Format> seq; Functions: FunctionName<'Format> seq; NestedTypes: TypeInfo<'Format> seq }
and TypeInfo<'Format> =
    | ObjectType of TypeName<'Format>
    | Module of Module<'Format> with
    static member Map (x, f) =
        match x with
        | ObjectType x -> map f x |> ObjectType    
        | Module {FullName = fullName; Fields = fields; Functions = functions; NestedTypes = nestedTypes} ->
            Module {FullName = map f fullName; Fields = map (map f) fields; Functions = map (map f) functions; NestedTypes = map (map f) nestedTypes}
    
module ModuleFullName =
    let format (ModuleName (RawQualifiedName name)) =
        name
        |> String.replace "+" "."
        |> String.replace "Module" ""
        |> FormattedQualifiedName
        |> ModuleName
    

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
                |> sortBy (fun x -> String.toLower x.FullName)
                |> map fromType
                
            Module <| {FullName = ModuleName <| RawQualifiedName t.FullName; Fields = fields; Functions = functions; NestedTypes = nestedTypes }
            
        | t -> (ObjectType << TypeName << RawQualifiedName) t.FullName
        
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
        let rec toModuleDefinition {FullName = (ModuleName (FormattedQualifiedName.SignificantName name) as fullName); Fields = fields; Functions = functions; NestedTypes = nestedTypes} =
            let fullName = 
                map (fun (FormattedQualifiedName.SignificantName name) -> stringBuilder {
                    "module"
                    " "
                    name
                    " ="
                } >> (AliasDefinition << StringBuffer.run)) fullName
                
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
        
    let inline getFullDefinitionBuffer typeInfos =
        let tab = "    "
        
        let rec getDefinitionForTypeInfo (prefix: string) = function
            | ObjectType (TypeName (AliasDefinition def)) -> stringBuilder {prefix;def}
            | Module {FullName = fullName; Fields = fields; Functions = functions; NestedTypes = nestedTypes} ->
                let (++) = StringBuilder.(++)
                let childrenPrefix = prefix + tab
                
                let constructDefinition ctor prefix: 'a -> StringBuffer =
                    ctor
                    >> ConstructName.value
                    >> AliasDefinition.value
                    >> ((++) prefix)
                    
                let moduleDef =
                    fullName
                    |> constructDefinition ConstructName.Module prefix
                
                let fieldsDef =
                    fields
                    |> map (constructDefinition ConstructName.Field childrenPrefix)
                    |> StringBuffer.reduce
                    
                let functionsDef =
                    functions
                    |> map (constructDefinition ConstructName.Function childrenPrefix)
                    |> StringBuffer.reduce
                    
                let nestedTypeDefs =
                    nestedTypes
                    |> map (getDefinitionForTypeInfo childrenPrefix)
                    |> StringBuffer.reduce
                    
                seq {
                    StringBuffer.append moduleDef fieldsDef
                    functionsDef
                    nestedTypeDefs
                }
                |> StringBuffer.reduce
                
        typeInfos
        |> map (getDefinitionForTypeInfo String.Empty)
        |> StringBuffer.reduce    

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
    
    let writeToFile path content =
            System.IO.File.WriteAllText(sprintf "%s/%s"__SOURCE_DIRECTORY__ path, content)
            
    let getAliasDefinitionsBufferForAssembly {Name = name} =
        Assembly.Load name
        |> Assembly.getRootTypes
        |> exclude (fun t -> t.FullName.StartsWith "<StartupCode$")
        |> exclude (fun t -> t.FullName.Contains "@")
        |> filter (fun t -> t.IsPublic)
        |> sortBy (fun x -> String.toLower x.FullName)
        |> map TypeInfo.fromType
        |> map TypeInfo.formatTypeName
        |> map TypeInfo.toDefinition
        |> TypeInfo.getFullDefinitionBuffer
        
    let buildFileContent namespace' clientAssemblyName (clientDefinitionsBuffer: StringBuffer) (serverDefinitionsBuffer: StringBuffer) =
        stringBuilder {
            seq {
            sprintf "namespace %s.%s" namespace' clientAssemblyName
            ""
            "#if FABLE_COMPILER"
            ""
            }
            yield! clientDefinitionsBuffer
            "\n#else\n\n"
            yield! serverDefinitionsBuffer
            "\n#endif"
        }
        
    let produceFile assemblyPair =
        let clientRootName = assemblyPair.Client.Name
        let clientDefinitions = getAliasDefinitionsBufferForAssembly assemblyPair.Client
        let serverDefinitions = getAliasDefinitionsBufferForAssembly assemblyPair.Server
        buildFileContent "Feliz.Isomorphic" clientRootName clientDefinitions serverDefinitions
        |> StringBuffer.run
        |> writeToFile ("../Feliz.Isomorphic/" + clientRootName + ".fs")
        
    [<EntryPoint>]
    let main argv =
        [felizPair
         felizBulmaPair]
        |> iter produceFile
        0
