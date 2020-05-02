# Feliz.Isomorphic
Polymorphic Type aliases for Server-Side Rendering development with Feliz
# The problem
In order to have shared rendering code betwen the Client and the Server, you would normally have to using compiler directives.
# Possible solutions
## Option #1
Include compiler directive in every file.

Ex:
```fsharp
namespace Counter

#if FABLE_COMPILER
open Feliz
open Feliz.Bulma

#else
open Feliz.ViewEngine
open Feliz.Bulma.ViewEngine

#endif
```
Works, but it is annoying. Annoyance scales with number of files.

## Option #2
Make a file with all the type aliases. Ex:

```fsharp
namespace Aliases

#if FABLE_COMPILER

type Html = Feliz.Html
type prop = Feliz.prop
...
#else

type Html = Feliz.ViewEngine.Html
type prop = Feliz.ViewEngine.prop

...
#endif
```
Works and shared files are clean! But, you have add type abbreviations by hand everytime. Annoyance scales with number of `Feliz` types.

# Enter Feliz.Isomorphic
Feliz.Isomorphic contains all types alias from the Feliz libraries (Feliz and Feliz bulma for now). Simply add the project to your solution and use its type alises!  

```fsharp
namespace Template

open Feliz.Isomorphic.Feliz
open Feliz.Isomorphic.Feliz.Bulma
open Feliz.Isomorphic.Feliz.Bulma.Bulma

[<RequireQualifiedAccess>]
module Counter =
    open Feliz.ElmishTypes
    
    type State = { Count: int }

    type Msg =
        | Increment
        | Decrement
    
    let render (state:State) (dispatch:Msg -> unit) =
        Html.div [
            button.button [
                color.isBlack
                prop.onClick (fun _ -> dispatch Increment)
                prop.text "Increment"
            ]
    
            button.button [
                prop.onClick (fun _ -> dispatch Decrement)
                prop.text "Decrement"
            ]
    
            Html.h1 state.Count
        ]
```
# Adding new `Feliz` libraries
To generate new files, add a JSON object in the src/Generator/pairs.json file

```json
[
  {
    "client": {
      "name": "Feliz",
      "includeAliases": [
        "Feliz.ReactElement"
      ]
    },
    "server": {
      "name": "Feliz.ViewEngine",
      "includeAliases": [
      ]
    }
  },
  {
    "client": {
      "name": "Name.Of.Client.Library.Assembly.Name",
      "includeAliases": [
      ]
    },
    "server": {
      "name": "Name.Of.Server.Library.Assembly.Name",
      "includeAliases": [
        "Darn.This.Was.An.Abbreviated.Type"
      ]
    }
  }
]
```

Once that is done, run `fake build` to generate the new code! **You will have to link the new file in the project manually**
