﻿open SuaveRestApi.Rest
open SuaveRestApi.Db
open Suave.Web
open Suave.Http
open Suave.Successful

[<EntryPoint>]
let main argv = 
    let personWebPart = rest "people" {
        GetAll = Db.getPeople
        Create = Db.createPerson
        Update = Db.updatePerson
        Delete = Db.deletePerson
    }
    startWebServer defaultConfig personWebPart
    0
