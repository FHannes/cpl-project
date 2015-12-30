module MailFetcher (ServerMails, fetch) where

import Http
import Json.Decode as Json exposing ( Decoder, (:=) )
import Task exposing ( Task )

import Static exposing ( Email )

type alias ServerMails = { mails: List Email }

mailRequest : Http.Request
mailRequest =
  { verb = "GET"
  , headers =
    [ ("Origin", "http://people.cs.kuleuven.be")
    , ("Access-Control-Request-Method", "GET")
    , ("Access-Control-Request-Headers", "X-Custom-Header")
    ]
  , url = "http://people.cs.kuleuven.be/~bob.reynders/2015-2016/emails.json"
  , body = Http.empty
  }

mailDecoder : Decoder ServerMails
mailDecoder =
  let
    mail = Json.object5 Email
      ("from" := Json.string)
      ("to" := Json.string)
      ("title" := Json.string)
      ("date" := Json.string)
      ("body" := Json.string)
    mails = Json.list mail
  in
    Json.object1 ServerMails ("emails" := mails)

fetch : Task Http.Error ServerMails
fetch =
  Http.fromJson mailDecoder <| Http.send Http.defaultSettings mailRequest
