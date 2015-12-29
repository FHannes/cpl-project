module MailFetcher where

import Http
import Json.Decode as Json exposing ( Decoder, (:=) )
import Task exposing ( Task )

import Static exposing ( Email )

type alias ServerMails = { mails: List Email }

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
  let
    mailURL = "http://people.cs.kuleuven.be/~bob.reynders/2015-2016/emails.json"
  in
    Http.get mailDecoder mailURL
