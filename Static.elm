module Static where

import Date exposing ( Date )

type alias Email =
  { from: String
  , to: String
  , title: String
  , body: String
  , date: String
  }

type alias Reminder =
  { body: String
  , created: String
  }

reminders : List Reminder
reminders =
  [ { body = "Take out the trash", created = "2016-09-30" }
  , { body = "Groceries", created = "2015-09-25" }
  ]

emails : List Email
emails =
  [ { from = "bossman@corporate.me"
    , to = "manager@corporate.me"
    , title = "Corporate Ipsum"
    , body = """Collaboratively administrate empowered markets via plug-and-play
                networks. Dynamically procrastinate B2C users after installed base
                benefits. Dramatically visualize customer directed convergence without
                revolutionary ROI.
                
                Efficiently unleash cross-media information without cross-media
                value. Quickly maximize timely deliverables for real-time
                schemas. Dramatically maintain clicks-and-mortar solutions
                without functional solutions.
                
                Completely synergize resource taxing relationships via premier
                niche markets. Professionally cultivate one-to-one customer
                service with robust ideas. Dynamically innovate
                resource-leveling customer service for state of the art customer
                service."""
    , date = "2015-01-30"
    }
  , { from = "hello@test.me"
    , to = "goodbye@test.me"
    , title = "Shorter than 200"
    , body = """This is the body of an email with less than 200 characters."""
    , date = "2015-09-30"
    }
  ]
