{-# LANGUAGE OverloadedStrings #-}
module RotaPrivateData where
import Web.Authenticate.OAuth

-- File where atom feed will live 
atomFile = "/www/rota.atom"

-- Rota file
rotaFile = "/etc/rota"

-- People on the rota
ps = [ "@ciderpunx"
     , "@example"
     , "@someone_else"
     ]

-- Jobs for the rota
js = [ "hoovering the stairs" 
     , "cleaning the cooker"
     , "cleaning the fridge"
     , "hoovering hall/landing"
     , "cleaning/mopping kitchen"
     , "cleaning/hoovering front room"
     , "cleaning/mopping upstairs bathroom"
     , "cleaning/mopping downstairs bathroom"
     , "gardening"
     , "recycling, garden waste, bins and food waste"
     , "trimming hedge"
     , "cleaning kitchen sink"
     ]

-- Creds for twitter. You need to create an app apps.twitter.com in order to get these creds
myoauth :: OAuth
myoauth = newOAuth { oauthServerName     = "api.twitter.com"
                   , oauthConsumerKey    = "SECRET"
                   , oauthConsumerSecret = "SECRET"
                   }

mycred :: Credential
mycred = newCredential "SECRET"
                       "SECRET"
