{-# LANGUAGE OverloadedStrings #-}
module RotaFeed where

import System.Time
import Text.Atom.Feed
import Text.Atom.Feed.Export
import qualified Text.Feed.Types as T
import Text.Feed.Util
import Text.XML.Light.Output

import RotaTweet

feed :: IO String
feed = do
  f <- makeFeed
  return . ppTopElement $ xmlFeed f

feedWithEntryCount :: IO (String,Int)
feedWithEntryCount = do
  f <- makeFeed
  let ec = length $ feedEntries f
  return (ppTopElement $ xmlFeed f,ec)

feedDate :: IO String
feedDate = do
  now <- getClockTime
  return $ toFeedDateString T.AtomKind now

feedAuthor :: IO Person
feedAuthor = 
    return Person { personName = "Rotabott"
                  , personURI = Just "https://twitter.com/rotabott"
                  , personEmail = Nothing
                  , personOther = []
                  }

entryLink :: IO Link
entryLink = 
   return Link { linkHref = "https://twitter.com/rotabott"
               , linkRel = Nothing
               , linkType = Nothing
               , linkHrefLang = Nothing
               , linkTitle = Nothing
               , linkLength = Nothing
               , linkAttrs = []
               , linkOther = []
               }

makeFeed :: IO Feed
makeFeed = do
  p <- feedAuthor
  d <- feedDate
  e <- entries
  return Feed { feedId = "Hendred Street cleaning rota feed"
              , feedTitle = TextString "Feed of cleaning jobs for Hendred street." 
              , feedUpdated = d
              , feedAuthors = [p]
              , feedCategories = []
              , feedContributors = []
              , feedGenerator = Nothing
              , feedIcon = Nothing
              , feedLinks = []
              , feedLogo = Nothing
              , feedRights = Nothing
              , feedSubtitle = Just $ HTMLString "Only exists because @twitter is too fucking braindead to let you tweet using the API unless you have a mobile phone. Wankers."
              , feedEntries = e
              , feedAttrs = []
              , feedOther = []
              }

entries :: IO [Entry]
entries = do 
  cmds <- getCmds
  mapM (tweetEntry . makeTweetFromCmd) cmds

tweetEntry :: String -> IO Entry
tweetEntry t = do
  p <- feedAuthor
  d <- feedDate
  l <- entryLink
  return Entry { entryId = t
               , entryTitle = TextString t
               , entryUpdated = d
               , entryAuthors = [p]
               , entryCategories = []
               , entryContent = Just $ TextContent t
               , entryContributor = []
               , entryLinks = [l]
               , entryPublished = Just d
               , entryRights = Nothing
               , entrySource = Nothing
               , entrySummary = Just $ TextString t
               , entryInReplyTo = Nothing
               , entryInReplyTotal = Nothing
               , entryAttrs = []
               , entryOther = []
               }
