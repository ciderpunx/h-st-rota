{-# LANGUAGE OverloadedStrings #-}
module RotaFeed where

import RotaEvaluator
import RotaParser
import RotaTweet

import System.Time
import Text.Atom.Feed
import Text.Atom.Feed.Export
import qualified Text.Feed.Types as T
import Text.Feed.Util
import Text.XML.Light.Output
import qualified Data.ByteString as B

feed = do
  f <- makeFeed
  return . ppTopElement $ xmlFeed f

feedWithEntryCount = do
  f <- makeFeed
  let ec = length $ feedEntries f
  return ((ppTopElement $ xmlFeed f),ec)

feedDate :: IO String
feedDate = do
  now <- getClockTime
  return $ toFeedDateString T.AtomKind now

feedAuthor :: IO Person
feedAuthor = do
    return (Person { personName = "Rotabott"
                   , personURI = Just "https://twitter.com/rotabott"
                   , personEmail = Nothing
                   , personOther = []
                   })

entryLink :: IO Link
entryLink = do
   return (Link { linkHref = "https://twitter.com/rotabott"
                , linkRel = Nothing
                , linkType = Nothing
                , linkHrefLang = Nothing
                , linkTitle = Nothing
                , linkLength = Nothing
                , linkAttrs = []
                , linkOther = []
                })

makeFeed = do
  p <- feedAuthor
  d <- feedDate
  e <- entries
  return ( Feed { feedId = "Hendred Street cleaning rota feed"
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
                })

makeTweetFromCmd :: Command -> IO String
makeTweetFromCmd c = 
  case c of
    Command (User u) StatusTD (Job j) -> 
            return . bsToStr $ strCat ["@",u,": don't forget #todo ", j, "!"]
    Command (User u) StatusDone (Job j) ->
            return . bsToStr $ strCat ["Yay! @", u, " just finished ", j, " #done"]
    Err e ->
            return . bsToStr $ strCat ["@", adminUser," @", selfUser, " encountered an #error: ", e
                                      , " -- better investigate"
                                      ]
    _ -> return $ "Oh dear, @rotabott hit a very strange error. Better investigate, @ciderpunx"

entries = do 
  cmds <- getCmds
  es <- mapM (tweetEntry . makeTweetFromCmd) cmds
  return es

tweetEntry :: IO String -> IO Entry
tweetEntry t = do
  p <- feedAuthor
  d <- feedDate
  c <- t
  l <- entryLink
  return (Entry { entryId = c
                , entryTitle = TextString c
                , entryUpdated = d
                , entryAuthors = [p]
                , entryCategories = []
                , entryContent = Just $ TextContent c
                , entryContributor = []
                , entryLinks = [l]
                , entryPublished = Just d
                , entryRights = Nothing
                , entrySource = Nothing
                , entrySummary = Just $ TextString c
                , entryInReplyTo = Nothing
                , entryInReplyTotal = Nothing
                , entryAttrs = []
                , entryOther = []
                })
