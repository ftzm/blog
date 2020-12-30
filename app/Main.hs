{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import           Control.Lens
import           Control.Monad
import           Data.Aeson                 as A
import           Data.Aeson.Lens
import           Data.Time
import           Data.Maybe (fromMaybe)
import           Development.Shake
import           Development.Shake.Classes
import           Development.Shake.Forward
import           Development.Shake.FilePath
import           GHC.Generics               (Generic)
import           Slick
import           Slick.Pandoc

import qualified Data.HashMap.Lazy as HML
import qualified Data.Text                  as T
import qualified Data.List as L


import Data.Map as M
import Data.Set as S
import Data.Function (on)
import Data.List (sortBy)

import Text.Pandoc hiding (getCurrentTime)

import Org


---Config-----------------------------------------------------------------------

siteMeta :: SiteMeta
siteMeta =
    SiteMeta { siteAuthor = "Matthew Fitzsimmons"
             , baseUrl = "https://ftzm.org"
             , siteTitle = "ftzm.org"
             , twitterHandle = Just "_ftzm"
             , githubUser = Just "ftzm"
             }

outputFolder :: FilePath
outputFolder = "docs/"

--Data models-------------------------------------------------------------------

withSiteMeta :: Value -> Value
withSiteMeta (Object obj) = Object $ HML.union obj siteMetaObj
  where
    Object siteMetaObj = toJSON siteMeta
withSiteMeta _ = error "only add site meta to objects"

data SiteMeta =
    SiteMeta { siteAuthor    :: String
             , baseUrl       :: String -- e.g. https://example.ca
             , siteTitle     :: String
             , twitterHandle :: Maybe String -- Without @
             , githubUser    :: Maybe String
             }
    deriving (Generic, Eq, Ord, Show, ToJSON)

-- | Data for the index page
data IndexInfo =
  IndexInfo
    { posts :: [Post]
    } deriving (Generic, Show, FromJSON, ToJSON)

type PostTag = String

data Tag = Tag
  { tag :: String
  , posts :: [Post]
  , tagUrl :: String
  } deriving (Generic, Show)

instance ToJSON Tag where
  toJSON Tag{..} = object
    [ "tag" A..= tag
    , "posts" A..= posts
    , "url" A..= tagUrl
    ]

-- | Data for a blog post
data Post =
    Post { title       :: String
         --, author      :: String
         , content     :: String
         , url         :: String
         , published   :: String
         , humanDate   :: String
         , tags        :: [PostTag]
         , teaser      :: String
         --, image       :: Maybe String
         , prev        :: Maybe Post
         , next        :: Maybe Post
         }
    deriving (Generic, Eq, Ord, Show, FromJSON, ToJSON, Binary)

data AtomData =
  AtomData { atomTitle        :: String
           , domain       :: String
           , author       :: String
           , posts        :: [Post]
           , currentTime  :: String
           , atomUrl      :: String } deriving (Generic, ToJSON, Eq, Ord, Show)

-- | given a list of posts this will build a table of contents
buildIndex :: [Post] -> Action ()
buildIndex posts' = do
  indexT <- compileTemplate' "site/templates/index.html"
  let indexInfo = IndexInfo {posts = posts'}
      asBlogPage =  _Object . at "blogPage" ?~ Bool True
      indexJson = asBlogPage . withSiteMeta $ toJSON indexInfo
      indexHTML = T.unpack $ substitute indexT indexJson
  writeFile' (outputFolder </> "index.html") indexHTML

-- | Find and build all posts
buildPosts :: Action [Post]
buildPosts = do
  pPaths <- getDirectoryFiles "." ["site/posts//*.md", "site/posts//*.org"]
  forP pPaths buildPost

buildPosts' :: Action [Post]
buildPosts' = do
  pPaths <- getDirectoryFiles "." ["site/posts//*.md", "site/posts//*.org"]
  posts <- sortByDate <$> forP pPaths parsePost
  let posts' = assignAdjacents posts
  _ <- forP posts' writePost
  liftIO $ mapM (\x -> do
                    print $ title x
                    print (maybe "nothing" title $ prev x)
                    putStrLn ""

                ) posts'
  return posts'

-------------------------------------------------------------------------------
-- About

aboutPath = "site/about.org"

buildAbout :: Action ()
buildAbout  = cacheAction ("build" :: T.Text, aboutPath) $ do
  liftIO . putStrLn $ "Rebuilding post: " <> aboutPath
  aboutContent <- readFile' aboutPath
  aboutData <- orgToHTML . T.pack $ aboutContent

  let aboutUrl = T.pack . dropDirectory1 $ aboutPath -<.> "html"
      asAboutPage =  _Object . at "aboutPage" ?~ Bool True
      withAboutUrl = _Object . at "url" ?~ String aboutUrl

  -- Add additional metadata we've been able to compute
  let fullAboutData = asAboutPage . withSiteMeta . withAboutUrl $ aboutData
  template <- compileTemplate' "site/templates/about.html"
  writeFile' (outputFolder </> T.unpack aboutUrl) . T.unpack $ substitute template fullAboutData

-------------------------------------------------------------------------------


customMarkdown txt = markdownToHTMLWithOpts defaultMarkdownOptions wops txt
  where wops = defaultHtml5Options
          { writerTableOfContents = True
          --, writerTemplate        = Just tocTemplate
          }

        -- When did it get so hard to compile a string to a Pandoc template?
        -- tocTemplate =
        --   either error id $ either (error . show) id $
        --   runPure $ runWithDefaultPartials $
        --   compileTemplate "" "$toc$\n$body$"


assignAdjacents :: [Post] -> [Post]
assignAdjacents posts = zipWith3 f
  posts
  (Nothing : (L.map Just $ init posts))
  ((tail $ L.map Just posts) ++ [Nothing])
  where f cur prev next = cur {prev = prev, next = next}

assignAdjacents' :: [Post] -> [Post]
assignAdjacents' posts =
  [ cur {prev = posts ^? ix (i-1), next = posts ^? ix (i+1)}
  | (cur, i) <- zip posts [0..]]

parsePost :: FilePath -> Action Post
parsePost srcPath = do
  --liftIO . putStrLn $ "Rebuilding post: " <> srcPath
  postContent <- readFile' srcPath

  -- load post content and metadata as JSON blob
  let parser = if (T.isSuffixOf ".org" $ T.pack srcPath) then orgToHTML else customMarkdown
  postData <- parser . T.pack $ postContent
  let postUrl = T.pack . dropDirectory1 $ srcPath -<.> "html"
      withPostUrl = _Object . at "url" ?~ String postUrl
      publishedString = postData ^? key "published" . _String
      humanDateString = T.pack . formatDateHuman . T.unpack $ fromMaybe "" publishedString
      withHumanDate = _Object . at "humanDate" ?~ String humanDateString

  -- Add additional metadata we've been able to compute
  let fullPostData = withHumanDate . withSiteMeta . withPostUrl $ postData
  convert fullPostData

writePost :: Post -> Action ()
writePost post = do
  template <- compileTemplate' "site/templates/post.html"
  writeFile' (outputFolder </> T.unpack (T.pack $ url post)) . T.unpack $ substitute template $ toJSON post


-- | Load a post, process metadata, write it to output, then return the post object
-- Detects changes to either post content or template
buildPost :: FilePath -> Action Post
buildPost srcPath = cacheAction ("build" :: T.Text, srcPath) $ do
  liftIO . putStrLn $ "Rebuilding post: " <> srcPath
  postContent <- readFile' srcPath

  -- load post content and metadata as JSON blob
  let parser = if (T.isSuffixOf ".org" $ T.pack srcPath) then orgToHTML else customMarkdown
  postData <- parser . T.pack $ postContent
  let postUrl = T.pack . dropDirectory1 $ srcPath -<.> "html"
      withPostUrl = _Object . at "url" ?~ String postUrl
      publishedString = postData ^? key "published" . _String
      withHumanDate = _Object . at "humanDate" ?~ String (T.pack . formatDateHuman . T.unpack $ fromMaybe "" publishedString)

  -- Add additional metadata we've been able to compute
  let fullPostData = withHumanDate . withSiteMeta . withPostUrl $ postData
  template <- compileTemplate' "site/templates/post.html"
  writeFile' (outputFolder </> T.unpack postUrl) . T.unpack $ substitute template fullPostData
  convert fullPostData

-- | Copy all static files from the listed folders to their destination
copyStaticFiles :: Action ()
copyStaticFiles = do
    filepaths <- getDirectoryFiles "./site/" ["images//*", "css//*", "js//*"]
    void $ forP filepaths $ \filepath ->
        copyFileChanged ("site" </> filepath) (outputFolder </> filepath)

formatDate :: String -> String
formatDate humanDate = toIsoDate parsedTime
  where
    parsedTime =
      parseTimeOrError True defaultTimeLocale "%Y-%M-%d" humanDate :: UTCTime

formatDateHuman :: String -> String
formatDateHuman humanDate = toHumanDate parsedTime
  where
    parsedTime =
      parseTimeOrError True defaultTimeLocale "%Y-%M-%d" humanDate :: UTCTime

rfc3339 :: Maybe String
rfc3339 = Just "%H:%M:SZ"

toIsoDate :: UTCTime -> String
toIsoDate = formatTime defaultTimeLocale (iso8601DateFormat rfc3339)

toHumanDate :: UTCTime -> String
toHumanDate = formatTime defaultTimeLocale "%d %b %Y"

sortByDate :: [Post] -> [Post]
sortByDate = sortBy (flip compareDates)
  where
    compareDates = compare `on` published

buildFeed :: [Post] -> Action ()
buildFeed posts = do
  now <- liftIO getCurrentTime
  let atomData =
        AtomData
          { atomTitle = siteTitle siteMeta
          , domain = baseUrl siteMeta
          , author = siteAuthor siteMeta
          , posts = mkAtomPost <$> posts
          , currentTime = toIsoDate now
          , atomUrl = "/atom.xml"
          }
  atomTempl <- compileTemplate' "site/templates/atom.xml"
  writeFile' (outputFolder </> "atom.xml") . T.unpack $ substitute atomTempl (toJSON atomData)
    where
      mkAtomPost :: Post -> Post
      mkAtomPost p = p { published = formatDate $ published p }

-- | Specific build rules for the Shake system
--   defines workflow to build the website
buildRules :: Action ()
buildRules = do
  allPosts <- buildPosts'
  allTags <- getTags allPosts
  buildTags allTags
  buildIndex allPosts
  buildFeed allPosts
  buildAbout
  copyStaticFiles

main :: IO ()
main = do
  let shOpts = shakeOptions { shakeVerbosity = Chatty, shakeLintInside = ["\\"]}
  shakeArgsForward shOpts buildRules

---

getTags :: [Post] -> Action [Tag]
getTags posts = do
   let tagToPostsSet = M.unionsWith mappend (toMap <$> posts)
       tagToPostsList = fmap S.toList tagToPostsSet
       tagObjects =
         foldMapWithKey
           (\tag ps -> [Tag {tag, posts = sortByDate ps, tagUrl = "/tag/" <> tag}])
           tagToPostsList
   return tagObjects
  where
    toMap :: Post -> Map String (Set Post)
    toMap p@Post {tags} = M.unionsWith mappend (embed p <$> tags)
    embed :: Post -> String -> Map String (Set Post)
    embed post tag = M.singleton tag (S.singleton post)

buildTags :: [Tag] -> Action ()
buildTags tags = do
    void $ forP tags writeTag

writeTag :: Tag -> Action ()
writeTag t@Tag{tagUrl} = do
  tagTempl <- compileTemplate' "site/templates/tag.html"
  writeFile' (outputFolder <> tagUrl -<.> "html") . T.unpack $ substitute tagTempl (toJSON t)
