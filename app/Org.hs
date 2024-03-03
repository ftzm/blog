{-# LANGUAGE OverloadedStrings     #-}

module Org where

import qualified Data.Aeson as A
import           Data.Char (toLower)
import           Data.List as L
import           Data.Map as M
import qualified Data.Text as T
import           Development.Shake
import           Slick.Pandoc
import           Text.Pandoc
import           Text.Pandoc.Shared
import           Text.Pandoc.Builder
import           Text.Pandoc.Parsing
import           Text.Pandoc.Walk
import           Data.UUID.V4

defaultOrgOptions :: ReaderOptions
defaultOrgOptions =
  def { readerExtensions = exts }
  where
    exts = mconcat
     [ extensionsFromList
       [ Ext_fenced_code_attributes
       , Ext_auto_identifiers
       ]
     ]

-- | Parse a RawBlock as an org metadata key-value pair.
orgMetaKV :: Block -> (T.Text, MetaValue)
orgMetaKV (RawBlock _ txt) =
  case (parse parser "" txt) of
    Left err  -> error $ show err
    Right xs  -> xs
  where
    parser = do
      _ <- string "#+"
      key <- manyTill anyChar $ string ": "
      value <- colonList <|> remainder
      return (T.pack $ L.map toLower key, toMetaValue value)
    colonList = toMetaValue <$> do
      _ <- char ':'
      endBy (many alphaNum) (char ':')
    remainder = toMetaValue <$> many anyChar
orgMetaKV _ = error "Invalid block type for org metadata"


sideNoteHtml :: T.Text -> T.Text
sideNoteHtml labelSuffix =
  let label = "sn-" <> labelSuffix
  in mconcat
    [ "<label for=\""
    , label
    , "\" class=\"margin-toggle sidenote-number\">"
    , "</label><input type=\"checkbox\" id=\""
    , label
    , "\" class=\"margin-toggle\"/>"
    ]

toSideNote :: Inline -> IO Inline
toSideNote (Note [Para is]) = do
  i <- T.pack . show <$> nextRandom
  return
    $ Span ("", [], [] )
      [RawInline "html" $ sideNoteHtml i, Span ("", ["sidenote"], []) is]
toSideNote i = return i

marginNoteHtml :: T.Text -> T.Text
marginNoteHtml labelSuffix =
  let label = "mn-" <> labelSuffix
  in mconcat
    [ "<label for=\""
    , label
    , "\" class=\"margin-toggle\">&#8853;</label>"
    , "<input type=\"checkbox\" id=\""
    , label
    , "\" class=\"margin-toggle\"/>"
    ]

toMarginNote :: Inline -> IO Inline
toMarginNote l@(Link _ is (targetUrl, _)) = do
  i <- T.pack . show <$> nextRandom
  if "mn:" `T.isPrefixOf` targetUrl
    then return $ Span ("", [], [] )
      [RawInline "html" $ marginNoteHtml i ,Span ("", ["marginnote"], []) is]
    else return l
toMarginNote i = return i

killFootnoteHeader :: Block -> Block
killFootnoteHeader (Header _ ("footnotes", _, _) _) = Null
killFootnoteHeader b = b

-- | Parse unparsed org metadata from pandoc blocks and move to Meta.
-- This is useful because Pandoc's org parser ignores all but a few
-- metadata keys by default.
orgAllMeta :: Pandoc -> Pandoc
orgAllMeta (Pandoc (Meta meta) blocks) = Pandoc expandedMeta remainderBlocks
  where
    expandedMeta = Meta $ M.union meta newMeta
    newMeta = M.fromList $ L.map orgMetaKV rawMeta
    (rawMeta, remainderBlocks) = span rawOrgBlock blocks
    rawOrgBlock b
      | RawBlock (Format "org") _ <- b = True
      | otherwise = False

orgToHTMLWithOpts :: ReaderOptions -> WriterOptions -> T.Text -> Action A.Value
orgToHTMLWithOpts rops wops txt =
  loadUsing
    parseOrg
    (writeHtml5String (wops {writerSectionDivs = True}))
    txt
  where
    parseOrg :: T.Text -> PandocIO Pandoc
    parseOrg input = do
      result <- walkM (liftIO . toSideNote) =<< walkM (liftIO . toMarginNote) =<< (fmap (walk killFootnoteHeader . headerShift 1 . orgAllMeta) $ readOrg rops input)
      liftIO $ print result
      return result

orgToHTML :: T.Text -> Action A.Value
orgToHTML txt =  orgToHTMLWithOpts defaultOrgOptions defaultHtml5Options txt

-- orgToHTML :: T.Text -> Action Value
-- orgToHTML txt =  orgToHTMLWithOpts defaultOrgOptions wops txt
--   where wops = defaultHtml5Options
--           { writerTableOfContents = True
--           , writerTemplate        = Just tocTemplate
--           }
--         tocTemplate :: Template T.Text
--         tocTemplate =
--           either error id $ either (error . show) id $
--           runPure $ runWithDefaultPartials $
--           compileTemplate "" "$toc$\n$content$"
