{-# LANGUAGE OverloadedStrings     #-}

module Org where

import           Data.Aeson as A
import           Data.Char (toLower)
import           Data.List as L
import           Data.Map as M
import           Data.Maybe (fromJust)
import qualified Data.Text as T
import           Development.Shake
import           Slick.Pandoc
import           Text.Pandoc
import           Text.Pandoc.Builder
import           Text.Pandoc.Parsing

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

orgToHTMLWithOpts
    :: ReaderOptions
    -> WriterOptions
    -> T.Text
    -> Action Value
orgToHTMLWithOpts rops wops txt =
  loadUsing
    (fmap orgAllMeta <$> readOrg rops) -- <$> is over partially applied func
    (writeHtml5String wops)
    txt

orgToHTML :: T.Text -> Action Value
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
