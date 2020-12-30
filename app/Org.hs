{-# LANGUAGE OverloadedStrings     #-}

module Org where

import           Development.Shake
import Text.Pandoc hiding (getCurrentTime)
import Text.Pandoc.Builder
import Slick.Pandoc
import Data.List as L
import Data.Maybe (fromJust)
import Data.Map as M
import Data.Char (toLower)
import Data.List.Split
import qualified Data.Text                  as T
import           Data.Aeson                 as A
import Text.Pandoc

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

orgAllMeta :: Pandoc -> Pandoc
orgAllMeta (Pandoc (Meta meta) blocks) = Pandoc meta' blocks'
  where
    meta' = Meta $ M.union meta newMeta
    newMeta = M.fromList $ L.map orgMetaKV rawMeta
    orgMetaKV (RawBlock _ txt) = (T.pack $ L.map toLower k, if L.isPrefixOf ":" v then toMetaValue $ L.map T.pack $ L.filter (not . L.null) $ splitOn ":" v else toMetaValue $ T.pack v)
      where stripped = fromJust $ stripPrefix "#+" $ T.unpack txt
            (k, v) = fmap (L.drop 2) $ L.splitAt (fromJust $ L.elemIndex ':' stripped) stripped
    (rawMeta, blocks') = span rawOrgBlock blocks
    rawOrgBlock b
      | RawBlock (Format "org") _ <- b = True
      | otherwise = False

readOrgFiltered :: PandocMonad m => (Pandoc -> Pandoc) -> ReaderOptions -> T.Text -> m Pandoc
readOrgFiltered f o t = f <$> readOrg o t

orgToHTMLWithOpts
    :: ReaderOptions  -- ^ Pandoc reader options to specify extensions or other functionality
    -> WriterOptions  -- ^ Pandoc writer options to modify output
    -> T.Text         -- ^ Text for conversion
    -> Action Value
orgToHTMLWithOpts rops wops txt =
  loadUsing
    (readOrgFiltered orgAllMeta rops)
    (writeHtml5String wops)
    txt

orgToHTML :: T.Text -> Action Value
orgToHTML txt =  orgToHTMLWithOpts defaultOrgOptions wops txt
  where wops = defaultHtml5Options
          { writerTableOfContents = True
          --, writerTemplate        = Just tocTemplate
          }

        -- When did it get so hard to compile a string to a Pandoc template?
        -- tocTemplate :: Template T.Text
        -- tocTemplate =
        --   either error id $ either (error . show) id $
        --   runPure $ runWithDefaultPartials $
        --   compileTemplate "" "$toc$\n$content$"
