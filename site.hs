{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad
import Data.List (isPrefixOf)
import qualified Data.Map as M
import qualified Data.Text as T
import Hakyll
import Skylighting (Syntax, parseSyntaxDefinition)
import System.Directory (removeFile)
import System.FilePath
import Text.Pandoc
import Text.Pandoc.Walk

postCtx :: Context String
postCtx =
  mconcat
    [ modificationTimeField "mtime" "%U",
      dateField "date" "%B %e, %Y",
      defaultContext
    ]

static :: Rules ()
static = void $ route idRoute >> compile copyFileCompiler

main :: IO ()
main = do
  futhark_syntax <-
    either (error . show) return
      =<< parseSyntaxDefinition "skylighting/futhark.xml"
  hakyllWith config $ do
    match "images/*" static

    match "skylighting/*" static

    match "robots.txt" static

    match "static/*" static

    match "publications/*" static

    match "student-projects/*" static

    match "benchmarks/programs/*" static

    match "css/*" $ do
      route idRoute
      compile compressCssCompiler

    match
      ( fromList
          [ "performance.md",
            "docs.md",
            "publications.md",
            "getinvolved.md",
            "index.md",
            "examples.md"
          ]
      )
      $ do
        route $ setExtension "html"
        compile $ do
          menu <- contentContext
          pandocFutCompiler futhark_syntax
            >>= loadAndApplyTemplate "templates/withtitle.html" menu
            >>= loadAndApplyTemplate "templates/default.html" menu
            >>= relativizeUrls

    let blogCompiler = do
          route $ setExtension "html"
          compile $ do
            postCtx' <- postContext
            pandocFutCompiler futhark_syntax
              >>= loadAndApplyTemplate "templates/post.html" postCtx'
              >>= saveSnapshot "content"
              >>= loadAndApplyTemplate "templates/withtitle.html" postCtx'
              >>= loadAndApplyTemplate "templates/default.html" postCtx'
              >>= relativizeUrls

    match "blog/*.rst" blogCompiler
    match "blog/*.md" blogCompiler
    match "blog/*.fut" static
    match "blog/*-img/*" static

    -- Post list
    create ["blog.html"] $ do
      route idRoute
      compile $ do
        menu <- getMenu
        posts <- recentFirst =<< loadAll (fromRegex "blog/.*\\.(rst|md)")
        let ctx =
              constField "title" "Developer Blog"
                <> listField "posts" postCtx (return posts)
                <> constField "menu" menu
                <> defaultContext
        makeItem ""
          >>= loadAndApplyTemplate "templates/posts.html" ctx
          >>= loadAndApplyTemplate "templates/withtitle.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls

    -- Atom feed
    create ["atom.xml"] $ do
      route idRoute
      compile $ do
        let feedCtx = postCtx `mappend` bodyField "description"
        posts <-
          fmap (take 10) . recentFirst
            =<< loadAllSnapshots (fromRegex "blog/.*\\.(rst|md)") "content"
        renderAtom feedConfiguration feedCtx posts

    -- Examples
    match "examples/*-img/*" static
    match "examples/*.fut" $ version "source" static
    match "examples/*.fut" $ do
      route $ setExtension "html"
      compile $ do
        menu <- contentContext
        futCompiler futhark_syntax
          >>= loadAndApplyTemplate "templates/default.html" menu
          >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

contentContext :: Compiler (Context String)
contentContext = do
  menu <- getMenu
  return $
    defaultContext
      `mappend` constField "menu" menu

getMenu :: Compiler String
getMenu = do
  myRoute <- getRoute =<< getUnderlying
  return $ case myRoute of
    Nothing -> showMenu "" menuContents
    Just me -> showMenu me menuContents

menuContents :: [(String, FilePath)]
menuContents =
  [ ("Overview", "index.html"),
    ("Examples", "examples.html"),
    ("Docs", "docs.html"),
    ("Publications", "publications.html"),
    ("Gotta Go Fast!", "performance.html"),
    ("Get Involved", "getinvolved.html"),
    ("Blog", "blog.html")
  ]

showMenu :: FilePath -> [(String, FilePath)] -> String
showMenu this items = "<ul id=\"menu\">" ++ concatMap li items ++ "</ul>"
  where
    li (name, item)
      | isThis item = "<li id=\"chosen\"><a href=\"/" ++ item ++ "\">" ++ name ++ "</a></li>"
      | otherwise = "<li><a href=\"/" ++ item ++ "\">" ++ name ++ "</a></li>"
    isThis item = dropExtension item `isPrefixOf` dropExtension this

--------------------------------------------------------------------------------
postContext :: Compiler (Context String)
postContext = do
  ctx <- contentContext
  return $ dateField "date" "%B %e, %Y" `mappend` ctx

--------------------------------------------------------------------------------

-- | By default, RST will make all top-level titles <h1>s, but we
-- prefer to only have a single <h1>: the one in the template.  We use
-- a technique from
-- http://maxdelgiudice.com/posts/2015-07-08-rst-headers.html to avoid
-- this.
shiftHeaderUp :: Block -> Block
shiftHeaderUp h@(Header n a b)
  | n < 6 = Header (n + 1) a b
  | otherwise = h
shiftHeaderUp x = x

-- | All headers should be links to themselves.
selfLinkHeader :: Block -> Block
selfLinkHeader (Header n (ident, classes, kvs) b) =
  Header n (ident, classes, kvs) [b']
  where
    b' = Link (ident <> "-link", ["titlelink"], []) b ("#" <> ident, ident)
selfLinkHeader x = x

pandocOptions :: Syntax -> (ReaderOptions, WriterOptions)
pandocOptions futhark_syntax =
  ( defaultHakyllReaderOptions {readerIndentedCodeClasses = ["Futhark"]},
    defaultHakyllWriterOptions {writerSyntaxMap = syntaxmap}
  )
  where
    syntaxmap =
      M.insert "Futhark" futhark_syntax $
        writerSyntaxMap defaultHakyllWriterOptions

pandocFutCompiler :: Syntax -> Compiler (Item String)
pandocFutCompiler futhark_syntax =
  pandocCompilerWithTransform ropts wopts $
    walk (selfLinkHeader . shiftHeaderUp)
  where
    (ropts, wopts) = pandocOptions futhark_syntax

futCompiler :: Syntax -> Compiler (Item String)
futCompiler futhark_syntax = do
  source <- getResourceFilePath
  void $ unixFilter "futhark" ["literate", "-v", source] mempty
  let mdfile = source `replaceExtension` "md"
  item <- makeItem =<< unsafeCompiler (readFile mdfile)
  let oldident = itemIdentifier item
  unsafeCompiler $ removeFile mdfile
  item' <-
    renderPandocWithTransform
      ropts
      wopts
      (addSourceLink source . walk selfLinkHeader)
      item {itemIdentifier = fromFilePath mdfile}
  pure item' {itemIdentifier = oldident}
  where
    (ropts, wopts) = pandocOptions futhark_syntax

    addSourceLink source (Pandoc meta blocks) =
      Pandoc meta $ Plain [srclink] : blocks
      where
        sourcename = T.pack $ takeFileName source
        attr = ("", ["sourcelink"], [])
        text = Str "Source file: "
        link = Link mempty [Str sourcename] (T.pack ("/" </> source), sourcename)
        srclink = Span attr [text, link]

--------------------------------------------------------------------------------
config :: Configuration
config =
  defaultConfiguration
    { deployCommand =
        "rsync --chmod=Do+rx,Fo+r --checksum -ave 'ssh -p 22' \
        \_site/* --exclude pub futhark@futhark-lang.org:/var/www/htdocs/futhark-lang.org"
    }

feedConfiguration :: FeedConfiguration
feedConfiguration =
  FeedConfiguration
    { feedTitle = "Futhark Developer Blog",
      feedDescription = "High-performance purely functional data-parallel array programming on the GPU",
      feedAuthorName = "Troels Henriksen",
      feedAuthorEmail = "athas@sigkill.dk",
      feedRoot = "http://futhark-lang.org"
    }
