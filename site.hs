{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad
import Data.List (isPrefixOf)
import qualified Data.Map as M
import qualified Data.Text as T
import Hakyll
import Skylighting (Syntax, parseSyntaxDefinition)
import System.Directory (copyFile, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Text.Pandoc
import Text.Pandoc.Shared (stringify)
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

    match "agitprop/*" static

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
            "examples.md",
            "license.md",
            "acknowledgements.md",
            "hedgehogs.md"
          ]
      )
      $ do
        route $ setExtension "html"
        compile $ do
          (title, body) <- pandocFutCompiler futhark_syntax
          ctx <- contentContext title
          loadAndApplyTemplate "templates/withtitle.html" ctx body
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls

    -- Tikz documents
    match "tikz/*" $ do
      route $ setExtension "svg"
      compile tikzCompiler

    let blogCompiler = do
          route $ setExtension "html"
          compile $ do
            (title, body) <- pandocFutCompiler futhark_syntax
            postCtx' <- postContext title
            loadAndApplyTemplate "templates/post.html" postCtx' body
              >>= saveSnapshot "content"
              >>= loadAndApplyTemplate "templates/withtitle.html" postCtx'
              >>= loadAndApplyTemplate "templates/default.html" postCtx'
              >>= relativizeUrls

    match "blog/*.rst" blogCompiler
    match "blog/*.md" blogCompiler
    match "blog/*.fut" static
    match "blog/*-img/*" static
    match "blog/*/*" static

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
        (title, body) <- futCompiler futhark_syntax
        ctx <- contentContext title
        loadAndApplyTemplate "templates/default.html" ctx body
          >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

contentContext :: Maybe String -> Compiler (Context String)
contentContext title = do
  menu <- getMenu
  return $
    metadataField
      <> maybe mempty (constField "title") title
      <> defaultContext
      <> constField "menu" menu

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
postContext :: Maybe String -> Compiler (Context String)
postContext title = do
  ctx <- contentContext title
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

-- | The title of a document, if it begins with a top level header. We look only
-- at the very first block; a header anywhere else is a section heading, not the
-- title of the page.
docHeading :: Pandoc -> Maybe String
docHeading (Pandoc _ (Header 1 _ inlines : _)) = Just $ T.unpack $ stringify inlines
docHeading _ = Nothing

-- | Remove the header extracted by 'docHeading', for pages where the template
-- renders the title itself.
dropHeading :: Pandoc -> Pandoc
dropHeading (Pandoc meta (Header 1 _ _ : blocks)) = Pandoc meta blocks
dropHeading doc = doc

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

-- | Also returns the title extracted from the content, which is removed from
-- the body, as the template renders it separately.
pandocFutCompiler :: Syntax -> Compiler (Maybe String, Item String)
pandocFutCompiler futhark_syntax = do
  doc <- readPandocWith ropts =<< getResourceBody
  pure
    ( docHeading $ itemBody doc,
      writePandocWith wopts $
        walk (selfLinkHeader . shiftHeaderUp) . dropHeading <$> doc
    )
  where
    (ropts, wopts) = pandocOptions futhark_syntax

-- | Unlike 'pandocFutCompiler', the title is left in the body, as these pages
-- do not go through @withtitle.html@.  We extract it only so it can be used for
-- the HTML @title@ element.
futCompiler :: Syntax -> Compiler (Maybe String, Item String)
futCompiler futhark_syntax = do
  source <- getResourceFilePath
  void $ unixFilter "futhark" ["literate", "-v", source] mempty
  let mdfile = source `replaceExtension` "md"
  item <- makeItem =<< unsafeCompiler (readFile mdfile)
  let oldident = itemIdentifier item
  unsafeCompiler $ removeFile mdfile
  doc <- readPandocWith ropts item {itemIdentifier = fromFilePath mdfile}
  let item' =
        writePandocWith wopts $
          addSourceLink source . walk selfLinkHeader <$> doc
  pure (docHeading $ itemBody doc, item' {itemIdentifier = oldident})
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

-- | Compile a standalone Tikz/.tex file into an SVG, by first running pdflatex
-- to produce a PDF, then pdf2svg to convert that PDF to SVG. The .tex file is
-- expected to be a complete, compilable LaTeX document (e.g. using the
-- standalone document class) containing a Tikz picture.
tikzCompiler :: Compiler (Item String)
tikzCompiler = do
  source <- getResourceFilePath

  makeItem <=< unsafeCompiler . withSystemTempDirectory "tikz" $ \tmpdir -> do
    -- pdflatex insists on writing its output next to the input
    -- (or to -output-directory), and wants a sensible basename.
    let texFile = tmpdir </> "input.tex"
        pdfFile = tmpdir </> "input.pdf"
        svgFile = tmpdir </> "input.svg"

    copyFile source texFile

    -- -halt-on-error: don't hang on errors waiting for input.
    -- -interaction=nonstopmode: ditto.
    -- -output-directory: keep all the .aux/.log/.pdf cruft contained.
    runProc
      "pdflatex"
      [ "-halt-on-error",
        "-interaction=nonstopmode",
        "-output-directory=" ++ tmpdir,
        texFile
      ]

    runProc "pdf2svg" [pdfFile, svgFile]

    readFile svgFile
  where
    runProc cmd args = do
      (ec, out, err) <- readProcessWithExitCode cmd args ""
      case ec of
        ExitSuccess -> pure ()
        ExitFailure _ ->
          error . unlines $
            [ "Command failed: " ++ unwords (cmd : args),
              "stdout:",
              out,
              "stderr:",
              err
            ]

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
