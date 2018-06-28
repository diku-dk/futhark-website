{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
import           Control.Monad
import           Data.Monoid ((<>))
import           Data.List (isPrefixOf)
import qualified Data.Map as M
import           System.FilePath
import           Hakyll
import           Skylighting (Syntax, parseSyntaxDefinition)
import           Text.Pandoc
import           Text.Pandoc.Walk

postCtx :: Context String
postCtx = mconcat
  [ modificationTimeField "mtime" "%U"
  , dateField "date" "%B %e, %Y"
  , defaultContext
  ]

static :: Rules ()
static = void $ route idRoute >> compile copyFileCompiler

main :: IO ()
main = do
  futhark_syntax <- either (error . show) return =<<
                    parseSyntaxDefinition "skylighting/futhark.xml"
  hakyllWith config $ do
    match "images/*" static

    match "robots.txt" static

    match "static/*" static

    match "publications/*" static

    match "student-projects/*" static

    match "benchmarks/programs/*" static

    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

    match (fromList ["performance.rst",
                     "docs.rst",
                     "getinvolved.rst",
                     "index.rst",
                     "examples.rst"]) $ do
        route $ setExtension "html"
        compile $ do
          menu <- contentContext
          pandocRstCompiler futhark_syntax
            >>= loadAndApplyTemplate "templates/default.html" menu
            >>= relativizeUrls

    match "blog/*" $ do
        route $ setExtension "html"
        compile $ do
          postCtx <- postContext
          pandocRstCompiler futhark_syntax
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    -- Post list
    create ["blog.html"] $ do
        route idRoute
        compile $ do
            menu <- getMenu
            posts <- recentFirst =<< loadAll "blog/*"
            let ctx = constField "title" "Developer Blog" <>
                      listField "posts" postCtx (return posts) <>
                      constField "menu" menu <>
                      defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls
    -- Atom feed
    create ["atom.xml"] $ do
      route idRoute
      compile $ do
        let feedCtx = postCtx `mappend` bodyField "description"
        posts <- fmap (take 10) . recentFirst =<<
            loadAllSnapshots "blog/*" "content"
        renderAtom feedConfiguration feedCtx posts

    match "templates/*" $ compile templateCompiler

contentContext :: Compiler (Context String)
contentContext = do
  menu <- getMenu
  return $
    defaultContext `mappend`
    constField "menu" menu

getMenu :: Compiler String
getMenu = do
  myRoute <- getRoute =<< getUnderlying
  return $ case myRoute of
             Nothing -> showMenu "" menuContents
             Just me -> showMenu me menuContents

menuContents :: [(String, FilePath)]
menuContents = [("Overview", "index.html"),
                ("Examples", "examples.html"),
                ("Docs", "docs.html"),
                ("Gotta Go Fast!", "performance.html"),
                ("Get Involved", "getinvolved.html"),
                ("Blog", "blog.html")]

showMenu :: FilePath -> [(String, FilePath)] -> String
showMenu this items = "<ul id=\"menu\">"++concatMap li items++"</ul>"
  where li (name, item)
          | isThis item = "<li id=\"chosen\"><a href=\"/"++item++"\">"++name++"</a></li>"
          | otherwise   = "<li><a href=\"/"++item++"\">"++name++"</a></li>"
        isThis item = dropExtension item `isPrefixOf` dropExtension this

--------------------------------------------------------------------------------
postContext :: Compiler (Context String)
postContext = do
  ctx <- contentContext
  return $ dateField "date" "%B %e, %Y" `mappend` ctx

--------------------------------------------------------------------------------

-- By default, RST will make all top-level titles <h1>s, but we prefer
-- to only have a single <h1>: the one in the template.  We use a
-- technique from
-- http://maxdelgiudice.com/posts/2015-07-08-rst-headers.html to avoid
-- this.

shiftHeaderUp :: Block -> Block
shiftHeaderUp h@(Header n a b)
    | n < 6     = Header (n+1) a b
    | otherwise = h
shiftHeaderUp x = x

shiftAll :: Pandoc -> Pandoc
shiftAll = walk shiftHeaderUp

pandocRstCompiler :: Syntax -> Compiler (Item String)
pandocRstCompiler futhark_syntax = pandocCompilerWithTransform
                                   defaultHakyllReaderOptions { readerIndentedCodeClasses = ["Futhark"] }
                                   defaultHakyllWriterOptions { writerSyntaxMap = syntaxmap }
                                   shiftAll
  where syntaxmap = M.insert "Futhark" futhark_syntax $
                    writerSyntaxMap defaultHakyllWriterOptions

--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration
  { deployCommand = "rsync --chmod=Do+rx,Fo+r --checksum -ave 'ssh -p 22' \
                     \_site/* --exclude pub futhark@sigkill.dk:/var/www/htdocs/futhark-lang.org"
  }

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Futhark Developer Blog"
    , feedDescription = "High-performance purely functional data-parallel array programming on the GPU"
    , feedAuthorName  = "Troels Henriksen"
    , feedAuthorEmail = "athas@sigkill.dk"
    , feedRoot        = "http://futhark-lang.org"
    }
