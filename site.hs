--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
import           Data.Monoid (mappend)
import           Hakyll

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do
    match "images/*" $ do
        route idRoute
        compile copyFileCompiler

    match "static/*" $ do
        route idRoute
        compile copyFileCompiler

    match "publications/*" $ do
        route idRoute
        compile copyFileCompiler

    match "benchmarks/programs/*" $ do
        route idRoute
        compile copyFileCompiler

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
          pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" menu
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ do
          postCtx <- postContext
          pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["blog.html"] $ do
        route idRoute
        compile $ do
            postCtx <- postContext
            posts <- recentFirst =<< loadAll "posts/*"
            menu <- contentContext
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    menu

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

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
menuContents = [("Futhark", "index.html"),
                ("Examples", "examples.html"),
                ("Docs", "docs.html"),
                ("Gotta Go Fast!", "performance.html"),
                ("Get Involved", "getinvolved.html"),
                ("Blog", "blog.html")]

showMenu :: FilePath -> [(String, FilePath)] -> String
showMenu this items = "<ul id=\"menu\">"++concatMap li items++"</ul>"
  where li (name, item)
          | item == this = "<li id=\"chosen\"><a href=\"/"++item++"\">"++name++"</a></li>"
          | otherwise    = "<li><a href=\"/"++item++"\">"++name++"</a></li>"

--------------------------------------------------------------------------------
postContext :: Compiler (Context String)
postContext = do
  ctx <- contentContext
  return $ dateField "date" "%B %e, %Y" `mappend` ctx

--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration
  { deployCommand = "rsync --chmod=Do+rx,Fo+r --checksum -ave 'ssh -p 22' \
                     \_site/* --exclude pub futhark@sigkill.dk:/var/www/htdocs/futhark-lang.org"
  }
