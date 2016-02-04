--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

import           Data.Monoid            (mappend)
import           Hakyll

import           Data.List.Split        (keepDelimsL, split, whenElt)
import qualified Data.Set               as S
import           Text.Pandoc.Definition (Block (..), Pandoc (..))
import           Text.Pandoc.Options    (Extension (..), HTMLMathMethod (..),
                                         WriterOptions (..), def)
import           Text.Pandoc.Walk       (walk)

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "fonts/*" $ do
        route   idRoute
        compile copyFileCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ customPandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend` defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

customPandocCompiler :: Compiler (Item String)
customPandocCompiler = pandocCompilerWithTransform
                         defaultHakyllReaderOptions
                         writerOptions
                         (markCode . sectionify)
  where
    writerOptions = def
      { writerExtensions     = S.insert Ext_tex_math_dollars (writerExtensions def)
      , writerHTMLMathMethod = MathJax "http://cdn.mathjax.org/mathjax/latest/MathJax.js"
      , writerHtml5          = True
      , writerSectionDivs    = True
      , writerHighlight      = True }

-- | Questa funzione va a mettere tutto quello che va da un header di livello
--   massimo, che in questo contesto e' il secondo, al successivo, in una
--   sezione.
sectionify :: Pandoc -> Pandoc
sectionify (Pandoc m b) = Pandoc m (sectionify' b)
  where
    sectionify' = map (Div myAttr) . split (keepDelimsL $ whenElt isHeader2)
    isHeader2 (Header 2 _ _) = True
    isHeader2 _              = False
    myAttr = ("", ["section"], [])

markCode :: Pandoc -> Pandoc
markCode = walk markCode'
  where
    markCode' cb@(CodeBlock (i,cs,kvs) s)
      | "haskell" `elem` cs  =  CodeBlock (i, "code":cs, kvs) s
      | otherwise               =  cb
    markCode' x = x
