{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Monoid (mappend)
import Hakyll

feedConfiguration :: FeedConfiguration
feedConfiguration =
  FeedConfiguration { feedTitle = "Code and Co"
                    , feedDescription = "Articles on functional programming, PLT, types, and other geekery"
                    , feedAuthorName = "Danny Gratzer"
                    , feedAuthorEmail = "danny.gratzer@gmail.com"
                    , feedRoot        = "http://jozefg.bitbucket.org"}

main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "resume.html" $ do
      let cxt = constField "title" "" `mappend` defaultContext
      route idRoute
      compile $ getResourceBody
        >>= loadAndApplyTemplate "templates/default.html" cxt

    match "favicon.ico" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.md", "contact.md"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")
    let cxtWithTags = postCtxWithTags tags

    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged \"" ++ tag ++ "\""
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title
                      `mappend` listField "posts" postCtx (return posts)
                      `mappend` defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" cxtWithTags
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" cxtWithTags
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- fmap (take 20) . recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    field "tags" (\_ -> renderTagList tags)   `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler
    create ["rss.xml"] $ do
      route idRoute
      compile $ do
        let feedCtx = postCtx `mappend` bodyField "description"
        posts <- fmap (take 10) . recentFirst =<<
            loadAllSnapshots "posts/*" "content"
        renderRss feedConfiguration feedCtx posts

    create ["atom.xml"] $ do
      route idRoute
      compile $ do
        let feedCtx = postCtx `mappend` bodyField "description"
        posts <- fmap (take 10) . recentFirst =<<
            loadAllSnapshots "posts/*" "content"
        renderAtom feedConfiguration feedCtx posts

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags `mappend` postCtx
