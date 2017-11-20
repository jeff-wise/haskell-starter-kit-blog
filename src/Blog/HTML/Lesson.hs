
-- {-| Lesson HTML
-- -}


-- {-# LANGUAGE OverloadedStrings #-}


-- module Blog.HTML.Lesson
--   ( document
--   , lessonHtml
--   ) where


-- import Tome.Site.Common
-- import Tome.Site.HTML.Combinators (anchorId, anchorLink)
-- import Tome.Site.HTML.Document 
--   ( SectionType (..)
--   , siteDocument
--   )
-- import Tome.Site.HTML.SVG
-- import Tome.Site.HTML.Yaml (yamlToHtml)
-- import Tome.Site.HTTP.Types
--   ( cssLink, imageLink, jsPath, svgLink, videoLink 
--   , tutorialCssPath, yamlCssPath
--   )
-- import Tome.Site.Types.Lesson
-- import Tome.Site.Types.Tutorial
-- import Tome.Site.Types.TutorialIndex 
--   ( IndexTree (..)
--   , nextLessonId, previousLessonId, parentLessonId
--   )

-- import Data.Foldable (concat, forM_, sequenceA_)
-- import Data.List (head, null, zip)
-- import qualified Data.Text.Lazy as LT (fromStrict)
-- import qualified Data.Text as T (pack)

-- import Text.Blaze (dataAttribute)
-- import Text.Blaze.Html5
--   ( Html
--   , (!)
--   , toHtml, toValue
--   , preEscapedToHtml
--   )
-- import qualified Text.Blaze.Html5.Attributes as A
-- import qualified Text.Blaze.Html5 as H

-- import Text.Markdown (markdown, defaultMarkdownSettings)



-- --------------------------------------------------------------------------------
-- -- DOCUMENT
-- --------------------------------------------------------------------------------

-- document :: Tutorial -> Lesson -> IndexTree -> SectionType -> Html
-- document tutorial lesson indexTree sectionType = 
--   siteDocument sectionType
--                (headerHtml tutorial
--                            (lessonPath lesson) 
--                            (lessonTitle lesson))
--                (lessonHtml lesson indexTree)
--                [tutorialCssPath, yamlCssPath, cssLink "highlight"]
--                [jsPath "lesson", jsPath "highlight"]


-- --------------------------------------------------------------------------------
-- -- LESSON HEADER
-- --------------------------------------------------------------------------------

-- headerHtml :: Tutorial -> LessonPath -> LessonTitle -> Html
-- headerHtml tutorial _lessonPath _lessonTitle = 
--   H.div ! A.id "site-lesson-header" $ 
--     breadcrumbsHtml tutorial _lessonPath _lessonTitle


-- breadcrumbsHtml :: Tutorial -> LessonPath -> LessonTitle -> Html
-- breadcrumbsHtml tutorial _lessonPath _lessonTitle =
--   H.div ! A.id "site-lesson-breadcrumbs" $ do
--     breadcrumbsTutorialHtml tutorial
--     breadcrumbsPathHtml _lessonPath
--     breadcrumbsTitleHtml _lessonTitle


-- breadcrumbsTutorialHtml :: Tutorial -> Html
-- breadcrumbsTutorialHtml tutorial = 
--   H.div ! A.id "site-lesson-breadcrumbs-tutorial" $ do
--     -- H.img ! A.src (toValue $ svgLink "training")
--     preEscapedToHtml bookSvg
--     let titleText = getTutorialTitle $ tutorialTitle tutorial
--     H.span ! A.class_ "tutorial" $ toHtml titleText


-- breadcrumbsPathHtml :: LessonPath -> Html
-- breadcrumbsPathHtml (LessonPath segments) = 
--   H.div ! A.id "site-lesson-breadcrumbs-path" $
--     forM_ segments lessonPathSegmentHtml


-- lessonPathSegmentHtml :: Text -> Html
-- lessonPathSegmentHtml pathSegment = do
--   H.div ! A.class_ "path-segment" $ do
--     -- H.img ! A.src (toValue $ svgLink "breadcrumbs-chevron")
--     preEscapedToHtml chevronRightSvg
--     H.span $ toHtml pathSegment


-- breadcrumbsTitleHtml :: LessonTitle -> Html
-- breadcrumbsTitleHtml (LessonTitle titleText) = 
--   H.div ! A.id "site-lesson-breadcrumbs-title" $ do
--     preEscapedToHtml chevronRightSvg
--     H.span $ toHtml titleText


-- --------------------------------------------------------------------------------
-- -- LESSON
-- --------------------------------------------------------------------------------

-- lessonHtml :: Lesson -> IndexTree ->  Html
-- lessonHtml lesson indexTree =
--   H.div ! A.id "lesson" $ do
--     H.div ! A.id "lesson-container" $ do
--       let idText = getLessonId $ lessonId lesson
--       indexHtml idText indexTree
--       H.div ! A.id "lesson-content" $ do
--         contentHtml lesson


-- contentHtml :: Lesson -> Html
-- contentHtml lesson = do
--   let titleText = getLessonTitle $ lessonTitle lesson
--   H.div ! A.id "lesson-title" $ H.h1 $ toHtml titleText
--   H.div ! A.id "lesson-body" $ do
--     case lessonIntroduction lesson of 
--       Just intro -> introductionHtml intro $ lessonScenarios lesson
--       Nothing    -> return ()
--     forM_ (zip (lessonScenarios lesson) [1..]) $ scenarioHtml


-- -- Introduction
-- --------------------------------------------------------------------------------

-- introductionHtml :: LessonIntroduction -> [LessonScenario] -> Html
-- introductionHtml (LessonIntroduction intro) _lessonScenarios = do
--   let textHtml = markdown defaultMarkdownSettings $ LT.fromStrict intro
--   H.div ! A.class_ "introduction" $ do
--     H.div ! A.class_ "body" $ textHtml
--     introductionIndexHtml _lessonScenarios


-- introductionIndexHtml :: [LessonScenario] -> Html
-- introductionIndexHtml _lessonScenarios =
--   H.div ! A.class_ "index" $ do
--     H.ul $ sequenceA_ $ 
--       concat $ fmap introductionIndexScenarioNodeHtmls $ zip _lessonScenarios [1..]


-- introductionIndexScenarioNodeHtmls :: (LessonScenario, Int) -> [Html]
-- introductionIndexScenarioNodeHtmls (lessonScenario, _) =
--   scenarioItemHtml
--     : (concat $ fmap introductionIndexStepNodeHtmls stepsWithIndex)
--   where
--     scenarioItemHtml = 
--       H.li ! A.class_ "scenario" $ 
--         H.a ! A.href (anchorLink $ "scenario-" <> scenarioName) $ 
--           toHtml scenarioName
--     -- scenarioName = (T.pack $ show index) <> ". " <> (lessonScenario ^. name.value)
--     scenarioName = getScenarioName $ lessonScenarioName lessonScenario
--     stepsWithIndex = zip (lessonScenarioSteps lessonScenario) [1..]


-- introductionIndexStepNodeHtmls :: (LessonStep, Int) -> [Html]
-- introductionIndexStepNodeHtmls (lessonStep, index) = [stepItemHtml]
--   where
--     stepItemHtml = 
--       H.li ! A.class_ "step" $ 
--         H.a ! A.href (anchorLink $ "step-" <> stepName) $ 
--           toHtml stepNameWithIndex
--     stepNameWithIndex = (T.pack $ show index) <> ". " <> stepName
--     stepName = getStepName $ lessonStepName lessonStep


-- -- Scenario
-- --------------------------------------------------------------------------------

-- scenarioHtml :: (LessonScenario, Int) -> Html
-- scenarioHtml (lessonScenario, _) = do
--   let scenarioName = getScenarioName $ lessonScenarioName lessonScenario
--       scenarioTextHtml = markdown defaultMarkdownSettings $ LT.fromStrict $ 
--                           getScenarioText $ lessonScenarioText lessonScenario
--   H.div ! A.class_ "scenario" $ do
--     H.h2 ! A.id (anchorId $ "scenario-" <> scenarioName) $ toHtml scenarioName 
--     H.header $ do
--       H.div ! A.class_ "body" $ scenarioTextHtml
--       H.div ! A.class_ "examples" $ do
--         let videos = lessonScenarioVideos lessonScenario
--         when (not $ null videos) $ do
--           let video = head videos 
--               videoPath = videoLink $ lessonVideoId video
--           H.video ! A.width "360" ! A.height "360" ! A.controls "" $ 
--             H.source ! A.src (toValue videoPath) ! A.type_ "video/mp4"
--     H.div ! A.class_ "steps" $ 
--       forM_ (zip (lessonScenarioSteps lessonScenario) [1..]) stepHtml


-- -- Step
-- --------------------------------------------------------------------------------

-- stepHtml :: (LessonStep, Int) -> Html
-- stepHtml (lessonStep, index) =
--   H.div ! A.class_ "step" $ do
--    -- stepHeaderHtml lessonStep index
--     stepContentHtml lessonStep index
--     stepCodeExamplesHtml $ lessonStepCodeExamples lessonStep


-- -- stepHeaderHtml :: LessonStep -> Int -> Html
-- -- stepHeaderHtml lessonStep index = 
-- --   where


-- stepContentHtml :: LessonStep -> Int -> Html
-- stepContentHtml lessonStep index =
--   H.div ! A.class_ "content" $ do
--     H.div ! A.class_ "left" $ do
--       let stepName = getStepName $ lessonStepName lessonStep
--           stepNameWithIndex = (T.pack $ show index) <> ". " <> stepName
--           stepText = markdown defaultMarkdownSettings $ LT.fromStrict $ 
--                       getStepText $ lessonStepText lessonStep
--       H.div ! A.class_ "title" $
--         H.h3 ! A.id (anchorId $ "step-" <> stepName) $ toHtml stepNameWithIndex
--       H.div ! A.class_ "text" $ toHtml stepText
--     H.div ! A.class_ "right" $ do
--       let tabCount = numberOfTabs lessonStep 
--       when (tabCount > 0) $ 
--         H.div ! A.class_ "tabs" $ stepTabsHtml tabCount
--       H.div ! A.class_ "images" $ 
--         forM_ (zip (lessonStepImages lessonStep) [1..]) stepImageHtml
     
   
-- -- Step > Image
-- --------------------------------------------------------------------------------

-- stepImageHtml :: (LessonImage, Int) -> Html
-- stepImageHtml (LessonImage (LessonImageId _imageId) _, index)
--   | index == 1 = H.img ! (A.src $ toValue $ imageLink _imageId)
--                        ! A.class_ "selected"
--                        ! dataAttribute "index" (toValue $ show index)
--   | otherwise  = H.img ! (A.src $ toValue $ imageLink _imageId)
--                        ! dataAttribute "index" (toValue $ show index)


-- -- Step > Code Examples
-- --------------------------------------------------------------------------------

-- stepCodeExamplesHtml :: [CodeExample] -> Html
-- stepCodeExamplesHtml codeExamples =
--   H.div ! A.class_ "code-examples" $ 
--     forM_ (zip codeExamples [1..]) codeExampleHtml


-- codeExampleHtml :: (CodeExample, Int) ->  Html
-- codeExampleHtml (codeExample, index) = do
--   H.div ! A.class_ "code-example" $ do
--     H.header $ do
--       H.h6 $ toHtml $ getCodeExampleTitle $ codeExampleTitle codeExample
--       H.div ! A.class_ "button copy" $ do
--         preEscapedToHtml copySvg
--         H.span "Copy to Clipboard"
--       when (codeExampleAction codeExample == CodeExampleActionNew) $
--         H.div ! A.class_ "button download" $ do
--           preEscapedToHtml downloadFileSvg
--           H.span "Download File"
--     let codeText = getCodeExampleCode $ codeExampleCode codeExample
--     preEscapedToHtml ("<pre><code>" :: Text)
--     toHtml codeText
--     preEscapedToHtml ("</pre></code>" :: Text)


-- -- Step > Tabs
-- --------------------------------------------------------------------------------

-- stepTabsHtml :: Int -> Html
-- stepTabsHtml tabCount = forM_ [1..tabCount] tabHtml
--   where
--     tabHtml :: Int -> Html
--     tabHtml 1     = 
--       H.div ! A.class_ "tab selected"
--             ! dataAttribute "index" (toValue ("1" :: Text))
--             $ H.span $ toHtml ("1" :: Text)
--     tabHtml index = 
--       H.div ! A.class_ "tab"
--             ! dataAttribute "index" (toValue $ show index)
--             $ H.span $ toHtml $ show index


-- --------------------------------------------------------------------------------
-- -- INDEX
-- --------------------------------------------------------------------------------

-- indexHtml :: Text -> IndexTree -> Html
-- indexHtml _lessonId tree =
--   H.div ! A.id "lesson-index" $ do
--     H.div ! A.id "lesson-index-header" $ indexHeaderHtml _lessonId tree
--     H.div ! A.id "lesson-index-list" $ 
--       H.ul $ sequenceA_ $ indexTreeHtml _lessonId tree


-- indexHeaderHtml :: Text -> IndexTree -> Html
-- indexHeaderHtml _lessonId indexTree = do
--   let prevLessonLink = fromMaybe "" $ previousLessonId _lessonId indexTree
--       parentLessonLink = fromMaybe "" $ parentLessonId _lessonId indexTree
--       nextLessonLink = fromMaybe "" $ nextLessonId _lessonId indexTree
--   H.a ! A.id "lesson-prev-button" 
--       ! A.class_ "button"
--       ! A.href (toValue prevLessonLink ) $ do
--         preEscapedToHtml chevronThickLeftSvg
--         H.span "PREV"
--   H.a ! A.id "lesson-up-button" 
--       ! A.class_ "button"
--       ! A.href (toValue parentLessonLink) $
--         H.span "UP"
--   H.a ! A.id "lesson-next-button" 
--       ! A.class_ "button"
--       ! A.href (toValue nextLessonLink) $ do
--         H.span "NEXT"
--         preEscapedToHtml chevronThickRightSvg


-- indexTreeHtml :: Text -> IndexTree -> [Html]
-- indexTreeHtml _lessonId tree = go 1 tree
--   where
--     go :: Int -> IndexTree -> [Html]
--     go level (IndexTree nodeText nodeId _ [])       = [indexNodeHtml nodeText nodeId _lessonId level]
--     go level (IndexTree nodeText nodeId _ children) = 
--       (indexNodeHtml nodeText nodeId _lessonId level):(concat $ fmap (go $ level + 1) children)


-- indexNodeHtml :: Text -> Text -> Text -> Int -> Html
-- indexNodeHtml _        _      _        1     = return () 
-- indexNodeHtml nodeText nodeId _lessonId level = 
--   let levelClass = case level of
--                   2 -> "level-one" 
--                   3 -> "level-two"
--                   4 -> "level-three"
--                   5 -> "level-four"
--                   _ -> ""
--       selectedClass = if (nodeId == _lessonId)
--                          then "selected"
--                          else ""
--       classes = "node " <> levelClass <> " " <> selectedClass
--       linkHtml = H.a ! (A.href $ toValue nodeId)
--                      $ toHtml nodeText
--   in  H.li ! A.class_ classes $ linkHtml

