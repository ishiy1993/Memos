module Main where

import Prelude

import Control.Monad.Aff (Aff(), runAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)

import Data.Array (null)
import Data.Date (now, Now())
import Data.Date.Locale (toLocaleString, Locale())
import Data.Either (Either(..))
import Data.Functor (($>))
import Data.Maybe

import Halogen
import Halogen.Util (appendToBody, onLoad)
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E
import qualified Halogen.HTML.Properties.Indexed as P

import qualified Network.HTTP.Affjax as A
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Combinators
import Data.Argonaut.Decode 
import Data.Argonaut.Encode

type AppEffects eff = HalogenEffects (ajax :: A.AJAX , now :: Now, locale :: Locale | eff)

data Query a = GetMemos a
             | PostMemo String String a
             | SetCn String a
             | SetTg String a
             | Reset a
             | GetDetail Int a
             | UpdateMemo Int a
             | PutMemo Int String String a
             | DeleteMemo Int a

type State = { busy :: Boolean
             , memoInfos :: Array MemoInfo
             , memoCn :: String
             , memoTg :: String
             , focus :: Int
             , memoDetail :: Maybe Memo
             , updateId :: Int
             }

newtype Memo = Memo {content :: String, tag :: String, date :: String}

instance encodeJsonMemo :: EncodeJson Memo where
    encodeJson (Memo m) =  "memoContent" := m.content
                        ~> "memoTag" := m.tag
                        ~> "memoDate" := m.date
                        ~> jsonEmptyObject

instance decodeJsonMemo :: DecodeJson Memo where
    decodeJson json = do
        obj <- decodeJson json
        cn <- obj .? "memoContent"
        tg <- obj .? "memoTag"
        dt <- obj .? "memoDate"
        pure $ Memo {content: cn, tag: tg, date: dt}

newtype MemoInfo = MemoInfo {cn :: String, id :: Int}

instance showMemoInfo :: Show MemoInfo where
    show (MemoInfo mis) = show mis.id ++ " " ++ mis.cn

instance decodeJsonMemoInfo :: DecodeJson MemoInfo where
    decodeJson json = do
        obj <- decodeJson json
        id <- obj .? "id"
        cn <- obj .? "cn"
        pure $ MemoInfo {cn: cn, id: id}

initialState :: State
initialState = { busy: false
               , memoInfos: []
               , memoCn: ""
               , memoTg: ""
               , focus: 0
               , memoDetail: Nothing
               , updateId: 0
               }

ui :: forall eff. Component State Query (Aff (AppEffects eff))
ui = component render eval
  where

  render :: State -> ComponentHTML Query
  render state =
      H.div 
        [ P.class_ $ H.className "container" 
        ]
        [ H.h1_
            [ H.text "Memos" ]
        , H.p_
            [ H.text "Write fast, Write easy." ]
        , H.h2_
            [ H.text "Create Memo" ]
        , memoForm state
        , H.h2_
            [ H.text "Memo List" ]
        , memoList state
        ]

  memoForm :: State -> ComponentHTML Query
  memoForm st = 
      H.div [ P.class_ $ H.className "memoForm" ]
            [ H.input
                [ P.inputType P.InputText
                , P.title "Content"
                , P.value st.memoCn
                , E.onValueInput $ E.input SetCn
                ]
            , H.input
                [ P.inputType P.InputText
                , P.title "Tag"
                , P.value st.memoTg
                , E.onValueInput $ E.input SetTg
                ]
            , H.button
                [ E.onClick $ E.input_ (PostMemo st.memoCn st.memoTg) ]
                [ H.text "Submit" ]
            ]

  memoList :: State -> ComponentHTML Query
  memoList st = if null st.memoInfos
                   then H.p_ [H.text "No memos"]
                   else H.div_ (map (memoBox st) st.memoInfos)

  memoBox :: State -> MemoInfo -> ComponentHTML Query 
  memoBox st (MemoInfo m) = 
      H.div [ P.class_ $ H.className "memoBox" ]
            [ H.p_ [H.text m.cn]
            , H.button
                [ E.onClick (E.input_ (GetDetail m.id))]
                [ H.text "Detail" ]
            , H.button
                [ E.onClick (E.input_ (UpdateMemo m.id))]
                [ H.text "Update" ]
            , H.button
                [ E.onClick (E.input_ (DeleteMemo m.id)) ]
                [ H.text "Delete" ]
            , if st.updateId /= m.id
                 then H.text ""
                 else updateForm st
            , if st.focus /= m.id
                 then H.text ""
                 else renderMemo st.memoDetail
            ]


  updateForm :: State -> ComponentHTML Query
  updateForm st =
      H.div [ P.class_ $ H.className "memoForm" ]
            [ H.input
                [ P.inputType P.InputText
                , P.title "Content"
                , P.value st.memoCn
                , E.onValueInput $ E.input SetCn
                ]
            , H.input
                [ P.inputType P.InputText
                , P.title "Tag"
                , P.value st.memoTg
                , E.onValueInput $ E.input SetTg
                ]
            , H.button
                [ E.onClick $ E.input_ (PutMemo st.updateId st.memoCn st.memoTg) ]
                [ H.text "Submit" ]
            ]

  renderMemo :: Maybe Memo -> ComponentHTML Query
  renderMemo Nothing = H.text ""
  renderMemo (Just (Memo m)) =
      H.div_
        [ H.p_ [ H.text $ "Content: " ++ m.content ]
        , H.p_ [ H.text $ "Tag: " ++ m.tag ]
        , H.p_ [ H.text $ "Date: " ++ m.date ]
        ]

  eval :: Natural Query (ComponentDSL State Query (Aff (AppEffects eff)))
  eval (GetMemos next) = do
    modify (_ {busy = true})
    result <- liftAff' fetchMemos
    modify (_ {busy = false, memoInfos = result})
    pure next
  eval (PostMemo cn tg next) = do
    dt <- liftEff' getDate
    let m = Memo { content: cn, tag: tg, date: dt }
    liftAff' $ postMemo m 
    eval (Reset next)
  eval (SetCn cn next) = modify (_ {memoCn = cn}) $> next
  eval (SetTg tg next) = modify (_ {memoTg = tg}) $> next
  eval (Reset next) = do
    modify (_ {memoCn = "", memoTg = "", focus = 0, memoDetail = Nothing})
    eval (GetMemos next)
  eval (GetDetail id next) = do
    result <- liftAff' $ fetchDetail id
    modify (_ {focus = id, memoDetail = result})
    pure next
  eval (UpdateMemo id next) = do
    modify (_ {memoCn = "", memoTg = "", updateId = id})
    pure next
  eval (PutMemo id cn tg next) = do
    dt <- liftEff' getDate
    let m = Memo { content: cn, tag: tg, date: dt }
    result <- liftAff' $ putMemo id m
    eval (GetMemos next)
    modify (_ {memoCn = "", memoTg = "", focus = id, memoDetail = result, updateId = 0})
    pure next
  eval (DeleteMemo id next) = do
    liftAff' $ deleteMemo id
    eval (GetMemos next)

baseUrl :: String
baseUrl = "http://localhost:8080/memos"

fetchMemos :: forall eff. Aff (ajax :: A.AJAX | eff) (Array MemoInfo)
fetchMemos = do
    result <- A.get baseUrl
    let response = result.response
    return case decodeJson response of
                Left _ -> []
                Right mis -> mis

postMemo :: forall e. Memo -> A.Affjax e Unit
postMemo m =
    A.post_ baseUrl $ encodeJson m

getDate :: forall e. Eff ( now :: Now, locale :: Locale | e) String
getDate = now >>= toLocaleString

fetchDetail :: forall e. Int -> Aff (ajax :: A.AJAX | e) (Maybe Memo)
fetchDetail id = do
    result <- A.get $ baseUrl ++ "/" ++ show id
    let response = result.response
    return case decodeJson response of
                Left _ -> Nothing
                Right m -> Just m

putMemo :: forall e. Int -> Memo -> Aff (ajax :: A.AJAX | e) (Maybe Memo)
putMemo id m = do
    result <- A.put (baseUrl ++ "/" ++ show id) $ encodeJson m
    let response = result.response
    return case decodeJson response of
                Left _ -> Nothing
                Right m -> Just m

deleteMemo :: forall eff. Int -> A.Affjax eff Unit
deleteMemo id = A.delete_ $ baseUrl ++ "/" ++ show id 

main :: Eff (AppEffects ()) Unit
main = runAff throwException (const (pure unit)) $ do
  app <- runUI ui initialState
  onLoad $ appendToBody app.node
  app.driver $ action GetMemos
