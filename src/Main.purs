module Main where

import Prelude

import Control.Monad.Reader.Trans (ReaderT, ask, local, runReaderT)
import Data.List as List
import Data.Map as Map
import Data.Maybe (fromJust)
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Ref as Ref
import Effect.Timer (setInterval)
import Partial.Unsafe (unsafePartial)
import Web.DOM (Node, Text)
import Web.DOM.Document (createElement, createTextNode)
import Web.DOM.Element as Element
import Web.DOM.Node (appendChild, setTextContent)
import Web.DOM.Text as Text
import Web.HTML as HTML
import Web.HTML.HTMLDocument (body, toDocument)
import Web.HTML.HTMLElement (toNode)
import Web.HTML.Window (document)

-- import Web.DOM as DOM

newtype BuildVarying a = BuildVarying (Effect a)

derive newtype instance buildVaryingMap :: Functor BuildVarying
derive newtype instance buildVaryingApply :: Apply BuildVarying
derive newtype instance buildVaryingApplicative :: Applicative BuildVarying
derive newtype instance buildVaryingBind :: Bind BuildVarying
derive newtype instance buildVaryingMonad :: Monad BuildVarying
derive instance newtypeBuildVarying :: Newtype (BuildVarying a) _

runVarying :: forall a. BuildVarying a -> Effect a
runVarying = unwrap

type Callback a = a -> Effect Unit

newtype Varying a = MkVarying {
  read :: Effect a,
  subscribe :: Callback a -> Effect (Effect Unit)
}

derive instance newtypeVarying :: Newtype (Varying a) _

-- Idea: exploit the idea of persistent datastructures to make updating the GUI painless and simple

-- Constant

constantVarying :: ∀ a. a -> Varying a
constantVarying a =MkVarying {
  read : pure a,
  subscribe : \cb -> pure (pure unit)
}

-- Settable

settableVarying :: ∀ a. a -> BuildVarying { set :: a -> Effect Unit, varying :: Varying a}
settableVarying initial = BuildVarying $ do 

  counter <- Ref.new 0
  subscribers <- Ref.new (Map.empty)
  current <- Ref.new initial

  let 
    read = Ref.read current

    subscribe = \cb -> do
      subId <- Ref.modify (\i -> i+1) counter
      Ref.modify_ (Map.insert subId cb) subscribers
      pure $ Ref.modify_ (Map.delete subId) subscribers

  pure $ {
    varying: MkVarying { read, subscribe },
    set: \x -> do
      Ref.write x current
      subs <- Ref.read subscribers
      traverse_ (\cb -> cb x) subs
  }

-- Instances

instance mapVarying :: Functor Varying where
  map f v = MkVarying {
    read : map f (unwrap v).read,
    subscribe : \cb -> (unwrap v).subscribe (\updt -> cb $ f updt)
  }

instance applyVarying :: Apply Varying where
  apply vf va = MkVarying {
    read : do
      f <- (unwrap vf).read
      a <- (unwrap va).read
      pure $ f a,

    subscribe : \cb -> do
      unsubA <- (unwrap vf).subscribe $ \ff -> do
        aa <- (unwrap va).read
        cb (ff aa)

      unsubF <- (unwrap va).subscribe $ \aa -> do
        ff <- (unwrap vf).read
        cb (ff aa)

      pure do
        unsubA
        unsubF
  }

-- List makers

-- listVaryingCons = 

varListCons :: forall a. Varying a -> Varying (List.List (Varying a)) -> Varying (List.List (Varying a))
varListCons va vf = map (List.Cons va) vf

varListNil :: forall a. Varying (List.List (Varying a))
varListNil = constantVarying List.Nil

--

clockTicker :: BuildVarying (Varying Int)
clockTicker = do

  s <- settableVarying 0

  intvalId <- BuildVarying $ setInterval 1000 $ do
    t <- (unwrap s.varying).read
    s.set $ t + 1

  pure s.varying

------------------

newtype MonadDomBuilder a = MonadDomBuilder (ReaderT Node Effect a)
derive newtype instance monadDomBuilderMap :: Functor MonadDomBuilder
derive newtype instance monadDomBuilderApply :: Apply MonadDomBuilder
derive newtype instance monadDomBuilderApplicative :: Applicative MonadDomBuilder
derive newtype instance monadDomBuilderBind :: Bind MonadDomBuilder
derive newtype instance monadDomBuilderMonad :: Monad MonadDomBuilder
derive instance newtypeDomBuilderMonad :: Newtype (MonadDomBuilder a) _

runDomBuilder :: forall a. Node -> MonadDomBuilder a -> Effect a
runDomBuilder n b = runReaderT (unwrap b) n

textNode :: String -> MonadDomBuilder Text
textNode content = MonadDomBuilder $ do
  parent <- ask
  liftEffect do
    w <- HTML.window
    d <- document w
    t <- createTextNode content (toDocument d)
    _ <- appendChild (Text.toNode t) (parent)
    pure t

elementNode :: forall a. String -> MonadDomBuilder a -> MonadDomBuilder a
elementNode tagname content = MonadDomBuilder $ do
  parent <- ask
  t <- liftEffect do
    w <- HTML.window
    d <- document w
    t <- createElement tagname (toDocument d)
    _ <- appendChild (Element.toNode t) (parent)
    pure t
  local (const (Element.toNode t)) (unwrap content)

varyingTextNode :: Varying String -> BuildVarying (MonadDomBuilder Unit)
varyingTextNode content = BuildVarying $ do

  init_content <- (unwrap content).read

  pure $ do
    t <- textNode init_content

    MonadDomBuilder $ do
      -- TODO Handle cleanup
      _ <- liftEffect $ (unwrap content).subscribe $ \newText -> setTextContent newText (Text.toNode t)
      pure unit

-- listNode :: Varying (List.List (Varying String)) -> BuildVarying (MonadDomBuilder Unit)
-- listNode 

main :: Effect Unit
main = do

  w <- HTML.window
  d <- document w
  b <- body d
  let body' = unsafePartial fromJust b

  db <- runVarying do
    time <- clockTicker

    elementNode "h1" <$> varyingTextNode (show <$> time)

  runDomBuilder (toNode body') db

  Console.log "Hello!"