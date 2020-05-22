module Main where

import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Routing.Hash (matchesWith)
import Routing.Duplex (parse)
import Data.Maybe (Maybe(..))
import Prelude
import Control.Alt (class Functor)
import Control.Alternative (class Applicative, class Apply, apply)
import Control.Bind (class Bind)
import Control.Monad.Reader (ReaderT(..))
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console (log)
import Halogen (HalogenM)
import Halogen as H
import Halogen.Component (ComponentSlot)
import Halogen.Component (mkComponent)
import Halogen.HTML as HH
import Halogen.Query.HalogenQ as HQ
import Halogen.Query.EventSource (EventSource(..), Emitter(..), affEventSource, emit)
import Halogen.Query.EventSource as ES
import Effect.Aff (error, forkAff, killFiber)
import Control.Monad.Rec.Class (forever)
import FRP.Event (Event, create, subscribe)
import Effect.Aff (Aff)
import Effect.Ref (Ref)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class (liftEffect)
import Effect.Ref (Ref, new, read, write)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Coroutine as CC
import Data.Either (Either(..))
import Data.Bifunctor (class Bifunctor)
import Effect.Aff.AVar as AV
import Halogen.Aff.Util (runHalogenAff, awaitBody)
import Halogen.VDom.Driver (runUI)
import Data.Symbol (SProxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Halogen.HTML.Events as HE
import Effect.Aff.Bus as Bus
import Effect.Aff.Bus (BusRW)
import Debug.Trace (trace)
import Halogen.Hooks as Hooks
import Data.Tuple.Nested ((/\))
import Data.Monoid (guard)

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    halogenIO <-
      runUI parentComponent {} body
    pure unit

parentComponent =
  Hooks.component \_ input -> Hooks.do
    recModalOpen /\ recModalOpenId <- Hooks.useState $ false
    Hooks.captures { recModalOpen } Hooks.useTickEffect do
      liftEffect $ log $ show recModalOpen
      pure Nothing
    let
      closeModal = do
        liftEffect $ log "Closing!"
        Hooks.modify_ recModalOpenId (const false)
        pure unit
    Hooks.pure
      $ HH.div
          []
          ( [ HH.div [ HE.onClick \_ -> Just (Hooks.modify_ recModalOpenId (const true)) ] [ HH.text "open" ] ]
              <> guard recModalOpen [ HH.slot (SProxy :: SProxy "recModal") unit modalComponent { close: closeModal } absurd ]
          )

modalComponent =
  Hooks.component \_ input -> Hooks.do
    let
      closeModal = do
        input.close
    Hooks.pure
      $ HH.div
          [ HE.onClick \_ -> Just closeModal ]
          [ HH.div
              []
              [ HH.text "foo"
              ]
          ]
