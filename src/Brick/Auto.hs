-- | A stand-in replacement for 'Brick', for use with the `auto` library.
-- 'appHandleEvent' has been replaced with 'appHandler'.
--
-- Currently, it is not possible to suspend and resume.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}

module Brick.Auto
    ( App(..)
    , EventM
    , Handler
    , Next
    , defaultMain
    , handler
    , continue
    , next
    , halt
    , HandleEvent(..)
    , module Brick.AttrMap
    , module Brick.Types
    , module Brick.Widgets.Core
    , module Brick.Util
    ) where

import Brick.AttrMap
import Brick.Types hiding (EventM, HandleEvent(..), Next)
import Brick.Util
import Brick.Widgets.Core
import Brick.Widgets.Edit
import Brick.Widgets.List
import Control.Auto
import Control.Auto.Blip.Internal
import Control.Auto.Switch
import Control.Monad.IO.Class
import Prelude hiding (id)

import qualified Brick        as Brick
import qualified Graphics.Vty as Vty

-- | An event handler.
type Handler s e = Auto EventM e (Maybe s)

-- | The "return type" of an event handler; what to do next after handling an
-- event. May be 'continue', 'next', or 'halt'.
data Next s e
    = Continue s
    | Next s (s -> Handler s e)
    | Halt

data App s e = App
    { appDraw         :: s -> [Brick.Widget]
    , appChooseCursor :: s -> [Brick.CursorLocation] -> Maybe Brick.CursorLocation
    , appHandler      :: s -> Handler s e
    , appStartEvent   :: s -> EventM s
    , appAttrMap      :: s -> Brick.AttrMap
    , appLiftVtyEvent :: Vty.Event -> Maybe e
    }

defaultMain :: forall s e. App s e -> s -> IO s
defaultMain App{..} s0 = fst <$> Brick.defaultMain app' (s0, appHandler s0)
  where
    app' :: Brick.App (s, Auto EventM e (Maybe s)) Vty.Event
    app' = Brick.App
        { Brick.appDraw         = \(s, _) -> appDraw s
        , Brick.appChooseCursor = \(s, _) cs -> appChooseCursor s cs
        , Brick.appHandleEvent  = handle_event
        , Brick.appStartEvent   = \(s, auto) -> let EventM action = appStartEvent s in (, auto) <$> action
        , Brick.appAttrMap      = \(s, _) -> appAttrMap s
        , Brick.appLiftVtyEvent = id
        }

    handle_event
        :: (s, Auto EventM e (Maybe s))
        -> Vty.Event
        -> Brick.EventM (Brick.Next (s, Auto EventM e (Maybe s)))
    handle_event (s, auto) vty_event = do
        case appLiftVtyEvent vty_event of
            Nothing -> Brick.continue (s, auto)
            Just e  -> do
                let EventM action = stepAuto auto e
                action >>= \case
                    (Nothing, auto') -> Brick.halt (s, auto')
                    (Just s', auto') -> Brick.continue (s', auto')

-- | Create a 'Handler' from its step function and initial state.
handler :: forall s e. (s -> e -> EventM (Next s e)) -> s -> Handler s e
handler step state0 = switchFrom_ (accumM_ step' (Just state0, NoBlip))
  where
    step' :: (Maybe s, Blip (Handler s e)) -> e -> EventM (Maybe s, Blip (Handler s e))
    step' (Nothing, _) _ = pure (Nothing, NoBlip)
    step' (Just st, _) evt = f <$> step st evt
      where
        f :: Next s e -> (Maybe s, Blip (Handler s e))
        f (Continue st') = (Just st', NoBlip)
        f (Next st' k)   = (Just st', Blip (k st'))
        f Halt           = (Nothing, NoBlip)

-- | Proceed with the current handler.
continue :: s -> EventM (Next s e)
continue st = pure (Continue st)

-- | Transition to a new handler just after emitting the given state.
next :: s -> (s -> Handler s e) -> EventM (Next s e)
next st k = pure (Next st k)

-- | Halt event handling.
halt :: EventM (Next s e)
halt = pure Halt

--------------------------------------------------------------------------------
-- Misc. boilerplate that will go away when EventM is made a newtype.

newtype EventM a = EventM (Brick.EventM a)
  deriving (Functor, Applicative, Monad, MonadIO)

class HandleEvent a where
    handleEvent :: Vty.Event -> a -> EventM a

instance HandleEvent Editor   where handleEvent e x = EventM (Brick.handleEvent e x)
instance HandleEvent (List e) where handleEvent e x = EventM (Brick.handleEvent e x)
