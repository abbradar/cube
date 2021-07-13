-- | Functions for efficiently handling SDL events.

module Cube.Input.Events
  ( EventKey(..)
  , splitSDLEvent
  , fanSDLEvent
  , KeyboardEventSelector
  , splitKeyboardEvent
  , subfanKeyboardEvent
  ) where

import Data.Functor.Misc
import Data.Functor.Identity
import Data.GADT.Compare.TH (deriveGEq, deriveGCompare)
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Reflex
import SDL.Event hiding (Event)
import SDL.Input.Keyboard

import Cube.Time

data EventKey a where
  WindowShownEventKey :: EventKey (TimeStep, WindowShownEventData)
  WindowHiddenEventKey :: EventKey (TimeStep, WindowHiddenEventData)
  WindowExposedEventKey :: EventKey (TimeStep, WindowExposedEventData)
  WindowMovedEventKey :: EventKey (TimeStep, WindowMovedEventData)
  WindowResizedEventKey :: EventKey (TimeStep, WindowResizedEventData)
  WindowSizeChangedEventKey :: EventKey (TimeStep, WindowSizeChangedEventData)
  WindowMinimizedEventKey :: EventKey (TimeStep, WindowMinimizedEventData)
  WindowMaximizedEventKey :: EventKey (TimeStep, WindowMaximizedEventData)
  WindowRestoredEventKey :: EventKey (TimeStep, WindowRestoredEventData)
  WindowGainedMouseFocusEventKey :: EventKey (TimeStep, WindowGainedMouseFocusEventData)
  WindowLostMouseFocusEventKey :: EventKey (TimeStep, WindowLostMouseFocusEventData)
  WindowGainedKeyboardFocusEventKey :: EventKey (TimeStep, WindowGainedKeyboardFocusEventData)
  WindowLostKeyboardFocusEventKey :: EventKey (TimeStep, WindowLostKeyboardFocusEventData)
  WindowClosedEventKey :: EventKey (TimeStep, WindowClosedEventData)
  KeyboardEventKey :: EventKey (TimeStep, KeyboardEventData)
  TextEditingEventKey :: EventKey (TimeStep, TextEditingEventData)
  TextInputEventKey :: EventKey (TimeStep, TextInputEventData)
  KeymapChangedEventKey :: EventKey TimeStep
  MouseMotionEventKey :: EventKey (TimeStep, MouseMotionEventData)
  MouseButtonEventKey :: EventKey (TimeStep, MouseButtonEventData)
  MouseWheelEventKey :: EventKey (TimeStep, MouseWheelEventData)
  JoyAxisEventKey :: EventKey (TimeStep, JoyAxisEventData)
  JoyBallEventKey :: EventKey (TimeStep, JoyBallEventData)
  JoyHatEventKey :: EventKey (TimeStep, JoyHatEventData)
  JoyButtonEventKey :: EventKey (TimeStep, JoyButtonEventData)
  JoyDeviceEventKey :: EventKey (TimeStep, JoyDeviceEventData)
  ControllerAxisEventKey :: EventKey (TimeStep, ControllerAxisEventData)
  ControllerButtonEventKey :: EventKey (TimeStep, ControllerButtonEventData)
  ControllerDeviceEventKey :: EventKey (TimeStep, ControllerDeviceEventData)
  AudioDeviceEventKey :: EventKey (TimeStep, AudioDeviceEventData)
  QuitEventKey :: EventKey TimeStep
  UserEventKey :: EventKey (TimeStep, UserEventData)
  SysWMEventKey :: EventKey (TimeStep, SysWMEventData)
  TouchFingerEventKey :: EventKey (TimeStep, TouchFingerEventData)
  TouchFingerMotionEventKey :: EventKey (TimeStep, TouchFingerMotionEventData)
  MultiGestureEventKey :: EventKey (TimeStep, MultiGestureEventData)
  DollarGestureEventKey :: EventKey (TimeStep, DollarGestureEventData)
  DropEventKey :: EventKey (TimeStep, DropEventData)
  ClipboardUpdateEventKey :: EventKey TimeStep
  UnknownEventKey :: EventKey (TimeStep, UnknownEventData)

deriveGEq ''EventKey
deriveGCompare ''EventKey

splitSDLEvent :: TimeStep -> EventPayload -> DMap EventKey Identity
splitSDLEvent step payload =
  case payload of
    WindowShownEvent info -> DM.singleton WindowShownEventKey (Identity (step, info))
    WindowHiddenEvent info -> DM.singleton WindowHiddenEventKey (Identity (step, info))
    WindowExposedEvent info -> DM.singleton WindowExposedEventKey (Identity (step, info))
    WindowMovedEvent info -> DM.singleton WindowMovedEventKey (Identity (step, info))
    WindowResizedEvent info -> DM.singleton WindowResizedEventKey (Identity (step, info))
    WindowSizeChangedEvent info -> DM.singleton WindowSizeChangedEventKey (Identity (step, info))
    WindowMinimizedEvent info -> DM.singleton WindowMinimizedEventKey (Identity (step, info))
    WindowMaximizedEvent info -> DM.singleton WindowMaximizedEventKey (Identity (step, info))
    WindowRestoredEvent info -> DM.singleton WindowRestoredEventKey (Identity (step, info))
    WindowGainedMouseFocusEvent info -> DM.singleton WindowGainedMouseFocusEventKey (Identity (step, info))
    WindowLostMouseFocusEvent info -> DM.singleton WindowLostMouseFocusEventKey (Identity (step, info))
    WindowGainedKeyboardFocusEvent info -> DM.singleton WindowGainedKeyboardFocusEventKey (Identity (step, info))
    WindowLostKeyboardFocusEvent info -> DM.singleton WindowLostKeyboardFocusEventKey (Identity (step, info))
    WindowClosedEvent info -> DM.singleton WindowClosedEventKey (Identity (step, info))
    KeyboardEvent info -> DM.singleton KeyboardEventKey (Identity (step, info))
    TextEditingEvent info -> DM.singleton TextEditingEventKey (Identity (step, info))
    TextInputEvent info -> DM.singleton TextInputEventKey (Identity (step, info))
    KeymapChangedEvent -> DM.singleton KeymapChangedEventKey (Identity step)
    MouseMotionEvent info -> DM.singleton MouseMotionEventKey (Identity (step, info))
    MouseButtonEvent info -> DM.singleton MouseButtonEventKey (Identity (step, info))
    MouseWheelEvent info -> DM.singleton MouseWheelEventKey (Identity (step, info))
    JoyAxisEvent info -> DM.singleton JoyAxisEventKey (Identity (step, info))
    JoyBallEvent info -> DM.singleton JoyBallEventKey (Identity (step, info))
    JoyHatEvent info -> DM.singleton JoyHatEventKey (Identity (step, info))
    JoyButtonEvent info -> DM.singleton JoyButtonEventKey (Identity (step, info))
    JoyDeviceEvent info -> DM.singleton JoyDeviceEventKey (Identity (step, info))
    ControllerAxisEvent info -> DM.singleton ControllerAxisEventKey (Identity (step, info))
    ControllerButtonEvent info -> DM.singleton ControllerButtonEventKey (Identity (step, info))
    ControllerDeviceEvent info -> DM.singleton ControllerDeviceEventKey (Identity (step, info))
    AudioDeviceEvent info -> DM.singleton AudioDeviceEventKey (Identity (step, info))
    QuitEvent -> DM.singleton QuitEventKey (Identity step)
    UserEvent info -> DM.singleton UserEventKey (Identity (step, info))
    SysWMEvent info -> DM.singleton SysWMEventKey (Identity (step, info))
    TouchFingerEvent info -> DM.singleton TouchFingerEventKey (Identity (step, info))
    TouchFingerMotionEvent info -> DM.singleton TouchFingerMotionEventKey (Identity (step, info))
    MultiGestureEvent info -> DM.singleton MultiGestureEventKey (Identity (step, info))
    DollarGestureEvent info -> DM.singleton DollarGestureEventKey (Identity (step, info))
    DropEvent info -> DM.singleton DropEventKey (Identity (step, info))
    ClipboardUpdateEvent -> DM.singleton ClipboardUpdateEventKey (Identity step)
    UnknownEvent info -> DM.singleton UnknownEventKey (Identity (step, info))

fanSDLEvent :: Reflex t => Event t (TimeStep, EventPayload) -> EventSelector t EventKey
fanSDLEvent event = fan $ fmap (uncurry splitSDLEvent) event

type KeyboardEventSelector t = EventSelector t (Const2 Keycode (TimeStep, KeyboardEventData))

splitKeyboardEvent :: TimeStep -> KeyboardEventData -> Map Keycode (TimeStep, KeyboardEventData)
splitKeyboardEvent step event = M.singleton (keysymKeycode $ keyboardEventKeysym event) (step, event)

subfanKeyboardEvent :: Reflex t => EventSelector t EventKey -> KeyboardEventSelector t
subfanKeyboardEvent selector = fanMap $ fmap (uncurry splitKeyboardEvent) $ select selector KeyboardEventKey
