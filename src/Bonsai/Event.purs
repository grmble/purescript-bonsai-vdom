-- |  Event listener utilities
-- |
-- |  For maximum performance, event handlers attached to the
-- |  virtual DOM should be comparable by object identity.
-- |  For this, you have call on with a top level function
-- |  event decoder.
-- |
-- |  So for top VDOM diff/patching performance, instead of
-- |  using onInput, you would make a top level function
-- |
-- |    decodeXxx event = eventDecoder Xxx targetValue
-- |
-- |    -- use it like
-- |    on "input" decodeXxx
-- |
-- |  Or you might just have a single event handler on a
-- |  parent element and look at the event target.

module Bonsai.Event
  ( onInput
  , onInputWithOptions
  , onClick
  , onClickWithOptions
  , onEnter
  , onSubmit
  , eventDecoder
  , preventDefaultStopPropagation
  , targetValue
  , targetFormValues
  , targetValues
  , ignoreEscape
  )
where

import Prelude


import Bonsai.Types (Cmd)
import Bonsai.VirtualDom (Options, Property, on, onWithOptions)
import Control.Plus (empty)
import Data.Array (range, catMaybes)
import Data.Foreign (F, Foreign, ForeignError(..), isNull, isUndefined, readInt, readString, fail)
import Data.Foreign.Index ((!))
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap, fromFoldable)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))


-- | Event listener property for the input event.
-- |
-- | Should be defined on an input. Will call
-- | the message constructor with the current value
-- | of the input element.
onInput :: forall msg. (String -> Cmd msg) -> Property msg
onInput fn =
  on "input" (eventDecoder fn targetValue)

onInputWithOptions :: forall msg. Options -> (String -> Cmd msg) -> Property msg
onInputWithOptions options fn =
  onWithOptions "input" options (eventDecoder fn targetValue)

-- | Event listener property for the click event.
onClick :: forall msg. Cmd msg -> Property msg
onClick cmd =
  on "click" (const $ pure cmd)

onClickWithOptions :: forall msg. Options -> Cmd msg -> Property msg
onClickWithOptions options cmd =
  onWithOptions "click" options (const $ pure cmd)

-- | Event listener property for the submit event.
-- |
-- | Should be defined on the form. Will prevent default
-- | and stop propagation and will call the constructor
-- | with a map of the current form values.
onSubmit :: forall msg. (StrMap String -> Cmd msg) -> Property msg
onSubmit fn =
  onWithOptions "submit" preventDefaultStopPropagation (eventDecoder fn targetValues)

-- | Emit commands on enter key presses
onEnter :: forall msg. Cmd msg -> Property msg
onEnter enter =
  on "keydown" $ \event -> do
    keyCode <- event ! "keyCode" >>= readInt
    case keyCode of
      13 -> -- Enter
        pure enter
      _ ->
        pure empty

preventDefaultStopPropagation :: Options
preventDefaultStopPropagation =
  { preventDefault: true
  , stopPropagation: true
  }


-- | Helper to make an EventDecoder from a constructor and a foreign decoder.
eventDecoder
  :: forall a msg
  .  (a -> Cmd msg)
  -> (Foreign -> F a)
  -> Foreign
  -> F (Cmd msg)
eventDecoder mapFn decoder =
  map mapFn <<< decoder

-- | Read the value of the target input element
targetValue :: Foreign -> F String
targetValue value =
  value ! "target" ! "value" >>= readString

-- ! Read the names and values of the target element's form.
targetFormValues :: Foreign -> F (StrMap String)
targetFormValues value =
  value ! "target" ! "form" >>= namesAndValues

-- | Read the names and values of target form, for form events.
targetValues :: Foreign -> F (StrMap String)
targetValues value = do
  value ! "target" >>= namesAndValues

-- | Read a strmap of values from a (fake) array of input elements
namesAndValues :: Foreign -> F (StrMap String)
namesAndValues arr = do
  len <- arr ! "length" >>= readInt
  (fromFoldable <<< catMaybes) <$> traverse (nameAndValue arr) (range 0 (len - 1))

nameAndValue :: Foreign -> Int -> F (Maybe (Tuple String String))
nameAndValue arr idx = do
  name <- arr ! idx ! "name"
  value <- arr ! idx ! "value"
  if (isNullOrUndefined name) || (isNullOrUndefined value)
    then pure Nothing
    else do
      n <- readString name
      v <- readString value
      pure (Just (Tuple n v))

isNullOrUndefined :: Foreign -> Boolean
isNullOrUndefined value =
  (isNull value) || (isUndefined value)

-- | Event decoder returns unit or fails
-- |
-- | hack or no hack?
ignoreEscape :: Foreign -> F Unit
ignoreEscape event = do
  keyCode <- event ! "keyCode" >>= readInt
  if keyCode == 27 -- ESC
    then pure unit
    else fail (ForeignError "there is no escape")