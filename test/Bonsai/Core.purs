module Test.Bonsai.Core
where

import Prelude

import Bonsai (BONSAI, Cmd, Program, emitMessage, emittingTask, emptyCommand, program, pureCommand, simpleTask, unitTask)
import Bonsai.Core (issueCommand, issueCommand')
import Bonsai.DOM (DOM, ElementId(..), affF, elementById, textContent, window)
import Bonsai.Html (button, div_, render, span, text, (!))
import Bonsai.Html.Attributes (id_)
import Bonsai.Html.Events (onClick)
import Bonsai.Types (TaskContext)
import Bonsai.VirtualDom (VNode)
import Control.Monad.Aff (Aff, Milliseconds(..), delay)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Free (Free)
import Data.Tuple (Tuple(..))
import Test.JSDOM (jsdomWindow, fireClick)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert as Assert

foreign import data CLIENTEFF :: Effect

type Model = Int

data Msg
  = Inc
  | Dec
  | Foo
  | Bar
  | Baz
  | Boo
  | TaskResult

update :: forall eff. Msg -> Model -> Tuple (Cmd (console::CONSOLE,clienteff::CLIENTEFF|eff) Msg) Model
update msg model =
  case msg of
    Inc ->
      Tuple emptyCommand $ model + 1
    Dec ->
      Tuple emptyCommand $ model - 1
    -- test compilation/types for some ways of starting tasks
    Foo ->
      Tuple (simpleTask simpleAff) model
    Bar ->
      Tuple (emittingTask emittingAff) model
    Baz ->
      Tuple (emittingTask pureAff) model
    Boo ->
      Tuple (unitTask consoleAff) model
    TaskResult ->
      Tuple emptyCommand $ model

view :: Model -> VNode Msg
view model =
  render $ div_ $ do
    span ! id_ "counter" $ do
      text $ show model
    button ! id_ "plusButton" ! onClick Inc $ text "+"
    button ! id_ "minusButton" ! onClick Dec $ text "-"

simpleAff :: forall eff. Aff (clienteff::CLIENTEFF|eff) Msg
simpleAff =
  pure TaskResult

emittingAff :: forall eff
  .  TaskContext (clienteff::CLIENTEFF|eff) Msg
  -> Aff (clienteff::CLIENTEFF|eff) Unit
emittingAff ctx = do
  emitMessage ctx TaskResult
  pure unit

pureAff :: forall eff
  .  TaskContext eff Msg
  -> Aff eff Unit
pureAff ctx = do
  emitMessage ctx TaskResult
  pure unit

consoleAff :: forall eff.  Aff (console::CONSOLE|eff) Unit
consoleAff = do
  liftEff $ log "Hello, world"
  pure unit

-- test using issueCommand from a main program
-- this is only here to make sure it compiles
main :: Eff (bonsai::BONSAI,dom::DOM,exception::EXCEPTION) Unit
main = do
  prg <- window # program (ElementId "main") update view 0
  issueCommand prg (simpleTask simpleAff)
  issueCommand prg (emittingTask emittingAff)
  pure unit


elementTextAfterRender
  :: forall eff model msg
  .  Program eff model msg
  -> ElementId
  -> Aff (dom::DOM|eff) String
elementTextAfterRender env id = do
  affF $ (\_ -> elementById id env.document >>= textContent) unit


tests :: forall eff. Free (TestF (bonsai::BONSAI,dom::DOM,clienteff::CLIENTEFF,console::CONSOLE,exception::EXCEPTION|eff)) Unit
tests =
  suite "Bonsai.Core" do
    test "program/taskContext" $ do
      env <- liftEff $
        jsdomWindow """<html><body id="main"></body></html>""" #
        program (ElementId "main") update view 0
      initialText <- elementTextAfterRender env (ElementId "counter")
      Assert.equal "0" initialText

      issueCommand' env $ pureCommand Inc
      x1 <- elementTextAfterRender env (ElementId "counter")
      Assert.equal "1" x1

      -- liftEff $ issueCommand env $ pureCommand Boo
      -- observe output

      _  <- affF $
        elementById (ElementId "plusButton") env.document >>=
        fireClick
      issueCommand' env emptyCommand
      x2 <- elementTextAfterRender env (ElementId "counter")
      Assert.equal "2" x2

    test "cmd monoid" do
      env <- liftEff $
        jsdomWindow """<html><body id="main"></body></html>""" #
        program (ElementId "main") update view 0
      initialText <- elementTextAfterRender env (ElementId "counter")
      Assert.equal "0" initialText

      let cmd = pureCommand Inc <> unitTask (delay (Milliseconds 200.0)) <> pureCommand Inc
      issueCommand' env cmd
      delay (Milliseconds 100.0)
      textAfterInc <- elementTextAfterRender env (ElementId "counter")
      Assert.equal "1" textAfterInc

      delay (Milliseconds 200.0)
      textAfterRest <- elementTextAfterRender env (ElementId "counter")
      Assert.equal "2" textAfterRest
