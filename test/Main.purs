module Test.Main where

import Prelude

import Bonsai.DOM (DOM, ElementId(..), affF, appendChild, elementById, innerHTML)
import Bonsai.JSDOM (jsdomDocument)
import Bonsai.VirtualDom (attribute, node, text, render)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Free (Free)
import Test.Unit (TestF, suite, test)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert


main :: forall t1.
  Eff
    ( avar :: AVAR
    , console :: CONSOLE
    , dom :: DOM
    , testOutput :: TESTOUTPUT
    | t1
    )
    Unit
main = runTest tests



tests :: forall eff. Free (TestF (console::CONSOLE,dom::DOM|eff)) Unit
tests = do
  suite "virtual dom" do
    test "render" do
      doc <- affF $ jsdomDocument """<html><body id="main"></body></html>"""
      let vdomNode = node "p" [ attribute "class" "hello"] [ text "Hello, world!" ]
      let elem = render doc (const $ pure false) vdomNode
      _ <- affF $ elementById (ElementId "main") doc >>= appendChild elem
      html <- affF $ elementById (ElementId "main") doc >>= innerHTML
      Assert.equal """<p class="hello">Hello, world!</p>""" html
