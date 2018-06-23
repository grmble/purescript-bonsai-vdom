module Test.Main where

import Prelude

import Bonsai.DOM (ElementId(..), affF, appendChild, elementById, innerHTML)
import Bonsai.JSDOM (jsdomDocument)
import Bonsai.VirtualDom (attribute, node, text, render)
import Effect (Effect)
import Control.Monad.Free (Free)
import Test.Unit (TestF, suite, test)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert


main :: Effect Unit
main = runTest tests



tests :: Free TestF Unit
tests = do
  suite "virtual dom" do
    test "render" do
      doc <- affF $ jsdomDocument """<html><body id="main"></body></html>"""
      let vdomNode = node "p" [ attribute "class" "hello"] [ text "Hello, world!" ]
      let elem = render doc (const $ pure false) vdomNode
      _ <- affF $ elementById (ElementId "main") doc >>= appendChild elem
      html <- affF $ elementById (ElementId "main") doc >>= innerHTML
      Assert.equal """<p class="hello">Hello, world!</p>""" html
