module Test.Generated.Main1449398756 exposing (main)

import Tests

import Test.Reporter.Reporter exposing (Report(..))
import Console.Text exposing (UseColor(..))
import Test.Runner.Node
import Test

main : Test.Runner.Node.TestProgram
main =
    [     Test.describe "Tests" [Tests.suite] ]
        |> Test.concat
        |> Test.Runner.Node.run { runs = Just 100, report = (ConsoleReport UseColor), seed = 176494123293388, processes = 8, globs = [], paths = ["E:\\An III sem I\\FP\\Project\\tests\\Tests.elm"]}