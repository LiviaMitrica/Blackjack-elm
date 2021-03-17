module Test.Generated.Main406865034 exposing (main)

import Tests
import Example

import Test.Reporter.Reporter exposing (Report(..))
import Console.Text exposing (UseColor(..))
import Test.Runner.Node
import Test

main : Test.Runner.Node.TestProgram
main =
    [     Test.describe "Tests" [Tests.suite],     Test.describe "Example" [Example.suite] ]
        |> Test.concat
        |> Test.Runner.Node.run { runs = Nothing, report = (ConsoleReport UseColor), seed = 16887517143972, processes = 8, globs = [], paths = ["E:\\An III sem I\\FP\\Project\\tests\\Example.elm","E:\\An III sem I\\FP\\Project\\tests\\Tests.elm"]}