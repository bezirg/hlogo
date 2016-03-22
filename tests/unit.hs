module Main where

import Test.Tasty
import Test.Tasty.Runners.Html
import System.Environment (getArgs, withArgs)

import AgentsetBuilding
-- import Interaction
-- import  RandomOrderInitialization
import  Agentsets
-- import  Layouts
-- import  Repeat
import  AnyAll
-- import  Let
-- import  ReporterTasks
import  Ask
-- import  Links
-- import  ResizeWorld
import  BooleanOperators
-- import  Lists
-- import  RGB
import  Breeds
-- import  Math
-- import  Run
import  CanMove
-- import  Member
-- import  SelfMyself
-- import  CommandTasks
-- import  MinMaxNOf
-- import  Sort
import  ComparingAgents
-- import  MoveTo
-- import  StackTraces
import  ControlStructures
-- import  Neighbors
-- import  Stop
-- import  DeadTurtles
-- import  NoAgents
-- import  Sum
-- import  Diffuse
-- import  Nsum
-- import  Ticks
-- import  Distance
-- import  Observer
-- import  Tie
-- import  Equality
-- import  OneOf
-- import  Tilt
-- import  Errors
-- import  Patch
import  Timer
-- import  Face
-- import  PatchAhead
-- import  Turtles
-- import  File
-- import  PatchAt
-- import  TurtlesHere
-- import  Generator
-- import  PatchSize
-- import  TurtlesOn
-- import  HubNet
-- import  Perspective
-- import  TypeChecking
-- import  ImportPatchesAndDrawing
-- import  Plot
-- import  UpAndDownhill
-- import  ImportWorld
-- import  Predicates
-- import  UserReporters
-- import  InCone
-- import  Procedures
-- import  WithLocalRandomness
-- import  Initialization
-- import  Random
-- import  Word
-- import  InRadius
-- import  RandomCors

main :: IO ()
main = do
  args <- getArgs -- append to stdin args
  withArgs ("-j1":args) $ -- don't run tests in parallel because it messes globals
   defaultMainWithIngredients (htmlRunner:defaultIngredients)(
    localOption (mkTimeout 1000000) $ -- timeouts any test at 1s
    testGroup "hlogo"
      [ testGroup "agentsetbuildingTestGroup" agentsetbuildingTestGroup --         interactionTestGroup,  randomOrderInitializationTestGroup,
      , testGroup "agentsetsTestGroup" agentsetsTestGroup --                layoutsTestGroup,      repeatTestGroup,
      , testGroup "anyallTestGroup" anyallTestGroup --                   letTestGroup,          reportertasksTestGroup,
       -- , testGroup "askTestGroup" askTestGroup --                      linksTestGroup,        resizeworldTestGroup,
      , testGroup "booleanoperatorsTestGroup" booleanoperatorsTestGroup --         listsTestGroup,        rgbTestGroup,
      , testGroup "breedsTestGroup" breedsTestGroup --                   mathTestGroup,         runTestGroup,
      , testGroup "canmoveTestGroup" canmoveTestGroup --                  memberTestGroup,       selfmyselfTestGroup,
       --  commandtasksTestGroup,             minmaxnofTestGroup,    sortTestGroup,
      , testGroup "comparingagentsTestGroup" comparingagentsTestGroup--          movetoTestGroup,       stacktracesTestGroup,
      , testGroup "controlstructuresTestGroup" controlstructuresTestGroup --,        neighborsTestGroup,    stopTestGroup,
       --  deadturtlesTestGroup,              noagentsTestGroup,     sumTestGroup,
       --  diffuseTestGroup,                  nsumTestGroup,         ticksTestGroup,
       --  distanceTestGroup,                 observerTestGroup,     tieTestGroup,
       --  equalityTestGroup,                 oneofTestGroup,        tiltTestGroup,
      , testGroup "timerTestGroup" timerTestGroup
       --  errorsTestGroup,                   patchTestGroup,        ,
       --  faceTestGroup,                     patchaheadTestGroup,   turtlesTestGroup,
       --  fileTestGroup,                     patchatTestGroup,      turtleshereTestGroup,
       --  generatorTestGroup,                patchsizeTestGroup,    turtlesonTestGroup,
       --  hubnetTestGroup,                   perspectiveTestGroup,  typecheckingTestGroup,
       --  importpatchesanddrawingTestGroup,  plotTestGroup,         upanddownhillTestGroup,
       --  importworldTestGroup,              predicatesTestGroup,   userreportersTestGroup,
       --  inconeTestGroup,                   proceduresTestGroup,   withlocalrandomnessTestGroup,
       --  initializationTestGroup,           randomTestGroup,       wordTestGroup,
       --  inradiusTestGroup,                 randomcorsTestGroup
       ])
        
