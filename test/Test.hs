module Main where

import Test.Framework

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


main = defaultMain [
        agentsetbuildingTestGroup, --         interactionTestGroup,  randomOrderInitializationTestGroup,
        agentsetsTestGroup, --                layoutsTestGroup,      repeatTestGroup,
       anyallTestGroup, --                   letTestGroup,          reportertasksTestGroup,
       askTestGroup , --                      linksTestGroup,        resizeworldTestGroup,
       booleanoperatorsTestGroup ,--         listsTestGroup,        rgbTestGroup,
       breedsTestGroup , --                   mathTestGroup,         runTestGroup,
       canmoveTestGroup , --                  memberTestGroup,       selfmyselfTestGroup,
       --  commandtasksTestGroup,             minmaxnofTestGroup,    sortTestGroup,
       comparingagentsTestGroup ,--          movetoTestGroup,       stacktracesTestGroup,
       controlstructuresTestGroup, --,        neighborsTestGroup,    stopTestGroup,
       --  deadturtlesTestGroup,              noagentsTestGroup,     sumTestGroup,
       --  diffuseTestGroup,                  nsumTestGroup,         ticksTestGroup,
       --  distanceTestGroup,                 observerTestGroup,     tieTestGroup,
       --  equalityTestGroup,                 oneofTestGroup,        tiltTestGroup,
       timerTestGroup
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
       ]
        
