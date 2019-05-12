import Test.Tasty
import Test.Tasty.HUnit

import Robots

main :: IO ()
main = defaultMain testsRobots

testsRobots :: TestTree
testsRobots = let
        walter = robot "Walter" 50 50
        beep = robot "Beep" 50 67
        wryy = robot "Wryy" 17 150
        glassCannon = robot "GC" 100 5
        deadBot = robot "DEAD" 5 0
        tank = robot "TANK" 5 100
        
    in testGroup "Unit tests for Robots task"
        [ testCase "Test for getName" $
            getName walter @?= "Walter"

        , testCase "Test for getAttack" $
            getAttack tank @?= 5

        , testCase "Test for getHealth" $
            getHealth walter @?= 50

        , testCase "Test for setName" $
            setName "Karasek" walter @?= ("Karasek", 50, 50)

        , testCase "Test for setAttack" $
            setAttack 9000 walter @?= ("Walter", 9000, 50)

        , testCase "Test for setHealth" $
            setHealth 9000 walter @?= ("Walter", 50, 9000)

        , testCase "Test for printRobot" $
            printRobot walter @?= "Walter, attack: 50, health: 50"

        , testCase "Test for damage" $
            (getHealth $ damage walter 9000) @?= -8950

        , testCase "isAlive works on alive robot" $
            isAlive walter @?= True

        , testCase "isAlive works on dead robot" $
            isAlive deadBot @?= False

        , testCase "Alive robot fights" $
            (getHealth $ fight walter beep) @?= 17

        , testCase "Dead robot fights" $
            (getHealth $ fight deadBot beep) @?= 67

        , testCase "Attacker knockouts defender in first round" $
            threeRoundFight glassCannon beep @?= ("GC", 100, 5)

        , testCase "Defender knockouts attacker in second round" $
            threeRoundFight walter beep @?= ("Beep", 50, 17)

        , testCase "Attacker wins, took 1 hit" $
            threeRoundFight beep tank @?= ("Beep", 50, 62)

        , testCase "Defender wins, took 2 hits" $
            threeRoundFight tank beep @?= ("Beep", 50, 57)

        , testCase "Attacker wins, equal hp" $
            threeRoundFight beep wryy @?= ("Beep", 50, 50)

        , testCase "Test for neueRobotAttack" $
            (getHealth $ neueRobotAttack tank) @?= 91
            
        , testCase "Test for survivors" $
            survivors @?= [robot "Strelizia" 1 1]
        ]
