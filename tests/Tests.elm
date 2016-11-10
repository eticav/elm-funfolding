module Tests exposing (..)

import Test exposing (..)

import Expect
import String
import FunFolding as FF

type alias Person = { name: String
                    , age : Int
                    , adr : String
                    }

all : Test
all =
  describe "test FunComposition" [ testCompareN
                                 , testAndN
                                 , testOrN
                                 , testAndNOrN
                                 ]

testAndN : Test
testAndN =
  let
    isEven x = (rem x 2) == 0
  in
  describe "test andN"
  [ test "Compare with 1 predicates" <|
        \() ->
         let
           
           fun = FF.andN [(\x->isEven x)]
           result = List.filter fun [1..10]
           expected = [2,4,6,8,10]
         in
           (Expect.equal result expected)
  , test "Compare with 2 predicates" <|
        \() ->
         let           
           fun = FF.andN [(\x->isEven x), (\x->x>5)]
           result = List.filter fun [1..10]
           expected = [6,8,10]
         in
           (Expect.equal result expected)

    , test "Compare with 4 predicates" <|
        \() ->
         let           
           fun = FF.andN [ (\x->x/=0)
                         , (\x->x>(-20))
                         , (\x->x<20)
                         , (\x->(rem x 10) == 0) ]
           result = List.filter fun [-100..100]
           expected = [-10,10]
         in
           (Expect.equal result expected)
  ]

testOrN : Test
testOrN =
  describe "test orN"
  [ test "Compare 3" <|
        \() ->
         let           
           fun = FF.orN [(\x->x==0), (\x->x<(-99)), (\x->x>99) ]
           result = List.filter fun [-100..100]
           expected = [-100,0,100]
         in
           (Expect.equal result expected)
  ]

testAndNOrN : Test
testAndNOrN =
  describe "test combined andN orN"
  [ test "Compare combined N" <|
        \() ->
         let
           fun = FF.orN [ (\x->x==0)
                        , (\x->x<(-99))
                        , (\x->x>99)
                        , FF.andN [ (\x->(rem x 7)==0)
                                  , (\x->x<20)
                                  , (\x->x>(-20))
                                  ]
                        ]
           result = List.filter fun [-100..100]
           expected = [-100,-14,-7,0,7,14,100]
         in
           (Expect.equal result expected)
  ]
  
testCompareN : Test
testCompareN =
    let
    george = Person "George" 15 "10 Elm Street"
    georgeV = Person "George" 5 "10 Elm Street"
    henry = Person "Henry" 44 "9 Elm Street"
    henryIV = Person "Henry" 4 "9 Elm Street"
    paul = Person "Paul" 35 "8 Elm Street"
    simon = Person "Simon" 27 "7 Elm Street"
    joshua = Person "Joshua" 13 "6 Elm Street"
    ludovic = Person "Ludovic" 10 "5 Elm Street"
    etienne = Person "Etienne" 45 "4 Elm Street"

    louis : Int->Int->Int->Person
    louis id age adr = Person
                       ("Louis"++(toString id))
                       age
                       ((toString adr))
    
    persons : List Person
    persons = [ george
              , henry
              , paul
              , simon
              , joshua
              , ludovic
              , etienne
              ]            
  in 
    describe "test CompareN"
    [ test "Compare name" <|
        \() ->
         let
           fun = FF.compareN [(\x y -> compare x.name y.name)]
           sortedList = List.sortWith fun [ george
                                          , henry
                                          , paul
                                          , simon
                                          , joshua
                                          , ludovic
                                          , etienne
                                          ]
           expected = [ etienne
                      , george
                      , henry
                      , joshua
                      , ludovic
                      , paul
                      , simon                            
                      ]
         in
           (Expect.equal sortedList expected)
      , test "Compare age" <|
          \() ->
            let
              fun = FF.compareN [(\x y -> compare x.age y.age)]
              sortedList = List.sortWith fun [ george
                                             , henry
                                             , etienne
                                             , georgeV
                                             , henryIV
                                             ]
              expected = [ henryIV
                         , georgeV                                 
                         , george
                         , henry
                         , etienne
                         ]
            in
              (Expect.equal sortedList expected)
                
      , test "Compare name age" <|
          \() ->
            let
              fun = FF.compareN [ (\x y -> compare x.name y.name)
                                      , (\x y -> compare x.age y.age)
                                      ]
              sortedList = List.sortWith fun [ george
                                             , henry
                                             , etienne
                                             , georgeV
                                             , henryIV
                                             ]
              expected = [ etienne
                         , georgeV
                         , george
                         , henryIV                                                        
                         , henry                            
                         ]
            in
              (Expect.equal sortedList expected)
      , test "Compare name age adress" <|
          \() ->
            let
              fun = FF.compareN [ (\x y -> compare x.name y.name)
                                      , (\x y -> compare x.age y.age)
                                      , (\x y -> compare x.adr y.adr)
                                      ]
              sortedList = List.sortWith fun [ louis 2 1 1                                               
                                             , louis 1 2 2
                                             , louis 1 1 1
                                             , louis 1 2 3                                                
                                             , louis 2 1 2
                                             , louis 1 2 2
                                             ]
              expected = [ louis 1 1 1
                         , louis 1 2 2
                         , louis 1 2 2
                         , louis 1 2 3
                         , louis 2 1 1
                         , louis 2 1 2
                         ]
            in
              (Expect.equal sortedList expected)
        ]
