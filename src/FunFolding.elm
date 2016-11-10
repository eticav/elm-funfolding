module FunFolding exposing ( (<<*)
                           , foldlFun
                           , foldlFun2
                           , compareN
                           , andN
                           , orN
                           )


{-| This library contains high-order functions for folding a list of functions together. foldlFun uses a accumulator function which is used to compose all the functions provided with the function list.

as an example, imagine you want to filter a list of integers [-100..100] using the following predicates
  (\x->x/=0)
  (\x->x>(-20))
  (\x->x<20)
  (\x->(rem x 10) == 0)
The expected result is : [-10,10]

We foldl all the predicates using the (&&) operator, we results in a single predicate function.

The folding function is defined by 
andN : List (a->Bool) -> a -> Bool
andN =
  foldlFun (&&) False

test "Compare with 4 predicates" <|
        \() ->
         let           
           fun = FC.andN [ (\x->x/=0)
                         , (\x->x>(-20))
                         , (\x->x<20)
                         , (\x->(rem x 10) == 0) ]
           result = List.filter fun [-100..100]
           expected = [-10,10]
         in
           (Expect.equal result expected)

@docs (<<*),  foldlFun, foldlFun2, compareN, andN , orN 

-}

{-| (<<*) composes functions f(a,b) and g(a) to obtain f(x,g(x)).
-}
(<<*) : (a -> b -> c) -> (a -> b) -> a -> c
(<<*) f g x = f x (g x)

{-| foldlFun folds a list of functions using composition function.
The composition function can be ssen as the accumulator function.
As an Example , the andN function is defined by :
 
   andN : List (a->Bool) -> a -> Bool
   andN = foldlFun (&&) False

-}              
foldlFun : (a->a->a)->a->List (b->a) -> b -> a
foldlFun acc initialValue list =
  case list of
    []->(\x->initialValue)
    hd1::[]->hd1
    hd1::hd2::[] -> ((acc << hd1) <<* hd2)
    hd1::hd2::tl -> List.foldl (\f g->(acc << f) <<* g)  ((acc << hd1) <<* hd2) tl

{-| foldlFun2 folds a list of two parameters functions using a composition function.
The composition function can be ssen as the accumulator function.
 As an Example , the compareN function is defined by :

compareN : List (a->a->Order)->a->a->Order
compareN = foldlFun2 andOrder EQ
-}  
foldlFun2 : (a->a->a)->a->List (b->b->a) -> b->b -> a
foldlFun2 acc initialValue list =
  curry (case list of
    []->(\x->initialValue)
    hd1::[]->(uncurry hd1)
    hd1::hd2::[] -> ((acc << (uncurry hd1)) <<* (uncurry hd2))
    hd1::hd2::tl ->
      let
        init = ((acc << (uncurry hd1)) <<* (uncurry hd2))
      in
        List.foldl (\f g->(acc << (g)) <<* (uncurry f))  init tl)
    
{-| andOrder is the composition function used by compareN. It maintains left to right consistancy whene composing many 'compare' functions.
-}  

andOrder : Order->Order->Order
andOrder l r =
  case (l,r) of
    (EQ,x)->x
    (any,_)->any

{-| compareN folds a list of compare function in such a way that left that left most compares have a bigger weight that the right most compares.

             type alias Person = { name: String
                                 , age : Int
                                 , adr : String

             louis id age adr = Person
                               ("Louis"++(toString id))
                               age
                               ((toString adr))}

              fun = FC.compareN [ (\x y -> compare x.name y.name)
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

-}  
compareN : List (a->a->Order)->a->a->Order
compareN =
  foldlFun2 andOrder EQ

and2 : (a->Bool) ->(a->Bool)->a->Bool
and2 f g =
  ((&&) << f) <<* g

and3 : (a->Bool) ->(a->Bool)->(a->Bool)->a->Bool
and3 f g h =
  let
    compose =  ((&&) << f) <<* g
  in
    ((&&) << h) <<*
                  (((&&) << f) <<* g)

and4 : (a->Bool) ->(a->Bool)->(a->Bool)->(a->Bool)->a->Bool
and4 f g h i=
  let
    compose =  ((&&) << f) <<* g
  in
    ((&&) << i) <<*
                  (((&&) << h) <<*
                                 (((&&) << f) <<* g))

{-| folds a list of predicates using the && operator. andN and orN can be combined together.

           fun = FC.andN [ (\x->x/=0)
                         , (\x->x>(-20))
                         , (\x->x<20)
                         , (\x->(rem x 10) == 0) ]
           result = List.filter fun [-100..100]
           expected = [-10,10]
-}
andN : List (a->Bool) -> a -> Bool
andN =
  foldlFun (&&) False
           
{-| folds a list of predicates using the || operator. andN and orN can be combined together.

           fun = FC.orN [(\x->x==0), (\x->x<(-99)), (\x->x>99) ]
           result = List.filter fun [-100..100]
           expected = [-100,0,100]
-}
orN : List (a->Bool) -> a -> Bool
orN =
  foldlFun (||) False
