 -- Used to be HW5ModelSoln

module Model where 

import Data.List

-- Section 6.3

-- Datatype for a domain of values
data Entity = A | B | C | D | E | F | G
            | H | I | J | K | L | M | N 
            | O | P | Q | R | S | T | U 
            | V | W | X | Y | Z | Unspec
     deriving (Eq,Show,Bounded,Enum)

-- A list of all the entities
entities :: [Entity]
entities =  [minBound..maxBound] 

-- constants that will be interpreted as elements of Entity
snowWhite, alice, dorothy, goldilocks, littleMook, atreyu
                                                :: Entity

snowWhite  = S
alice      = A
dorothy    = D
goldilocks = G 
littleMook = M
atreyu     = Y
-- TODO: May need to change later 
isaacNewton = B


-- Types for predicates on the model.  Notice all are "Curried"
type OnePlacePred   = Entity -> Bool
type TwoPlacePred   = Entity -> Entity -> Bool
type ThreePlacePred = Entity -> Entity -> Entity -> Bool

-- Convert a list of entities to a characteristic function for the list
-- This makes it easy to define relations by specifying the elements of type
-- Entity that makes them true.
list2OnePlacePred :: [Entity] -> OnePlacePred
list2OnePlacePred xs = \ x -> elem x xs

-- Sample one-place predicates representing nouns
girl, boy, princess, dwarf, giant, wizard, sword, dagger, telescope, universe, astronomer
                                         :: OnePlacePred

-- Define characteristic functions for each unary relation
girl     = list2OnePlacePred [S,A,D,G]
boy      = list2OnePlacePred [M,Y,B]
princess = list2OnePlacePred [E]
dwarf    = list2OnePlacePred [B,R]
giant    = list2OnePlacePred [T]
wizard   = list2OnePlacePred [W,V]
sword    = list2OnePlacePred [F]
dagger   = list2OnePlacePred [X]
-- TODO: May need to change to a specific list 
-- There are 2 telescopes in the world.
telescope = list2OnePlacePred [Z]
-- TODO: According to modern physics, there could be infinite # of universe
universe = list2OnePlacePred entities
-- TODO: According to modern physics, there should be infinite # of 
-- universe
star = list2OnePlacePred entities
astronomer = list2OnePlacePred [B, P, M, Y]

-- Predicates defined from earlier predicates
child, person, man, woman, male, female, thing :: OnePlacePred

child  = \ x -> (girl x  || boy x)
person = \ x -> (child x || princess x || dwarf x 
                         || giant x    || wizard x || astronomer x) 
man    = \ x -> (dwarf x || giant x || wizard x) 
woman  = \ x -> princess x 
male   = \ x -> (man x || boy x) 
female = \ x -> (woman x || girl x)
thing  = \ x -> not (person x || x == Unspec)

-- Sample one place predicates (unary relations) representing verbs
laugh, cheer, shudder, shine :: OnePlacePred
-- Characteristic functions for intransitive verbs
laugh   = list2OnePlacePred [A,G,E]
cheer   = list2OnePlacePred [M,D]
shudder = list2OnePlacePred [S]
-- TODO: may need to change it later 
shine   = list2OnePlacePred entities
shines  = shine 


-- One place predicates (unary relations) representing verbs 
red, dangerous :: OnePlacePred
red       = list2OnePlacePred [B, R, X]
dangerous = list2OnePlacePred [F, G, X, W, V]

-- Sample two place predicates representing transitive verbs
love, admire, help, defeat, own :: TwoPlacePred
-- characteristic functions for binary relations for transitive verbs
love   = curry (`elem` [(Y,E),(B,S),(R,S)])
admire = curry (`elem` [(x,B) | x <- entities, astronomer x])
help   = curry (`elem` [(Z, B)])
defeat = curry (`elem` [(x,y) | x <- entities, 
                                y <- entities,
                                dwarf x && giant y]
                    ++ [(A,W),(A,V)])
-- TODO: May need to y so that only thing can be owned is objects 
own    = curry (`elem` [(B,y) | y <- entities])
-- TODO: May need to y so that only thing can be owned is objects
study  = curry (`elem` [(x,y) | x <- entities, y <- entities])
was    = curry (`elem` [(x,y) | x <- [S,A,D,G, M,Y, E, B, R, T, W, V, P], y <- [B, P]])

-- Transform a function that takes three arguments into its curried form
curry3 :: ((a,b,c) -> d) -> a -> b -> c -> d
curry3 f x y z = f (x,y,z)

-- Sample three place predicates representing ditransitive verbs
give :: ThreePlacePred
give = curry3 (`elem` [(T,S,X),(A,E,S)])
-- define (Curried) characteristic function for kill
kill :: ThreePlacePred
kill = curry3 (`elem` [(Y,T,F),(Unspec,D,X),
                       (Unspec,M,Unspec)])

-- Convert a transitive verb into its passive form
-- When make passive, replae subject by unspecified.
passivize :: TwoPlacePred -> OnePlacePred
passivize r = \ x -> r Unspec x

-- Converted function to a new form where the first and second arguments
-- must be the same
self ::  (a -> a -> b) -> a -> b
self p = \ x -> p x x 

