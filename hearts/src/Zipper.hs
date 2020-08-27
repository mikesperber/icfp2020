module Zipper where

import Prelude hiding (length)
import qualified Prelude as Prelude

data Zipper a =
  Zipper { reverseFront :: [a], focus :: a, back :: [a] }
  deriving (Show, Eq, Ord)

instance Functor Zipper  where
  fmap f (Zipper reverseFront focus back) = Zipper (map f reverseFront) (f focus) (map f back)

-- exercise: Foldable

fromList :: [a] -> Zipper a
fromList (focus:rest) = Zipper [] focus rest

toList :: Zipper a -> [a]
toList (Zipper reverseFront focus back) = reverse reverseFront ++ [focus] ++ back

cursor :: Zipper a -> a
cursor (Zipper _ focus _) = focus

left :: Zipper a -> Zipper a
left  (Zipper (newFocus:reverseFront) focus back) =
  Zipper reverseFront newFocus (focus:back)
left _ = error "Zipper can't go further left"

safeLeft :: Zipper a -> Maybe (Zipper a)
safeLeft (Zipper (newFocus:reverseFront) focus back) =
  Just (Zipper reverseFront newFocus (focus:back))
safeLeft _ = Nothing

cycleLeft :: Zipper a -> Zipper a
cycleLeft (Zipper (newFocus:reverseFront) focus back) =
  Zipper reverseFront newFocus (focus:back)
cycleLeft (zipper@(Zipper [] focus [])) = zipper
cycleLeft (Zipper [] focus back) =
  let (newFocus:reverseBack) = reverse (focus:back)
  in Zipper reverseBack newFocus []

right :: Zipper a -> Zipper a
right (Zipper reverseFront focus (newFocus:back)) =
  Zipper (focus:reverseFront) newFocus back
right _ = error "Zipper can't go further right"

safeRight :: Zipper a -> Maybe (Zipper a)
safeRight (Zipper reverseFront focus (newFocus:back)) =
  Just (Zipper (focus:reverseFront) newFocus back)
safeRight _ = Nothing

cycleRight :: Zipper a -> Zipper a
cycleRight (Zipper reverseFront focus (newFocus:back)) =
  Zipper (focus:reverseFront) newFocus back
cycleRight (zipper@(Zipper [] focus [])) = zipper
cycleRight (Zipper reverseFront focus []) =
  let (newFocus:front) = reverse (focus:reverseFront)
  in Zipper [] newFocus front

focusTo :: Eq a => a -> Zipper a -> Zipper a
focusTo element zipper0 =
  let found (zipper@(Zipper reverseFront focus back)) cont =
        if focus == element
        then zipper
        else cont
    
      focusToLeft zipper =
        found zipper
          (case safeLeft zipper of
            Nothing -> focusToRight zipper0
            Just zipper' -> focusToLeft zipper')

      focusToRight zipper =
        found zipper
          (case safeRight zipper of
            Nothing -> error "focusTo: element not found"
            Just zipper' -> focusToRight zipper')
  in focusToLeft zipper0

         

length :: Zipper a -> Int
length (Zipper reverseFront focus back) =
  (Prelude.length reverseFront) + 1 + (Prelude.length back)

