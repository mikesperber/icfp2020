{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, LambdaCase, PolyKinds, RankNTypes, ScopedTypeVariables, TypeApplications, TypeOperators, TypeFamilies, UnicodeSyntax #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE UndecidableInstances #-}

module PolysemyUtil where

import Polysemy
import Polysemy.Internal
import Polysemy.Internal.Union

raise2Under :: ∀ e3 e2 e1 r a. Sem (e1 ': e2 ': r) a -> Sem (e1 ': e2 ': e3 ': r) a
raise2Under = hoistSem $ hoist raise2Under . weakenUnder
  where
    weakenUnder :: ∀ m x. Union (e1 ': e2 ': r) m x -> Union (e1 ': e2 ': e3 ': r) m x
    weakenUnder (Union Here a) = Union Here a
    weakenUnder (Union (There Here) a) = Union (There Here) a
    weakenUnder (Union (There (There n)) a) = Union (There (There (There n))) a
    {-# INLINE weakenUnder #-}
{-# INLINE raise2Under #-}
