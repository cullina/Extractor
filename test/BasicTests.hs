import Test.QuickCheck

prop_monoid_id x = mempty `mappend` x == x && x `mappend` mempty == x

prop_monoid_assoc x y z =
  (x `mappend` y) `mappend` z == x `mappend` (y `mappend` z)