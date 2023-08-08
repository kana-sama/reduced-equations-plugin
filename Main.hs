{-# OPTIONS_GHC -fplugin=Plugin #-}

f _ (x:_) = x
f x = const x


main = pure ()

