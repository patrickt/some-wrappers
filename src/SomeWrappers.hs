module SomeWrappers
       ( someFunc
       ) where

someFunc :: IO ()
someFunc = putStrLn ("someFunc" :: String)
