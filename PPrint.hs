-- rk418291

module PPrint where

writeln :: String -> IO ()
writeln = putStrLn

showsPair :: Show a => (String, a) -> ShowS
showsPair (k,v) =
    (showString k)
    .(showString ": ")
    .(shows v)

pprH, pprV :: [ShowS] -> ShowS
pprV = intercalateS (showString "\n")
pprH = intercalateS (showString " ")

intercalateS :: ShowS -> [ShowS] -> ShowS
intercalateS sep list =
    aux sep list
    where
        aux _ [x] = x
        aux sep (x:xs) = x.sep.(aux sep xs)
        aux _ [] = id

pprListWith :: (a -> ShowS) -> [a] -> ShowS
pprListWith = \x -> pprV.(map x)

runShows :: ShowS -> IO ()
runShows = putStrLn . ($"")
