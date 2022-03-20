-- hb417666
module PPrint where

writeln :: String -> IO ()
writeln = putStrLn

showsPair :: Show a => (String, a) -> ShowS
showsPair (k, v) = showString k . showString ": " . shows v

pprH, pprV :: [ShowS] -> ShowS
pprV = intercalateS (showString "\n")
pprH = intercalateS (showString " ")

intercalateS :: ShowS -> [ShowS] -> ShowS
intercalateS sep [] = showString ""
intercalateS sep [s] = s
intercalateS sep (f : s : t) = intercalateS sep (f . sep . s : t)

pprListWith :: (a -> ShowS) -> [a] -> ShowS
pprListWith fn l = intercalateS (showString "\n") (map fn l) 

runShows :: ShowS -> IO ()
runShows = putStrLn . ($ "")
