import System.Environment

main :: IO ()
main = getArgs >>= print . haqify . head

haqify :: String -> String
haqify s = "Haq!" ++ s

