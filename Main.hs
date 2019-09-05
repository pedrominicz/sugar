import NanoParsec

main :: IO ()
main = do
  putStrLn $ "Expect 'a': " ++ (show $ runParser item "a")
  putStrLn $ "Exception!\n" ++ (show $ runParser item "ab")
