

-- State [(Bs,Lisp)]



--input = BS.getLine >>= (\x -> return $ parse (manyL (itemL) <**> itemL) x)

--inpu :: [(BS.ByteString,)]

--ff :: Parser [BS.ByteString]
--ff =  many1 $ stringL $ BC.pack "abcd"

--inp' = BS.getLine >>= (\x -> return $ parse record x)

--inp'' = BS.getLine >>= (\x -> return $ parse itemN x)

--inp' = BS.getLine >>= (\x -> return $ parse rakudaL x)

--inp = BS.getLine >>= (\x -> return $ parse expr x)
  
--inp = BS.getLine >>= (\x -> return $ parse (temp4 $ BC.pack "\\n" )  x)

--inp' = BS.getLine >>= (\x -> return $ parse (satisfys 3) x)
  
--inpp = BS.getContents >>= (\x -> return $ parse (many item) x)

--

readd =
  --item >== (\x -> "\\" :: Bs)
           
  --many1
  -- (string $ BS.pack [92,110]) >==
  -- (\x -> r' x) <|> item <**> readd
  
  item >==
  (\x -> case BS.head x of
      92 -> item >==
        (\y -> case BS.head y of
            110 -> r' $ BS.append x y
            _   -> readd
        )
      _  -> readd
  )
  
--not' :: Parser a -> Parser a
not' a = a >== (\x -> r'  x) <|> not' a

temp4 s =
  (satisfy ( h /= ) ) >==
  (\_ -> (satisfy (t /=) ) >==
         (\_ -> (r' s))
  )
  
  where h = BS.singleton $ BS.head s
        t = BS.tail s
        
  
temp5 = satisfyNot ( "\\" == ) <**> satisfyNot ( "n" == )

--temp3 = satisfy ( "\\" /= ) <**> satisfy ( "n" /= )


nono = not' $ string (BC.pack "\\n")
  
fileIn =
  openFile "ex1.txt" ReadMode
  >>=
  (\h ->
     BS.hGetContents h >>=
--     (\x -> return $ parse getALine x )
     (\x -> return $ parse getALine x )
     
--     (\x -> return $ parse getALine x )
  )
     --(string "\n")
     --(\x -> return $ parse item x) --(char ("\n" :: Bs)) x) --(string "\n")
  
fileOut =
  
  openFile "ex2.txt" AppendMode >>=
  --(\h -> return $ (hPutStrLn h) $ "contents" :: [Char] )
  (\h -> hPutStrLn h ("abcdefg" :: [Char]) >>=
    (\_  -> hClose h >>=
      (\c -> return c)
    )
  )
  
  
copy =
  openFile "ex2.txt" ReadWriteMode >>=
  (\h1 ->
     hGetContents h1 >>=
     (\x1 ->
        let x = hPutStr h1 "hello"
            
        in hClose h1 >>=
       (\y -> return y)
     )
     -- openFile "ex2.txt" WriteMode >>=
     -- (\h2 -> return h2)
  )

fg = myInteract ( BS.pack . BS.unpack )

sss fname =
  openFile fname ReadMode >>=
  (\x -> return x)
  
gh = fmap (\fn -> openFile fn ReadMode >>= (\x -> f2 x) ) ["ex2.txt","ex1,txt"]
     
f2 h = hGetContents h >>= (\x -> return x)
  
ss =
  fmap sss ["ex2.txt","ex1.txt"]
  >>= (\ioh ->
         let x = zz ioh
         in return x ) -- >>= (\h -> return [h]))
  
  -- >>= (\ioh -> ioh >>= (\h -> return [h]))
  
zz ioh = ioh >>= (\x -> hGetContents x >>= (\c -> return c))

  
myInteract f =
  BS.getContents >>= (\x -> BS.putStr (f x))
  
  
  -- do
  --   s <- getContents
  --   putStr (f s)
    
  
readFiles :: [FilePath] -> IO BS.ByteString
readFiles = fmap BS.concat . mapM BS.readFile
  
co = do
  al <- readFiles ["ex2.txt","ex2.txt"]
  print $ foldl (\x y -> x + 1) 0 (BC.words al)
  
  --a <- fmap (\x -> openFile x ReadMode ) ["ex2,txt"]
  --return $ putStrLn "aaa"
  
  --hClose (head a)
  --[ _ | h  <- openFile "ex2.txt" ReadMode ]
  -- , c <- hClose h ]
  
  -- openFile "ex2.txt" AppendMode >>=
  -- (\h -> hPutStrLn h ("abcdefg" :: [Char]) >>=
  --   (\_  -> hClose h >>=
  --     (\c -> return c)
  --   )
  -- )
  
  --
  --(\h -> mapM_ (hPutStrLn h) $ "contents" :: String  >> return $ hClose h)
  
  --(\h -> return $ mapM_ (hPutStrLn h) $ "contents" :: String >>= (\_ -> hClose h) )

  --hClose handle
  
  
fileIn' =
  openFile "ex1.txt" ReadMode
  >>= (\h -> readL h)
  
  
readL h =
  hIsEOF h >>=
  (\a -> case a of
      True -> return []
      False ->
        BS.hGetLine h >>=
        (\x -> let d = myF x 
               in readL h >>=
                  (\y -> return $ [d] ++ y )
          --(\y -> return $ parse item x  )
        )
  )
  
  
myF x = BS.unpack x

  
--inpu = BS.getLine >>= (\x -> return $ (parse (itemL <**> (string $ BC.pack "abc"))) x)
--inpu = BS.getLine >>= (\x -> return $ parse (strings (BC.pack "abc")) x)





