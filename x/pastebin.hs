trim :: String -> String
trim ln0 =
        case reverse ln of
          c:r_cs | isSpace c -> reverse $ dropWhile isSpace r_cs
          _                  -> ln
      where
        ln = dropWhile isSpace ln0
