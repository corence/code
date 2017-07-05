fac n = do {
    a <- auto 1;
    i <- auto n;
    while (i >. 0) $ do {
        a *= i;
        i -= 1;
    };
    a;
  }
