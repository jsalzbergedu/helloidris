module Main

-- <Xal> if you run `ldd` on the output executable it'll tell you what libraries
--       are needed at runtime  [20:28]
-- <Xal> it's probably okay for libc to be needed, but I'll try getting gmp to
--       link statically.  Give me a second to try it  [20:29]
-- <Xal> aha!  [20:30]
-- <Xal> passing --cg-opt '-static' should work


interface Doot a where
  total doot : a -> String

Doot String where
  doot a = id a

total cond : (List (Bool, Lazy a), Lazy a) -> a
cond (lst, other) = let options = (filter fst lst) in
  case (head' options) of
    Just (_, a) => a
    Nothing => other

dootdoot : (List a, a) -> List a
dootdoot (dt, dtdt) = dt

main : IO ()
main = do putStrLn (show (cond ([(False, 10), (False, 20), (False, 30)], 40)))
