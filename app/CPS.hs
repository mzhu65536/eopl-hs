module CPS where

main1 :: IO ()
main1 = do putStrLn "Computing fib: n = "
           line <- getLine
           ans <- fibK (read line) endKont
           putStrLn $ "Answer = " ++ show ans

fact :: Integer -> Integer
fact n | n <= 0 = 1
       | otherwise = n * fact (n - 1)

-- Version One CPS in Datatype
data Con = KEnd
         | KFact Con Integer

applyCont :: Con -> Integer -> IO Integer
applyCont KEnd v = do putStrLn $ "End of Computation: " ++ show v
                      return v
applyCont (KFact k i) v = do putStrLn $ "Computing Cont: i = " ++ show i
                             applyCont k (i * v)
          
factK :: Integer -> Con -> IO Integer
factK n k | n <= 0 = applyCont k 1
          | otherwise = do putStrLn $ "Generate Cont with n = " ++ show n
                           factK (n - 1) (KFact k n)

-- Version Two CPS as Procedure

endKont :: Show a => a -> IO a
endKont v = do putStrLn $ "End of Computation: " ++ show v
               return v

-- Discussion: What's the type for factKont in Procedural Representation?
factKont :: Integer -> (Integer -> IO Integer) -> Integer -> IO Integer
factKont n k = \v -> do putStrLn $ "Computing Cont: i = " ++ show n
                        k (n * v) -- omit eta

type ConP = Integer -> IO Integer

factKProc :: Integer -> ConP -> IO Integer
factKProc n k | n <= 0 = do putStrLn "Converge"
                            k 1
              | otherwise = do putStrLn $ "Generate Cont with n = " ++ show n
                               factKProc (n - 1) $ factKont n k

-- Version 1 fib in Direct Style
fib :: Integer -> Integer
fib n | n < 2 = 1
      | otherwise = fib (n - 1) + fib (n - 2)

-- Version 2 fib in Procedural Style
-- fib n g = g (fib n)

fibK :: Integer -> (Integer -> IO Integer) -> IO Integer
fibK n k | n < 2 =  k 1
         | otherwise = fibK (n - 1)
                       (\v1 -> fibK (n - 2)
                               (\v2 -> k $ v1 + v2))

{-|
Exercise 6.1

In fact/k, pc will only change when touching the base case where passing 1 to
the continuation. After the computation enter apply-cont, it will not execute
fact-k any more.

-}

{-|
Exercise 6.2
W.T.S: fibK n g  == g (fib n)

Base case:
{-
fibK 1 g = g 1 
-}

Inductive Hypothesis:
{-
fibK n g = g (fib n)
-}

Inductive Step:
{-
fibK (n + 1) g = fibK n (\v1 -> fibK (n - 1) (\v2 -> g (v1 + v2)))
               = (\v1 -> fibK (n - 1) (\v2 -> g (v1 + v2))) (fib n)
               = fibK (n - 1) (\v2 -> g ((fib n) + v2))
               = (\v2 -> g ((fib n) + v2)) (fib (n - 1))
               = g ((fib n) + (fib (n - 1))) = g (fib (n + 1))
-}

-}

-- | Exercise 6.3
-- 1. (lambda (x y) (p (+ 8 x) (q y)))
-- \x y k -> (q y (\v1 -> p (+ 8 x) v1 k))
-- 2. (lambda (x y u v) (+ 1 (f (g x y) (+ u v))))
-- \x y u v k -> (g x y (\v1 -> f v1 (+ u v) (\v2 -> k (1 + v2))))
-- 3. (+ 1 (f (g x y) (+ u (h v))))
-- \k -> g x y (\vg -> h v (\vh -> f vg (u + vh) (\vf -> k (1 + vf))))
-- 4. (zero? (if a (p x) (p y)))
-- \k -> if a then p x (vp -> k (zero? vp)) else p y (vp -> k (zero? vp))
-- 5. (zero? (if (f a) (p x) (p y)))
-- \k -> f a
--      (\vf -> if vf then p x (vp -> k (zero? vp))
--                    else p y (vp -> k (zero? vp)))
-- 6. (let ((x (let ((y 8)) (p y)))) x)
-- \k -> let y = 8 in p y (\vp -> let x = vp in x)
-- 7. (let ((x (if a (p x) (p y))))x)
-- \k -> (if a then p x else p y) (\vp -> let x = vp in x)

-- | Exercise 6.4
type Cont a = a -> IO a

removeFirst :: [a] -> [a]
removeFirst [] = []
removeFirst (_ : xs) = xs

removeFirstK :: Show a => [a] -> Cont [a] -> IO [a]
removeFirstK [] k = k []
removeFirstK (_ : xs) k = k xs

endCont :: a -> IO a
endCont v = do putStrLn "End of Computation"
               return v

testRemoveFirstK = removeFirstK [1, 2, 3] endCont

-- Skip the rest

main :: IO ()
main = do ans <- testRemoveFirstK
          putStrLn $ "ans: " ++ show ans

-- | Exercise 6.5 Skipped

-- | Exercise 6.6
-- (lambda (x y) (+ (f (g x)) (h (j y))))
-- 1. vg -> vj -> vf -> vh
-- 2. vg -> vj -> vh -> vf
-- 3. vg -> vf -> vj -> vh
-- ... Skipped

-- | Exercise 6.7
-- skipped

-- | Exercise 6.8 Meaningless skipped


