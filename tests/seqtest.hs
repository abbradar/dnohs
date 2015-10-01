data List a = Cons a (List a) | Nil

test (Cons a (Cons b Nil)) = b
-- test (Cons a (Cons b Nil)) = seq a b

-- main = test (Cons 4 (Cons 5 Nil))
main = test (Cons undefined (Cons 5 Nil))
