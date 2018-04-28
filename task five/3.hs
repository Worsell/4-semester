import Control.Monad

f [] = Nothing
f (x:y:z:xs) = mplus (if ((x<y) &&(y>z))then Just y else Nothing) (f (y:z:xs))

--  ��� ���������� ��-�� ����������� mplus ��� maby ������� ����� http://www.haskell.ru/monad.html
--  instance  MonadPlus Maybe  where
--    mzero                 = Nothing
--    Nothing `mplus` ys    = ys
--    xs      `mplus` ys    = xs
-- ������ � �� ����� �������, ��� �� ������ � ����.