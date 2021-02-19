data Parity = Even | Odd

instance Parity Eq where
	(==) Even Even = True
    (==) Odd Odd = True

instance Parity Ord where
	(>) Even Odd = True