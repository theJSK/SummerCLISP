; SLIME 2014-10-10
(format t "Enter two integers.  Press return after each integer.~%")
(setq dividend (read))
(setq divisor (read))
(format t "~D divided by ~D is~%" dividend divisor)
(setq quotient (floor dividend divisor))
(setq remainder (mod dividend divisor))
(format t "~D remainder ~D "quotient remainder)
