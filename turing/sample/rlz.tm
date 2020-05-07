# Remove leading zeroes from a binary number.
q0
(q0, 0) -> (q0, $, >)
(q0, 1) -> (qhalt, 1, _)
(q0, $) -> (qhalt, 0, _)