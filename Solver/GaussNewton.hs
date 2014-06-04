module GaussNewton where

import Space.Class


gaussNewton f mean var ek eps xlast = x
    where
        jac = (f (x |+| eps *^ ek) |-| mean ) - (f (x |+|(-eps)*^ ek) |-| mean )
        jacobian = (1/(2*eps)) *^ jac
        x = xlast |+|  (- jac)

