definition module Data.Maybe.Ord

/**
 * This contains a possible `<` and with this an `Ord` instance of the `?`
 * type. As there is no natural order and therefore an arbitrary choice is
 * made, the instances is provided in a separate module. This makes is easier
 * to use another ordering where needed.
 */

from StdOverloaded import class <

instance < (?a) | < a
