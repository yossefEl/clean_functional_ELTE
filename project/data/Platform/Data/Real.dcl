definition module Data.Real

/**
 * @property-bootstrap
 *   import StdEnv
 */

from System._Architecture import IF_INTEL
from StdInt import IF_INT_64_OR_32

/**
 * Compile-time check whether the x86 extended precision format is used for
 * intermediate values.
 */
IF_INTERMEDIATE_80BIT_REAL_PRECISION yes no :== IF_INTEL (IF_INT_64_OR_32 no yes) no

LargestReal    :== 1.7976931348623157E+308
/**
 * Smallest normalized 64 bit real > 0.0
 */
LowestReal     :== 2.2250738585072014E-308
Epsilon        :== 2.220446049250313E-16
Epsilon80      :== 1.08420217248550443E-19

/**
 * Compare two numbers with a given epsilon (error margin).
 *
 * The value of epsilon depends on the application.
 *
 * With a sensible epsilon (non-NaN and non-Infinity), Infinity values are only
 * approximately equal to the same infinity value, and NaN values are not
 * approximately equal to any value (including NaN).
 *
 * @param epsilon
 * @param first number
 * @param second number
 * @result equality
 *
 * @property symmetry: A.e :: Real; a :: Real; b :: Real:
 *   approximatelyEqual e a b =.= approximatelyEqual e b a
 * @property non-NaN values are equal to themselves: A.e :: Real; a :: Real:
 *   not (isNaN a) ==> approximatelyEqual e a a
 * @property NaN is not equal to anything: A.e :: Real; a :: Real:
 *   not (approximatelyEqual e a NaN)
 */
approximatelyEqual :: !Real !Real !Real -> Bool

/**
 * Pretty print real with given number of decimals
 * @param The number of decimals
 * @param The real to be printed
 */
printRealWithDecimals :: !Int !Real -> String
