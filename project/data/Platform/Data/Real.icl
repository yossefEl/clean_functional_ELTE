implementation module Data.Real

import Data.Func
import StdEnv
import Text

// https://floating-point-gui.de/errors/comparison/
approximatelyEqual :: !Real !Real !Real -> Bool
approximatelyEqual epsilon a b
	| a == b
		= True
	| a == 0.0 || b == 0.0 || abs a + abs b < LowestReal
		= abs (a - b) < epsilon*LowestReal
	| otherwise
		= abs (a - b) / min (abs a + abs b) LargestReal < epsilon

printRealWithDecimals :: !Int !Real -> String
printRealWithDecimals decimals r
	// First we move the desired decimals left of the .
	# shifted = r * 10.0 ^ (toReal decimals)
	// Call toInt to round our real (rounds to nearest integer) and convert it to a string
	# shifted_string = toString $ toInt shifted
	// We might have lost leading zeros during our conversion to string. In particular, any string with leading zeros
	// will lose those leading zeros. This can lead to ".4" where "0.004" might have been the desired output.
	// To account for this, we add zeros to the beginning of the string to reach the desired length. repeatn will
	// return [] when our number is negative.
	# shifted_string = (toString $ repeatn (decimals + 1 - size shifted_string) '0') +++ shifted_string
	// Take the portion before and after the decimal, they are finally concatenated
	# x = shifted_string % (0, size shifted_string - decimals - 1)
	# y = shifted_string % (size shifted_string - decimals, size shifted_string - 1)
	= concat3 x "." y
