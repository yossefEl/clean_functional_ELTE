implementation module System.Time.Gast

import System.Time
from Gast import generic ggen, generic genShow, generic gPrint, :: GenState, :: PrintState, class PrintOutput
import Data.Int
import Gast.Gen
import StdEnum
import StdOverloadedList

ggen{|Timespec|} _ = [! {tv_sec=a, tv_nsec=b} \\ (a,b) <|- diagBent numbersSec numbersNsec]
where
	// Numbers 0.1000 are included to test the interval.
	// 1632845456 is the unix epoch time since writing the test.
	// 2263997456 is the unix epoch time since writing the test + 20 years.
	numbersSec = [! 0..1000] ++$ [! 1632845456..2263997456]
	// Numbers >= 1000000000 are not used for nsec as they may not be correctly handled.
	// The extreme values are generated first to make sure they are tested.
	numbersNsec = [! 0, 999999999] ++$ [! 1..999999998]

derive genShow Timespec
derive gPrint Timespec

