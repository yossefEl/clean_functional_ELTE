definition module Gast

/*
	GAST: A Generic Automatic Software Test-system

	Pieter Koopman, 2004-2008
	Radboud Universty, Nijmegen
	The Netherlands
	pieter@cs.ru.nl
*/

from Data.Set import :: Set
import Math.Random, Data.GenEq
import Gast.GenLibTest, Gast.Testable, Gast.StdProperty, Gast.ConfSM
from Gast.Gen import
	generic ggen, :: GenState {..}, genState, aStream, ggenString

class Gast a | ggen{|*|}, genShow{|*|}, gPrint{|*|} a
