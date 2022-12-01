definition module Gast.Testable

/*
	GAST: A Generic Automatic Software Test-system

	testable: the test algorithm for logical properties

	Pieter Koopman, 2002-2012
	Radboud Universty, Nijmegen
	The Netherlands
	pieter@cs.ru.nl
*/

from Data.Map import :: Map
import Gast.GenLibTest
from Gast.StdProperty import ::Property // for instance of testable
from Gast.Gen import
	generic ggen, :: GenState, :: TypeName, :: RecFieldName, :: RandomStream
from Testing.TestEvents import :: TestLocation, :: CounterExample, :: FailedAssertion
from Text.GenPrint import class PrintOutput, :: PrintState, generic gPrint

//--- basics --//

:: Admin =
	{ labels                :: ![String]
	, args                  :: ![String]
	, argsRepresentation    :: ![String]
	, namePath              :: ![String]
	, res                   :: !Result
	, failedAssertions      :: ![(FailedAssertion, String, String)] //* Failed assertion & string representation of args
	, recFieldValueNrLimits :: !Map (TypeName, RecFieldName) Int    //* Restricts the number of values generated for record fields
	}

:: Result
	= Undef       //* An undefined result
	| Rej         //* The test case was rejected
	| Pass        //* The test passed, but prefer subsequent terms of \/ clauses
	| OK          //* The test passed; stop evaluating
	| CE [String] //* Counter-example; the strings are possible reasons

newAdmin :: Admin

derive gLess Result
instance == Result

//* Property which is tested through generic test generation (see ggen).
:: Property = Prop String (?TestLocation) (GenState Admin -> [!Admin])

//* Convert a Testable to a Property.
prop :: a -> Property | Testable a

class TestArg a | genShow{|*|}, ggen{|*|}, gPrint{|*|} a

class Testable a
where
	evaluate :: !a GenState !Admin -> [!Admin]
	testname :: a -> String

	testlocation :: a -> ?TestLocation
	testlocation _ = ?None

instance Testable Bool
instance Testable Result
instance Testable Property
/**
 * It is possible to test functions including a single argument.
 * Gast then generates arguments which are used to test the function
 * when applied to the generated argument. This requires implementing
 * ggen for the argument type so Gast knows how to generate tests for the type.
 */
instance Testable (a->b) | Testable b & genShow{|*|} a & ggen{|*|} a & TestArg a
instance Testable [a] | Testable a

MaxExists	:== 1000

//--- for implementation of properties ---//

diagonal :: ![[!a]] -> [!a]
forAll :: !(a->b) ![!a] GenState !Admin -> [!Admin] | Testable b & TestArg a

generateAll :: !GenState -> [!a] | ggen{|*|} a

//--- testing --//

:: Testoption
	= Tests           !Int // How many tests to perform
	| Fails           !Int // Maximum amount of errors to print
	| Args            !Int // Maximum amount of arguments to generate
	| RandomSeed      !Int
	| RandomList      ![Int]
	| Skew            !Int // Use skew test generation
	| Bent // Use Bent test generation.
	| MaxDepth        !Int // Maximum test depth (upper bound on number of tests to perform).
	| MaxStringLength !Int // Maximum length of generated strings.
	| ArgTypes        ![GenType]

/**
 * The combined results of all tests for a single property.
 * This is in contrast to {{Result}} which represents the result of a single test.
 */
:: TestsResult = { maxTests   :: !Int        //* Maximum number of tests
                 , nRej       :: !Int        //* Rejected test arguments
                 , resultType :: !ResultType //* Type of the result
                 }
/**
 * The type of the combined result, together with information
 * specific to that type of result.
 */
:: ResultType = Proof        !Int                 //* Proof by exhaustive testing: nTests
              | PassedTest   !Int !Int !Int !Bool //* Passed test: maxArgs, nTests, nUnd, all possible args generated?
              | CounterExpls !Int !Int !Int       //* Counterexamples found: nTests nUnd nCounterExamples
              | Undefined    !Int                 //* Undefined result: nUnd
              | NoTests      !Int !Int !Int       //* No tests performed: maxArgs nTests nUnd

/**
 * A counter example.
 */
:: CounterExampleRes =
	{ maxTests            :: !Int      //* Maximal number of tests for run in which counter example is found
	, nTests              :: !Int      //* maxTests MINUS number of test at which counter example is found
	, nE                  :: !Int      //* Number of counter examples
	, args                :: ![String] //* Arguments used for test (string representation)
	, argsRepresentation  :: ![String] //* Arguments used for test ({{`gPrint`}} encoding)
	, name                :: !String   //* Name of property
	, failedAssertions    :: ![(FailedAssertion, String, String)] //* Failed assertions leading to counter example & string representation of arguments
	, possibleFailReasons :: ![String] //* Human-readable reasons why the test may have failed
	}

:: GastEvent
	= GE_TestStarted !(?TestLocation) !String
	| GE_TestFinished !(?TestLocation) !String !TestsResult ![CounterExampleRes] ![(String,Int)]
	| GE_CounterExample !CounterExampleRes
	| GE_Tick !Int !Admin

:: PrintOption
	= Verbose //* Use verbosePrintConfig to print the test output.
	| Trace //* Use tracePrintConfig to print the test output.
	| Concise Int //* The Int tells how often a test count should be displayed
	| Quiet //* use quietPrintConfig to print the test output.
	| OutputTestEvents //* output test results as event specified in clean-platform {{Testing.TestEvents}}

:: PrintConfig =
	{ everyOutput          :: Int Admin -> String
		//* Function returning output to print for every test, Int is test number.
	, counterExampleOutput :: CounterExampleRes -> String
		//* Function returning output to print which Gast finds a CE.
	, beforeStartOutput    :: (?TestLocation) String -> String
		//* Function returning output to print before the test output.
	, resultOutput         :: (?TestLocation) String TestsResult [CounterExampleRes] [(String, Int)] -> String
		//* Function which returns output given the test results.
	}

/**
 * Prints a list of GastEvents obtained using Test or TestList using the given PrintConfig.
 *
 * @param The printing configuration that specifies how to transform the GastEvents to output
 * @param The list of GastEvents obtained using Test or TestList
 * @result The test output
 */
printEvents :: !PrintConfig ![!GastEvent] -> [!String]
/**
 * Customize a printConfig by applying a list of PrintOptions to verbosePrintConfig.
 *
 * @param A lits of PrintOptions to apply to verbosePrintConfig
 * @result The resulting PrintConfig
 */
toPrintConfig :: ([PrintOption] -> PrintConfig)

/**
 *  Tests a Testable applying the given TestOptions to the defaultTestConfig.
 *
 *  @param The testoptions to apply to defaultTestConfig
 *  @param The Testable which should be tested
 *  @param A list of GastEvents which can be converted to output using printEvents
 */
Test     :: ![Testoption] !p   -> [!GastEvent] | Testable p
/**
 *  Tests a list of Testables applying the given TestOptions to the defaultTestConfig.
 *
 *  @param The testoptions to apply to defaultTestConfig
 *  @param The list of Testables which should be tested
 *  @param A list of GastEvents which can be converted to output using printEvents
 */
TestList :: ![Testoption] ![p] -> [!GastEvent] | Testable p

/**
 * Test a given Testable 1000 times returning verbose test output.
 *
 * @param A random stream of Ints (e.g aStream)
 * @param The Testable which should be tested
 * @result The test output
 */
verbose     ::      !RandomStream !p -> [!String] | Testable p
/**
 * Test a given Testable n times returning verbose test output.
 *
 * @param The number of times to test the Testable
 * @param A random stream of Ints (e.g aStream)
 * @param The Testable which should be tested
 * @result The test output
 */
verbosen    :: !Int !RandomStream !p -> [!String] | Testable p
/**
 * Test a given Testable 1000 times returning consise test output.
 *
 * @param The number of times to test the Testable
 * @param A random stream of Ints (e.g aStream)
 * @param The Testable which should be tested
 * @result The test output
 */
concise     ::      !RandomStream !p -> [!String] | Testable p
/**
 * Test a given Testable n times returning consise test output.
 *
 * @param The number of times to test the Testable
 * @param A random stream of Ints (e.g aStream)
 * @param The Testable which should be tested
 * @result The test output
 */
concisen    :: !Int !RandomStream !p -> [!String] | Testable p
/**
 * Test a given Testable 1000 times returning quiet test output.
 * Output format (pass):
 * "<testname> passed after 1000 tests"
 * Output format (fail):
 * "<testname> Counterexample 1 <= i <= 1000 found after 1000 tests: <CE args>"
 * "<testname> n counterexample(s) found after 1000 tests"

 * @param A random stream of Ints (e.g aStream)
 * @param The Testable which should be tested
 * @result The test output
 */
quiet       ::      !RandomStream !p -> [!String] | Testable p
/**
 * Test a given Testable n times returning quiet test output.
 * Output format (pass):
 * "<testname> passed after n tests"
 * Output format (fail):
 * "<testname> Counterexample 1 <= i <= n found after n tests: <CE args>"
 * "<testname> m counterexample(s) found after n tests"

 * @param The number of times to test the Testable
 * @param A random stream of Ints (e.g aStream)
 * @param The Testable which should be tested
 * @result The test output
 */
quietn      :: !Int !RandomStream !p -> [!String] | Testable p
/**
 * Test a given Testable n times returning quiet test output.
 * Print a maximum of m errors.
 * Output format (pass):
 * "<testname> passed after n tests"
 * Output format (fail):
 * "<testname> Counterexample 1 <= i <= n found after n tests: <CE args>"
 * "<testname> m counterexample(s) found after n tests"
 *
 * @param The number of times to test the Testable
 * @param The maximum number of errors to print
 * @param A random stream of Ints (e.g aStream)
 * @param The Testable which should be tested
 * @result The test output
 */
quietnm     :: !Int !Int !RandomStream !p -> [!String] | Testable p
/**
 * Test a given Testable 1000 times returning output in JSON format.
 * Every test returns 2 json objects in the below format, in order:
 * 1: {"name" : propertyName, "event":"start", ?(location:<the location of the test>)}}
 * 2: { "name" : propertyName
 *    , "message" : "<(success|fail|skip)Message>"
 *    , "event" : "passed|failed|skipped"
 *    , ?("location":"<the location of the test>"")
 *    , ?("time" : "<the elapsed time since starting the test in MS>"
 *    , ?("failReason" : "<description of CE>")
 *    }
 *
 * @param A random stream of Ints (e.g aStream)
 * @param The Testable which should be tested
 * @result The test output
 */
testEvents  ::      !RandomStream !p -> [!String] | Testable p
/**
 * Test a given Testable n times returning output in JSON format.
 * Every test returns 2 json objects in the below format, in order:
 * 1: {"name" : propertyName, "event":"start", ?(location:<the location of the test>)}}
 * 2: { "name" : propertyName
 *    , "message" : "<(success|fail|skip)Message>"
 *    , "event" : "passed|failed|skipped"
 *    , ?("location":"<the location of the test>"")
 *    , ?("time" : "<the elapsed time since starting the test in MS>"
 *    , ?("failReason" : "<description of CE>")
 *    }
 *
 * @param The number of times to test the Testable
 * @param A random stream of Ints (e.g aStream)
 * @param The Testable which should be tested
 * @param The test output
 */
testEventsn :: !Int !RandomStream !p -> [!String] | Testable p

/**
 * Test a given Testable 1000 times returning verbose test output.
 *
 * @param The Testable which should be tested
 * @result The test output
 */
test :: !p -> [!String] | Testable p
/**
 * Test a given Testable n times returning verbose test output.
 *
 * @param The amount of times to test the Testable
 * @param The Testable which should be tested
 * @result The test output
 */
testn :: !Int !p -> [!String] | Testable p
/**
 * Test a given Testable n times returning verbose test output,
 * Print a maximum of m errors.
 *
 * @param The amount of times to test the Testable
 * @param The maximum amount of errors to print
 * @param The Testable which should be tested
 * @result The test output
 */
testnm :: !Int !Int !p -> [!String] | Testable p
