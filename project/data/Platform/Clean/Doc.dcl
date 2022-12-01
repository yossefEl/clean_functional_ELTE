definition module Clean.Doc

/**
 * Parsing and storing Clean documentation
 */

import StdGeneric
from StdOverloaded import class toString

from Data.Either import :: Either
from Data.GenDefault import generic gDefault

from Clean.Types import :: Type

/**
 * A wrapper around the {{`String`}} type which makes sure that multi-line
 * documentation blocks get trimmed w.r.t. whitespace.
 */
:: MultiLineString =: MultiLine String

class docDescription            d :: !d -> ?Description
class docComplexity             d :: !d -> ?String
class docParams                 d :: !d -> [ParamDoc]
class docVars                   d :: !d -> [Description]
class docResults                d :: !d -> [Description]
class docType                   d :: !d -> ?Type
class docThrows                 d :: !d -> [Description]
class docMembers                d :: !d -> [?ClassMemberDoc]
class docFields                 d :: !d -> ?[?Description]
class docConstructors           d :: !d -> ?[?ConstructorDoc]
class docRepresentation         d :: !d -> ?(?Description)

class docPropertyBootstrap      d :: !d -> ?PropertyBootstrapDoc
class docPropertyTestWith       d :: !d -> [PropertyVarInstantiation]
class docPropertyTestGenerators d :: !d -> [PropertyTestGenerator]
class docProperties             d :: !d -> [Property]
class docPreconditions          d :: !d -> [String]

/**
 * Documentation of a Clean module.
 */
:: ModuleDoc =
	{ description        :: !?Description
	, property_bootstrap :: !?PropertyBootstrapDoc      //* For generating unit tests with clean-test-properties
	, property_test_with :: ![PropertyVarInstantiation] //* With which types to test the properties
	, property_test_generators :: ![PropertyTestGenerator]
	  //* Functions to generate values of types for which Gast's {{`ggen`}} is not good enough, like {{`Map`}}
	}

instance docDescription ModuleDoc
instance docPropertyBootstrap ModuleDoc
instance docPropertyTestWith ModuleDoc
instance docPropertyTestGenerators ModuleDoc
derive gDefault ModuleDoc, PropertyBootstrapDoc

//* Belongs to `property_bootstrap` in `ModuleDoc`.
:: PropertyBootstrapDoc =
	{ bootstrap_content                 :: !MultiLineString
	, bootstrap_without_default_imports :: !Bool
		//* Don't generate a default set of imports (e.g. to avoid name clashes with Gast)
	}

/**
 * Documentation of a Clean function.
 */
:: FunctionDoc =
	{ description        :: !?Description
	, complexity         :: !?String       //* E.g. "O(n log n)"
	, params             :: ![ParamDoc]    //* Descriptions of the parameters
	, vars               :: ![Description] //* Descriptions of the type variables (for generics)
	, results            :: ![Description] //* Descriptions of the result(s, for tuples)
	, type               :: !?Type         //* The type (for macros)
	, throws             :: ![Description] //* The exceptions it may throw (iTasks)
	, properties         :: ![Property]    //* Properties of this function
	, property_test_with :: ![PropertyVarInstantiation] //* With which types to test the properties
	, preconditions      :: ![String]      //* Preconditions for the properties
	}

instance docDescription FunctionDoc
instance docComplexity FunctionDoc
instance docParams FunctionDoc
instance docVars FunctionDoc
instance docResults FunctionDoc
instance docType FunctionDoc
instance docThrows FunctionDoc
instance docProperties FunctionDoc
instance docPropertyTestWith FunctionDoc
instance docPreconditions FunctionDoc

/**
 * Documentation of a class instance.
 */
:: InstanceDoc =
	{ description        :: !?Description
	, complexity         :: !?String    //* E.g. "O(n log n)"
	, properties         :: ![Property] //* Properties of this instance
	, property_test_with :: ![PropertyVarInstantiation] //* With which types to test the properties
	, preconditions      :: ![String]   //* Preconditions for the properties
	}

instance docDescription InstanceDoc
instance docComplexity InstanceDoc
instance docProperties InstanceDoc
instance docPropertyTestWith InstanceDoc
instance docPreconditions InstanceDoc

/**
 * Documentation of a function parameter.
 */
:: ParamDoc =
	{ name        :: !?String      //* An optional name for the parameter
	, description :: !?Description //* An optional description
	}

instance toString ParamDoc
instance docDescription ParamDoc

/**
 * A property of a function.
 * Typically, the property can be tested with Gast.
 *
 * - `ForAll`: the right-hand side (the third argument) holds for all values of
 *   the arguments (the second argument). The first argument is the name.
 */
:: Property
	= ForAll !String ![(String,Type)] !String

/**
 * When a property type contains type variables, a `PropertyVarInstantiation`
 * can be used to instantiate those variables when generating test cases.
 */
:: PropertyVarInstantiation = PropertyVarInstantiation !(!String, !Type)

/**
 * A test generator generates values of some type. There are different ways to
 * write test generators:
 *
 * - `PTG_Function`: The first argument is the function type of the generator,
 *   for instance `[(k,v)] -> {{Map}} k v`. This receives arguments of type
 *   `[(k,v)]` and transforms them into generated values of type `Map k v`. The
 *   second argument is the Clean implementation, which should assume the
 *   generator is called `gen` (e.g.: `gen elems = ...`).
 * - `PTG_List`: The first argument is the type for which values are to be
 *   generated (e.g.: `Int`). The second argument is a Clean expression for a
 *   list of that type (e.g.: `[0..10]`).
 */
:: PropertyTestGenerator
	= PTG_Function !Type !String
	| PTG_List !Type !String

derive gDefault FunctionDoc, InstanceDoc, Property, PropertyVarInstantiation, PropertyTestGenerator

/**
 * Documentation of a Clean class member.
 * For an explanation of the fields, see the documentation on {{`FunctionDoc`}}.
 */
:: ClassMemberDoc =
	{ description :: !?Description
	, complexity  :: !?String
	, params      :: ![ParamDoc]
	, results     :: ![Description]
	, type        :: !?Type
	, throws      :: ![Description]
	}

instance docDescription ClassMemberDoc
instance docComplexity ClassMemberDoc
instance docParams ClassMemberDoc
instance docResults ClassMemberDoc
instance docType ClassMemberDoc
instance docThrows ClassMemberDoc
derive gDefault ClassMemberDoc

/**
 * Documentation of a Clean ADT constructor.
 * For an explanation of the fields, see the documentation on {{`FunctionDoc`}}.
 */
:: ConstructorDoc =
	{ description :: !?Description
	, params      :: ![ParamDoc]
	}

instance docDescription ConstructorDoc
instance docParams ConstructorDoc
derive gDefault ConstructorDoc

/**
 * Documentation of a Clean class.
 */
:: ClassDoc =
	{ description :: !?Description
	, vars        :: ![Description]     //* The type variables
	, members     :: ![?ClassMemberDoc] //* Documentation on the members
	}

instance docDescription ClassDoc
instance docVars ClassDoc
instance docMembers ClassDoc
derive gDefault ClassDoc

/**
 * Documentation of a Clean type.
 */
:: TypeDoc =
	{ description    :: !?Description
	, vars           :: ![Description]      //* Type variables
	, representation :: !?(?Description)    //* For synonym types
	, fields         :: !?[?Description]    //* For records
	, constructors   :: !?[?ConstructorDoc] //* For ADTs
	, invariants     :: ![Property]         //* For Gast test generation
	}

instance docDescription TypeDoc
instance docVars TypeDoc
instance docFields TypeDoc
instance docConstructors TypeDoc
instance docRepresentation TypeDoc
derive gDefault TypeDoc

/**
 * Description of a Clean syntax element
 */
:: Description :== String

/**
 * Parse error for parsing Clean documentation; no documentation could be found
 */
:: ParseError
	= MissingAsterisk !String //* At least one line did not start with a *
	| MissingField !String    //* A required field was missing
	| UnknownError !String    //* Another error
	| InternalNoDataError

/**
 * Parse warning while parsing Clean documentation; the parser has made a
 * best-effort result nevertheless
 */
:: ParseWarning
	= UnknownField !String   //* Unknown @-field
	| IllegalField !String   //* This @-field is not allowed in this docblock
	| NoDescription          //* The main description is missing
	| UsedReturn             //* Used @return instead of @result
	| UnparsableType !String //* Could not parse a @type field as a type

/**
 * Convert a ConstructorDoc to a FunctionDoc.
 */
constructorToFunctionDoc :: !ConstructorDoc -> FunctionDoc

/**
 * Convert a FunctionDoc to a ClassMemberDoc.
 */
functionToClassMemberDoc :: !FunctionDoc -> ClassMemberDoc

/**
 * Add a class member to an existing class definition
 *
 * @param The documentation to add the member to
 * @param The documentation on the class member
 * @result The new ClassDoc
 */
addClassMemberDoc :: !ClassDoc !(?ClassMemberDoc) -> ClassDoc

/**
 * Parse a single docstring, removing the asterisk and trimming whitespace.
 */
parseSingleLineDoc :: (String -> String)

/**
 * Parse a documentation block. The magic happens in {{`docBlockToDoc`}}.
 */
parseDoc :: !String -> Either ParseError (!d, ![ParseWarning]) | docBlockToDoc{|*|} d

/**
 * A documentation block.
 * @representation An order list of key-value pairs. A key can occur multiple
 *   times. The description has key `description`.
 */
:: DocBlock :== [(String, String)]

/**
 * The magic for {{`parseDoc`}}. Usually, a record type like {{`FunctionDoc`}}
 * will derive a convenient parser. In some cases, it may be necessary to
 * override the default, such as in the instance for {{`Type`}}, where parsing
 * of the type happens.
 * @var The thing to parse
 */
generic docBlockToDoc d :: !(Either [String] DocBlock) -> Either ParseError (!d, ![ParseWarning])

derive docBlockToDoc UNIT, PAIR, EITHER, CONS, OBJECT, FIELD of {gfd_name}, RECORD
derive docBlockToDoc String, [], ?, Type
derive docBlockToDoc ModuleDoc, FunctionDoc, InstanceDoc, ClassMemberDoc,
	ClassDoc, ConstructorDoc, TypeDoc

/**
 * Print a documentation block as a string. The magic happens in
 * {{`docToDocBlock`}}.
 */
printDoc :: !d -> String | docToDocBlock{|*|} d

/**
 * The magic for {{`printDoc`}}.
 */
generic docToDocBlock d :: !d -> Either [String] DocBlock

derive docToDocBlock ModuleDoc, FunctionDoc, InstanceDoc, ClassMemberDoc,
	ClassDoc, ConstructorDoc, TypeDoc

/**
 * Trace a list of ParseWarnings like StdDebug might do it
 */
traceParseWarnings :: ![ParseWarning] !a -> a

/**
 * Trace a ParseError like StdDebug might do it
 */
traceParseError :: !ParseError !a -> a
