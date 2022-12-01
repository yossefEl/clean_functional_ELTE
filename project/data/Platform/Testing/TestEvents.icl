implementation module Testing.TestEvents

import StdEnv
import Text.GenJSON, Control.Monad, Data.Maybe, Control.Applicative, Data.Func
import Data.Functor
import Data.List

emptyEndEvent :: EndEvent
emptyEndEvent =
	{ name     = ""
	, location = ?None
	, event    = Skipped
	, message  = ""
	, time     = ?None
	}

JSONEncode{|TestEvent|} c (StartEvent se) = JSONEncode{|*|} c se
JSONEncode{|TestEvent|} c (EndEvent ee)   = JSONEncode{|*|} c ee

JSONDecode{|TestEvent|} b json = case JSONDecode{|*|} b json of
	(?Just se, json) -> (?Just (StartEvent se), json)
	_                -> case JSONDecode{|*|} b json of
		(?Just ee, json) -> (?Just (EndEvent ee), json)
		(?None,    json) -> (?None, json)

JSONEncode{|StartEvent|} _ startEvent = [ JSONObject
	[ ("name",  JSONString startEvent.StartEvent.name)
	, ("event", JSONString "start")
	: case startEvent.StartEvent.location of
		?None   -> []
		?Just l -> case JSONEncode{|*|} True l of
			[json] -> [("location",json)]
			_      -> abort "error in JSONEncode_StartEvent\n"
	]]

JSONDecode{|StartEvent|} _ [JSONObject objFields : rest] = (mbEvent, rest)
where
	mbEvent :: ?StartEvent
	mbEvent =
		getField "name"  >>= \name  ->
		getField "event" >>= \event ->
		if (event == "start")
			(pure {StartEvent | name = name, location = getField "location"})
			mzero

	getField :: String -> ?a | JSONDecode{|*|} a
	getField field = lookup field objFields >>= fromJSON
JSONDecode{|StartEvent|} _ _ = (?None, [])

JSONEncode{|EndEvent|} _ endEvent = [JSONObject
	[ ("name", JSONString endEvent.EndEvent.name)
	, ("message", JSONString endEvent.message)
	, ("event", JSONString (typeToString endEvent.event))
	: case endEvent.EndEvent.location of
		?None   -> []
		?Just l -> case JSONEncode{|*|} True l of
			[json] -> [("location",json)]
			_      -> abort "error in JSONEncode_EndEvent\n"
	++ case endEvent.time of
		?None   -> []
		?Just t -> case JSONEncode{|*|} True t of
			[json] -> [("time",json)]
			_      -> abort "error in JSONEncode_EndEvent\n"
	++ case endEvent.event of
		Failed (?Just r) -> [("failReason", case JSONEncode{|*|} False r of
			[JSONArray r] -> JSONArray r
			r             -> JSONArray r)]
		_        -> []
	]]
where
	typeToString :: EndEventType -> String
	typeToString Passed     = "passed"
	typeToString (Failed r) = "failed"
	typeToString Skipped    = "skipped"

JSONDecode{|EndEvent|} _ [JSONObject fields:rest] = (mbEvent, rest)
where
	mbEvent :: ?EndEvent
	mbEvent =
		getField "name" >>= \name ->
		getField "event" >>= \event ->
		getField "message" >>= \message ->
		let e = {name=name, message=message, event=Passed, location=getField "location", time=getField "time"} in case event of
			"passed"  -> pure e
			"failed"  -> pure {e & event = Failed $ getField "failReason"}
			"skipped" -> pure {e & event=Skipped}
            _         -> ?None

	getField :: String -> ?a | JSONDecode{|*|} a
	getField field = lookup field fields >>= fromJSON
JSONDecode{|EndEvent|} _ _ = (?None, [])

instance toString Expression
where
	toString (JSON json) = toString json
	toString (GPrint s)  = s

JSONEncode{|Expression|} _ e = case e of
	JSON json -> [JSONArray [JSONString "JSON",json]]
	GPrint e  -> [JSONArray [JSONString "GPrint",JSONString e]]

JSONDecode{|Expression|} _ [JSONArray [JSONString type,json]:rest] = case type of
	"JSON"   -> (?Just (JSON json),rest)
	"GPrint" -> case json of
		JSONString e -> (?Just (GPrint e),rest)
		_            -> (?None, [])
	_ -> (?None, [])
JSONDecode{|Expression|} _ _ = (?None, [])

JSONEncode{|FailedAssertion|} _ fa = [JSONArray arr]
where
	arr = case fa of
		ExpectedRelation x r y ->
			[ JSONString "expected"
			, hd (JSONEncode{|*|} False x)
			, hd (JSONEncode{|*|} False r)
			, hd (JSONEncode{|*|} False y)
			]

JSONDecode{|FailedAssertion|} _ [JSONArray arr:rest] = (mbFA, rest)
where
	mbFA = case arr of
		[JSONString "expected":x:r:y:[]] -> case JSONDecode{|*|} False [r] of
			(?Just r, []) -> case JSONDecode{|*|} False [x] of
				(?Just x, []) -> case JSONDecode{|*|} False [y] of
					(?Just y, []) -> ?Just (ExpectedRelation x r y)
					_ -> ?None
				_ -> ?None
			_ -> ?None
		_ -> ?None
JSONDecode{|FailedAssertion|} _ _ = (?None, [])

instance toString Relation
where
	toString r = case r of
		Eq      -> "=="
		Ne      -> "<>"
		Lt      -> "<"
		Le      -> "<="
		Gt      -> ">"
		Ge      -> ">="
		Other f -> f

JSONEncode{|Relation|} _ r = [JSONString (toString r)]

JSONDecode{|Relation|} _ [JSONString s:rest] = (mbRel, rest)
where
	mbRel = case s of
		"==" -> ?Just Eq
		"<>" -> ?Just Ne
		"<"  -> ?Just Lt
		"<=" -> ?Just Le
		">"  -> ?Just Gt
		">=" -> ?Just Ge
		f    -> ?Just (Other f)
JSONDecode{|Relation|} _ _ = (?None, [])

derive JSONEncode TestLocation, FailReason, CounterExample
derive JSONDecode TestLocation, FailReason, CounterExample
