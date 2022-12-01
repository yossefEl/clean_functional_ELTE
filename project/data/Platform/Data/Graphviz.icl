// Peter Divianszky, 2007
// Code extended and adapted for using generics by Peter Achten, 2007, Pieter Koopman 2011, Jurriën Stutterheim 2013

implementation module Data.Graphviz

import StdEnv
import Data.Maybe, Data.List
import Text.GenPrint, Data.GenEq

derive gEq    EdgeStyle, NodeStyle, DirType, NodeShape, Side, ArrowShape, ArrowType, Arrow, Color
derive gPrint EdgeStyle, NodeStyle, DirType, NodeShape, Side, ArrowShape,
              ?, CompassPoint, StartStyle, ClusterMode, OutputMode,
              PageDir, RankDir, RankType
derive printNameValuePair GraphAttribute, NodeAttribute, EdgeAttribute

//  Almost regular toString instances:
instance toString EdgeStyle      where toString es  = quote (skipName "E" (printToString es))
instance toString NodeStyle      where toString ns  = quote (skipName "NStyle" (printToString ns))
instance toString DirType        where toString dir = quote (skipName "DT" (printToString dir))
instance toString NodeShape      where toString ns  = skipName "NShape" (printToString ns)
instance toString Side           where toString s   = skipName "Side" (printToString s)
instance toString ArrowShape     where toString s   = skipName "AShape" (printToString s)
instance toString CompassPoint   where toString cp  = quote (skipName "CP" (printToString cp))
instance toString ClusterMode    where toString cm  = quote (skipName "CM" (printToString cm))
instance toString OutputMode     where toString om  = quote (skipName "OM" (printToString om))
instance toString PageDir        where toString pd  = quote (skipNameUp "PD" (printToString pd))
instance toString RankDir        where toString rd  = quote (skipNameUp "RD" (printToString rd))
instance toString RankType       where toString rt  = quote (skipName "RT" (printToString rt))
instance toString StartStyle     where toString ss  = quote (skipName "SS" (printToString ss))
instance toString NodeAttribute  where toString na  = printNameValuePair{|*|} "NAtt" na
instance toString EdgeAttribute  where toString ea  = printNameValuePair{|*|} "EAtt" ea
instance toString GraphAttribute where toString ga  = printNameValuePair{|*|} "GAtt" ga
//  Less regular toString instances:
instance toString Arrow where
  toString {open,side,shape}  = if open "o" "" +++ if (isJust side) (toString (fromJust side)) "" +++ toString shape
instance toString ArrowType where
  toString {closest,furthest}  = quote (toString closest +++ if (isJust furthest) (toString (fromJust furthest)) "")
instance toString Color where
  toString (Color name)    = name

  toString (HSV h s v)    = "\"" $> toS h $> " " $> toS s $> " " $> toS v $> "\""
  where
    toS x
      | x<0.0 || x>1.0  = abort "HSV value out of range.\n" 
      | otherwise      = toString (toReal (toInt (1000.0*x)) / 1000.0)

  toString (RGB r g b)    = "\"#" $> toS r $> toS g $> toS b $> "\""
  where
    toS x 
      | x<0 || x>255    = abort "RGB value out of range.\n" 
      | otherwise      = toString [toC (x/16), toC (x rem 16)] 
    toC x 
      | x < 10      = toChar (x + fromChar '0')
      | otherwise      = toChar (x - 10 + fromChar 'A')
instance toString DotPoint where
  toString (DotPoint x y fix)  = x >$ "," >$ y >$ if fix "!" ""
instance toString LayerId where
  toString layerid      = case layerid of
                  LayerAll      = "all"
                  LayerNr   nr  = toString nr
                  LayerName str = str
instance toString LayerList where
  toString (LayerList names)  = foldr (\next before -> before $> layersep $> next) "" names
instance toString LayerRange where
  toString (LayerRange id ids)= foldr (\next before -> before $> layersep $> next) (toString id) ids
instance toString Margin where
  toString margin        = case margin of
                  SingleMargin a   = toString a
                  DoubleMargin a b = a >$ "," $> b
instance toString Pad where
  toString pad        = case pad of
                  SinglePad a   = toString a
                  DoublePad a b = a >$ "," $> b
instance toString Pointf where
  toString (Pointf x y)    = quote (x >$ "," $> y)
instance toString Ratio where
  toString ratio        = case ratio of
                  AspectRatio r = quote (toString r)
                  RFill        = quote "fill"
                  RCompress    = quote "compress"
                  RExpand      = quote "expand"
                  RAuto        = quote "auto"
instance toString Rect where
  toString {llx,lly,urx,ury}  = llx >$ "," $> lly >$ "," $> urx >$ "," $> ury
instance toString Sizef where      // PA++
  toString (Sizef x y True)  = "\"" +++ toString x +++ "," +++ toString y +++ "!\""
  toString (Sizef x y False)  = "\"" +++ toString x +++ "," +++ toString y +++ "\""
instance toString StartType where
  toString {startStyle,startSeed}
                = if (isJust startStyle) (toString (fromJust startStyle)) "" +++ 
                  if (isJust startSeed)  (toString (fromJust startSeed )) ""
instance toString ViewPort where
  toString {vpW,vpH,vpZ,vpXY}
                = (vpW >$ "," $> vpH)                          +++
                  if (isJust vpZ ) ("," $> (fromJust vpZ )) "" +++
                  if (isJust vpXY) ("," $> (fromJust vpXY)) ""

//  Print name=value pairs for algebraic data types with unary data constructors in XXX_name constructor name format.
generic printNameValuePair a :: !String !a -> String
printNameValuePair{|Int|}          pre x      = toString x
printNameValuePair{|Real|}         pre x      = toString x
printNameValuePair{|Char|}         pre x      = toString x
printNameValuePair{|String|}       pre x      = quote    x
printNameValuePair{|Bool|}         pre x      = firstCharLowerCase (toString x)
printNameValuePair{|UNIT|}         pre x      = ""
printNameValuePair{|PAIR|}   px py pre (PAIR x y)  = px pre x +++ " " +++ py "" y
printNameValuePair{|EITHER|} pl pr pre (LEFT   x)  = pl pre x
printNameValuePair{|EITHER|} pl pr pre (RIGHT  y)  = pr pre y
printNameValuePair{|OBJECT|} px    pre (OBJECT x)  = px pre x
printNameValuePair{|CONS of d|} px pre (CONS   x)  = skipName pre d.gcd_name +++ "=" +++ px "" x
// Specializations of printNameValuePair:
printNameValuePair{|ArrowType|}    pre x      = toString x
printNameValuePair{|Color|}        pre x      = toString x
printNameValuePair{|ClusterMode|}  pre x      = toString x
printNameValuePair{|CompassPoint|} pre x      = toString x
printNameValuePair{|DirType|}      pre x      = toString x
printNameValuePair{|DotPoint|}     pre x      = toString x
printNameValuePair{|EdgeStyle|}    pre x      = toString x
printNameValuePair{|LayerList|}    pre x      = toString x
printNameValuePair{|LayerRange|}   pre x      = toString x
printNameValuePair{|Margin|}       pre x      = toString x
printNameValuePair{|NodeShape|}    pre x      = toString x
printNameValuePair{|NodeStyle|}    pre x      = toString x
printNameValuePair{|OutputMode|}   pre x      = toString x
printNameValuePair{|Pad|}          pre x      = toString x
printNameValuePair{|PageDir|}      pre x      = toString x
printNameValuePair{|Pointf|}       pre x      = toString x
printNameValuePair{|RankDir|}      pre x      = toString x
printNameValuePair{|RankType|}     pre x      = toString x
printNameValuePair{|Ratio|}        pre x      = toString x
printNameValuePair{|Rect|}         pre x      = toString x
printNameValuePair{|Sizef|}        pre x      = toString x    // PA++
printNameValuePair{|StartType|}    pre x      = toString x
printNameValuePair{|ViewPort|}     pre x      = toString x

instance == EdgeStyle where (==) a b      = gEq{|*|} a b
instance == NodeStyle where (==) a b      = gEq{|*|} a b
instance == DirType   where (==) a b      = gEq{|*|} a b
instance == NodeShape where (==) a b      = gEq{|*|} a b
instance == ArrowType where (==) a b      = gEq{|*|} a b
instance == Color     where (==) a b      = gEq{|*|} a b


digraphTitle :: !Digraph -> String
digraphTitle (Digraph title _ _ _)        = title

digraphAtts :: !Digraph -> [GraphAttribute]
digraphAtts (Digraph _ atts _ _)        = atts

digraphNodes :: !Digraph -> [NodeDef]
digraphNodes (Digraph _ _ nodes _)        = nodes

digraphSelectedItem :: !Digraph -> ?SelectedItem
digraphSelectedItem (Digraph _ _ _ selected)  = selected

pointNode :: [NodeAttribute]
pointNode                    =: [NAttShape NShapePoint]

hiddenNode :: [NodeAttribute]
hiddenNode                    =: [NAttShape NShapePoint,NAttStyle NStyleInvis]

commaseparatedlist :: [String] -> String
commaseparatedlist []              = ""
commaseparatedlist l              = "[" +++ (foldr (+++) "" (intersperse "," l)) +++ "]"

printDigraph :: !Digraph -> [String]
printDigraph (Digraph title atts nodes _)    = map (\x->x+++"\n") (prelude title (graphAtts atts) (contents nodes))

createGraphName :: !String -> String
createGraphName ""                = "G"
createGraphName x                = quote x

prelude :: !String ![String] ![String] -> [String]
prelude title graphAtts contents        = [ "digraph " +++ createGraphName title +++ " {"
                          , "label="   +++ quote title 
                          ]            ++ 
                          graphAtts    ++ 
                          contents     ++ 
                          [ "overlap=false","}" ]

graphAtts :: ![GraphAttribute] -> [String]
graphAtts graphAtts                = map (printNameValuePair{|*|} "GAtt") graphAtts

contents :: ![NodeDef] -> [String]
contents nodeDefs                = map snd (mergeBy (\(x,_) (y,_)= x<y) nodes` edges`)
where
  (nodes,edges)                = unzip (mapSt f nodeDefs 1)
  where
    f (NodeDef id st na edges) num      = (  ((num,id,na)
                            ,[(n,id,id`,ea) \\ (id`,ea)<- edges & n<-[num+1..]]
                          )
                          , num + 1 + length edges
                          )
  
  nodes` = map (\(num, id, atts) = (num, id >$ commaseparatedlist (map toString atts))) nodes
  edges` = map (\(num,source,target,atts) = (num,source >$ "->" $> target >$ commaseparatedlist (map toString atts))) (flatten edges)

//  Utility functions:

mapSt :: (a b -> (c,b)) [a] b -> [c]
mapSt f [] st      = []
mapSt f [h:t] st 
  #! (x, st)      = f h st
  = [x : mapSt f t st]

quote :: !String -> String
quote a          = "\"" $> (flatten (map f (fromString a))) >$ "\""
where
  f '\"'        = ['\\\"']
  f x          = [x]

skipXXX_InConstructorName :: !String -> String
skipXXX_InConstructorName str
  = case dropWhile ((<>) '_') [c \\ c<-:str] of
    []        = str
    underscoreName  = str % (n-length underscoreName+1,n-1)
where
  n          = size str

skipName :: !String !String -> String
skipName pre str = {toLower c \\ c <-: skipNameUp pre str}

skipNameUp :: !String !String -> String
skipNameUp pre str | n >= m && pre == str % (0, m-1)
	= str % (m, n-1)
	= str
where
	n = size str
	m = size pre

firstCharLowerCase :: !String -> String
firstCharLowerCase str
  | size str > 0    = str := (0,toLower str.[0])
  | otherwise      = str

($>) infixr 5 :: !String !a -> String | toString a
($>) str arg      = str +++ toString arg

(>$) infixr 5 :: !a !String -> String | toString a
(>$) arg str      = toString arg +++ str


