definition module Text.HTML.GenJSON

from Text.HTML    import :: HtmlTag, :: HtmlAttr
from Text.HTML    import :: SVGElt, :: SVGAttr, :: SVGAlign, :: SVGColor, :: SVGDefer, :: SVGFillOpacity, :: SVGFuncIRI, :: SVGLengthAdjust, :: SVGICCColor
from Text.HTML    import :: SVGLengthUnit, :: SVGLineCap, :: SVGFillRule, :: SVGLineJoin, :: SVGMeetOrSlice, :: SVGStrokeMiterLimit, :: SVGPaint, :: SVGLength
from Text.HTML    import :: SVGStrokeDashArray, :: SVGStrokeDashOffset, :: SVGStrokeWidth, :: SVGTransform, :: SVGZoomAndPan
from Text.GenJSON import generic JSONEncode, generic JSONDecode, :: JSONNode

derive JSONEncode HtmlTag, HtmlAttr
derive JSONEncode SVGElt, SVGAttr, SVGAlign, SVGColor, SVGDefer, SVGFillOpacity, SVGFuncIRI, SVGLengthAdjust, SVGLengthUnit, SVGLineCap, SVGFillRule, SVGLineJoin, SVGMeetOrSlice, SVGStrokeMiterLimit, SVGPaint, SVGStrokeDashArray, SVGStrokeDashOffset, SVGStrokeWidth, SVGTransform, SVGZoomAndPan, SVGICCColor, SVGLength
derive JSONDecode HtmlTag, HtmlAttr
derive JSONDecode SVGElt, SVGAttr, SVGAlign, SVGColor, SVGDefer, SVGFillOpacity, SVGFuncIRI, SVGLengthAdjust, SVGLengthUnit, SVGLineCap, SVGFillRule, SVGLineJoin, SVGMeetOrSlice, SVGStrokeMiterLimit, SVGPaint, SVGStrokeDashArray, SVGStrokeDashOffset, SVGStrokeWidth, SVGTransform, SVGZoomAndPan, SVGICCColor, SVGLength
