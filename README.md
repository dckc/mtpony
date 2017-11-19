# A Monte interpreter in Go

## Motivation: performance

The current goal is to run the `brot.mt` benchmark.

## Current Status

```
$ go test -run ExampleModule

2017/11/19 03:16:32 MCall: <_>.run([<package>] [])
2017/11/19 03:16:32 WARNING! method guard not implemented: Map.get(Str, DeepFrozen)
2017/11/19 03:16:32 MCall: <package>.import(["lib/complex"] [])
2017/11/19 03:16:32 import: "lib/complex"
2017/11/19 03:16:32 WARNING! implements not implemented: [Complex]
2017/11/19 03:16:32 MCall: <_>.dependencies([] [])
2017/11/19 03:16:32 WARNING! method guard not implemented: List.get(Str)
2017/11/19 03:16:32 MCall: _makeList.run([] [])
2017/11/19 03:16:32 deps: []
2017/11/19 03:16:32 MCall: <_>.run([<package>] [])
2017/11/19 03:16:32 WARNING! method guard not implemented: Map.get(Str, DeepFrozen)
2017/11/19 03:16:32 MCall: <context of @@...>.getFQNPrefix([] [])
2017/11/19 03:16:32 WARNING! getFQNPrefix not implemented for meta.context()
2017/11/19 03:16:32 MCall: "@@prefix".add(["Complex_T"] [])
2017/11/19 03:16:32 MCall: _makeList.run([] [])
2017/11/19 03:16:32 MCall: _makeList.run([] [])
2017/11/19 03:16:32 MCall: _makeList.run([] [])
2017/11/19 03:16:32 MCall: &{_makeMessageDesc}.run([null "real" [] Double] [])
2017/11/19 03:16:32 WARNING! stub: _makeMessageDesc
2017/11/19 03:16:32 MCall: _makeList.run([] [])
2017/11/19 03:16:32 MCall: &{_makeMessageDesc}.run([null "imag" [] Double] [])
2017/11/19 03:16:32 WARNING! stub: _makeMessageDesc
2017/11/19 03:16:32 MCall: _makeList.run([] [])
2017/11/19 03:16:32 MCall: &{_makeMessageDesc}.run([null "abs" [] Double] [])
2017/11/19 03:16:32 WARNING! stub: _makeMessageDesc
2017/11/19 03:16:32 MCall: &{_makeParamDesc}.run(["other" Any] [])
2017/11/19 03:16:32 WARNING! stub: _makeParamDesc
2017/11/19 03:16:32 MCall: _makeList.run(["_makeParamDesc"] [])
2017/11/19 03:16:32 MCall: &{_makeMessageDesc}.run([null "add" ["_makeParamDesc"] Any] [])
2017/11/19 03:16:32 WARNING! stub: _makeMessageDesc
2017/11/19 03:16:32 MCall: &{_makeParamDesc}.run(["other" Any] [])
2017/11/19 03:16:32 WARNING! stub: _makeParamDesc
2017/11/19 03:16:32 MCall: _makeList.run(["_makeParamDesc"] [])
2017/11/19 03:16:32 MCall: &{_makeMessageDesc}.run([null "multiply" ["_makeParamDesc"] Any] [])
2017/11/19 03:16:32 WARNING! stub: _makeMessageDesc
2017/11/19 03:16:32 MCall: &{_makeParamDesc}.run(["other" Any] [])
2017/11/19 03:16:32 WARNING! stub: _makeParamDesc
2017/11/19 03:16:32 MCall: _makeList.run(["_makeParamDesc"] [])
2017/11/19 03:16:32 MCall: &{_makeMessageDesc}.run([null "subtract" ["_makeParamDesc"] Any] [])
2017/11/19 03:16:32 WARNING! stub: _makeMessageDesc
2017/11/19 03:16:32 MCall: _makeList.run(["_makeMessageDesc" "_makeMessageDesc" "_makeMessageDesc" "_makeMessageDesc" "_makeMessageDesc" "_makeMessageDesc"] [])
2017/11/19 03:16:32 MCall: &{_makeProtocolDesc}.run(["Complex numbers in ℂ." "@@prefixComplex_T" [] [] ["_makeMessageDesc", "_makeMessageDesc", "_makeMessageDesc", "_makeMessageDesc", "_makeMessageDesc", "_makeMessageDesc"]] [])
2017/11/19 03:16:32 WARNING! stub: _makeProtocolDesc
2017/11/19 03:16:32 WARNING! TODO guard checking: "_makeProtocolDesc": DeepFrozen
2017/11/19 03:16:32 MCall: _makeList.run(["Complex" "_makeProtocolDesc"] [])
2017/11/19 03:16:32 MCall: _makeList.run(["makeComplex" <makeComplex>] [])
2017/11/19 03:16:32 MCall: _makeList.run([["Complex", "_makeProtocolDesc"] ["makeComplex", <makeComplex>]] [])
2017/11/19 03:16:32 MCall: _makeMap.fromPairs([[["Complex", "_makeProtocolDesc"], ["makeComplex", <makeComplex>]]] [])
2017/11/19 03:16:54 MCall: <_>.run([<package>] [])
2017/11/19 03:16:54 WARNING! method guard not implemented: Map.get(Str, DeepFrozen)
2017/11/19 03:16:54 MCall: <package>.import(["lib/complex"] [])
2017/11/19 03:16:54 import: "lib/complex"
2017/11/19 03:16:54 WARNING! implements not implemented: [Complex]
2017/11/19 03:16:54 MCall: <_>.dependencies([] [])
2017/11/19 03:16:54 WARNING! method guard not implemented: List.get(Str)
2017/11/19 03:16:54 MCall: _makeList.run([] [])
2017/11/19 03:16:54 deps: []
2017/11/19 03:16:54 MCall: <_>.run([<package>] [])
2017/11/19 03:16:54 WARNING! method guard not implemented: Map.get(Str, DeepFrozen)
2017/11/19 03:16:54 MCall: <context of @@...>.getFQNPrefix([] [])
2017/11/19 03:16:54 WARNING! getFQNPrefix not implemented for meta.context()
2017/11/19 03:16:54 MCall: "@@prefix".add(["Complex_T"] [])
2017/11/19 03:16:54 MCall: _makeList.run([] [])
2017/11/19 03:16:54 MCall: _makeList.run([] [])
2017/11/19 03:16:54 MCall: _makeList.run([] [])
2017/11/19 03:16:54 MCall: &{_makeMessageDesc}.run([null "real" [] Double] [])
2017/11/19 03:16:54 WARNING! stub: _makeMessageDesc
2017/11/19 03:16:54 MCall: _makeList.run([] [])
2017/11/19 03:16:54 MCall: &{_makeMessageDesc}.run([null "imag" [] Double] [])
2017/11/19 03:16:54 WARNING! stub: _makeMessageDesc
2017/11/19 03:16:54 MCall: _makeList.run([] [])
2017/11/19 03:16:54 MCall: &{_makeMessageDesc}.run([null "abs" [] Double] [])
2017/11/19 03:16:54 WARNING! stub: _makeMessageDesc
2017/11/19 03:16:54 MCall: &{_makeParamDesc}.run(["other" Any] [])
2017/11/19 03:16:54 WARNING! stub: _makeParamDesc
2017/11/19 03:16:54 MCall: _makeList.run(["_makeParamDesc"] [])
2017/11/19 03:16:54 MCall: &{_makeMessageDesc}.run([null "add" ["_makeParamDesc"] Any] [])
2017/11/19 03:16:54 WARNING! stub: _makeMessageDesc
2017/11/19 03:16:54 MCall: &{_makeParamDesc}.run(["other" Any] [])
2017/11/19 03:16:54 WARNING! stub: _makeParamDesc
2017/11/19 03:16:54 MCall: _makeList.run(["_makeParamDesc"] [])
2017/11/19 03:16:54 MCall: &{_makeMessageDesc}.run([null "multiply" ["_makeParamDesc"] Any] [])
2017/11/19 03:16:54 WARNING! stub: _makeMessageDesc
2017/11/19 03:16:54 MCall: &{_makeParamDesc}.run(["other" Any] [])
2017/11/19 03:16:54 WARNING! stub: _makeParamDesc
2017/11/19 03:16:54 MCall: _makeList.run(["_makeParamDesc"] [])
2017/11/19 03:16:54 MCall: &{_makeMessageDesc}.run([null "subtract" ["_makeParamDesc"] Any] [])
2017/11/19 03:16:54 WARNING! stub: _makeMessageDesc
2017/11/19 03:16:54 MCall: _makeList.run(["_makeMessageDesc" "_makeMessageDesc" "_makeMessageDesc" "_makeMessageDesc" "_makeMessageDesc" "_makeMessageDesc"] [])
2017/11/19 03:16:54 MCall: &{_makeProtocolDesc}.run(["Complex numbers in ℂ." "@@prefixComplex_T" [] [] ["_makeMessageDesc", "_makeMessageDesc", "_makeMessageDesc", "_makeMessageDesc", "_makeMessageDesc", "_makeMessageDesc"]] [])
2017/11/19 03:16:54 WARNING! stub: _makeProtocolDesc
2017/11/19 03:16:54 WARNING! TODO guard checking: "_makeProtocolDesc": DeepFrozen
2017/11/19 03:16:54 MCall: _makeList.run(["Complex" "_makeProtocolDesc"] [])
2017/11/19 03:16:54 MCall: _makeList.run(["makeComplex" <makeComplex>] [])
2017/11/19 03:16:54 MCall: _makeList.run([["Complex", "_makeProtocolDesc"] ["makeComplex", <makeComplex>]] [])
2017/11/19 03:16:54 MCall: _makeMap.fromPairs([[["Complex", "_makeProtocolDesc"], ["makeComplex", <makeComplex>]]] [])
--- FAIL: ExampleModule (0.00s)
got:
@@matchBind not implmented for via (_mapExtract.run("makeComplex")) [makeComplex :DeepFrozen, _]
want:
X
FAIL
exit status 1
```
