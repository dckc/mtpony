# A Monte interpreter in Go

## Motivation: performance

The current goal is to run the `brot.mt` benchmark.

## Current Status: import complex, export brotCount

```
$ go test -run ExampleModule

2017/11/19 16:56:52 MCall: <_>.run([<package>] [])
2017/11/19 16:56:52 WARNING! method guard not implemented: Map.get(Str, DeepFrozen)
2017/11/19 16:56:52 MCall: <package>.import(["lib/complex"] [])
2017/11/19 16:56:52 import: "lib/complex"
2017/11/19 16:56:52 WARNING! implements not implemented: [Complex]
2017/11/19 16:56:52 MCall: <_>.dependencies([] [])
2017/11/19 16:56:52 WARNING! method guard not implemented: List.get(Str)
2017/11/19 16:56:52 MCall: _makeList.run([] [])
2017/11/19 16:56:52 deps: []
2017/11/19 16:56:52 MCall: <_>.run([<package>] [])
2017/11/19 16:56:52 WARNING! method guard not implemented: Map.get(Str, DeepFrozen)
2017/11/19 16:56:52 MCall: <context of @@...>.getFQNPrefix([] [])
2017/11/19 16:56:52 WARNING! getFQNPrefix not implemented for meta.context()
2017/11/19 16:56:52 MCall: "@@prefix".add(["Complex_T"] [])
2017/11/19 16:56:52 MCall: _makeList.run([] [])
2017/11/19 16:56:52 MCall: _makeList.run([] [])
2017/11/19 16:56:52 MCall: _makeList.run([] [])
2017/11/19 16:56:52 MCall: &{_makeMessageDesc}.run([null "real" [] Double] [])
2017/11/19 16:56:52 WARNING! stub: _makeMessageDesc
2017/11/19 16:56:52 MCall: _makeList.run([] [])
2017/11/19 16:56:52 MCall: &{_makeMessageDesc}.run([null "imag" [] Double] [])
2017/11/19 16:56:52 WARNING! stub: _makeMessageDesc
2017/11/19 16:56:52 MCall: _makeList.run([] [])
2017/11/19 16:56:52 MCall: &{_makeMessageDesc}.run([null "abs" [] Double] [])
2017/11/19 16:56:52 WARNING! stub: _makeMessageDesc
2017/11/19 16:56:52 MCall: &{_makeParamDesc}.run(["other" Any] [])
2017/11/19 16:56:52 WARNING! stub: _makeParamDesc
2017/11/19 16:56:52 MCall: _makeList.run(["_makeParamDesc"] [])
2017/11/19 16:56:52 MCall: &{_makeMessageDesc}.run([null "add" ["_makeParamDesc"] Any] [])
2017/11/19 16:56:52 WARNING! stub: _makeMessageDesc
2017/11/19 16:56:52 MCall: &{_makeParamDesc}.run(["other" Any] [])
2017/11/19 16:56:52 WARNING! stub: _makeParamDesc
2017/11/19 16:56:52 MCall: _makeList.run(["_makeParamDesc"] [])
2017/11/19 16:56:52 MCall: &{_makeMessageDesc}.run([null "multiply" ["_makeParamDesc"] Any] [])
2017/11/19 16:56:52 WARNING! stub: _makeMessageDesc
2017/11/19 16:56:52 MCall: &{_makeParamDesc}.run(["other" Any] [])
2017/11/19 16:56:52 WARNING! stub: _makeParamDesc
2017/11/19 16:56:52 MCall: _makeList.run(["_makeParamDesc"] [])
2017/11/19 16:56:52 MCall: &{_makeMessageDesc}.run([null "subtract" ["_makeParamDesc"] Any] [])
2017/11/19 16:56:52 WARNING! stub: _makeMessageDesc
2017/11/19 16:56:52 MCall: _makeList.run(["_makeMessageDesc" "_makeMessageDesc" "_makeMessageDesc" "_makeMessageDesc" "_makeMessageDesc" "_makeMessageDesc"] [])
2017/11/19 16:56:52 MCall: &{_makeProtocolDesc}.run(["Complex numbers in ℂ." "@@prefixComplex_T" [] [] ["_makeMessageDesc", "_makeMessageDesc", "_makeMessageDesc", "_makeMessageDesc", "_makeMessageDesc", "_makeMessageDesc"]] [])
2017/11/19 16:56:52 WARNING! stub: _makeProtocolDesc
2017/11/19 16:56:52 WARNING! TODO guard checking: "_makeProtocolDesc": DeepFrozen
2017/11/19 16:56:52 MCall: _makeList.run(["Complex" "_makeProtocolDesc"] [])
2017/11/19 16:56:52 MCall: _makeList.run(["makeComplex" <makeComplex>] [])
2017/11/19 16:56:52 MCall: _makeList.run([["Complex", "_makeProtocolDesc"] ["makeComplex", <makeComplex>]] [])
2017/11/19 16:56:52 MCall: _makeMap.fromPairs([[["Complex", "_makeProtocolDesc"], ["makeComplex", <makeComplex>]]] [])
2017/11/19 16:56:52 MCall: _mapExtract.run(["makeComplex"] [])
2017/11/19 16:56:52 MCall: _mapExtract("makeComplex").run([["Complex" => "_makeProtocolDesc", "makeComplex" => <makeComplex>] throw] [])
2017/11/19 16:56:52 WARNING! TODO guard checking: <makeComplex>: DeepFrozen
2017/11/19 16:56:52 WARNING! TODO guard checking: 170: Int
2017/11/19 16:56:52 MCall: _makeList.run(["brotCount" <brotCount>] [])
2017/11/19 16:56:52 MCall: _makeList.run([["brotCount", <brotCount>]] [])
2017/11/19 16:56:52 MCall: _makeMap.fromPairs([[["brotCount", <brotCount>]]] [])
PASS
ok  	_/home/connolly/projects/mtpony	0.003s
```
