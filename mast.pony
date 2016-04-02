use "files"
use "collections"

actor Main
  let _d: MASTDecoder
  let _log: OutStream
  var _file: (File | None) = None

  new create(env: Env) =>
    let log = env.err
    _log = log
    _d = MASTDecoder(_log, recover GotExpr(log) end)
    let caps = recover val FileCaps.set(FileRead).set(FileStat) end

    try
      let file = OpenFile(FilePath(env.root, env.args(1), caps)) as File
      log.print("magic size:" + MAST.magic().codepoints().string())
      log.print("file size:" + file.size().string())
      if MAST.decodeMagic(file) then
        "" // log.print("correct magic!")
      else
        log.print("bad magic")
        return
      end
      _file = file
      _reading()
    else
      return
    end

  be _reading() =>
    match _file
    | None => return
    | let f :File =>
      let bs = f.read(1024)
      let qty = bs.size()
      // _log.print("bytes read:" + qty.string())
      _d.feed(consume bs)
      if qty > 0 then
        _reading()
      end
    end

class GotExpr
  let _log :OutStream

  new create(log: OutStream) =>
    _log = log

  fun ref apply(result: Expr val) =>
    _log.print("got result:" + result.string())

primitive MAST
  fun magic(): String => "Mont\xe0MAST\x00"

  fun decodeMagic(data: File): Bool =>
    let expected = MAST.magic().runes() 
    let actual = data.read(MAST.magic().codepoints()).values()
    
    for rune in expected do
      let item = try
        actual.next()
      else
        return false
      end
      if rune != item.u32() then
        return false
      end
    end
    true


class Brackets[I: Stringable #read]
  let open: String
  let close: String
  let sep: String

  new create(open': String, close': String, sep': String) =>
    open = open'
    close = close'
    sep = sep'

  fun box format(items: Array[I] val): String iso^ =>
    var sep' = ""
    var out: String iso = recover String end
    out.append(open)
    for item in items.values() do
      out.append(sep')
      out.append(item.string())
      sep' = sep
    end
    out.append(close)
    out
    

trait Expr is Stringable
  fun val maybe(): (Expr val| None) => this
    
class IntExpr is Expr
  let value: I64

  new from_zz(zz: I64) =>
    let sign: I64 = if (zz and 1) == 1 then -1 else 1 end
    value = (zz / 2) * sign

  fun string(fmt: FormatSettings = FormatSettingsDefault): String iso^ =>
    value.string(fmt)

class StrExpr is Expr
  let value: String

  new create(s: String) =>
    value = s
  fun string(fmt: FormatSettings = FormatSettingsDefault): String iso^ =>
    ("\"" + value + "\"").string(fmt) // TODO: real quoting

class NounExpr is Expr
  let name: String

  new create(name': String) =>
    name = name'
  fun string(fmt: FormatSettings = FormatSettingsDefault): String iso^ =>
    name.string(fmt)

class CallExpr is Expr
  let target: Expr val
  let verb: String
  let args: Array[Expr val] val
  let namedArgs: Array[(Expr val, Expr val)] val

  new create(target': Expr val,
             verb': String,
             args': Array[Expr val] val,
             namedArgs': Array[(Expr val, Expr val)] val) =>
    target = target'
    verb = verb'
    args = args'
    namedArgs = namedArgs'

  fun string(fmt: FormatSettings = FormatSettingsDefault): String iso^ =>
    var out: String iso = recover String end
    out.append(target.string())
    out.append(".")
    out.append(verb)
    out.append(Brackets[Expr val]("(", ")", ", ").format(args))
    // TODO: named args
    out

class DefExpr is Expr
  let patt: Patt val
  let exitOpt: (Expr val | None)
  let expr: Expr val

  new create(patt': Patt val,
             exitOpt': (Expr val | None),
             expr': Expr val) =>
    patt = patt'
    exitOpt = exitOpt'
    expr = expr'

  fun string(fmt: FormatSettings = FormatSettingsDefault): String iso^ =>
    var out: String iso = recover String end
    out.append("def ")
    out.append(patt.string())
    match exitOpt
    | let e: Expr val =>
        out.append(" exit ")
        out.append(e.string())
    else
      "" // noop
    end
    out.append(" := ")
    out.append(expr.string())
    out

class SequenceExpr is Expr
  let exprs: Array[Expr val] val

  new create(exprs': Array[Expr val] val) =>
    exprs = exprs'

  fun val maybe(): (Expr val | None) =>
    if exprs.size() == 0 then None else this end

  fun string(fmt: FormatSettings = FormatSettingsDefault): String iso^ =>
    Brackets[Expr val]("{ ", " }", "\n").format(exprs)

class Object is Expr
  let doc: String
  let name: Patt val
  let asExpr: (Expr val | None)
  let implements: Array[Expr val] val
  let methods: Array[Expr val] val // TODO: methods
  // TODO: matchers

  new create(doc': String, name': Patt val,
             asExpr': (Expr val | None),
             implements': Array[Expr val] val,
             methods': Array[Expr val] val) =>
    doc = doc'
    name = name'
    asExpr = asExpr'
    implements = implements'
    methods = methods'

  fun string(fmt: FormatSettings = FormatSettingsDefault): String iso^ =>
    let out: String iso = recover String end
    out.append("object " + name.string())
    // TODO: asExpr, implements
    out.append(Brackets[Expr val](" {\n", " }\n", "\n").format(methods))
    out

class Method is Expr // CHEATING! It's not, really
  let doc: String
  let verb: String
  let params: Array[Patt val] val
  // let namedParams: Array[(String, Patt val)] val
  let guardOpt: (Expr val | None)
  let body: Expr val

  new create(doc': String, verb': String, params': Array[Patt val] val,
    //namedParams': Array[(String, Patt val)] val,
    guardOpt': (Expr val | None),
    body': Expr val) =>
    doc = doc'
    verb = verb'
    params = params'
    //namedParams = namedParams'
    guardOpt = guardOpt'
    body = body'

  fun string(fmt: FormatSettings = FormatSettingsDefault): String iso^ =>
    let out: String iso = recover String end
    out.append("to " + verb)
    out.append(Brackets[Patt val]("(", ")", ", ").format(params))
    // TODO: named params
    let p = IgnorePatt(None)
    out.append(p.fmtGuard(guardOpt))
    out.append(" {\n")
    out.append(body.string())
    out.append(" }\n")
    out

trait Patt is Stringable
  fun fmtGuard(go: (Expr val | None)): String =>
    match go
    | let e: Expr val => " :" + e.string()
    else
      ""
    end

class FinalPatt is Patt
  let name: String
  let guardOpt: (Expr val| None)
  new create(name': String, g: (Expr val| None)) =>
    name = name'
    guardOpt = g

  fun string(fmt: FormatSettings = FormatSettingsDefault): String iso^ =>
    (name + fmtGuard(guardOpt)).string(fmt)

class IgnorePatt is Patt
  let guardOpt: (Expr val| None)
  new create(g: (Expr val| None)) =>
    guardOpt = g

  fun string(fmt: FormatSettings = FormatSettingsDefault): String iso^ =>
    ("_" + fmtGuard(guardOpt)).string(fmt)

class ViaPatt is Patt
  let patt: Patt val
  let test: Expr val
  new create(patt': Patt val, test': Expr val) =>
    patt = patt'
    test = test'

  fun string(fmt: FormatSettings = FormatSettingsDefault): String iso^ =>
    let ps = patt.string()
    let ts = " ? (" + test.string() + ")"
    (ps + ts).string(fmt)

class ListPatt is Patt
  let patts: Array[Patt val] val
  new create(patts': Array[Patt val] val) =>
    patts = patts'

  fun string(fmt: FormatSettings = FormatSettingsDefault): String iso^ =>
    Brackets[Patt val]("[", "]", ", ").format(patts)


interface MASTNotify
  fun ref apply(result: Expr val) =>
    None

class MASTStream
  let _blocks: Array[Array[U8] val] ref = Array[Array[U8] val]
  var _offset: USize = 0

  fun ref push(data: Array[U8] val) =>
    _blocks.push(data)

  fun ref nextByte(): U8 ? =>
    if atEnd() then
      error
    end
    try
      _blocks(0)(_offset = _offset + 1)
    else
      _blocks.shift()
      _offset = 0
      nextByte()
    end

  fun ref nextTag(): String ? =>
    let b = nextByte()
    recover String.from_utf32(b.u32()) end

  fun ref nextInt(): I64 ? =>
    nextByte().i64()

  fun ref nextStr(): String ? =>
    var ix = nextInt()
    let out = recover String end
    while ix > 0 do
      out.push(nextByte())
      ix = ix - 1
    end
    out

  fun atEnd(): Bool =>
    try
      _blocks(0).size() == 0
    else
      false
    end

  fun string() :String =>
    try
      (_offset.string() + " of " + _blocks(0).size().string() +
       " bytes into 1st of " +
       _blocks.size().string() + " blocks")
    else
      "empty stream"
    end

actor MASTDecoder
  let _exprs :Array[Expr val] = Array[Expr val]
  let _patts :Array[Patt val] = Array[Patt val]
  let _notify :MASTNotify iso
  let _stream :MASTStream
  let _log: OutStream
  var finished: Bool = false

  new create(log: OutStream, notify: MASTNotify iso) =>
    _notify = consume notify
    _stream = MASTStream
    _log = log

  be feed(data: Array[U8] val) =>
    """
    feed an empty array to signal end of input.
    """
    _stream.push(data)
    // _log.print("fed: " + _stream.string())
    _decode()

  be _decode() =>
    // _log.print("decode: " + _stream.string())
    if _stream.atEnd() then
      _finish()
      return
    end
    let t = try _stream.nextTag() else return end

    _log.print("tag:" + t.string())
    match t
    | "L" => _exprs.push(try _decodeLiteral() else return end)
    | "P" => _patts.push(try _decodePattern() else return end)
    | "O" => try
        let doc = _stream.nextStr()
        let name = _getPatt()
        let asExpr = _getExpr().maybe()
        let implements = _getExprs()
        let methods = _getExprs()
        let matchers = _getExprs()
        _exprs.push(recover Object(doc, name, asExpr, implements,
                                   methods) end)
      else
        _log.print("bad object?!")
        return
      end
    | "M" => try
        let doc = _stream.nextStr()
        let verb = _stream.nextStr()
        let params = _getPatts()
        let npqty = _stream.nextInt()
        if npqty != 0 then
          _log.print("TODO! named parameters in methods")
          error // TODO
        end
        let guardOpt = _getExpr().maybe()
        let body = _getExpr()
        _exprs.push(recover Method(doc, verb, params, guardOpt, body) end)
      else
        _log.print("bad method?!")
        return
      end
    | "R" => return // TODO matcher
    else
      _exprs.push(try _decodeExpr(t) else return end)
    end
    _decode()
    try
      let e = _exprs(_exprs.size() - 1)
      _log.print("last expr: " + e.string())
    else None end
    try
      let p = _patts(_patts.size() - 1)
      _log.print("last patt: " + p.string())
    else None end

  fun ref _decodeLiteral(): Expr val ? =>
    let t = _stream.nextTag()
    match t
    | "I" => let zz = _stream.nextInt()
             recover IntExpr.from_zz(zz) end
    | "N" => recover SequenceExpr(recover Array[Expr val] end) end
    | "S" => let s = _stream.nextStr()
             recover StrExpr(s) end
    else
      _log.print("other literal: " + t)
      error
    end
    
  fun ref _decodeExpr(t: String): Expr val ? =>
    _log.print("expr tag:" + t)
    match t
    | "C" => let target = _getExpr()
             let verb = _stream.nextStr()
             let args = _getExprs()
             let namedArgs = _getNamedExprs()
             recover CallExpr(target, verb, args, namedArgs) end
    | "D" => let patt = _getPatt()
             let exitOpt = _getExpr().maybe()
             let value = _getExpr()
             recover DefExpr(patt, exitOpt, value) end
    | "N" => let s = _stream.nextStr()
             recover NounExpr(s) end
    | "S" => let exprs = _getExprs()
             recover SequenceExpr(exprs) end
    else
      _log.print("other expr: " + t)
      error
    end
    
  fun ref _decodePattern(): Patt val ? =>
    let t = _stream.nextTag()
    match t
    | "A" => let test = _getExpr()
             let patt = _getPatt()
             recover ViaPatt(patt, test) end
    | "F" => let s = _stream.nextStr()
             let guard = _getExpr().maybe()
             recover FinalPatt(s, guard) end
    | "I" => let guard = _getExpr().maybe()
             recover IgnorePatt(guard) end
    | "L" => let patts = _getPatts()
             recover ListPatt(patts) end
    else
      _log.print("other pattern: " + t)
      error
    end

  fun ref _getExpr(): Expr val ? =>
    let ix = _stream.nextInt().usize()
    _exprs(ix)

  fun ref _getExprs(): Array[Expr val] val ? =>
    var qty = _stream.nextInt()
    let out = recover Array[Expr val] end
    while qty > 0 do
      out.push(_getExpr())
      qty = qty - 1
    end
    out

  fun ref _getPatt(): Patt val ? =>
    let ix = _stream.nextInt().usize()
    _patts(ix)

  fun ref _getPatts(): Array[Patt val] val ? =>
    var qty = _stream.nextInt()
    let out = recover Array[Patt val] end
    while qty > 0 do
      out.push(_getPatt())
      qty = qty - 1
    end
    out

  fun ref _getNamedExprs(): Array[(Expr val, Expr val)] val ? =>
    var qty = _stream.nextInt()
    let out = recover Array[(Expr val, Expr val)] end
    while qty > 0 do
      let k = _getExpr()
      let v = _getExpr()
      out.push((k, v))
      qty = qty - 1
    end
    out

  be _finish() =>
    if finished then
      return
    end
    finished = true
    let result = try _exprs.pop() else return end
    _notify(result)
