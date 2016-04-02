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


trait Expr is Stringable
  fun val maybe(): (Expr val| None) => this
    
trait Patt is Stringable

class IntExpr is Expr
  let value: I64

  new from_zz(zz: I64) =>
    let sign: I64 = if (zz and 1) == 1 then -1 else 1 end
    value = (zz / 2) * sign

  fun string(fmt: FormatSettings = FormatSettingsDefault): String iso^ =>
    value.string(fmt)

class NounExpr is Expr
  let name: String

  new create(name': String) =>
    name = name'
  fun string(fmt: FormatSettings = FormatSettingsDefault): String iso^ =>
    name.string(fmt)

class SequenceExpr is Expr
  let exprs: Array[Expr val]

  new create(exprs': Array[Expr val]) =>
    exprs = exprs'

  fun val maybe(): (Expr val | None) =>
    if exprs.size() == 0 then None else this end

  fun string(fmt: FormatSettings = FormatSettingsDefault): String iso^ =>
    var out: String iso = recover String end
    out.append("{ ")
    for e in exprs.values() do
      out.append(e.string(fmt))
      out.append(";")
    end
    out.append(" }")
    out

class IgnorePatt is Patt
  let guardOpt: (Expr val| None)
  new create(g: (Expr val| None)) =>
    guardOpt = g

  fun string(fmt: FormatSettings = FormatSettingsDefault): String iso^ =>
    let gs: String = match guardOpt
    | let e: Expr val => e.string()
    else ""
    end
    ("_" + gs).string(fmt)

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
    | "M" => return // TODO method
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
    | "N" => recover SequenceExpr(Array[Expr val]) end
    else
      _log.print("other literal: " + t)
      error
    end
    
  fun ref _decodeExpr(t: String): Expr val ? =>
    _log.print("expr tag:" + t)
    match t
    | "N" => let s = _stream.nextStr()
             _log.print("noun: '" + s + "'")
             recover NounExpr(s) end
    else
      _log.print("other expr: " + t)
      error
    end
    
  fun ref _decodePattern(): Patt val ? =>
    let t = _stream.nextTag()
    match t
    | "I" => let guard = _getExpr().maybe()
             recover IgnorePatt(guard) end
    else
      _log.print("other pattern: " + t)
      error
    end

  fun ref _getExpr(): Expr val ? =>
    let ix = _stream.nextInt().usize()
    _exprs(ix)

  be _finish() =>
    if finished then
      return
    end
    finished = true
    let result = try _exprs.pop() else return end
    _notify(result)
