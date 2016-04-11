use "collections"

actor Main
  new create(env: Env) =>
    let safeScope: Map[String, MTObject ref] = Map[String, MTObject ref]

    // safeScope.update("simple__quasiParser", Null)  // TODO: prelude?
    safeScope.update("traceln", TraceLn(env.out))

    try
      let result = Module1.eval(safeScope)
      env.out.print(result.string())
    else
      env.out.print("error!")
    end

type Args is Array[MTObject]
type NamedArgs is Map[String, MTObject]

primitive Refused
  fun string(fmt: FormatSettings = FormatSettingsDefault): String iso^ => "Refused!".string()
primitive WrongType
  fun string(fmt: FormatSettings = FormatSettingsDefault): String iso^ => "WrongType!".string()
class Ejecting
  let obj: MTObject
  new create(obj': MTObject) =>
    obj = obj'
  fun string(fmt: FormatSettings = FormatSettingsDefault): String iso^ => "Ejecting(@@obj)".string()
class Exception
  let obj: MTObject
  new create(obj': MTObject) =>
    obj = obj'
  fun string(fmt: FormatSettings = FormatSettingsDefault): String iso^ =>
    let out = recover String end
    out.append("Exception(")
    out.append(obj.string())
    out.append(")")
    out

type MTErr is (Refused | WrongType | Ejecting | Exception)

trait MTObject is Stringable
  fun ref call(verb: String, args: Args, namedArgs: NamedArgs): MTObject ?
  fun string(fmt: FormatSettings = FormatSettingsDefault): String iso^

class Null is MTObject
  fun call(verb: String, args: Args, namedArgs: NamedArgs): MTObject ? =>
    error  // TODO: Refused
  fun string(fmt: FormatSettings = FormatSettingsDefault): String iso^ =>
    "null".string()

class Int is MTObject
  // TODO: arbitrary precision
  let value: I64
  new create(value': I64) =>
    value = value'
  fun call(verb: String, args: Args, namedArgs: NamedArgs): MTObject ? =>
    match (verb, args.size())
    | ("add", 1) => match try args(0) else Monte.null() end
                    | let iobj: Int => Int(value + iobj.value)
                    else
                      error // TODO: WrongType
                    end
    else
      error // TODO: Refused
    end
  fun string(fmt: FormatSettings = FormatSettingsDefault): String iso^ =>
    value.string()

class Char is MTObject
  let code: U32

  new create(code': U32) =>
    code = code'

  fun call(verb: String, args: Args, namedArgs: NamedArgs): MTObject ? =>
    match (verb, args.size())
    | ("add", 1) => match try args(0) else Monte.null() end
                    | let iobj: Int => Char(code + iobj.value.u32())
                    else
                      error // TODO: WrongType
                    end
    else
      error // TODO: Refused
    end
  fun string(fmt: FormatSettings = FormatSettingsDefault): String iso^ =>
    // TODO: quoting
    ("'" + String.from_utf32(code) + "'").string()

class Str is MTObject
  let value: String

  new create(value': String) =>
    value = value'

  fun call(verb: String, args: Args, namedArgs: NamedArgs): MTObject ? =>
    error // TODO

  fun string(fmt: FormatSettings = FormatSettingsDefault): String iso^ =>
    // TODO: quoting
    value.string()

class TraceLn is MTObject
  let _out: OutStream

  new create(out: OutStream) =>
    _out = out

  fun call(verb: String, args: Args, namedArgs: NamedArgs): MTObject ? =>
    match (verb)
    | "run" =>
      var sep = ""
      _out.write("[TRACE: ")
      for arg in args.values() do
        _out.write(sep)
        _out.write(arg.string())
        sep = ", "
      end
      _out.print("]")
      Null
    else
      error  // TODO: refused
    end

  fun string(fmt: FormatSettings = FormatSettingsDefault): String iso^ =>
    "<traceln>".string()

primitive Monte
  fun emptyArgs(): Args => Args
  fun argsWith(init: Args, last: MTObject): Args =>
    init.push(last)
    init

  fun emptyNamedArgs(): NamedArgs => NamedArgs
  fun namedArgsWith(init: NamedArgs, n: String, v: MTObject): NamedArgs =>
    init.update(n, v)
    init

  fun null(): MTObject => Null  // how to cache this?
  fun makeInt(numeral: String): MTObject =>
    let value = try numeral.i64() else 0 end
    Int(value)

  fun char(code: U32): MTObject => Char(code)
  fun makeStr(value: String): MTObject => Str(value)
