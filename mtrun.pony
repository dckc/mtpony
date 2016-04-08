use "collections"

actor Main
  new create(env: Env) =>
    env.out.print(Module1.eval().string())

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

type Err is (Refused | WrongType | Ejecting | Exception)

trait MTObject
  fun ref call(verb: String, args: Args, namedArgs: NamedArgs): (MTObject | Err)
  fun string(fmt: FormatSettings = FormatSettingsDefault): String iso^

class Null is MTObject
  fun call(verb: String, args: Args, namedArgs: NamedArgs): (MTObject | Err) =>
    Refused
  fun string(fmt: FormatSettings = FormatSettingsDefault): String iso^ =>
    "null".string()

class Int is MTObject
  // TODO: arbitrary precision
  let value: I64
  new create(value': I64) =>
    value = value'
  fun call(verb: String, args: Args, namedArgs: NamedArgs): (MTObject | Err) =>
    match (verb, args.size())
    | ("add", 1) => match try args(0) else Monte.null() end
                    | let iobj: Int => Int(value + iobj.value)
                    else
                      WrongType
                    end
    else
      Refused
    end
  fun string(fmt: FormatSettings): String iso^ =>
    value.string()

class Char is MTObject
  let code: U32

  new create(code': U32) =>
    code = code'

  fun call(verb: String, args: Args, namedArgs: NamedArgs): (MTObject | Err) =>
    match (verb, args.size())
    | ("add", 1) => match try args(0) else Monte.null() end
                    | let iobj: Int => Char(code + iobj.value.u32())
                    else
                      WrongType
                    end
    else
      Refused
    end
  fun string(fmt: FormatSettings): String iso^ =>
    // TODO: quoting
    ("'" + String.from_utf32(code) + "'").string()

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
