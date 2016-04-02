use "files"
use "collections"

actor Main
  new create(env: Env) =>

    let caps = recover val FileCaps.set(FileRead).set(FileStat) end

    var fileString = ""
    try
      with file = OpenFile(FilePath(env.root, env.args(1), caps)) as File do
          Mast.decode(env.out, file)
      end
    end


primitive Mast
  fun magic(): String => "Mont\xe0MAST\x00"

  fun decodeMagic(data: File): Bool =>
    let expected = Mast.magic().runes() 
    let actual = data.read(Mast.magic().size()).values()
    
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

  fun decode(log: OutStream, data: File) =>
    if Mast.decodeMagic(data) then
      log.print("correct magic!")
    else
      log.print("bad magic")
    end
