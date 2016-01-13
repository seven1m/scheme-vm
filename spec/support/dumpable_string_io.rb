# we don't actually care to dump or load this
class DumpableStringIO < StringIO
  def marshal_dump
    []
  end

  def marshal_load(_data)
  end
end
