require_relative './spec_helper'
require_relative './support/dumpable_string_io'
require 'stringio'

out = DumpableStringIO.new
program = Program.new('(import (scheme base) (assert))', stdout: out)
program.run
cached_program = Marshal.dump(program)

Dir[File.expand_path('../lib/**/*.scm', __FILE__)].each do |path|
  describe File.split(path).last do
    code = File.read(path).sub(/\(import \(scheme base\)\s+\(assert\)\s*\)/, '')
    focus = !(code =~ /^; focus/).nil?
    skip = !(code =~ /^; skip/).nil?
    it 'passes all tests', focus: focus, skip: skip do
      failed = false
      out = DumpableStringIO.new
      program = Marshal.load(cached_program)
      program.filename = path
      program.stdout = out
      program.run(code: code)
      out.rewind
      result = out.read
      if result != ''
        puts
        puts result
        failed = true
      end
      fail 'spec failed' if failed
    end
  end
end
