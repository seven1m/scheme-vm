require_relative './spec_helper'
require 'stringio'

Dir[File.expand_path('../lib/**/*.scm', __FILE__)].each do |path|
  describe File.split(path).last do
    code = File.read(path)
    focus = !(code =~ /^; focus/).nil?
    skip = !(code =~ /^; skip/).nil?
    it 'passes all tests', focus: focus, skip: skip do
      failed = false
      out = StringIO.new
      Program.new(code, filename: path, stdout: out).run
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
