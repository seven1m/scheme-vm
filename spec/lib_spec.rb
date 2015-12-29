require_relative './spec_helper'
require 'stringio'

Dir[File.expand_path('../lib/**/*.scm', __FILE__)].each do |path|
  describe File.split(path).last do
    it 'passes all tests' do
      failed = false
      out = StringIO.new
      Program.new(File.read(path), filename: File.split(path).last, stdout: out).run
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
