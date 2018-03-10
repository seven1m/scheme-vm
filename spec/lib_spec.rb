require_relative './spec_helper'
require 'stringio'

Dir[File.expand_path('../lib/**/*.scm', __FILE__)].each do |path|
  describe File.split(path).last do
    code = File.read(path)
    focus = !(code =~ /^;; focus/).nil? || ENV['SCM_FILE'] == path[-ENV['SCM_FILE'].to_s.size..-1]
    skip = !(code =~ /^;; skip/).nil?
    debug = code =~ /^;; debug/ ? 2 : 0
    it 'passes all tests', focus: focus, skip: skip do
      out = StringIO.new
      program = Program.new(code, filename: path, stdout: out)
      program.debug = debug
      program.run
      out.rewind
      result = out.read
      raise result unless result == ''
    end
  end
end
