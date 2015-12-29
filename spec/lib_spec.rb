require_relative './spec_helper'
require 'stringio'

describe 'Libs' do
  let(:out) { StringIO.new }

  it 'passes all tests' do
    failed = false
    Dir[File.expand_path('../lib/**/*.scm', __FILE__)].each do |path|
      Program.new(File.read(path), filename: File.split(path).last, stdout: out).run
      out.rewind
      result = out.read
      if result != ''
        puts
        puts result
        failed = true
      end
    end
    fail 'spec failed' if failed
  end
end
