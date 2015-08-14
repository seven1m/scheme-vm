require_relative './spec_helper'
require 'stringio'

describe 'Libs' do
  let(:out) { StringIO.new }

  it 'passes all tests' do
    Dir[File.expand_path('../lib/**/*.scm', __FILE__)].each do |path|
      Program.new(File.read(path), stdout: out).run
      out.rewind
      expect(out.read).to eq('')
    end
  end
end
