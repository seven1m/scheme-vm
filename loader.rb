require 'pathname'
require_relative './vm'

class Loader
  ROOT_PATH = VM::ROOT_PATH
  LOAD_PATH = [File.join(ROOT_PATH, 'lib')].freeze

  def initialize(filename, relative_to: nil)
    @filename = filename
    @relative_to = relative_to
  end

  attr_reader :filename, :relative_to, :path, :code

  def load
    @path = find_path
    raise "File #{filename} not found in load path #{LOAD_PATH.join(';')}" unless @path
    @code = File.read(@path)
  end

  private

  def find_path
    if filename.start_with?('.') && relative_to
      File.expand_path(File.join(File.dirname(relative_to), filename))
    else
      LOAD_PATH.map { |p| File.expand_path(File.join(p, filename)) }.detect { |p| File.exist?(p) }
    end
  end
end
