require_relative '../program'
require_relative '../vm'
require_relative '../compiler'
require_relative '../parser'

RSpec.configure do |c|
  c.filter_run focus: true
  c.run_all_when_everything_filtered = true

  def d(instructions, skip_libs: true)
    pretty = VM::PrettyPrinter.new(instructions).format
    if skip_libs
      pretty.slice_after('VM::ENDL').to_a.last
    else
      pretty
    end
  end
end
