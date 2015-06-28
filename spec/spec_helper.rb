require_relative '../vm'
require_relative '../compiler'

RSpec.configure do |c|
  c.filter_run focus: true
  c.run_all_when_everything_filtered = true
end
