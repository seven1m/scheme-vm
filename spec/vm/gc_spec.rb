require_relative '../spec_helper'

describe GC do
  let(:stdout) { StringIO.new }

  let(:instructions) do
    [
      VM::PUSH_FUNC,

      VM::PUSH_ARG,
      VM::NAME_ARG, 'my-2',

      VM::PUSH_ARG,
      VM::NAME_ARG, 'my-3',

      VM::PUSH_VAR, 'y',
      VM::DEFINE_VAR, 'my-y',

      VM::HALT, # pause execution here
      VM::RETURN,
      VM::ENDF,
      VM::DEFINE_VAR, 'fn',

      # # # # # #

      VM::PUSH_NUM, '1',
      VM::DEFINE_VAR, 'y',

      VM::PUSH_NUM, '2',
      VM::DUP,                # save this for an arg below
      VM::PUSH_NUM, 1,
      VM::PUSH_LIST,
      VM::DEFINE_VAR, 'z',

      VM::PUSH_NUM, '3',
      VM::PUSH_NUM, 2,
      VM::SET_ARGS,           # [2, 3]

      VM::PUSH_VAR, 'fn',
      VM::CALL,
      VM::HALT
    ]
  end

  subject { VM.new }

  before { subject.execute(instructions) }

  it 'cleans up objects no longer referenced by variables' do
    expect(subject.heap[instructions.size..-1]).to match_array([
      VM::Int.new(1),          # y
      VM::Int.new(2),          # part of list at top-level
      VM::EmptyList.instance,
      VM::Pair,                # list defined at top level
      VM::Int.new(3),          # argument passed to function (not set at the top level)
      nil
    ])
    subject.execute            # resume execution
    expect(subject.heap[instructions.size..-1]).to match_array([
      VM::Int.new(1),          # y
      VM::Int.new(2),          # still part of list at top-level
      VM::EmptyList.instance,
      VM::Pair,                # list defined at top level
      nil,                     # argument passed to function, now out of scope
      nil
    ])
  end
end
