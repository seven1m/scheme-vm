require_relative '../compiler'
require_relative '../vm'

describe Compiler do

  it 'compiles def' do
    subject.expressions << [:def, :foo, 10]
    subject.expressions << [:def, :bar, 20]
    expect(subject.compiled).to eq([
      VM::PUSHNUM, 1,
      VM::PUSHNUM, 0,
      VM::ALLOC,
      VM::PUSHNUM, 10,
      VM::PUSHNUM, 0,
      VM::ASSIGN,

      VM::PUSHNUM, 1,
      VM::PUSHNUM, 1,
      VM::ALLOC,
      VM::PUSHNUM, 20,
      VM::PUSHNUM, 1,
      VM::ASSIGN,

      VM::PUSHNUM, 0,
      VM::RETURN
    ])
  end

end
