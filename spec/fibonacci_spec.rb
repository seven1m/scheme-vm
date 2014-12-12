require_relative '../vm'

describe VM do

  describe 'fibonacci' do
    # fib = [n] { if n < 2,
    #                n,
    #                { (fib n - 1) + (fib n - 2) } }
    before do
      subject.load([
        # fib
        VM::PUSH, 0,
        VM::FUNC,
        VM::POP,     # discard the argument count
        VM::DUP,
        VM::PUSH, 1, # size
        VM::PUSH, 1, # index
        VM::ALLOC,
        VM::PUSH, 1,
        VM::ASSIGN,  # store argument in index 1
        VM::PUSH, 2,
        VM::LT,      # compare with 2
        VM::NOT,
        VM::JIF, 5,  # if arg1 >= 2, jump down
        VM::PUSH, 1,
        VM::RETR,    # put argument back on stack
        VM::RETURN,  # else, return the passed in value
        # reduce with n - 1
        VM::PUSH, 1,
        VM::RETR,    # put argument back on stack
        VM::PUSH, 1,
        VM::SUB,     # arg1 - 1
        VM::PUSH, 1, # arg count
        VM::PUSH, 0,
        VM::CALL,    # call self
        # reduce with n - 2
        VM::PUSH, 1,
        VM::RETR,    # put argument back on stack
        VM::PUSH, 2,
        VM::SUB,     # arg1 - 2
        VM::PUSH, 1, # arg count
        VM::PUSH, 0,
        VM::CALL,    # call self
        # add the two reductions
        VM::ADD,
        VM::RETURN,
        VM::ENDF,

        # call fib with value 8
        VM::PUSH, 8,
        VM::PUSH, 1,
        VM::PUSH, 0,
        VM::CALL,
        VM::RETURN
      ])
    end

    it 'returns 21' do
      subject.execute
      expect(subject.ret_val).to eq(21)
    end
  end

end
