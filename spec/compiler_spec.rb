require_relative './spec_helper'

describe Compiler do
  describe '#compile' do
    context 'def' do
      before do
        @result = subject.compile([
          [:def, :x, 1]
        ])
      end

      it 'compiles into vm instructions' do
        expect(@result).to eq([
          VM::PUSH_NUM, 1,
          VM::SET_LOCAL, 0
        ])
      end
    end
  end
end
