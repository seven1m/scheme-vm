require_relative '../spec_helper'
require 'stringio'

describe 'Library' do
  let(:stdout) { StringIO.new }

  let(:subject) { Program.new(code, stdout: stdout) }

  before { subject.run }

  describe 'pair?' do
    context 'given an empty list' do
      let(:code) do
        <<-END
          (pair? (list))
        END
      end

      it 'returns #f' do
        expect(subject.vm.pop_val).to eq(VM::BoolFalse.instance)
      end
    end

    context 'given a list with one value' do
      let(:code) do
        <<-END
          (pair? (list 1))
        END
      end

      it 'returns #t' do
        expect(subject.vm.pop_val).to eq(VM::BoolTrue.instance)
      end
    end
  end
end
