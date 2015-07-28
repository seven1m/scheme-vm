require_relative '../spec_helper'
require 'stringio'

describe 'Library' do
  let(:stdout) { StringIO.new }

  let(:subject) { Program.new(code, stdout: stdout) }

  before { subject.run }

  describe 'boolean?' do
    context 'given #t' do
      let(:code) do
        <<-END
          (boolean? #t)
        END
      end

      it 'returns #t' do
        expect(subject.vm.pop_val).to eq(VM::BoolTrue.instance)
      end
    end

    context 'given #f' do
      let(:code) do
        <<-END
          (boolean? #f)
        END
      end

      it 'returns #t' do
        expect(subject.vm.pop_val).to eq(VM::BoolTrue.instance)
      end
    end

    context 'given a list' do
      let(:code) do
        <<-END
          (boolean? (list))
        END
      end

      it 'returns #f' do
        expect(subject.vm.pop_val).to eq(VM::BoolFalse.instance)
      end
    end
  end
end
