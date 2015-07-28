require_relative '../spec_helper'
require 'stringio'

describe 'Library' do
  let(:stdout) { StringIO.new }

  let(:subject) { Program.new(code, stdout: stdout) }

  before { subject.run }

  describe 'empty?' do
    context 'given an empty list' do
      let(:code) do
        <<-END
          (define empty-list
            (list))
          (print (empty? empty-list))
        END
      end

      it 'returns #t' do
        stdout.rewind
        expect(stdout.read).to eq('#t')
      end
    end

    context 'given a non-empty list' do
      let(:code) do
        <<-END
          (define non-empty-list
            (list 1 2 3))
          (print (empty? non-empty-list))
        END
      end

      it 'returns #f' do
        stdout.rewind
        expect(stdout.read).to eq('#f')
      end
    end
  end

  describe 'length' do
    context 'given an empty list' do
      let(:code) do
        <<-END
          (define empty-list
            (list))
          (print (length empty-list))
        END
      end

      it 'returns 0' do
        stdout.rewind
        expect(stdout.read).to eq('0')
      end
    end

    context 'given a non-empty list' do
      let(:code) do
        <<-END
          (define non-empty-list
            (list 1 2 3))
          (print (length non-empty-list))
        END
      end

      it 'returns the list length' do
        stdout.rewind
        expect(stdout.read).to eq('3')
      end
    end
  end
end
