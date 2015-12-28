require_relative './spec_helper'

describe Compiler do
  def d(instructions)
    subject.pretty_format(instructions)
  end

  context 'define-syntax and syntax-rules' do
    context 'given an empty template' do
      before do
        @result = subject.compile([
          ['define-syntax', 'and',
            ['syntax-rules', [],
              [['and'], '#t']]],
          ['and']
        ])
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_TRUE',
          'VM::HALT'
        ])
      end
    end

    context 'given a template with one argument' do
      before do
        @result = subject.compile([
          ['define-syntax', 'and',
            ['syntax-rules', [],
              [['and', 'test'], 'test']]],
          ['and', '10'],
          ['and', ['write', '11']]
        ])
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_NUM', '10',
          'VM::POP',
          'VM::PUSH_NUM', '11',
          'VM::INT', VM::INT_WRITE,
          'VM::HALT'
        ])
      end
    end

    context 'given a template with two arguments and a nested template' do
      before do
        @result = subject.compile([
          ['define-syntax', 'listify',
            ['syntax-rules', [],
              [['listify', 'first', 'second'], ['list', ['list', 'first'], ['list', 'second']]]]],
          ['listify', '1', '2']
        ])
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_NUM', '1',
          'VM::PUSH_NUM', 1,
          'VM::PUSH_LIST',
          'VM::PUSH_NUM', '2',
          'VM::PUSH_NUM', 1,
          'VM::PUSH_LIST',
          'VM::PUSH_NUM', 2,
          'VM::PUSH_LIST',
          'VM::HALT'
        ])
      end
    end

    context 'given multiple templates, recursive expansion' do
      before do
        @result = subject.compile([
          ['define-syntax', 'and',
            ['syntax-rules', [],
              [['and'], '#t'],
              [['and', 'test'], 'test'],
              [['and', 'test1', 'test2', '...'],
                ['if', 'test1', ['and', 'test2', '...'], '#f']]]],
          ['and', '1', '2', '3']
        ])
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_NUM', '1',
          'VM::JUMP_IF_FALSE', 12,
          'VM::PUSH_NUM', '2',
          'VM::JUMP_IF_FALSE', 5,
          'VM::PUSH_NUM', '3',
          'VM::JUMP', 2,
          'VM::PUSH_FALSE',
          'VM::JUMP', 2,
          'VM::PUSH_FALSE',
          'VM::HALT'
        ])
      end
    end
  end
end
