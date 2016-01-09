require_relative './spec_helper'

describe Compiler do
  subject { described_class.new(filename: 'macro_spec.rb') }

  context 'define-syntax and syntax-rules' do
    context 'given a template with two arguments and a nested template' do
      before do
        @result = subject.compile(<<-END)
          (define-syntax listify
            (syntax-rules ()
              ((listify first second) (list (list first) (list second)))))
          (listify 1 2)
        END
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
          'VM::POP',
          'VM::HALT'
        ])
      end
    end

    context 'given multiple templates, recursive expansion' do
      before do
        @result = subject.compile(<<-END)
          (define-syntax and
            (syntax-rules ()
              ((and) #t)
              ((and test) test)
              ((and test1 test2 ...)
                (if test1 (and test2 ...) #f))))
          (and 1 2 3)
        END
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
          'VM::POP',
          'VM::HALT'
        ])
      end
    end

    context 'given multiple templates, recursive expansion' do
      before do
        @result = subject.compile(<<-END)
          (import (only (scheme base) lambda))
          (define-syntax let
            (syntax-rules ()
              ((let ((name val) ...) body1 body2 ...)
                ((lambda (name ...) body1 body2 ...)
                val ...))))
          (let ((x 1) (y 2) (z 3)) (list x y z))
        END
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_NUM', '1',
          'VM::PUSH_NUM', '2',
          'VM::PUSH_NUM', '3',
          'VM::PUSH_NUM', 3,
          'VM::SET_ARGS',
          'VM::PUSH_FUNC',
          'VM::PUSH_ARG',
          'VM::SET_ARG', 'x',
          'VM::PUSH_ARG',
          'VM::SET_ARG', 'y',
          'VM::PUSH_ARG',
          'VM::SET_ARG', 'z',
          'VM::PUSH_VAR', 'x',
          'VM::PUSH_VAR', 'y',
          'VM::PUSH_VAR', 'z',
          'VM::PUSH_NUM', 3,
          'VM::PUSH_LIST',
          'VM::RETURN',
          'VM::ENDF',
          'VM::CALL',
          'VM::HALT'
        ])
      end
    end
  end
end
