require_relative './spec_helper'

describe Parser do
  describe '#parse' do
    before do
      @result = subject.parse(<<-END)
        ; comment
        'foo
        '(1 2)
        '()
        ,foo
        ,(foo bar) #; (baz) #;6
        #| this is a
           multi-line comment |#
        (print |space in identifier|)
        (if (< 1 2) #;(2 3)
            x ; another comment
            (foo (bar (baz "this is a string"))))
      END
    end

    it 'parses s-expressions' do
      expect(@result).to eq([
        ['quote', 'foo'],
        ['quote', ['1', '2']],
        ['list'],
        ['unquote', 'foo'],
        ['unquote', ['foo', 'bar']], nil, nil,
        ['print', 'space in identifier'],
        ['if', ['<', '1', '2'], nil,
               'x',
               ['foo', ['bar', ['baz', '"this is a string"']]]]
      ])
    end
  end
end
