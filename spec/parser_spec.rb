require_relative './spec_helper'

describe Parser do
  describe '#parse' do
    before do
      @result = subject.parse(<<-END)
        ; comment
        'foo
        '(1 2)
        ,foo
        ,(foo bar)
        (if (< 1 2)
            x ; another comment
            (foo (bar (baz "this is a string"))))
      END
    end

    it 'parses s-expressions' do
      expect(@result).to eq([
        ['quote', 'foo'],
        ['quote', '1', '2'],
        ['unquote', 'foo'],
        ['unquote', 'foo', 'bar'],
        ['if', ['<', '1', '2'],
               'x',
               ['foo', ['bar', ['baz', '"this is a string"']]]]
      ])
    end
  end
end
