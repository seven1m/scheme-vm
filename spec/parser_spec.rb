require_relative './spec_helper'

describe Parser do
  describe '#parse' do
    def stringify(sexp)
      if sexp.is_a?(Array)
        sexp.map { |s| stringify(s) }
      elsif sexp.is_a?(Parslet::Slice)
        sexp.to_s
      else
        sexp
      end
    end

    before do
      @result = subject.parse(<<-END)
        ; comment
        'foo
        '(1 2)
        '()
        ,foo
        ,(foo bar)
        #| this is a
           multi-line comment |#
        (print |space in identifier|)
        (if (< 1 2) #;(2 3)
            x ; another comment
            (foo (bar (baz "this is a string"))))
      END
    end

    it 'parses s-expressions' do
      expect(stringify(@result)).to eq([
        ['quote', 'foo'],
        ['quote', ['1', '2']],
        ['list'],
        ['unquote', 'foo'],
        ['unquote', ['foo', 'bar']],
        ['print', 'space in identifier'],
        ['if', ['<', '1', '2'], nil,
               'x',
               ['foo', ['bar', ['baz', '"this is a string"']]]]
      ])
    end
  end
end
