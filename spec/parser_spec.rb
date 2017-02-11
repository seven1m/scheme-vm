require_relative './spec_helper'

fdescribe Parser do
  describe '#parse' do
    subject do
      described_class.new(<<-END)
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
      expect(subject.parse).to eq([
        ['quote', 'foo'],
        ['quote', ['1', '2']],
        ['quote', []],
        ['unquote', 'foo'],
        ['unquote', ['foo', 'bar']],
        ['print', 'space in identifier'],
        ['if', ['<', '1', '2'],
               'x',
               ['foo', ['bar', ['baz', '"this is a string"']]]]
      ])
    end
  end
end
