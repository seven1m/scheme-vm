class VM
  class Pair
    include Enumerable

    attr_reader :address
    attr_accessor :next_node

    def initialize(address, next_node, heap:)
      @address = address
      @next_node = next_node
      @heap = heap
    end

    def raw
      to_a
    end

    def to_s
      items = to_a.flat_map do |item|
        if item.is_a?(Fixnum) # address
          @heap[item].to_s
        else
          ['.', item.to_s]
        end
      end
      "(#{items.join(' ')})"
    end

    def each
      pair = self
      while pair.is_a?(Pair) && (next_address = pair.next_node)
        yield pair.address
        pair = @heap[next_address]
      end
      yield pair unless pair.is_a?(Pair) || pair.is_a?(EmptyList)
    end

    def car
      @heap[address]
    end

    def cdr
      @heap[next_node]
    end

    def size
      @size ||= to_a.size
    end

    def empty?
      false
    end

    def to_ruby
      to_a.tap do |ary|
        ary.each_with_index do |address, index|
          part = @heap[address]
          ary[index] = part.to_ruby
        end
      end
    end

    def inspect
      "#<VM::Pair size=#{size}, car=#{car.inspect}>"
    end
  end
end
