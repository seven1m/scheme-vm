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
      "(#{raw.map(&:raw).join(' ')})"
    end

    def each
      current = self
      yield @heap[current.address]
      while (next_address = current.next_node)
        current = @heap[next_address]
        break if current == EmptyList.instance
        yield @heap[current.address]
      end
    end

    def size
      @size ||= to_a.size
    end

    def inspect
      "#<VM::Pair @address=#{@address}, @next_node=#{@next_node}, @value=#{@heap[@address].inspect}>"
    end
  end
end
