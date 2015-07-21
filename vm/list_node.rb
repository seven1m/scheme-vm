class VM
  class ListNode
    include Enumerable

    attr_reader :address
    attr_accessor :next_node

    def initialize(address, heap:, next_node: nil)
      @address = address
      @next_node = next_node
      @heap = heap
    end

    def raw
      to_a
    end

    def to_s
      raw.map(&:raw).to_s
    end

    def each
      current = self
      yield @heap[current.address]
      while (next_address = current.next_node)
        current = @heap[next_address]
        yield @heap[current.address]
      end
    end

    def size
      @size ||= to_a.size
    end

    def inspect
      "#<VM::ListNode @address=#{@address}, @next_node=#{@next_node}, @value=#{@heap[@address].inspect}>"
    end
  end
end
