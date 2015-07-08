class VM
  class ListNode
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

    def to_a
      current = self
      [@heap[current.address]].tap do |all|
        while (next_address = current.next_node)
          current = @heap[next_address]
          all << @heap[current.address]
        end
      end
    end
  end
end
