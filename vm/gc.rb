class VM
  class GC
    def initialize(vm)
      @vm = vm
    end

    def run(debug: 0)
      @vm.writable_heap.each_with_index do |value, location|
        next if value.nil?
        next if active?(location)
        puts "garbage collecting #{value.inspect} at #{location}" if debug >= 3
        fail "heap[#{@ip}] is not writable" unless @vm.writable?(location)
        @vm.heap[location] = nil
      end
    end

    private

    def active?(candidate)
      return true if singleton?(candidate)
      (closure_locations + call_stack_locations + op_stack_locations).uniq.compact.each do |location|
        return true if location == candidate
        value = @vm.heap[location]
        return true if value.is_a?(VM::Pair) && active_in_pair?(candidate, value)
      end
      false
    end

    # def lib_locations
    #   @vm.libs.values.flat_map { |l| l[:locals].keys }
    # end

    def closure_locations
      @vm.closures.values.flat_map { |c| c[:locals].values }.uniq
    end

    def call_stack_locations
      @vm.call_stack.flat_map { |f| f[:named_args].values }.uniq
    end

    def op_stack_locations
      @vm.stack
    end

    def singleton?(candidate)
      value = @vm.heap[candidate]
      value.class.respond_to?(:instance)
    end

    def active_in_pair?(candidate, pair)
      return true if pair.address == candidate
      return true if pair.next_node == candidate
      node = pair.address && @vm.heap[pair.address]
      next_node = @vm.heap[pair.next_node]
      return true if node.is_a?(VM::Pair) && active_in_pair?(candidate, node)
      return true if next_node.is_a?(VM::Pair) && active_in_pair?(candidate, next_node)
      false
    end
  end
end
