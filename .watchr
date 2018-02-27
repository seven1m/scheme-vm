def run_tests(files, scm_file = nil)
  if (ft = focused_tests).any?
    puts "Running focused test(s)..."
    files = ft.join(' ')
  end
  if scm_file
    puts scm_file
    env = { 'SCM_FILE' => scm_file }
  else
    puts files
    env = {}
  end
  cmd = IO.popen(env, "rspec --color --tty #{files} 2>&1")
  print cmd.getc until cmd.eof?
end

def compile
  puts
  puts '========================================='
  puts
  cmd = IO.popen('cargo build')
  print cmd.getc until cmd.eof?
end

def focused_tests
  Dir['spec/**/*_spec.rb'].to_a.select do |path|
    !path.end_with?('lib_spec.rb') && \
      File.read(path).match(/focus:|fdescribe|fcontext|fit ['"]/) rescue nil
  end
end

watch('^spec/.*_spec\.rb')  { |m| run_tests(m.to_s) }
watch('^spec/.*-spec\.scm') { |m| run_tests('spec/lib_spec.rb', m.to_s) }
watch('^lib')               { |m| run_tests('spec/lib_spec.rb') }
watch('^vm.*')              { |m| run_tests('spec/vm_spec.rb spec/vm/*') }
watch('^compiler.*')        { |m| run_tests('spec/compiler_spec.rb spec/compiler/*') }
watch('^program.*')         { |m| run_tests('spec/program_spec.rb') }
watch('^parser.*')          { |m| run_tests('spec/parser_spec.rb') }
watch('^src/.*')            { compile }

Signal.trap('QUIT') { run_tests('spec') } # Ctrl-\
