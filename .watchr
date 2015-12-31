def run_suite
  cmd = IO.popen('rspec --color --tty spec 2>&1')
  print cmd.getc until cmd.eof?
end

watch('.*') { run_suite }

Signal.trap('QUIT') { run_suite } # Ctrl-\
