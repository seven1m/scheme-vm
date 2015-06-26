watch('.*') do
  cmd = IO.popen('rspec spec 2>&1')
  print cmd.getc until cmd.eof?
end
