require 'rake/clean'

INCLUDE = "include"

ERLC_FLAGS = "-I#{INCLUDE} +warn_unused_vars +warn_unused_import"

SRC = FileList['src/*.erl']
OBJ = SRC.pathmap("%{src,ebin}X.beam")

CLEAN.include("ebin/*.beam")

directory 'ebin'

rule ".beam" => ["%{ebin,src}X.erl"] do |t|
	sh "erlc +debug_info -D EUNIT -pa ebin -W #{ERLC_FLAGS} -o ebin #{t.source} "
end

task :compile => ['ebin'] + OBJ

task :default => :compile

task :test =>  [:compile] do
	puts "Modules under test:"
	OBJ.each do |obj|
		obj[%r{.*/(.*).beam}]
		mod = $1
		next if mod == 'test_coverage'
		test_output = `erl -pa ebin -sname cpx -s test_coverage start #{mod} -run init stop`

		if /\*failed\*/ =~ test_output
			puts test_output.split("\n")[1..-1].map{|x| x.include?('1>') ? x.gsub(/\([a-zA-Z0-9\-@]+\)1>/, '') : x}.join("\n")
		else
			test_output[/1>\s*(.*)\n/]
			puts "#{mod}: #{$1}"
		end
	end
end
