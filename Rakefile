require 'rake/clean'

INCLUDE = "include"

ERLC_FLAGS = "-I#{INCLUDE} +warn_unused_vars +warn_unused_import"

SRC = FileList['src/*.erl']
OBJ = SRC.pathmap("%{src,ebin}X.beam").reject{|x| x.include? 'test_coverage'}
DEBUGOBJ = SRC.pathmap("%{src,debug_ebin}X.beam")

CLEAN.include("ebin/*.beam")
CLEAN.include("debug_ebin/*.beam")

verbose(true)

directory 'ebin'
directory 'debug_ebin'

rule ".beam" => ["%{ebin,src}X.erl"] do |t|
	sh "erlc -pa ebin -W #{ERLC_FLAGS} -o ebin #{t.source} "
end

rule ".beam" => ["%{debug_ebin,src}X.erl"] do |t|
	sh "erlc +debug_info -D EUNIT -pa debug_ebin -W #{ERLC_FLAGS} -o debug_ebin #{t.source} "
end

task :compile => ['ebin'] + OBJ

desc "Compile .beam files with -DEUNIT and +debug_info => debug_ebin"
task :compile_debug => ['debug_ebin'] + DEBUGOBJ

task :default => :compile

task :release => :compile

namespace :test do
	desc "run eunit tests, the dialyzer and output coverage reports"
	task :all => [:compile_debug, :eunit, :dialyzer]

	desc "run only the eunit tests"
	task :eunit =>  [:compile_debug] do
		puts "Modules under test:"
		DEBUGOBJ.each do |obj|
			obj[%r{.*/(.*).beam}]
			mod = $1
			next if mod == 'test_coverage'
			test_output = `erl -pa debug_ebin -sname testpx -s test_coverage start #{mod} -run init stop`

			if /\*failed\*/ =~ test_output
				puts test_output.split("\n")[1..-1].map{|x| x.include?('1>') ? x.gsub(/\([a-zA-Z0-9\-@]+\)1>/, '') : x}.join("\n")
			else
				test_output[/1>\s*(.*)\n/]
				puts "#{mod}: #{$1}"
			end
		end
	end

	desc "run the dialyzer"
	task :dialyzer do
		print "running dialyzer..."
		`dialyzer --check_plt`
		if $?.exitstatus != 0
			puts 'no PLT'
			puts "The dialyzer can't find the initial PLT, you can try building one using `rake build_plt`. This can take quite some time."
			exit(1)
		end
		STDOUT.flush
		dialyzer_output = `dialyzer --src -I include -c #{SRC.reject{|x| x =~ /test_coverage/}.join(' ')}`
		#puts dialyzer_output
		if $?.exitstatus.zero?
			puts 'ok'
		else
			puts 'not ok'
			puts dialyzer_output
		end
	end

	desc "try to create the dialyzer's initial PLT"
	task :build_plt do
		out = `which erlc`
		foo = out.split('/')[0..-3].join('/')+'/lib/erlang/lib'
		sh "dialyzer --build_plt -r #{foo}/kernel*/ebin #{foo}/stdlib*/ebin #{foo}/mnesia*/ebin"
	end
end
