require 'rake/clean'

def percent_to_color(per)
	if ENV['COLORTERM'].to_s.downcase == 'yes' or ENV['TERM'] =~ /-color$/
		if per >= 90.0
			colorstart = "\e[1;32m"
		elsif per >= 75.0
			colorstart = "\e[0;32m"
		elsif per >= 50.0
			colorstart = "\e[0;33m"
		elsif per >= 25.0
			colorstart = "\e[0;31m"
		else
			colorstart = "\e[1;31m"
		end
		return [colorstart, "\e[0m"]
	else
		return ["", ""]
	end
end


INCLUDE = "include"

vertest = `erl -noshell -eval "io:format(\\"~n~s~n\\", [erlang:system_info(otp_release)])." -s erlang halt`.chomp.split("\n")[-1]
if vertest =~ /(R\d\d[AB])/
	OTPVERSION = $1
else
	STDERR.puts "unable to determine OTP version! (I got #{vertest})"
	exit -1
end
ERLC_FLAGS = "-I#{INCLUDE} -D #{OTPVERSION} +warn_unused_vars +warn_unused_import +warn_exported_vars +warn_untyped_record"

SRC = FileList['src/*.erl']
HEADERS = FileList['include/*.hrl']
OBJ = SRC.pathmap("%{src,ebin}X.beam")
CONTRIB = FileList['contrib/*']
DEBUGOBJ = SRC.pathmap("%{src,debug_ebin}X.beam")
COVERAGE = SRC.pathmap("%{src,coverage}X.txt")
RELEASE = FileList['src/*.rel.src'].pathmap("%{src,ebin}X")

# check to see if gmake is available, if not fall back on the system make
if res = `which gmake` and $?.exitstatus.zero? and not res =~ /no gmake in/
	MAKE = File.basename(res.chomp)
else
	MAKE = 'make'
end

@maxwidth = SRC.map{|x| File.basename(x, 'erl').length}.max

CLEAN.include("ebin/*.beam")
CLEAN.include("ebin/*.app")
CLEAN.include("ebin/*.script")
CLEAN.include("ebin/*.boot")
CLEAN.include("ebin/*.rel")
CLEAN.include("debug_ebin/*.beam")
CLEAN.include("debug_ebin/*.app")
CLEAN.include("debug_ebin/*.script")
CLEAN.include("debug_ebin/*.boot")
CLEAN.include("coverage/*.txt")
CLEAN.include("coverage/*.txt.failed")
CLEAN.include("coverage/*.html")
CLEAN.include("doc/*.html")

verbose(true) unless ENV['quiet']

directory 'ebin'
directory 'debug_ebin'
directory 'coverage'
#directory 'doc'

rule ".beam" => ["%{ebin,src}X.erl"] + HEADERS do |t|
	sh "erlc -pa ebin -W #{ERLC_FLAGS} +warn_missing_spec -o ebin #{t.source} "
end

rule ".beam" => ["%{debug_ebin,src}X.erl"] + HEADERS do |t|
	sh "erlc +debug_info -D EUNIT -pa debug_ebin -W #{ERLC_FLAGS} -o debug_ebin #{t.source} "
end

rule ".rel" => ["%{ebin,src}X.rel.src"] do |t|
	contents = File.read(t.source)
	#p contents
	while contents =~ /^[\s\t]*([-a-zA-Z0-9_]+),[\s\t]*$/
		app = $1
		if app == "erts"
			version = `erl -noshell -eval "io:format(\\"~n~s~n\\", [erlang:system_info(version)])." -s erlang halt`.chomp.split("\n")[-1]
		else
			version = `erl -noshell -eval "application:load(#{app}), io:format(\\"~n~s~n\\", [proplists:get_value(#{app}, lists:map(fun({Name, Desc, Vsn}) -> {Name, Vsn} end, application:loaded_applications()))])." -s erlang halt`.chomp.split("\n")[-1]
		end
		if md = /(\d+\.\d+(\.\d+(\.\d+|)|))/.match(version)
			contents.sub!(app, "{#{app}, \"#{md[1]}\"}")
		else
			STDERR.puts "Cannot find application #{app} mentioned in release file!"
			exit 1
		end
	end
	File.open(t.name, 'w') do |f|
		f.puts contents
	end
end

rule ".txt" => ["%{coverage,debug_ebin}X.beam"] do |t|
	mod = File.basename(t.source, '.beam')
	if ENV['modules'] and not ENV['modules'].split(',').include? mod
		puts "skipping tests for #{mod}"
		next
	end

	print "  #{mod.ljust(@maxwidth - 1)} : "
	STDOUT.flush
	test_output = `erl -noshell -pa debug_ebin -pa contrib/mochiweb/ebin -sname testpx -eval "cover:start(), cover:compile_beam(\\"#{t.source}\\"), try eunit:test(#{mod}, [verbose]) of _Any -> cover:analyse_to_file(#{mod}, \\"coverage/#{mod}.txt\\"), cover:analyse_to_file(#{mod}, \\"coverage/#{mod}.html\\", [html]) catch _:_ -> io:format(\\"This module does not provide a test() function~n\\"), ok end." -s erlang halt`
	if /(All \d+ tests (successful|passed)|There were no tests to run|This module does not provide a test\(\) function|Test (successful|passed))/ =~ test_output
		File.delete(t.to_s+'.failed') if File.exists?(t.to_s+'.failed')
		if ENV['verbose']
			puts test_output.split("\n")[1..-1].map{|x| x.include?('1>') ? x.gsub(/\([a-zA-Z0-9\-@]+\)1>/, '') : x}.join("\n")
		else
			out = $1
			if /(All \d+ tests (successful|passed)|Test (successful|passed))/ =~ test_output
				colorstart, colorend = percent_to_color(80)
			#elsif /This module does not provide a test\(\) function/ =~ test_output
				#colorstart, colorend = percent_to_color(50)
			else
				colorstart, colorend = percent_to_color(50)
				#colorstart, colorend = ["", ""]
			end
			puts "#{colorstart}#{out}#{colorend}"
			#puts "  #{mod.ljust(@maxwidth - 1)} : #{out}"
		end
	else
		puts "\e[1;35mFAILED\e[0m"
		puts test_output.split("\n")[1..-1].map{|x| x.include?('1>') ? x.gsub(/\([a-zA-Z0-9\-@]+\)1>/, '') : x}.join("\n")
		puts "  #{mod.ljust(@maxwidth - 1)} : \e[1;35mFAILED\e[0m"
		File.delete(t.to_s) if File.exists?(t.to_s)
		File.new(t.to_s+'.failed', 'w').close
	end
end

task :compile => ['ebin', :contrib, :keygen] + HEADERS + OBJ + RELEASE do
	Dir["ebin/*.rel"].each do |rel|
		rel = File.basename(rel, '.rel')
		sh "erl -noshell -eval \"systools:make_script(\\\"ebin/#{rel}\\\", [{outdir, \\\"ebin\\\"}]).\" -s erlang halt -pa ebin"
	end
end

task :contrib do
	if File.directory? ".git"
		sh "git submodule init"
		sh "git submodule update"
	end
	CONTRIB.each do |cont|
		if File.exists? File.join(cont, 'Makefile')
			sh "#{MAKE} -C #{cont}"
		elsif File.exists? File.join(cont, 'Rakefile')
			pwd = Dir.pwd
			Dir.chdir(cont)
			sh "#{$0}"
			Dir.chdir(pwd)
		else
			STDERR.puts "Don't know how to build for #{cont}"
		end
		unless Dir["#{cont}/ebin/*.beam"].length.zero?
			sh "cp #{cont}/ebin/*.beam ebin"
		end
	end
	unless Dir["src/*.app"].length.zero?
		sh "cp src/*.app ebin/"
	end
end

task :default => :compile

task :release => :compile

desc "Alias for test:all"
task :test => "test:all"

desc "Generate Documentation"
task :doc do
	sh('mkdir doc') unless File.directory? 'doc'
	sh("rm -rf doc/*.html && cd doc && erl -noshell -run edoc files ../#{SRC.join(" ../")} -run init stop")
end

desc "Generate a RSA key to encrypt HTTP passwords"
task :keygen do
	unless File.exists?("key")
		puts "Generating RSA key..."
		`ssh-keygen -t rsa -f key -N ""`
		if $?.exitstatus != 0
			STDERR.puts "RSA key generation FAILED!"
			exit -1
		end
	end
end

namespace :test do
	desc "Compile .beam files with -DEUNIT and +debug_info => debug_ebin"
	task :compile => ['debug_ebin', :contrib] + HEADERS + DEBUGOBJ

	task :contrib do
		if File.directory? ".git"
			sh "git submodule init"
			sh "git submodule update"
		end
		CONTRIB.each do |cont|
			if File.exists? File.join(cont, 'Makefile')
				sh "#{MAKE} -C #{cont}"
			elsif File.exists? File.join(cont, 'Rakefile')
				pwd = Dir.pwd
				Dir.chdir(cont)
				sh "#{$0}"
				Dir.chdir(pwd)
			else
				STDERR.puts "Don't know how to build for #{cont}"
			end
			unless Dir["#{cont}/ebin/*.beam"].length.zero?
				sh "cp #{cont}/ebin/*.beam debug_ebin"
			end
		end
		unless Dir["src/*.app"].length.zero?
			sh "cp src/*.app debug_ebin/"
		end
	end

	desc "run eunit tests and output coverage reports"
	task :all => [:compile, :eunit, :report_coverage]

	desc "run only the eunit tests"
	task :eunit =>  [:compile, 'coverage'] + COVERAGE

	desc "rerun any outstanding tests and report the coverage"
	task :report_coverage =>  [:eunit, :report_current_coverage]

	desc "report the percentage code coverage of the last test run"
	task :report_current_coverage do
		global_total = 0
		files = (Dir['coverage/*.txt'] + Dir['coverage/*.txt.failed']).sort
		maxwidth = files.map{|x| x = File.basename(x, '.failed'); File.basename(x, ".txt").length}.max
		puts "Code coverage:"
		files.each do |file|
			if file =~ /\.txt\.failed$/
				if ENV['COLORTERM'].to_s.downcase == 'yes' or ENV['TERM'] =~ /-color$/
					puts "  #{File.basename(file, ".txt.failed").ljust(maxwidth)} : \e[1;35mFAILED\e[0m"
				else
					puts "  #{File.basename(file, ".txt.failed").ljust(maxwidth)} : FAILED"
				end
			else
				total = 0
				tally = 0
				File.read(file).each do |line|
					if line =~ /^\s+[1-9][0-9]*\.\./
						total += 1
						tally += 1
					elsif line =~ /^\s+0\.\./ and not line =~ /^-module/
						total += 1
					end
				end
				per = tally/total.to_f * 100
				colorstart, colorend = percent_to_color(per)
				puts "  #{File.basename(file, ".txt").ljust(maxwidth)} : #{colorstart}#{sprintf("%.2f%%", (tally/(total.to_f)) * 100)}#{colorend}"
				global_total += (tally/(total.to_f)) * 100
			end
		end
		colorstart, colorend = percent_to_color(global_total/files.length)
		puts "Overall coverage: #{colorstart}#{sprintf("%.2f%%", global_total/files.length)}#{colorend}"
	end

	task :report_missing_specs do
		unspecced = []
		ignored = %w{handle_info handle_cast handle_call code_change terminate init}
		puts "Functions missing specs:"
		SRC.each do |src|
			contents = File.read(src)
			contents.each do |line|
				if md = /^([a-z_]+)\(.*?\) ->/.match(line) and not ignored.include?(md[1]) and not md[1][-5..-1] == '_test' and not md[1][-6..-1] == '_test_'
					unless /^-spec\(#{md[1]}\//.match(contents)
						unspecced << File.basename(src, '.erl') + ':'+ md[1]
					end
				end
			end
		end
		puts "  "+unspecced.uniq.join("\n  ")
	end

	desc "run the dialyzer"
	task :dialyzer do
		print "running dialyzer..."
		`dialyzer --check_plt`
		if $?.exitstatus != 0
			puts 'no PLT'
			puts "The dialyzer can't find the initial PLT, you can try building one using `rake test:build_plt`. This can take quite some time."
			exit(1)
		end
		STDOUT.flush
		# Add -DEUNIT=1 here to make dialyzer evaluate the code in the test cases. This generates some spurious warnings so 
		# it's not set normally but it can be very helpful occasionally.
		dialyzer_flags = ""
		dialyzer_flags += " -DEUNIT=1" if ENV['dialyzer_debug']
		dialyzer_flags += " -Wunderspecs" if ENV['dialyzer_underspecced']
		contribfiles = Dir['contrib/*/src/*.erl'].join(' ')
		dialyzer_output = `dialyzer -D#{OTPVERSION}=1 #{dialyzer_flags} --src -I include -c #{SRC.join(' ')} #{contribfiles}`
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
		sh "dialyzer --build_plt -r #{foo}/kernel*/ebin #{foo}/stdlib*/ebin #{foo}/mnesia*/ebin #{foo}/crypto*/ebin #{foo}/eunit*/ebin"
	end
end
