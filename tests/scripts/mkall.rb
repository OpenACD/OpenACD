#! /usr/bin/env ruby

require 'rubygems'
require 'json'

now = Time.new

class CallQueue
	def initialize(name)
		@name = name
		@medias = []
	end

	def make_media(start = Time.new.to_i - 60 * 10, ended = Time.new.to_i - 60 * 5, didAbandon = false)
		@medias.push Media.new(
			@name + "_" + @medias.length.to_s, 
			start,
			ended,
			didAbandon)
	end

	def to_json(*a)
		{"name" => @name,
		"medias" => @medias}.to_json(*a)
	end
end

class Media
	def initialize(id, start, ended, abandoned)
		@id = id
		@start = start
		@ended = ended
		@abandoned = abandoned
	end

	def to_json(*a)
		{"id" => @id,
		"time" => @start.to_i,
		"queued" => @start.to_i,
		"brand" => "undefined",
		"node" => "textpx@nonode",
		"type" => "voice",
		"priority" => 10,
		"direction" => "inbound",
		"didAbandon" => @abandoned,
		"ended" => @ended.to_i}.to_json
	end
end

default_q = CallQueue.new("default_queue")
doobie_q = CallQueue.new("doobie")

default_medias = [
	[now - (60 * 60 * 2), now - (60 * 60 * 1.5), false],
	[now - (60 * 30), now - (60 * 15), false]
]

doobie_medias = [
	[now - (60 * 60 * 2), now - (60 * 60 * 1.5), false],
	[now - (60 * 30), now - (60 * 15), true]
]

default_medias.each{|arr|
	default_q.make_media(arr[0], arr[1], arr[2])
}

doobie_medias.each{|arr|
	doobie_q.make_media(arr[0], arr[1], arr[2])
}

outhash = {
	"queueGroups" => [
		{"name" => "Default",
		"queues" => [
			default_q,
			doobie_q
		]}
	]}

f = File.new("auto.json", "w+")
f.write outhash.to_json
