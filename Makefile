all: cleancompile

clean:
	rm -rf ebin

compile: folders
	erlc -W -I include -o ebin src/*.erl

folders:
	mkdir -p ebin

cleancompile: clean compile

