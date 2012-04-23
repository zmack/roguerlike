BIN=ebin
SRC=src
ERL=erlc
FLAGS=-W -I include -o $(BIN)

all: cleancompile

clean:
	rm -rf $(BIN)

compile: folders
	$(ERL) $(FLAGS) $(SRC)/*.erl

folders:
	mkdir -p $(BIN)

cleancompile: clean compile

