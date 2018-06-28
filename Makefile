all: 
	jbuilder build Hangman.exe

test:
	jbuilder test

clean:
	jbuilder clean

doc:
	jbuilder build @doc