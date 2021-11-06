EXE	=	_build/default/main.exe
NAME	=	pgoc

all:		$(NAME)

$(NAME):
		dune build @all
		cp $(EXE) $(NAME)

test:		$(NAME)
		./tests/launch.sh
		

export-%:
		cp test.go ../tests/exec/$*.go
		go run test.go > ../tests/exec/$*.out

clean:
		dune clean
		$(RM) $(RMFLAGS) $(NAME)


.PHONY: $(NAME) all clean test
