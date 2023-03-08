##
## EPITECH PROJECT, 2023
## wolfram
## File description:
## makefile
##


NAME = wolfram

all:
	stack build
	cp $(shell stack path --local-install-root)/bin/$(NAME)-exe ./$(NAME)

clean:
	stack clean

fclean: clean
		rm -f $(NAME)

re:    fclean all

time:
	stack build --profile
	cp $(shell stack path --local-install-root)/bin/$(NAME)-exe ./$(NAME)
	stack exec --profile -- wolfram-exe +RTS -p -RTS --rule 90 --lines 4000

.PHONY: all clean fclean re