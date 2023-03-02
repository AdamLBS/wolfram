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

.PHONY: all clean fclean re