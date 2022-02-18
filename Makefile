##
## EPITECH PROJECT, 2020
## nothing
## File description:
## nothing
##

BINARY_PATH 	:=	$(shell stack path --local-install-root)
NAME 			= 	funEvalExpr

all:
	stack build
	cp $(BINARY_PATH)/bin/$(NAME)-exe ./$(NAME)
	rm -f stack.yaml.lock
	rm -f funEvalExpr.cabal
clean:
	stack clean

fclean: clean
	rm -f $(NAME)
	rm -f stack.yaml.lock
	rm -f funEvalExpr.cabal

re: fclean all

.PHONY: all clean fclean re
## n'affiche pas les commandes
.SILENT:

## ne compare pas les fichiers

##export xLDFLAGS
