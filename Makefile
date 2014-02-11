NAME=	syndic
LEX=	ocamllex -q
YACC=	menhir --infer

ML =	syndic.ml

MLI =	syndic.mli

CMI = $(MLI:.mli=.cmi)
CMO = $(ML:.ml=.cmo)
CMX = $(ML:.ml=.cmx)


OCAMLDEP = ocamlfind ocamldep
CAMLFLAGS = -w Aelz -linkpkg -package xmlm,netstring
OCAMLC = ocamlfind ocamlc $(CAMLFLAGS)
OCAMLOPT = ocamlfind ocamlopt $(CAMLFLAGS)
OCAMLDOC = ocamlfind ocamldoc -html -d $(ROOT)/doc

CMA=	
CMXA=	

all:		.depend $(CMI) $(NAME)

byte:		.depend $(CMI) $(NAME).byte


$(NAME):	$(CMX)
		@echo "[--] [$(CMX)] > [$@]"
		@$(OCAMLOPT) $(CMXA) -o $@ $(CMX)
		@echo "[OK] $(NAME) linked"

$(NAME).byte:	$(CMO)
		@echo "[--] [$<] > [$@]"
		@$(OCAMLC) $(CMA) -o $@ $(CMO)
		@echo "[OK] $(NAME).byte linked"

%.cmx:		%.ml
		@echo "[--] [$<] > [$@]"
		$(OCAMLOPT) $(CMXA) -c $<
		@echo "[OK] [$<] builded"

%.cmo:		%.ml
		@echo "[--] [$<] > [$@]"
		@$(OCAMLC) $(CMA) -c $<
		@echo "[OK] [$<] builded"

%.cmi:		%.mli
		@echo "[--] [$<] > [$@]"
		@$(OCAMLC) -c $<
		@echo "[OK] [$<] builded"

%.ml:		%.mly
		@echo "[--] [$<] > [$@]"
		@$(YACC) --infer $<
		@echo "[OK] [$<] builded"

%.ml:		%.mll
		@echo "[--] [$<] > [$@]"
		@$(YACC) --infer $<
		@$(LEX) $<
		@echo "[OK] [$<] builded"

%.mli:		%.mly
		@echo "[--] [$<] > [$@]"
		@$(YACC) --infer $<
		@echo "[OK] [$<] builded"

doc:		$(CMI)
		@echo "[--] Documentation"
		@$(OCAMLDOC) $(MLI)
		@echo "[OK] Documentation"


re:		fclean all


clean:
		@/bin/rm -f *.cm* *.o .depend *~
		@echo "[OK] clean"


fclean: 	clean
		@/bin/rm -f $(NAME) $(NAME).byte
		@echo "[OK] fclean"


.depend:
		@/bin/rm -f .depend
		@$(OCAMLDEP) $(MLI) $(ML) > .depend
		@echo "[OK] dependencies"
