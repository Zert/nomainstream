.PHONY: deps docs

all: check-deps
	@./rebar compile

deps:
	@./rebar get-deps

check-deps:
	@./rebar check-deps

app.config:
	@cp app.config.orig app.config

clean:
	@./rebar clean

distclean: clean
	@./rebar delete-deps

docs:
	@./rebar skip_deps=true doc

test: test-eunit test-ct

test-eunit:
	@./rebar eunit skip_deps=true

test-ct:
	@./rebar ct skip_deps=true verbose=1

PLT_NAME=.nms_dialyzer.plt

$(PLT_NAME):
	@ERL_LIBS=deps dialyzer --build_plt --output_plt $(PLT_NAME) \
		--apps erts kernel stdlib sasl crypto lager jiffy ibrowse gproc || true

dialyze: $(PLT_NAME)
	@dialyzer apps/nms/ebin --plt $(PLT_NAME) --no_native \
		-Werror_handling -Wunderspecs

NODE_NAME=nms@localhost
COOKIE=nms

run: check-deps app.config
	ERL_LIBS=deps:apps erl -sname $(NODE_NAME) -setcookie $(COOKIE) -boot start_sasl -config app.config +P 2000000 -s nms

REM_NODE_NAME="nms_remsh_${shell echo $$$$}@localhost"

attach:
	erl -sname $(REM_NODE_NAME) -remsh $(NODE_NAME) -setcookie $(COOKIE)
